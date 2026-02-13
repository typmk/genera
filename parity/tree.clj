#!/usr/bin/env clojure
;; =============================================================================
;; tree.clj — complete dependency tree with host contract resolved
;;
;; Merges depgraph (what Clojure source uses) with langmap (what JVM provides).
;; For each function, shows: deps → host refs → native methods needed.
;;
;; Input: two EDN files (produced by depgraph --edn and langmap --edn)
;; Output: merged tree (human-readable or --edn)
;;
;; Usage:
;;   par tree <clojure-src>                    # human-readable
;;   par tree <clojure-src> --edn              # machine-readable
;;   bb tree.clj <graph.edn> <host.edn>       # direct
;; =============================================================================

(ns tree
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [parity.color :refer [bold dim green yellow cyan red white
                                  heading sub-heading *color*]]))

;; =============================================================================
;; Host resolution
;; =============================================================================

(defn resolve-host-ref
  "Given a parsed host ref {:pkg :class :member}, look up in host contract.
   Returns the matching interface/concrete/bridge entry, or nil."
  [host-data {:keys [pkg class member]}]
  (let [iface (first (filter #(= (:name %) class) (:interfaces host-data)))
        abstract (first (filter #(= (:name %) class) (:abstracts host-data)))
        concrete (first (filter #(= (:name %) class) (:concretes host-data)))
        bridge-entry (get (:bridge host-data) class)]
    (cond
      iface    {:kind :interface :name class :member member
                :methods (count (:methods iface))}
      abstract {:kind :abstract :name class :member member
                :methods (count (:methods abstract))}
      ;; Bridge before concrete: RT/Numbers/Util are both, but bridge is more useful
      (and bridge-entry member) {:kind :bridge :name class :member member
                                  :overloads (get (:ops bridge-entry) member 0)}
      concrete {:kind :concrete :name class :member member
                :methods (count (:methods concrete))
                :interfaces (:interfaces concrete)}
      bridge-entry {:kind :bridge :name class :member member
                    :overloads 0}
      (= pkg "clojure.lang") {:kind :lang-other :name class :member member}
      (= (str pkg) ":interop") {:kind :interop :member member}
      :else {:kind :java :name (str pkg "." class) :member member})))

(defn enrich-node
  "Enrich a graph node with resolved host contract info."
  [host-data [qn info]]
  [qn (assoc info
             :host-resolved
             (vec (for [p (:host-parsed info)
                        :let [resolved (resolve-host-ref host-data p)]
                        :when resolved]
                    resolved)))])

;; =============================================================================
;; Print — human-readable
;; =============================================================================

(defn print-tree
  "Human-readable dependency tree with ANSI color."
  [graph host-data]
  (let [max-layer (apply max 0 (map :layer (vals graph)))
        by-layer (group-by :layer (vals graph))
        by-class (group-by :class (vals graph))
        n-host (count (get by-class :host))
        n-pure (- (count graph) n-host)
        all-host-resolved (mapcat :host-resolved (vals graph))

        ifaces    (distinct (filter #(= :interface (:kind %)) all-host-resolved))
        bridges   (distinct (filter #(= :bridge (:kind %)) all-host-resolved))
        concretes (distinct (filter #(= :concrete (:kind %)) all-host-resolved))
        interop   (distinct (filter #(= :interop (:kind %)) all-host-resolved))
        n-ifaces   (count (distinct (map :name ifaces)))
        n-bridge   (count (distinct (map (juxt :name :member) bridges)))
        n-concrete (count (distinct (map :name concretes)))
        n-interop  (count (distinct (map :member interop)))]

    ;; ── LAYERS ───────────────────────────────────────────────────────────
    (println (heading "LAYERS  (implement bottom-up)"))
    (println)
    (let [bands [[0 0 "Foundation"]
                 [1 5 "Core"]
                 [6 15 "Standard library"]
                 [16 40 "Derived"]
                 [41 (max 41 max-layer) "Leaf / top-level"]]]
      (doseq [[lo hi label] bands
              :when (<= lo max-layer)]
        (let [hi (min hi max-layer)
              nodes (mapcat #(get by-layer %) (range lo (inc hi)))
              host-count (count (filter #(seq (:host-refs %)) nodes))
              pure-count (- (count nodes) host-count)
              pct (if (zero? (count graph)) 0 (* 100.0 (/ (count nodes) (count graph))))]
          (println (format "  %-22s layers %2d-%-2d  %4d defs  %s %s  %s"
                           (bold label) lo hi (count nodes)
                           (green (format "%3d pure" pure-count))
                           (yellow (format "%3d host" host-count))
                           (dim (format "%4.0f%%" pct)))))))
    (println)

    ;; Per-layer detail for small layers
    (println (str "  " (dim "Per-layer detail (layers with <= 12 defs):")))
    (doseq [l (range (inc max-layer))]
      (let [nodes (sort-by :name (get by-layer l))]
        (when (and (seq nodes) (<= (count nodes) 12))
          (println (format "    %s %s" (dim (format "L%2d:" l))
                           (str/join ", " (map :name nodes)))))))
    (println)

    ;; ── HOST SURFACE ─────────────────────────────────────────────────────
    (println (heading "HOST SURFACE  (what genera must implement natively)"))
    (println)

    (println (sub-heading (format "  Interfaces (%d):" n-ifaces)))
    (doseq [[iname refs] (sort-by key (group-by :name ifaces))]
      (let [members (sort (distinct (keep :member refs)))]
        (println (format "    %-26s %s" (yellow iname)
                         (if (seq members) (dim (str/join ", " (take 8 members))) (dim "(marker)"))))))
    (println)

    (println (sub-heading (format "  Bridge ops (%d unique):" n-bridge)))
    (doseq [[bname refs] (sort-by key (group-by :name bridges))]
      (let [members (sort (distinct (keep :member refs)))
            shown (take 10 members)
            more (- (count members) (count shown))]
        (println (format "    %-10s %s%s" (yellow bname) (dim (str/join ", " shown))
                         (if (pos? more) (dim (format " (+%d)" more)) "")))))
    (println)

    (println (sub-heading (format "  Concrete types (%d):" n-concrete)))
    (doseq [[cname refs] (sort-by key (group-by :name concretes))]
      (let [members (sort (distinct (keep :member refs)))]
        (println (format "    %-26s %s" (yellow cname)
                         (if (seq members) (dim (str/join ", " (take 6 members))) "")))))
    (println)

    (when (seq interop)
      (let [members (sort (distinct (map :member interop)))
            shown (take 20 members)
            more (- (count members) (count shown))]
        (println (sub-heading (format "  Interop (.method) calls (%d):" n-interop)))
        (println (str "    " (dim (str/join ", " shown))
                      (if (pos? more) (dim (format " (+%d)" more)) "")))
        (println)))

    ;; ── FOUNDATION ───────────────────────────────────────────────────────
    (println (heading "FOUNDATION  (layer 0 — implement first)"))
    (println)
    (let [foundation (sort-by :name (get by-layer 0))
          pure-fns (filter #(empty? (:host-refs %)) foundation)
          host-fns (filter #(seq (:host-refs %)) foundation)
          pure-by-ns (group-by :ns pure-fns)]

      (println (sub-heading (format "  Pure (%d) — no host deps:" (count pure-fns))))
      (doseq [[ns-name fns] (sort-by key pure-by-ns)]
        (let [names (sort (map :name fns))
              shown (take 10 names)
              more (- (count names) (count shown))]
          (println (format "    %s %s%s" (dim (format "%-14s" (str ns-name ":")))
                           (green (str/join ", " shown))
                           (if (pos? more) (dim (format " (+%d)" more)) "")))))
      (println)

      (let [sorted-host (sort-by (comp - :dependents) host-fns)
            shown (take 30 sorted-host)
            rest-count (- (count host-fns) (count shown))]
        (println (sub-heading (format "  Host-bound (%d) — need native primitives:" (count host-fns))))
        (doseq [node shown]
          (let [refs (take 3 (map #(str (:name %) (when (:member %) (str "/" (:member %))))
                                   (:host-resolved node)))]
            (println (format "    %-30s  %s  %s %s"
                             (yellow (str (:ns node) "/" (:name node)))
                             (format "used by %-3d" (:dependents node))
                             (dim "<-")
                             (red (str/join ", " refs))))))
        (when (pos? rest-count)
          (println (dim (format "    ... +%d more (used by 0, low priority)" rest-count))))))
    (println)

    ;; ── IMPLEMENTATION ROADMAP ───────────────────────────────────────────
    (println (heading "IMPLEMENTATION ROADMAP  (unblocks the most downstream defs)"))
    (println)
    (println (format "  %-35s %6s %6s  %s"
                     (bold "Definition") (bold "Used") (bold "Layer") (bold "Kind")))
    (println (dim (format "  %-35s %6s %6s  %s"
                          (apply str (repeat 35 "─")) "──────" "──────" "────")))
    (let [top (take 40 (sort-by :dependents > (vals graph)))]
      (doseq [node top]
        (let [host? (seq (:host-refs node))]
          (println (format "  %-35s %6d %6d  %s"
                           (str (:ns node) "/" (:name node))
                           (:dependents node)
                           (:layer node)
                           (if host? (red "HOST") (green "pure")))))))
    (println)

    ;; ── SUMMARY ──────────────────────────────────────────────────────────
    (println (bold (cyan "SUMMARY")))
    (println (dim (apply str (repeat 72 "─"))))
    (println)
    (println (format "  %s  across %s  %s deep"
                     (bold (str (count graph) " definitions"))
                     (bold (str (count (distinct (map :ns (vals graph)))) " namespaces,"))
                     (bold (str (inc max-layer) " layers"))))
    (println (format "  %s  |  %s"
                     (green (format "%d pure (%.0f%%)" n-pure (* 100.0 (/ n-pure (count graph)))))
                     (yellow (format "%d host-bound (%.0f%%)" n-host (* 100.0 (/ n-host (count graph)))))))
    (println (format "  Host surface: %s interfaces, %s bridge ops, %s types, %s interop"
                     (bold (str n-ifaces)) (bold (str n-bridge))
                     (bold (str n-concrete)) (bold (str n-interop))))
    (println)
    (println (dim "  Use `par tree <src> --edn` for machine-readable output."))))

;; =============================================================================
;; Print — EDN
;; =============================================================================

(defn print-edn-tree
  "Machine-readable merged tree."
  [graph host-data]
  (let [all-host-resolved (mapcat :host-resolved (vals graph))
        by-layer (group-by :layer (vals graph))]
    (prn {:definitions (count graph)
          :layers (into (sorted-map) (map (fn [[l ns]] [l (count ns)])) by-layer)
          :host-surface
          {:interfaces (vec (sort (distinct (map :name (filter #(= :interface (:kind %)) all-host-resolved)))))
           :bridge-ops (vec (sort (distinct (map #(str (:name %) "/" (:member %))
                                                 (filter #(= :bridge (:kind %)) all-host-resolved)))))
           :concretes  (vec (sort (distinct (map :name (filter #(= :concrete (:kind %)) all-host-resolved)))))
           :interop    (vec (sort (distinct (map :member (filter #(= :interop (:kind %)) all-host-resolved)))))}
          :graph graph})))

;; =============================================================================
;; Main
;; =============================================================================

(defn -main [& args]
  (let [graph-file (first (remove #(str/starts-with? % "--") args))
        host-file  (second (remove #(str/starts-with? % "--") args))
        edn-mode   (some #{"--edn"} args)
        no-color   (some #{"--no-color"} args)]
    (when (or (nil? graph-file) (nil? host-file))
      (println "Usage: bb tree.clj <graph.edn> <host.edn> [--edn] [--no-color]")
      (println "  graph.edn from: par deps <src> --edn")
      (println "  host.edn from:  par discover --edn")
      (System/exit 1))
    (binding [*color* (not no-color)]
      (let [graph-data (edn/read-string (slurp graph-file))
            host-data  (edn/read-string (slurp host-file))
            raw-graph  (:graph graph-data)
            enriched   (into (sorted-map) (map (partial enrich-node host-data)) raw-graph)]
        (if edn-mode
          (print-edn-tree enriched host-data)
          (print-tree enriched host-data))))))

(apply -main *command-line-args*)
