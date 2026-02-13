(ns observatory.views.cache
  "Cache hierarchy visualization.

   Shows the actual structure of CPU caches:
   - L1/L2/L3 tabs
   - Sets displayed as rows
   - Ways displayed as columns
   - Each cell is a cache line (clickable)

   Color coding:
   - Empty (gray) - no data
   - Valid (blue) - clean data
   - Modified (orange) - dirty data
   - Recent access (pulse animation)"
  (:require [re-frame.core :as rf]
            [observatory.state :as state]
            [observatory.cache :as cache]))

;; =============================================================================
;; Cache Line Cell
;; =============================================================================

(defn line-cell
  "A single cache line cell. Clickable to inspect."
  [{:keys [set-index way-index line config]}]
  (let [has-data? (some? line)
        state (:state line)
        recent? (and line
                     (< (- (js/Date.now) (:last-access line 0)) 1000))]
    [:div.cache-line
     {:class [(when-not has-data? "empty")
              (when has-data? (name state))
              (when recent? "recent")]
      :on-click (fn [e]
                  (.stopPropagation e)
                  (when has-data?
                    (rf/dispatch [::state/select
                                  {:type :cache-line
                                   :data (assoc line
                                                :set-index set-index
                                                :way-index way-index)}])))}
     (when has-data?
       [:div.line-content
        [:div.tag (str "0x" (.toString (:tag line) 16))]
        [:div.state-indicator {:class (name state)}]])]))

;; =============================================================================
;; Cache Set Row
;; =============================================================================

(defn cache-set-row
  "A single set (row) showing all its ways."
  [{:keys [set-index ways config]}]
  [:div.cache-set
   [:div.set-index (str "Set 0x" (.toString set-index 16))]
   [:div.ways
    (for [[way-index way] (map-indexed vector ways)]
      ^{:key way-index}
      [line-cell {:set-index set-index
                  :way-index way-index
                  :line (:line way)
                  :config config}])]])

;; =============================================================================
;; Cache Level View
;; =============================================================================

(defn cache-level-view
  "View for a single cache level (L1/L2/L3)."
  [{:keys [level cache]}]
  (let [config (:config cache)
        sets (:sets cache)
        stats (:stats cache)
        ;; Only show a subset of sets for large caches
        visible-sets (take 32 sets)
        total-sets (count sets)]
    [:div.cache-level-view
     [:div.level-header
      [:h3.level-name (str (name level) " Cache")]
      [:div.level-stats
       [:span.stat (str (:size-kb config) "KB")]
       [:span.stat (str (:associativity config) "-way")]
       [:span.stat (str (count sets) " sets")]
       [:span.stat (str (:line-size config 64) "B lines")]]]

     [:div.level-metrics
      [:div.metric
       [:span.label "Hits"]
       [:span.value (:hits stats 0)]]
      [:div.metric
       [:span.label "Misses"]
       [:span.value (:misses stats 0)]]
      [:div.metric
       [:span.label "Hit Rate"]
       [:span.value
        (if (pos? (+ (:hits stats 0) (:misses stats 0)))
          (str (int (* 100 (/ (:hits stats 0)
                              (+ (:hits stats 0) (:misses stats 0))))) "%")
          "-")]]
      [:div.metric
       [:span.label "Evictions"]
       [:span.value (:evictions stats 0)]]]

     [:div.way-headers
      [:div.set-index-header "Set"]
      (for [w (range (:associativity config))]
        ^{:key w}
        [:div.way-header (str "Way " w)])]

     [:div.sets-container
      (for [[idx cache-set] (map-indexed vector visible-sets)]
        ^{:key idx}
        [cache-set-row {:set-index (:index cache-set)
                        :ways (:ways cache-set)
                        :config config}])

      (when (> total-sets (count visible-sets))
        [:div.truncation-notice
         (str "Showing " (count visible-sets) " of " total-sets " sets. "
              "Scroll or filter to see more.")])]]))

;; =============================================================================
;; Cache Hierarchy Overview
;; =============================================================================

(defn hierarchy-overview
  "Visual overview of the full cache hierarchy."
  []
  (let [hierarchy @(rf/subscribe [::state/cache-hierarchy])]
    [:div.hierarchy-overview
     [:svg.hierarchy-diagram {:viewBox "0 0 400 200"}
      ;; CPU
      [:g.cpu {:transform "translate(175, 10)"}
       [:rect {:width 50 :height 30 :rx 4 :class "cpu-block"}]
       [:text {:x 25 :y 20 :text-anchor "middle"} "CPU"]]

      ;; L1
      [:g.l1 {:transform "translate(150, 60)"}
       [:rect {:width 100 :height 25 :rx 3 :class "cache-block l1"}]
       [:text {:x 50 :y 17 :text-anchor "middle"} "L1 (32KB)"]]

      ;; L2
      [:g.l2 {:transform "translate(125, 100)"}
       [:rect {:width 150 :height 25 :rx 3 :class "cache-block l2"}]
       [:text {:x 75 :y 17 :text-anchor "middle"} "L2 (256KB)"]]

      ;; L3
      [:g.l3 {:transform "translate(75, 140)"}
       [:rect {:width 250 :height 25 :rx 3 :class "cache-block l3"}]
       [:text {:x 125 :y 17 :text-anchor "middle"} "L3 (8MB)"]]

      ;; RAM
      [:g.ram {:transform "translate(25, 180)"}
       [:rect {:width 350 :height 15 :rx 2 :class "ram-block"}]
       [:text {:x 175 :y 12 :text-anchor "middle"} "RAM"]]

      ;; Connecting lines
      [:path.connection {:d "M200,40 L200,60"}]
      [:path.connection {:d "M200,85 L200,100"}]
      [:path.connection {:d "M200,125 L200,140"}]
      [:path.connection {:d "M200,165 L200,180"}]]]))

;; =============================================================================
;; Cache Simulation Controls
;; =============================================================================

(defn simulation-controls
  "Controls for cache simulation mode."
  []
  (let [address-input (atom "0x7ff612340000")]
    (fn []
      [:div.simulation-controls
       [:h4 "Simulate Access"]
       [:div.input-row
        [:input.address-input
         {:type "text"
          :value @address-input
          :on-change #(reset! address-input (-> % .-target .-value))
          :placeholder "Enter address (hex)"}]
        [:button.simulate-btn
         {:on-click #(let [addr (js/parseInt @address-input 16)]
                       (when-not (js/isNaN addr)
                         (rf/dispatch [::state/simulate-access addr])))}
         "Access"]]
       [:div.quick-patterns
        [:span.label "Quick patterns:"]
        [:button.pattern-btn
         {:on-click #(doseq [i (range 100)]
                       (rf/dispatch [::state/simulate-access (+ 0x1000 (* i 64))]))}
         "Sequential"]
        [:button.pattern-btn
         {:on-click #(doseq [_ (range 100)]
                       (rf/dispatch [::state/simulate-access
                                     (+ 0x1000 (* (rand-int 1000) 64))]))}
         "Random"]]])))

;; =============================================================================
;; Main Cache View
;; =============================================================================

(defn cache-view
  "Complete cache visualization view."
  []
  (let [cache-state @(rf/subscribe [::state/cache-state])
        active-level (rf/subscribe [::state/current-tab])]
    [:div.cache-view
     [:div.view-header
      [:h2 "Cache Hierarchy"]
      [:p.hint "Click any cache line to inspect. Colors show MESI state."]]

     [hierarchy-overview]

     [simulation-controls]

     [:div.cache-tabs
      [:button.tab {:class "active"} "L1 Data"]
      [:button.tab "L2"]
      [:button.tab "L3"]]

     (when (:l1d cache-state)
       [cache-level-view {:level :l1d :cache (:l1d cache-state)}])]))
