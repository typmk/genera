;; test/alloc.clj — Allocation pass tests
;;
;; Tests V_ALLOC, V_SCOPE, V_DYNAMIC views produced by analyze-alloc!
;; Unlike pass comparison tests, these verify specific expectations
;; since alloc passes are Clojure-only (no C equivalent).
;;
;; Pattern: world-parse! → c-analyze! → analyze-alloc! → check views

;; Helper: parse + analyze + alloc, return node count
(defn alloc-setup! [src]
  (world-parse! src)
  (c-analyze!)
  (analyze-alloc!)
  (gn-count))

;; Helper: count nodes with a given view set
(defn view-count [vid n]
  (loop [i 0 c 0]
    (if (< i n)
      (recur (inc i) (if (view? vid i) (inc c) c))
      c)))

;; Helper: find first node with view set (skip root 0)
(defn view-first [vid n]
  (loop [i 1]
    (if (< i n)
      (if (view? vid i) i (recur (inc i)))
      0)))

(describe! "alloc passes")

;; --- V_ALLOC: which nodes allocate ---

(let [n (alloc-setup! "(+ 1 2)")]
  (it! "arithmetic: no alloc" (= 0 (view-count V_ALLOC n))))

(let [n (alloc-setup! "(fn [x] x)")]
  (it! "fn literal: 1 alloc (FnObj)" (> (view-count V_ALLOC n) 0)))

(let [n (alloc-setup! "(let [a 1] a)")]
  (it! "let: 1 alloc (Env)" (> (view-count V_ALLOC n) 0)))

(let [n (alloc-setup! "(loop [i 0] (if (>= i 10) i (recur (inc i))))")]
  (it! "loop: 1 alloc (Env)" (> (view-count V_ALLOC n) 0)))

(let [n (alloc-setup! "'(1 2 3)")]
  (it! "quoted list: alloc (cons cells)" (> (view-count V_ALLOC n) 0)))

(let [n (alloc-setup! "[1 2 3]")]
  (it! "vec literal: 1 alloc (PVec)" (> (view-count V_ALLOC n) 0)))

(let [n (alloc-setup! "{:a 1}")]
  (it! "map literal: 1 alloc (PMap)" (> (view-count V_ALLOC n) 0)))

(let [n (alloc-setup! "42")]
  (it! "integer: no alloc" (= 0 (view-count V_ALLOC n))))

(let [n (alloc-setup! ":foo")]
  (it! "keyword: no alloc" (= 0 (view-count V_ALLOC n))))

;; --- V_ALLOC_OFF: prefix sum offsets ---

(let [n (alloc-setup! "(let [a 1] (fn [x] x))")]
  (let [first-id (view-first V_ALLOC n)]
    (it! "alloc offset starts at 0" (and (> first-id 0) (= 0 (val-get first-id))))))

;; --- V_SCOPE: escape analysis ---

;; Top-level let has scope=0, can't be V_SCOPE (no enclosing scope)
(let [n (alloc-setup! "(let [a 1] a)")]
  (it! "top-level let: no V_SCOPE" (= 0 (view-count V_SCOPE n))))

;; Non-tail let inside fn: scope > 0, not tail → V_SCOPE
(let [n (alloc-setup! "(defn f [] (do (let [a 1] a) 42))")]
  (it! "non-tail let in defn: V_SCOPE"
    (> (view-count V_SCOPE n) 0)))

;; --- V_DYNAMIC: unbounded allocation ---

(let [n (alloc-setup! "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")]
  (it! "recursive non-tail calls: V_DYNAMIC"
    (> (view-count V_DYNAMIC n) 0)))

(let [n (alloc-setup! "(loop [i 0] (if (>= i 10) i (recur (inc i))))")]
  (it! "tail-recursive loop: no V_DYNAMIC"
    (= 0 (view-count V_DYNAMIC n))))

;; --- Combined: alloc + scope work together ---

(let [n (alloc-setup! "(defn add [x y] (+ x y))")]
  (it! "simple defn: no dynamic" (= 0 (view-count V_DYNAMIC n))))
