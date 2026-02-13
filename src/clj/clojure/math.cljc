(ns clojure.math
  "Portable math operations.

   Thin wrapper over clojure.host - no more MathEngine protocol."
  (:require [clojure.host :as h]))

;; =============================================================================
;; Constants
;; =============================================================================

(def PI 3.141592653589793)
(def E  2.718281828459045)
(def TAU (* 2 PI))

(def pi PI)
(def e E)

;; =============================================================================
;; Rounding
;; =============================================================================

(defn floor [x] (h/-math-floor (h/host) x))
(defn ceil  [x] (h/-math-ceil (h/host) x))
(defn round [x] (h/-math-round (h/host) x))
(defn rint  [x] (double (round x)))

;; =============================================================================
;; Power & Roots
;; =============================================================================

(defn pow  [base exp] (h/-math-pow (h/host) base exp))
(defn sqrt [x] (h/-math-sqrt (h/host) x))
(defn cbrt [x] (pow x (/ 1.0 3.0)))
(defn exp  [x] (pow E x))

;; =============================================================================
;; Logarithms
;; =============================================================================

(defn log   [x] (h/-math-log (h/host) x))
(defn log10 [x] (/ (log x) (log 10)))
(defn log2  [x] (/ (log x) (log 2)))

;; =============================================================================
;; Trigonometry
;; =============================================================================

(defn sin [x] (h/-math-sin (h/host) x))
(defn cos [x] (h/-math-cos (h/host) x))
(defn tan [x] (/ (sin x) (cos x)))

;; Inverse trig - computed from other functions
(defn asin [x]
  ;; atan2(x, sqrt(1-x^2))
  (let [sqrt-val (sqrt (- 1 (* x x)))]
    (if (zero? sqrt-val)
      (if (pos? x) (/ PI 2) (/ PI -2))
      (Math/atan2 x sqrt-val))))

(defn acos [x]
  (- (/ PI 2) (asin x)))

(defn atan [x]
  ;; Use series approximation or delegate to host if available
  (asin (/ x (sqrt (+ 1 (* x x))))))

(defn atan2 [y x]
  ;; Full atan2 implementation
  (cond
    (pos? x) (atan (/ y x))
    (and (neg? x) (>= y 0)) (+ (atan (/ y x)) PI)
    (and (neg? x) (neg? y)) (- (atan (/ y x)) PI)
    (and (zero? x) (pos? y)) (/ PI 2)
    (and (zero? x) (neg? y)) (/ PI -2)
    :else 0))

;; Hyperbolic
(defn sinh [x] (/ (- (exp x) (exp (- x))) 2))
(defn cosh [x] (/ (+ (exp x) (exp (- x))) 2))
(defn tanh [x] (/ (sinh x) (cosh x)))

;; =============================================================================
;; Conversion
;; =============================================================================

(defn to-radians [degrees] (* degrees (/ PI 180)))
(defn to-degrees [radians] (* radians (/ 180 PI)))

;; =============================================================================
;; Misc
;; =============================================================================

(defn abs [x] (if (neg? x) (- x) x))
(defn signum [x] (cond (neg? x) -1 (pos? x) 1 :else 0))
(defn sign [x] (signum x))

(defn min-val [a b] (if (< a b) a b))
(defn max-val [a b] (if (> a b) a b))

(defn hypot [x y] (sqrt (+ (* x x) (* y y))))

(defn clamp [x min-v max-v]
  (max-val min-v (min-val max-v x)))

(defn lerp [a b t]
  (+ a (* (- b a) t)))

;; =============================================================================
;; Random
;; =============================================================================

(defn random
  "Returns random double between 0.0 and 1.0."
  []
  (h/-math-random (h/host)))

(defn random-int
  "Returns random integer from 0 (inclusive) to n (exclusive)."
  [n]
  (int (floor (* (random) n))))
