
;; Correct
(defn foo [x y] (+ x y))

;; Type mismatch: vector opened, paren closed
(defn bar [x y) (+ x y))

;; Type mismatch: map opened, vector closed
{:a 1 :b 2]

;; Type mismatch: set opened, paren closed
#{1 2 3)
