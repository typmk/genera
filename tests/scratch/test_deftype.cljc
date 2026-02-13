;; Simple deftype test
(deftype Point [x y])

(def p (->Point 10 20))
(println "Point x:" (.-x p))
(println "Point y:" (.-y p))
(println "deftype works!")
