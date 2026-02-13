(println "Testing Macros:")

;; when
(when true
  (println "When works"))

;; if-not
(if-not false
  (println "If-not works")
  (println "If-not fail"))

;; cond
(cond
  false (println "Cond fail")
  true  (println "Cond works"))

;; threading
(println (-> 10 inc inc dec)) ;; 10 -> 11 -> 12 -> 11

;; comp
;; ((comp println inc) 99) ;; 100

(println "Done")
