(println "Hello from ClojurePHP!")
(println [1 2 3])
(println (.getMessage (new Exception "Hello from PHP Exception")))

;; Test direct operators
(def sum (+ 1 2 3))
(def diff (- 10 3 2))
(def prod (* 2 3 4))
(def quot (/ 100 5 2))
(def modulo (mod 17 5))
(def neg (- 5))

(println "sum:" sum)
(println "diff:" diff)
(println "prod:" prod)
(println "quot:" quot)
(println "modulo:" modulo)
(println "neg:" neg)

;; Test comparisons
(println "5 > 3:" (> 5 3))
(println "2 < 7:" (< 2 7))
(println "5 >= 5:" (>= 5 5))
(println "3 <= 4:" (<= 3 4))
