(ns hello-clojure.core
  "A simple hello world Clojure namespace to test the development setup.")

(defn greet
  "Returns a greeting message for the given name."
  [name]
  (str "Hello, " name "! Welcome to Clojure development."))

(defn add
  "Adds two numbers together."
  [a b]
  (+ a b))

(defn factorial
  "Computes the factorial of n."
  [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))

(defn -main
  "Main entry point for the application."
  [& args]
  (println (greet "World"))
  (println "2 + 3 =" (add 2 3))
  (println "Factorial of 5 =" (factorial 5)))
