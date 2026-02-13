;; Test ExceptionInfo error display with structured data
(ns test-exinfo-display)

(defn divide [a b]
  (if (= b 0)
    (throw (ex-info "Division by zero"
                    {:cljp.error/type :arithmetic
                     :cljp.error/phase :execution
                     :cljp.error/hint "Check for zero before dividing"
                     :dividend a
                     :divisor b}))
    (/ a b)))

(defn calculate [x]
  (let [result (divide x 0)]
    (println "Result:" result)))

(defn main []
  (println "Testing ExceptionInfo display...")
  (calculate 42))

(main)
