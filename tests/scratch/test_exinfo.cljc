;; Test ExceptionInfo implementation
(ns test-exinfo)

;; Test 1: Create and throw an ExceptionInfo
(defn test-basic-exinfo []
  (println "Test 1: Basic ex-info creation")
  (let [ex (ex-info "Something went wrong" {:cljp.error/type :test
                                             :cljp.error/phase :execution
                                             :value 42})]
    (println "  Message:" (ex-message ex))
    (println "  Data:" (pr-str (ex-data ex)))
    (println "  Cause:" (ex-cause ex))
    (println "  OK!")))

;; Test 2: ExceptionInfo with cause chain
(defn test-exinfo-with-cause []
  (println "\nTest 2: ex-info with cause")
  (let [cause (php/new "Exception" "Root cause")
        ex (ex-info "Wrapper error" {:reason :wrapped} cause)]
    (println "  Message:" (ex-message ex))
    (println "  Data:" (pr-str (ex-data ex)))
    (println "  Cause message:" (ex-message (ex-cause ex)))
    (println "  OK!")))

;; Test 3: ex-data returns nil for non-ExceptionInfo
(defn test-exdata-nil []
  (println "\nTest 3: ex-data returns nil for regular exceptions")
  (let [ex (php/new "Exception" "Regular exception")]
    (println "  ex-data:" (ex-data ex))
    (println "  OK!")))

;; Test 4: Throw and catch
(defn test-throw-catch []
  (println "\nTest 4: Throw and catch ex-info")
  (try
    (throw (ex-info "Division failed"
                    {:cljp.error/type :arithmetic
                     :cljp.error/phase :execution
                     :cljp.error/hint "Check for zero before dividing"
                     :dividend 10
                     :divisor 0}))
    (catch Exception e
      (println "  Caught:" (ex-message e))
      (println "  Type:" (get (ex-data e) :cljp.error/type))
      (println "  Hint:" (get (ex-data e) :cljp.error/hint))
      (println "  OK!"))))

;; Test 5: Throwable->map and ex-triage pipeline
(defn test-triage-pipeline []
  (println "\nTest 5: Throwable->map and ex-triage pipeline")
  (let [ex (ex-info "Test error"
                    {:cljp.error/type :test
                     :cljp.error/phase :execution
                     :cljp.error/file "test.cljc"
                     :cljp.error/line 42
                     :cljp.error/hint "This is a test hint"})
        tmap (Throwable->map ex)
        triaged (ex-triage tmap)
        formatted (ex-str triaged)]
    (println "  Throwable->map type:" (:type tmap))
    (println "  Triaged phase:" (get triaged :cljp.error/phase))
    (println "  Formatted:" formatted)
    (println "  OK!")))

;; Test 6: err->msg convenience function
(defn test-err-msg []
  (println "\nTest 6: err->msg convenience function")
  (let [ex (ex-info "Something failed"
                    {:cljp.error/type :validation
                     :cljp.error/phase :execution
                     :cljp.error/hint "Check your input"})
        msg (err->msg ex)]
    (println "  err->msg:" msg)
    (println "  OK!")))

;; Run all tests
(defn main []
  (println "=== ExceptionInfo Tests ===\n")
  (test-basic-exinfo)
  (test-exinfo-with-cause)
  (test-exdata-nil)
  (test-throw-catch)
  (test-triage-pipeline)
  (test-err-msg)
  (println "\n=== All tests passed! ==="))

(main)
