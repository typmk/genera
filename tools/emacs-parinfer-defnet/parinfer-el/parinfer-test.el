;;; parinfer-test.el --- Tests for parinfer.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Claude Code Project

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test suite for parinfer.el using ERT (Emacs Regression Testing).
;;
;; Tests are based on the official Parinfer JSON test suite:
;; - test/indent-mode.json
;; - test/paren-mode.json
;; - test/smart-mode.json
;;
;; Run tests with:
;;   emacs -batch -L . -l parinfer-test.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'json)
(require 'parinfer)

;; ---------------------------------------------------------------------------
;; Test Utilities

(defvar parinfer-test--base-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Base directory for parinfer-test.el.")

(defvar parinfer-test--test-dir
  (expand-file-name "test" parinfer-test--base-dir)
  "Directory containing JSON test files.")

(defun parinfer-test--load-json-file (filename)
  "Load and parse JSON test file FILENAME."
  (let ((filepath (expand-file-name filename parinfer-test--test-dir)))
    (unless (file-exists-p filepath)
      (error "Test file not found: %s" filepath))
    (with-temp-buffer
      (insert-file-contents filepath)
      (json-read))))

(defun parinfer-test--convert-options (options-alist)
  "Convert OPTIONS-ALIST from JSON to Elisp plist.
JSON uses camelCase keys, we use kebab-case with keyword symbols."
  (when options-alist
    (let ((plist '()))
      (dolist (pair options-alist)
        (let* ((key (car pair))
               (val (cdr pair))
               (key-str (symbol-name key))
               ;; Convert camelCase to kebab-case
               (elisp-key (intern (concat ":"
                                          (replace-regexp-in-string
                                           "\\([A-Z]\\)" "-\\1" key-str t)
                                          ""))))
          ;; Handle nested objects
          (when (and val (listp val) (not (eq (car val) 'vector)))
            (setq val (parinfer-test--convert-options val)))
          (push elisp-key plist)
          (push val plist)))
      (nreverse plist))))

(defun parinfer-test--run-test-case (test-case mode-fn)
  "Run a single TEST-CASE using MODE-FN (indent-mode, paren-mode, or smart-mode)."
  (let* ((text (cdr (assoc 'text test-case)))
         (options-alist (cdr (assoc 'options test-case)))
         (expected-result (cdr (assoc 'result test-case)))
         (options (parinfer-test--convert-options options-alist))
         (actual-result (condition-case err
                            (funcall mode-fn text options)
                          (error (list :error (error-message-string err))))))

    ;; Compare results
    (let ((expected-success (cdr (assoc 'success expected-result)))
          (expected-text (cdr (assoc 'text expected-result)))
          (actual-success (plist-get actual-result :success))
          (actual-text (plist-get actual-result :text))
          (actual-error (plist-get actual-result :error)))

      ;; Check success status
      (should (eq expected-success actual-success))

      ;; Check text output (if successful)
      (when expected-success
        (should (equal expected-text actual-text)))

      ;; TODO: Check cursor positions, tab stops, paren trails, etc.
      ;; TODO: Check error details if not successful

      actual-result)))

(defun parinfer-test--generate-tests-from-json (json-file mode-fn mode-name)
  "Generate ERT tests from JSON-FILE using MODE-FN with MODE-NAME."
  (let* ((test-cases (parinfer-test--load-json-file json-file))
         (test-count (length test-cases)))
    (dotimes (i test-count)
      (let* ((test-case (aref test-cases i))
             (test-number (1+ i))
             (source-info (cdr (assoc 'source test-case)))
             (source-line (cdr (assoc 'lineNo source-info)))
             (test-name (intern (format "parinfer-test-%s-%03d-line-%d"
                                        mode-name test-number source-line))))
        (eval
         `(ert-deftest ,test-name ()
            ,(format "Test %s case %d (source line %d)"
                     mode-name test-number source-line)
            (parinfer-test--run-test-case
             ',(append test-case nil)  ; Convert vector to list
             #',mode-fn))
         t)))))

;; ---------------------------------------------------------------------------
;; Test Generation

;; Generate tests for Indent Mode
;; (parinfer-test--generate-tests-from-json "indent-mode.json"
;;                                           #'parinfer-indent-mode
;;                                           "indent-mode")

;; Generate tests for Paren Mode
;; (parinfer-test--generate-tests-from-json "paren-mode.json"
;;                                           #'parinfer-paren-mode
;;                                           "paren-mode")

;; Generate tests for Smart Mode
;; (parinfer-test--generate-tests-from-json "smart-mode.json"
;;                                           #'parinfer-smart-mode
;;                                           "smart-mode")

;; ---------------------------------------------------------------------------
;; Manual Smoke Tests (for initial development)

(ert-deftest parinfer-test-smoke-basic-indent ()
  "Smoke test: basic indentation inference."
  :expected-result :failed  ; Will fail until we implement the algorithm
  (let* ((text "(defn foo\n  [arg\n  ret")
         (result (parinfer-indent-mode text nil)))
    (should (plist-get result :success))
    (should (equal (plist-get result :text)
                   "(defn foo\n  [arg]\n  ret)"))))

(ert-deftest parinfer-test-smoke-basic-paren ()
  "Smoke test: basic paren mode."
  :expected-result :failed  ; Will fail until we implement the algorithm
  (let* ((text "(defn foo\n  [arg]\nret)")
         (result (parinfer-paren-mode text nil)))
    (should (plist-get result :success))
    (should (equal (plist-get result :text)
                   "(defn foo\n  [arg]\n  ret)"))))

(ert-deftest parinfer-test-structure-creation ()
  "Test that we can create result structures."
  (let ((result (parinfer--get-initial-result "(foo)" nil parinfer--INDENT-MODE)))
    (should (parinfer--result-p result))
    (should (eq (parinfer--result-mode result) parinfer--INDENT-MODE))
    (should (equal (parinfer--result-orig-text result) "(foo)"))
    (should (vectorp (parinfer--result-input-lines result)))
    (should (= (length (parinfer--result-input-lines result)) 1))
    (should (equal (aref (parinfer--result-input-lines result) 0) "(foo)"))))

(ert-deftest parinfer-test-string-operations ()
  "Test string helper functions."
  (should (equal (parinfer--replace-within-string "abc" 0 2 "x") "xc"))
  (should (equal (parinfer--replace-within-string "abcdef" 3 25 "") "abc"))
  (should (equal (parinfer--repeat-string "a" 3) "aaa"))
  (should (equal (parinfer--repeat-string "" 5) ""))
  (should (equal (parinfer--get-line-ending "foo\nbar") "\n"))
  (should (equal (parinfer--get-line-ending "foo\r\nbar") "\r\n")))

(ert-deftest parinfer-test-stack-operations ()
  "Test stack helper functions."
  (let ((stack (vector 'a 'b 'c)))
    (should (eq (parinfer--peek stack 0) 'c))
    (should (eq (parinfer--peek stack 1) 'b))
    (should (eq (parinfer--peek stack 2) 'a))
    (should (null (parinfer--peek stack 3)))
    (should (null (parinfer--peek stack -1))))
  (should (parinfer--stack-empty-p (vector)))
  (should-not (parinfer--stack-empty-p (vector 'a))))

(ert-deftest parinfer-test-char-helpers ()
  "Test character helper functions."
  (should (equal (parinfer--get-char-from-string "abc" 0) "a"))
  (should (equal (parinfer--get-char-from-string "abc" 1) "b"))
  (should (equal (parinfer--get-char-from-string "abc" 2) "c"))
  (should (null (parinfer--get-char-from-string "abc" 3))))

(ert-deftest parinfer-test-char-classification ()
  "Test character classification functions."
  ;; Test open-paren-p
  (let ((open-chars (vector ?\( ?\[ ?\{)))
    (should (parinfer--open-paren-p "(" open-chars))
    (should (parinfer--open-paren-p "[" open-chars))
    (should (parinfer--open-paren-p "{" open-chars))
    (should-not (parinfer--open-paren-p ")" open-chars))
    (should-not (parinfer--open-paren-p "a" open-chars)))

  ;; Test close-paren-p
  (let ((close-chars (vector ?\) ?\] ?\})))
    (should (parinfer--close-paren-p ")" close-chars))
    (should (parinfer--close-paren-p "]" close-chars))
    (should (parinfer--close-paren-p "}" close-chars))
    (should-not (parinfer--close-paren-p "(" close-chars))
    (should-not (parinfer--close-paren-p "a" close-chars)))

  ;; Test valid-close-paren-p
  (let ((stack (vector (list :ch "(" :x 0 :line-no 0)
                       (list :ch "[" :x 1 :line-no 0))))
    (should (parinfer--valid-close-paren-p stack "]"))
    (should-not (parinfer--valid-close-paren-p stack ")"))
    (should-not (parinfer--valid-close-paren-p stack "}")))
  (should-not (parinfer--valid-close-paren-p (vector) ")"))

  ;; Test whitespace-p
  (let ((result (parinfer--get-initial-result "" nil parinfer--INDENT-MODE)))
    (setf (parinfer--result-ch result) " ")
    (setf (parinfer--result-is-escaped result) nil)
    (should (parinfer--whitespace-p result))
    (setf (parinfer--result-ch result) "  ")
    (should (parinfer--whitespace-p result))
    (setf (parinfer--result-ch result) "a")
    (should-not (parinfer--whitespace-p result))
    (setf (parinfer--result-ch result) " ")
    (setf (parinfer--result-is-escaped result) t)
    (should-not (parinfer--whitespace-p result))))

(ert-deftest parinfer-test-error-handling ()
  "Test error handling functions."
  (let ((result (parinfer--get-initial-result "" nil parinfer--INDENT-MODE)))
    ;; Set up some position info
    (setf (parinfer--result-line-no result) 5)
    (setf (parinfer--result-x result) 10)
    (setf (parinfer--result-input-line-no result) 5)
    (setf (parinfer--result-input-x result) 10)

    ;; Test cache-error-pos
    (let ((cached-pos (parinfer--cache-error-pos result parinfer--ERROR-UNCLOSED-QUOTE)))
      (should (equal (plist-get cached-pos :line-no) 5))
      (should (equal (plist-get cached-pos :x) 10))
      (should (equal (plist-get cached-pos :input-line-no) 5))
      (should (equal (plist-get cached-pos :input-x) 10)))

    ;; Test create-error with cached position
    (setf (parinfer--result-line-no result) 7)
    (setf (parinfer--result-x result) 15)
    (let ((err (parinfer--create-error result parinfer--ERROR-UNCLOSED-QUOTE)))
      (should (plist-get err :parinfer-error))
      (should (equal (plist-get err :name) parinfer--ERROR-UNCLOSED-QUOTE))
      (should (stringp (plist-get err :message)))
      ;; Should use cached position (5, 10), not current (7, 15)
      (should (equal (plist-get err :line-no) 5))
      (should (equal (plist-get err :x) 10)))

    ;; Test create-error without cache
    ;; Update input position
    (setf (parinfer--result-input-line-no result) 8)
    (setf (parinfer--result-input-x result) 20)
    (let ((err (parinfer--create-error result parinfer--ERROR-EOL-BACKSLASH)))
      (should (equal (plist-get err :name) parinfer--ERROR-EOL-BACKSLASH))
      ;; Should use current input position since not cached (partial-result defaults to nil)
      (should (equal (plist-get err :line-no) 8))
      (should (equal (plist-get err :x) 20)))))

(ert-deftest parinfer-test-state-machine ()
  "Test state machine character handlers."
  (let ((result (parinfer--get-initial-result "" nil parinfer--INDENT-MODE)))
    ;; Test on-quote: opening a string
    (setf (parinfer--result-ch result) "\"")
    (parinfer--on-quote result)
    (should (parinfer--result-is-in-str result))

    ;; Test on-quote: closing a string
    (parinfer--on-quote result)
    (should-not (parinfer--result-is-in-str result))

    ;; Test on-quote in comment: toggle quote-danger
    (setf (parinfer--result-is-in-comment result) t)
    (parinfer--on-quote result)
    (should (parinfer--result-quote-danger result))
    (parinfer--on-quote result)
    (should-not (parinfer--result-quote-danger result))
    (setf (parinfer--result-is-in-comment result) nil)

    ;; Test on-backslash
    (parinfer--on-backslash result)
    (should (parinfer--result-is-escaping result))

    ;; Test after-backslash
    (setf (parinfer--result-ch result) "x")
    (parinfer--after-backslash result)
    (should-not (parinfer--result-is-escaping result))
    (should (parinfer--result-is-escaped result))

    ;; Test on-comment-char
    (setf (parinfer--result-is-escaped result) nil)
    (setf (parinfer--result-x result) 5)
    (parinfer--on-comment-char result)
    (should (parinfer--result-is-in-comment result))
    (should (equal (parinfer--result-comment-x result) 5))

    ;; Test on-newline
    (parinfer--on-newline result)
    (should-not (parinfer--result-is-in-comment result))
    (should (equal (parinfer--result-ch result) ""))

    ;; Test on-tab
    (setf (parinfer--result-ch result) "\t")
    (setf (parinfer--result-is-in-code result) t)
    (parinfer--on-tab result)
    (should (equal (parinfer--result-ch result) parinfer--DOUBLE-SPACE))))

;; ---------------------------------------------------------------------------
;; Test Runner

(defun parinfer-test-run-all ()
  "Run all parinfer tests interactively."
  (interactive)
  (ert t))

(defun parinfer-test-run-smoke ()
  "Run only smoke tests."
  (interactive)
  (ert "^parinfer-test-smoke-"))

(defun parinfer-test-run-helpers ()
  "Run helper function tests."
  (interactive)
  (ert "^parinfer-test-\\(structure\\|string\\|stack\\|char\\)-"))

;; ---------------------------------------------------------------------------
;; Paren Stack Tests

(ert-deftest parinfer-test-paren-stack-on-open ()
  "Test opening paren handling."
  (let* ((result (parinfer--get-initial-result "(foo" nil parinfer--INDENT-MODE))
         (paren-stack nil))
    ;; Set up result to be in code
    (setf (parinfer--result-is-in-code result) t)
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 0)
    (setf (parinfer--result-ch result) ?\()
    (setf (parinfer--result-indent-delta result) 0)

    ;; Call on-open-paren
    (parinfer--on-open-paren result)

    ;; Check paren stack has one opener
    (setq paren-stack (parinfer--result-paren-stack result))
    (should (= (length paren-stack) 1))

    ;; Check opener properties
    (let ((opener (aref paren-stack 0)))
      (should (= (plist-get opener :line-no) 0))
      (should (= (plist-get opener :x) 0))
      (should (char-equal (plist-get opener :ch) ?\())
      (should (= (plist-get opener :max-child-indent) parinfer--UINT-NULL)))

    ;; Check tracking state
    (should (eq (parinfer--result-tracking-arg-tab-stop result) 'space))))

(ert-deftest parinfer-test-paren-stack-on-matched-close ()
  "Test matched closing paren handling."
  (let* ((result (parinfer--get-initial-result "()" nil parinfer--INDENT-MODE))
         (opener (list :line-no 0 :x 0 :ch ?\(
                       :input-line-no 0 :input-x 0
                       :indent-delta 0 :max-child-indent parinfer--UINT-NULL)))
    ;; Set up result
    (setf (parinfer--result-is-in-code result) t)
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 1)
    (setf (parinfer--result-ch result) ?\))
    (setf (parinfer--result-paren-stack result) (vector opener))

    ;; Initialize paren trail
    (let ((trail (parinfer--result-paren-trail result)))
      (setf (parinfer--paren-trail-line-no trail) 0)
      (setf (parinfer--paren-trail-start-x trail) 0)
      (setf (parinfer--paren-trail-end-x trail) 0))

    ;; Call on-matched-close-paren
    (parinfer--on-matched-close-paren result)

    ;; Check paren stack is now empty
    (should (= (length (parinfer--result-paren-stack result)) 0))

    ;; Check paren trail was extended
    (let ((trail (parinfer--result-paren-trail result)))
      (should (= (parinfer--paren-trail-end-x trail) 2))
      (should (= (length (parinfer--paren-trail-openers trail)) 1)))

    ;; Check tracking state cleared
    (should (null (parinfer--result-tracking-arg-tab-stop result)))))

(ert-deftest parinfer-test-paren-stack-on-unmatched-close ()
  "Test unmatched closing paren handling."
  (let* ((result (parinfer--get-initial-result ")" nil parinfer--INDENT-MODE)))
    ;; Set up result in INDENT_MODE
    (setf (parinfer--result-mode result) parinfer--INDENT-MODE)
    (setf (parinfer--result-is-in-code result) t)
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 0)
    (setf (parinfer--result-ch result) ?\))

    ;; Call on-unmatched-close-paren
    (parinfer--on-unmatched-close-paren result)

    ;; Check character was removed
    (should (string= (parinfer--result-ch result) ""))

    ;; Check error was cached
    (let ((cache (parinfer--result-error-pos-cache result)))
      (should (gethash parinfer--ERROR-UNMATCHED-CLOSE-PAREN cache)))))

(ert-deftest parinfer-test-paren-stack-on-close-dispatcher ()
  "Test close paren dispatcher."
  (let* ((result (parinfer--get-initial-result "()" nil parinfer--INDENT-MODE))
         (opener (list :line-no 0 :x 0 :ch "("  ; Must be string, not char
                       :input-line-no 0 :input-x 0
                       :indent-delta 0 :max-child-indent parinfer--UINT-NULL)))
    ;; Set up result with matching opener
    (setf (parinfer--result-is-in-code result) t)
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 1)
    (setf (parinfer--result-ch result) ")")  ; Must be string, not char
    (setf (parinfer--result-paren-stack result) (vector opener))

    ;; Initialize paren trail
    (let ((trail (parinfer--result-paren-trail result)))
      (setf (parinfer--paren-trail-line-no trail) 0)
      (setf (parinfer--paren-trail-start-x trail) 0)
      (setf (parinfer--paren-trail-end-x trail) 0))

    ;; Call on-close-paren (should dispatch to on-matched)
    (parinfer--on-close-paren result)

    ;; Check paren stack is empty (matched case)
    (should (= (length (parinfer--result-paren-stack result)) 0))))

;; ---------------------------------------------------------------------------
;; Line Processing Tests

(ert-deftest parinfer-test-line-init ()
  "Test line initialization."
  (let* ((result (parinfer--get-initial-result "foo\nbar" nil parinfer--INDENT-MODE)))
    ;; Set up some state
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 10)
    (setf (parinfer--result-indent-x result) 5)
    (setf (parinfer--result-comment-x result) 8)
    (setf (parinfer--result-indent-delta result) 3)
    (setf (parinfer--result-is-in-str result) nil)

    ;; Cache some errors
    (parinfer--cache-error-pos result parinfer--ERROR-UNMATCHED-CLOSE-PAREN)

    ;; Call init-line
    (parinfer--init-line result)

    ;; Check line-no incremented
    (should (= (parinfer--result-line-no result) 1))

    ;; Check x reset
    (should (= (parinfer--result-x result) 0))

    ;; Check line-specific state reset
    (should (= (parinfer--result-indent-x result) parinfer--UINT-NULL))
    (should (= (parinfer--result-comment-x result) parinfer--UINT-NULL))
    (should (= (parinfer--result-indent-delta result) 0))

    ;; Check error cache cleared
    (let ((cache (parinfer--result-error-pos-cache result)))
      (should (null (gethash parinfer--ERROR-UNMATCHED-CLOSE-PAREN cache))))

    ;; Check tracking state
    (should (null (parinfer--result-tracking-arg-tab-stop result)))
    (should (parinfer--result-tracking-indent result))))

(ert-deftest parinfer-test-line-commit-char ()
  "Test character commit to line."
  (let* ((result (parinfer--get-initial-result "abc" nil parinfer--INDENT-MODE)))
    ;; Set up result - lines vector must be initialized
    (setf (parinfer--result-lines result) (vector "abc"))
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 0)
    (setf (parinfer--result-ch result) "x")  ; Changed from original "a"
    (setf (parinfer--result-indent-delta result) 0)

    ;; Commit character that changed
    (parinfer--commit-char result "a")

    ;; Check line was modified
    (should (string= (aref (parinfer--result-lines result) 0) "xbc"))

    ;; Check x advanced
    (should (= (parinfer--result-x result) 1))

    ;; Check indentDelta unchanged (same length)
    (should (= (parinfer--result-indent-delta result) 0))))

(ert-deftest parinfer-test-line-commit-char-unchanged ()
  "Test character commit when character unchanged."
  (let* ((result (parinfer--get-initial-result "abc" nil parinfer--INDENT-MODE)))
    ;; Set up result - lines vector must be initialized
    (setf (parinfer--result-lines result) (vector "abc"))
    (setf (parinfer--result-line-no result) 0)
    (setf (parinfer--result-x result) 0)
    (setf (parinfer--result-ch result) "a")  ; Same as original
    (setf (parinfer--result-indent-delta result) 0)

    ;; Commit unchanged character
    (parinfer--commit-char result "a")

    ;; Check line unchanged
    (should (string= (aref (parinfer--result-lines result) 0) "abc"))

    ;; Check x advanced
    (should (= (parinfer--result-x result) 1))

    ;; Check indentDelta unchanged
    (should (= (parinfer--result-indent-delta result) 0))))

;; ---------------------------------------------------------------------------
;; Cursor Position Helper Tests

(ert-deftest parinfer-test-cursor-affected ()
  "Test cursor affected detection."
  (let* ((result (parinfer--get-initial-result "abc" nil parinfer--INDENT-MODE)))
    ;; Cursor at position 5
    (setf (parinfer--result-cursor-x result) 5)

    ;; Edit before cursor (should affect)
    (should (parinfer--is-cursor-affected result 0 3))

    ;; Edit at cursor (should not affect unless at 0)
    (should-not (parinfer--is-cursor-affected result 5 5))

    ;; Cursor at 0 and edit at 0 (should affect)
    (setf (parinfer--result-cursor-x result) 0)
    (should (parinfer--is-cursor-affected result 0 0))))

(ert-deftest parinfer-test-cursor-shift ()
  "Test cursor shift on edit."
  (let* ((result (parinfer--get-initial-result "abcdef" nil parinfer--INDENT-MODE)))
    ;; Cursor at position 5
    (setf (parinfer--result-cursor-line result) 0)
    (setf (parinfer--result-cursor-x result) 5)

    ;; Replace 2 chars with 3 chars before cursor
    (parinfer--shift-cursor-on-edit result 0 2 4 "xyz")

    ;; Cursor should shift by +1 (3 new - 2 old)
    (should (= (parinfer--result-cursor-x result) 6))))

(ert-deftest parinfer-test-replace-within-line ()
  "Test replace within line."
  (let* ((result (parinfer--get-initial-result "abcdef" nil parinfer--INDENT-MODE)))
    ;; Set up result - lines vector must be initialized
    (setf (parinfer--result-lines result) (vector "abcdef"))
    (setf (parinfer--result-cursor-line result) 0)
    (setf (parinfer--result-cursor-x result) 5)

    ;; Replace "bc" with "xyz"
    (parinfer--replace-within-line result 0 1 3 "xyz")

    ;; Check line modified
    (should (string= (aref (parinfer--result-lines result) 0) "axyzdef"))

    ;; Check cursor shifted
    (should (= (parinfer--result-cursor-x result) 6))))

;; ---------------------------------------------------------------------------
;; Paren Trail Management Tests

(ert-deftest parinfer-test-reset-paren-trail ()
  "Test paren trail reset."
  (let* ((result (parinfer--get-initial-result "(foo))" nil parinfer--INDENT-MODE))
         (trail (parinfer--result-paren-trail result)))
    ;; Set up some state
    (setf (parinfer--paren-trail-line-no trail) 5)
    (setf (parinfer--paren-trail-start-x trail) 10)
    (setf (parinfer--paren-trail-end-x trail) 15)
    (setf (parinfer--paren-trail-openers trail) (vector 'opener1 'opener2))
    (setf (parinfer--paren-trail-clamped-start-x trail) 8)

    ;; Reset trail
    (parinfer--reset-paren-trail result 3 7)

    ;; Check all fields reset
    (should (= (parinfer--paren-trail-line-no trail) 3))
    (should (= (parinfer--paren-trail-start-x trail) 7))
    (should (= (parinfer--paren-trail-end-x trail) 7))
    (should (= (length (parinfer--paren-trail-openers trail)) 0))
    (should (= (parinfer--paren-trail-clamped-start-x trail) parinfer--UINT-NULL))
    (should (= (parinfer--paren-trail-clamped-end-x trail) parinfer--UINT-NULL))
    (should (= (length (parinfer--paren-trail-clamped-openers trail)) 0))))

;; ---------------------------------------------------------------------------
;; Test Statistics

(defun parinfer-test-count-tests ()
  "Count total number of tests in JSON files."
  (interactive)
  (let ((indent-tests (length (parinfer-test--load-json-file "indent-mode.json")))
        (paren-tests (length (parinfer-test--load-json-file "paren-mode.json")))
        (smart-tests (length (parinfer-test--load-json-file "smart-mode.json"))))
    (message "Test counts: indent=%d paren=%d smart=%d total=%d"
             indent-tests paren-tests smart-tests
             (+ indent-tests paren-tests smart-tests))))

(provide 'parinfer-test)

;;; parinfer-test.el ends here
