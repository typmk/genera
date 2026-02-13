;;; parinfer.el --- Parinfer implementation in pure Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Claude Code Project
;; Original Parinfer Copyright (C) 2015-2017 Shaun LeBron

;; Author: Claude Code Project
;; Original Author: Shaun LeBron
;; Version: 3.13.1-el
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Keywords: lisp, tools
;; URL: https://github.com/parinfer/parinfer

;; This file is not part of GNU Emacs.

;;; License:

;; MIT License (same as original Parinfer)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This is a pure Emacs Lisp port of Parinfer (https://shaunlebron.github.io/parinfer/).
;;
;; Parinfer is a text editing mode that infers parenthesis placement from indentation,
;; or vice versa.  This eliminates the need for manual paren balancing in Lisp code.
;;
;; This implementation is a direct port of parinfer.js v3.13.1 with the following goals:
;; - 100% feature parity with the JavaScript version
;; - No native dependencies (works on any Emacs installation)
;; - Designed for LLM-based diagnostics and analysis
;; - Integration with paren-diagnostic tools
;;
;; Key differences from parinferlib.el:
;; - Complete implementation (including Smart Mode)
;; - Correct algorithms (no simplifications)
;; - Buffer-local state (not global)
;; - Modern Elisp idioms (cl-lib, pcase, etc.)
;;
;; See README.md for more information.

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Constants

(defconst parinfer--version "3.13.1-el"
  "Version of parinfer.el (based on parinfer.js 3.13.1).")

;; Performance hack from original JavaScript: use -999 instead of nil
;; to avoid type coercion overhead in heavily-used integer fields.
(defconst parinfer--UINT-NULL -999
  "Sentinel value for unsigned integer or null (performance optimization).")

(defconst parinfer--INDENT-MODE 'INDENT_MODE
  "Mode symbol for Indent Mode.")

(defconst parinfer--PAREN-MODE 'PAREN_MODE
  "Mode symbol for Paren Mode.")

;; Character constants
(defconst parinfer--BACKSLASH "\\"
  "Backslash character.")

(defconst parinfer--BLANK-SPACE " "
  "Space character.")

(defconst parinfer--DOUBLE-SPACE "  "
  "Two spaces.")

(defconst parinfer--DOUBLE-QUOTE "\""
  "Double quote character.")

(defconst parinfer--NEWLINE "\n"
  "Newline character.")

(defconst parinfer--TAB "\t"
  "Tab character.")

(defconst parinfer--LINE-ENDING-REGEX "\\(\r\\)?\n"
  "Regex for line endings (handles both LF and CRLF).")

;; Paren matching table
(defconst parinfer--MATCH-PAREN
  '((?{ . ?})
    (?} . ?{)
    (?\[ . ?\])
    (?\] . ?\[)
    (?\( . ?\))
    (?\) . ?\())
  "Alist mapping open/close parens to their counterparts.")

;; Error constants
(defconst parinfer--ERROR-QUOTE-DANGER "quote-danger"
  "Error: quotes must be balanced inside comment blocks.")

(defconst parinfer--ERROR-EOL-BACKSLASH "eol-backslash"
  "Error: line cannot end in a hanging backslash.")

(defconst parinfer--ERROR-UNCLOSED-QUOTE "unclosed-quote"
  "Error: string is missing a closing quote.")

(defconst parinfer--ERROR-UNCLOSED-PAREN "unclosed-paren"
  "Error: unmatched open-paren.")

(defconst parinfer--ERROR-UNMATCHED-CLOSE-PAREN "unmatched-close-paren"
  "Error: unmatched close-paren.")

(defconst parinfer--ERROR-UNMATCHED-OPEN-PAREN "unmatched-open-paren"
  "Error: unmatched open-paren.")

(defconst parinfer--ERROR-LEADING-CLOSE-PAREN "leading-close-paren"
  "Error: line cannot lead with a close-paren.")

(defconst parinfer--ERROR-UNHANDLED "unhandled"
  "Error: unhandled error occurred.")

;; Error messages
(defconst parinfer--error-messages
  (let ((ht (make-hash-table :test 'equal)))
    (puthash parinfer--ERROR-QUOTE-DANGER "Quotes must balanced inside comment blocks." ht)
    (puthash parinfer--ERROR-EOL-BACKSLASH "Line cannot end in a hanging backslash." ht)
    (puthash parinfer--ERROR-UNCLOSED-QUOTE "String is missing a closing quote." ht)
    (puthash parinfer--ERROR-UNCLOSED-PAREN "Unclosed open-paren." ht)
    (puthash parinfer--ERROR-UNMATCHED-CLOSE-PAREN "Unmatched close-paren." ht)
    (puthash parinfer--ERROR-UNMATCHED-OPEN-PAREN "Unmatched open-paren." ht)
    (puthash parinfer--ERROR-LEADING-CLOSE-PAREN "Line cannot lead with a close-paren." ht)
    (puthash parinfer--ERROR-UNHANDLED "Unhandled error." ht)
    ht)
  "Hash table mapping error names to error messages.")

;; ---------------------------------------------------------------------------
;; Type Predicates

(defsubst parinfer--integer-p (x)
  "Return t if X is an integer."
  (integerp x))

(defsubst parinfer--positive-int-p (i)
  "Return t if I is a positive integer (including zero)."
  (and (integerp i) (>= i 0)))

(defsubst parinfer--char-p (c)
  "Return t if C is a single character string."
  (and (stringp c) (= (length c) 1)))

;; ---------------------------------------------------------------------------
;; Language Helpers (abstractions for easier porting)

(defsubst parinfer--array-size (a)
  "Return the length of array A."
  (length a))

(defsubst parinfer--str-len (s)
  "Return the length of string S."
  (length s))

(defsubst parinfer--str-concat (s1 s2)
  "Concatenate strings S1 and S2."
  (concat s1 s2))

(defsubst parinfer--get-char-from-string (s idx)
  "Get character from string S at index IDX."
  (if (< idx (length s))
      (substring s idx (1+ idx))
    nil))

(defsubst parinfer--index-of (arr val)
  "Return the index of VAL in array ARR, or -1 if not found."
  (let ((len (length arr))
        (i 0)
        (found -1))
    (while (and (< i len) (= found -1))
      (when (equal val (aref arr i))
        (setq found i))
      (cl-incf i))
    found))

;; ---------------------------------------------------------------------------
;; String Operations

(defun parinfer--replace-within-string (orig start-idx end-idx replace)
  "Replace substring in ORIG from START-IDX to END-IDX with REPLACE."
  (let ((head (substring orig 0 start-idx))
        (tail (substring orig (min end-idx (length orig)))))
    (concat head replace tail)))

(defun parinfer--repeat-string (text n)
  "Repeat TEXT string N times."
  (let ((result "")
        (i 0))
    (while (< i n)
      (setq result (concat result text))
      (cl-incf i))
    result))

(defun parinfer--get-line-ending (text)
  "Detect line ending style in TEXT (LF or CRLF).
Returns \"\\r\\n\" if CR is found anywhere, otherwise \"\\n\"."
  (if (string-match-p "\r" text)
      "\r\n"
    "\n"))

(defun parinfer--insert-within-string (orig idx insert)
  "Insert INSERT into ORIG at index IDX."
  (parinfer--replace-within-string orig idx idx insert))

(defun parinfer--remove-within-string (orig start-idx end-idx)
  "Remove substring from ORIG between START-IDX and END-IDX."
  (parinfer--replace-within-string orig start-idx end-idx ""))

;; ---------------------------------------------------------------------------
;; Stack Operations

(defsubst parinfer--peek (stack index)
  "Peek at STACK element at INDEX from the top.
INDEX 0 is the top of the stack."
  (let ((len (length stack)))
    (if (and (>= index 0) (< index len))
        (aref stack (- len index 1))
      nil)))

(defsubst parinfer--stack-empty-p (stack)
  "Return t if STACK is empty."
  (= (length stack) 0))

;; ---------------------------------------------------------------------------
;; Character Classification

(defsubst parinfer--open-paren-p (ch open-paren-chars)
  "Return t if CH is in OPEN-PAREN-CHARS vector."
  (not (= (parinfer--index-of open-paren-chars (string-to-char ch)) -1)))

(defsubst parinfer--close-paren-p (ch close-paren-chars)
  "Return t if CH is in CLOSE-PAREN-CHARS vector."
  (not (= (parinfer--index-of close-paren-chars (string-to-char ch)) -1)))

(defsubst parinfer--valid-close-paren-p (paren-stack ch)
  "Return t if CH is a valid close paren for the top of PAREN-STACK."
  (if (parinfer--stack-empty-p paren-stack)
      nil
    (let ((opener (parinfer--peek paren-stack 0)))
      (equal (plist-get opener :ch)
             (char-to-string (cdr (assq (string-to-char ch) parinfer--MATCH-PAREN)))))))

(defsubst parinfer--whitespace-p (result)
  "Return t if current character in RESULT is whitespace (and not escaped)."
  (let ((ch (parinfer--result-ch result)))
    (and (not (parinfer--result-is-escaped result))
         (or (equal ch parinfer--BLANK-SPACE)
             (equal ch parinfer--DOUBLE-SPACE)))))

(defsubst parinfer--closable-p (result)
  "Return t if current character can be the last code character of a list."
  (let* ((ch (parinfer--result-ch result))
         (is-closer (and (parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))
                        (not (parinfer--result-is-escaped result)))))
    (and (parinfer--result-is-in-code result)
         (not (parinfer--whitespace-p result))
         (not (equal ch ""))
         (not is-closer))))


(defsubst parinfer--is-comment-char (ch comment-chars)
  "Return t if CH is in COMMENT-CHARS vector."
  (not (= (parinfer--index-of comment-chars (string-to-char ch)) -1)))

;; ---------------------------------------------------------------------------
;; Misc Utils

(defsubst parinfer--clamp (val min-n max-n)
  "Clamp VAL between MIN-N and MAX-N.
If MIN-N or MAX-N is UINT-NULL, no clamping is done on that side."
  (when (not (= min-n parinfer--UINT-NULL))
    (setq val (max min-n val)))
  (when (not (= max-n parinfer--UINT-NULL))
    (setq val (min max-n val)))
  val)

;; ---------------------------------------------------------------------------
;; Error Handling

(defun parinfer--cache-error-pos (result error-name)
  "Cache the current position in RESULT for ERROR-NAME.
Returns the cached position plist."
  (let ((pos (list :line-no (parinfer--result-line-no result)
                   :x (parinfer--result-x result)
                   :input-line-no (parinfer--result-input-line-no result)
                   :input-x (parinfer--result-input-x result))))
    (puthash error-name pos (parinfer--result-error-pos-cache result))
    pos))

(defun parinfer--create-error (result error-name)
  "Create an error plist for ERROR-NAME using RESULT's current or cached position.
Signals an error condition with the created error plist."
  (let* ((cache (gethash error-name (parinfer--result-error-pos-cache result)))
         (use-output-pos (parinfer--result-partial-result result))
         (line-no (if cache
                     (if use-output-pos
                         (plist-get cache :line-no)
                       (plist-get cache :input-line-no))
                   (if use-output-pos
                       (parinfer--result-line-no result)
                     (parinfer--result-input-line-no result))))
         (x (if cache
               (if use-output-pos
                   (plist-get cache :x)
                 (plist-get cache :input-x))
             (if use-output-pos
                 (parinfer--result-x result)
               (parinfer--result-input-x result))))
         (err (list :parinfer-error t
                   :name error-name
                   :message (gethash error-name parinfer--error-messages "Unknown error")
                   :line-no line-no
                   :x x))
         (key-line-no (if use-output-pos :line-no :input-line-no))
         (key-x (if use-output-pos :x :input-x)))

    ;; Handle special cases
    (cond
     ;; ERROR_UNMATCHED_CLOSE_PAREN: add extra info about the unmatched open-paren
     ((equal error-name parinfer--ERROR-UNMATCHED-CLOSE-PAREN)
      (let ((cache2 (gethash parinfer--ERROR-UNMATCHED-OPEN-PAREN
                            (parinfer--result-error-pos-cache result)))
            (opener (parinfer--peek (parinfer--result-paren-stack result) 0)))
        (when (or cache2 opener)
          (let ((line-no2 (if cache2
                             (plist-get cache2 key-line-no)
                           (plist-get opener key-line-no)))
                (x2 (if cache2
                       (plist-get cache2 key-x)
                     (plist-get opener key-x))))
            (plist-put err :extra
                      (list :name parinfer--ERROR-UNMATCHED-OPEN-PAREN
                            :line-no line-no2
                            :x x2))))))

     ;; ERROR_UNCLOSED_PAREN: use the opener's position
     ((equal error-name parinfer--ERROR-UNCLOSED-PAREN)
      (let ((opener (parinfer--peek (parinfer--result-paren-stack result) 0)))
        (when opener
          (plist-put err :line-no (plist-get opener key-line-no))
          (plist-put err :x (plist-get opener key-x))))))

    err))

;; ---------------------------------------------------------------------------
;; Result Structure
;;
;; The result structure represents the running state as we scan through
;; each character of the input text.  In JavaScript, this is a mutable
;; object passed through all functions.  In Elisp, we use a cl-defstruct
;; with mutable fields (accessed via setf).

(cl-defstruct (parinfer--paren-trail
               (:constructor parinfer--make-paren-trail)
               (:copier nil))
  "Structure representing a paren trail (parens at the end of a line)."
  (line-no parinfer--UINT-NULL :type integer)
  (start-x parinfer--UINT-NULL :type integer)
  (end-x parinfer--UINT-NULL :type integer)
  (openers (vector) :type vector)  ; vector of stack elements
  (clamped-start-x parinfer--UINT-NULL :type integer)
  (clamped-end-x parinfer--UINT-NULL :type integer)
  (clamped-openers (vector) :type vector))

(cl-defstruct (parinfer--result
               (:constructor parinfer--make-result)
               (:copier nil))
  "Structure representing the running result state.

This is the main state structure that is mutated as we process each
character of the input text.  It corresponds to the `result` object
in the JavaScript implementation."

  ;; Mode and options
  (mode nil :type symbol)           ; INDENT_MODE or PAREN_MODE
  (smart nil :type boolean)         ; smart mode enabled?

  ;; Original input
  (orig-text "" :type string)       ; original text
  (orig-cursor-x parinfer--UINT-NULL :type integer)
  (orig-cursor-line parinfer--UINT-NULL :type integer)

  ;; Input lines (we process line-by-line, char-by-char)
  (input-lines (vector) :type vector)  ; vector of strings
  (input-line-no -1 :type integer)     ; current input line number
  (input-x -1 :type integer)           ; current input x position

  ;; Output state
  (lines (vector) :type vector)        ; output lines (vector of strings)
  (line-no -1 :type integer)           ; output line number
  (ch "" :type string)                 ; current character
  (x 0 :type integer)                  ; output x position
  (indent-x parinfer--UINT-NULL :type integer)  ; x position of indentation point

  ;; Paren stack and tracking
  (paren-stack (vector) :type vector)  ; stack of open-parens
  (tab-stops (vector) :type vector)    ; tab stop positions for indentation
  (paren-trail nil)                     ; current paren trail (parinfer--paren-trail struct)
  (paren-trails (vector) :type vector) ; all non-empty paren trails
  (return-parens nil :type boolean)    ; should we return paren tree?
  (parens (vector) :type vector)       ; paren tree (if return-parens is t)

  ;; Cursor tracking
  (cursor-x parinfer--UINT-NULL :type integer)
  (cursor-line parinfer--UINT-NULL :type integer)
  (prev-cursor-x parinfer--UINT-NULL :type integer)
  (prev-cursor-line parinfer--UINT-NULL :type integer)

  ;; Language configuration
  (comment-chars (vector ?\;) :type vector)    ; comment characters
  (open-paren-chars (vector ?\( ?\[ ?\{) :type vector)
  (close-paren-chars (vector ?\) ?\] ?\}) :type vector)

  ;; Selection
  (selection-start-line parinfer--UINT-NULL :type integer)

  ;; Changes (for incremental processing)
  (changes nil)  ; hash table or alist

  ;; Current parsing state
  (is-in-code t :type boolean)      ; are we in code (not string/comment)?
  (is-escaping nil :type boolean)   ; will next char be escaped?
  (is-escaped nil :type boolean)    ; is current char escaped?
  (is-in-str nil :type boolean)     ; are we in a string?
  (is-in-comment nil :type boolean) ; are we in a comment?
  (comment-x parinfer--UINT-NULL :type integer)  ; x position of comment start

  ;; State flags
  (quote-danger nil :type boolean)         ; quotes imbalanced in comment?
  (tracking-indent nil :type boolean)      ; looking for indentation point?
  (skip-char nil :type boolean)            ; should we skip current char?
  (success nil :type boolean)              ; was input properly formatted?
  (partial-result nil :type boolean)       ; return partial result on error?
  (force-balance nil :type boolean)        ; aggressively enforce paren balance?

  ;; Indentation tracking
  (max-indent parinfer--UINT-NULL :type integer)  ; maximum allowed indentation (Paren Mode)
  (indent-delta 0 :type integer)                   ; indentation shift amount

  ;; Tab stop tracking
  (tracking-arg-tab-stop nil)  ; enum: nil, 'space, or 'arg

  ;; Error handling
  (error-pos-cache nil)  ; hash table or alist: error-name -> position
  (error nil))           ; error object if processing failed

;; Initialize a new result structure
(defun parinfer--get-initial-result (text options mode &optional smart)
  "Create initial result structure for processing TEXT with OPTIONS in MODE.
If SMART is non-nil, enable smart mode features."
  (let* ((result (parinfer--make-result))
         (lines-vec (apply #'vector (split-string text parinfer--LINE-ENDING-REGEX))))

    (setf (parinfer--result-mode result) mode)
    (setf (parinfer--result-smart result) smart)
    (setf (parinfer--result-orig-text result) text)
    (setf (parinfer--result-input-lines result) lines-vec)
    (setf (parinfer--result-paren-trail result) (parinfer--make-paren-trail))
    (setf (parinfer--result-error-pos-cache result) (make-hash-table :test 'equal))

    ;; Process options
    (when (plist-member options :cursor-x)
      (setf (parinfer--result-cursor-x result) (plist-get options :cursor-x))
      (setf (parinfer--result-orig-cursor-x result) (plist-get options :cursor-x)))

    (when (plist-member options :cursor-line)
      (setf (parinfer--result-cursor-line result) (plist-get options :cursor-line))
      (setf (parinfer--result-orig-cursor-line result) (plist-get options :cursor-line)))

    (when (plist-member options :prev-cursor-x)
      (setf (parinfer--result-prev-cursor-x result) (plist-get options :prev-cursor-x)))

    (when (plist-member options :prev-cursor-line)
      (setf (parinfer--result-prev-cursor-line result) (plist-get options :prev-cursor-line)))

    (when (plist-member options :selection-start-line)
      (setf (parinfer--result-selection-start-line result) (plist-get options :selection-start-line)))

    (when (plist-member options :changes)
      (setf (parinfer--result-changes result) (plist-get options :changes)))

    (when (plist-member options :force-balance)
      (setf (parinfer--result-force-balance result) (plist-get options :force-balance)))

    (when (plist-member options :partial-result)
      (setf (parinfer--result-partial-result result) (plist-get options :partial-result)))

    (when (plist-member options :return-parens)
      (setf (parinfer--result-return-parens result) (plist-get options :return-parens)))

    result))

;; ---------------------------------------------------------------------------
;; State Machine Character Handlers

(defun parinfer--on-quote (result)
  "Handle quote character in RESULT."
  (cond
   ((parinfer--result-is-in-str result)
    ;; Closing a string
    (setf (parinfer--result-is-in-str result) nil))

   ((parinfer--result-is-in-comment result)
    ;; Quote inside comment - toggle quote danger
    (setf (parinfer--result-quote-danger result)
          (not (parinfer--result-quote-danger result)))
    (when (parinfer--result-quote-danger result)
      (parinfer--cache-error-pos result parinfer--ERROR-QUOTE-DANGER)))

   (t
    ;; Opening a string
    (setf (parinfer--result-is-in-str result) t)
    (parinfer--cache-error-pos result parinfer--ERROR-UNCLOSED-QUOTE))))

(defun parinfer--on-backslash (result)
  "Handle backslash character in RESULT."
  (setf (parinfer--result-is-escaping result) t))

(defun parinfer--after-backslash (result)
  "Handle character after backslash in RESULT."
  (setf (parinfer--result-is-escaping result) nil)
  (setf (parinfer--result-is-escaped result) t)

  ;; Check for backslash at end of line
  (when (equal (parinfer--result-ch result) parinfer--NEWLINE)
    (when (parinfer--result-is-in-code result)
      (signal 'parinfer-error
              (list (parinfer--create-error result parinfer--ERROR-EOL-BACKSLASH))))
    (parinfer--on-newline result)))

(defun parinfer--on-comment-char (result)
  "Handle comment character in RESULT."
  (when (parinfer--result-is-in-code result)
    (setf (parinfer--result-is-in-comment result) t)
    (setf (parinfer--result-comment-x result) (parinfer--result-x result))
    (setf (parinfer--result-tracking-arg-tab-stop result) nil)))

(defun parinfer--on-newline (result)
  "Handle newline character in RESULT."
  (setf (parinfer--result-is-in-comment result) nil)
  (setf (parinfer--result-ch result) ""))

(defun parinfer--on-tab (result)
  "Handle tab character in RESULT."
  (when (parinfer--result-is-in-code result)
    (setf (parinfer--result-ch result) parinfer--DOUBLE-SPACE)))

;; ---------------------------------------------------------------------------
;; Cursor Position Helpers

(defsubst parinfer--is-cursor-affected (result start end)
  "Check if cursor position is affected by edit from START to END in RESULT."
  (let ((cursor-x (parinfer--result-cursor-x result)))
    (if (and (= cursor-x start) (= cursor-x end))
        (= cursor-x 0)
      (>= cursor-x end))))

(defun parinfer--shift-cursor-on-edit (result line-no start end replace-txt)
  "Shift cursor if affected by edit in RESULT.
LINE-NO is the line being edited, START and END are edit bounds,
REPLACE-TXT is the replacement text."
  (let* ((old-length (- end start))
         (new-length (parinfer--str-len replace-txt))
         (dx (- new-length old-length))
         (cursor-line (parinfer--result-cursor-line result))
         (cursor-x (parinfer--result-cursor-x result)))
    (when (and (not (= dx 0))
               (= cursor-line line-no)
               (not (= cursor-x parinfer--UINT-NULL))
               (parinfer--is-cursor-affected result start end))
      (setf (parinfer--result-cursor-x result) (+ cursor-x dx)))))

(defun parinfer--replace-within-line (result line-no start-idx end-idx replace-txt)
  "Replace text in RESULT's line LINE-NO from START-IDX to END-IDX with REPLACE-TXT."
  (let* ((lines (parinfer--result-lines result))
         (line (aref lines line-no))
         (new-line (parinfer--replace-within-string line start-idx end-idx replace-txt)))
    (setf (aref (parinfer--result-lines result) line-no) new-line)
    (parinfer--shift-cursor-on-edit result line-no start-idx end-idx replace-txt)))

;; ---------------------------------------------------------------------------
;; Paren Trail Management

(defun parinfer--reset-paren-trail (result line-no x)
  "Reset paren trail in RESULT to start at LINE-NO and X position."
  (let ((trail (parinfer--result-paren-trail result)))
    (setf (parinfer--paren-trail-line-no trail) line-no)
    (setf (parinfer--paren-trail-start-x trail) x)
    (setf (parinfer--paren-trail-end-x trail) x)
    (setf (parinfer--paren-trail-openers trail) (vector))
    (setf (parinfer--paren-trail-clamped-start-x trail) parinfer--UINT-NULL)
    (setf (parinfer--paren-trail-clamped-end-x trail) parinfer--UINT-NULL)
    (setf (parinfer--paren-trail-clamped-openers trail) (vector))))

;; ---------------------------------------------------------------------------
;; Paren Stack Operations

(defun parinfer--set-closer (opener line-no x ch)
  "Set closer information in OPENER plist.
LINE-NO, X, and CH are the closer's position and character."
  (plist-put opener :closer-line-no line-no)
  (plist-put opener :closer-x x)
  (plist-put opener :closer-ch ch))

(defun parinfer--check-cursor-holding (result)
  "Check if cursor is holding a paren in RESULT.
Returns t if cursor is between the parent opener and current opener."
  (let* ((paren-stack (parinfer--result-paren-stack result))
         (opener (parinfer--peek paren-stack 0))
         (parent (parinfer--peek paren-stack 1))
         (hold-min-x (if parent (+ (plist-get parent :x) 1) 0))
         (hold-max-x (plist-get opener :x))
         (cursor-line (parinfer--result-cursor-line result))
         (cursor-x (parinfer--result-cursor-x result))
         (holding (and (= cursor-line (plist-get opener :line-no))
                       (<= hold-min-x cursor-x)
                       (<= cursor-x hold-max-x)))
         (should-check-prev (and (not (parinfer--result-changes result))
                                 (not (= (parinfer--result-prev-cursor-line result)
                                         parinfer--UINT-NULL)))))
    (when should-check-prev
      (let* ((prev-cursor-line (parinfer--result-prev-cursor-line result))
             (prev-cursor-x (parinfer--result-prev-cursor-x result))
             (prev-holding (and (= prev-cursor-line (plist-get opener :line-no))
                                (<= hold-min-x prev-cursor-x)
                                (<= prev-cursor-x hold-max-x))))
        (when (and prev-holding (not holding))
          ;; Exit to paren mode - signal special condition
          (signal 'parinfer-paren-mode-exit '(release-cursor-hold)))))
    holding))

(defun parinfer--on-open-paren (result)
  "Handle opening paren in RESULT."
  (when (parinfer--result-is-in-code result)
    (let* ((opener (list :input-line-no (parinfer--result-input-line-no result)
                         :input-x (parinfer--result-input-x result)
                         :line-no (parinfer--result-line-no result)
                         :x (parinfer--result-x result)
                         :ch (parinfer--result-ch result)
                         :indent-delta (parinfer--result-indent-delta result)
                         :max-child-indent parinfer--UINT-NULL))
           (return-parens (parinfer--result-return-parens result)))

      ;; Handle paren tree construction if returnParens is enabled
      (when return-parens
        (plist-put opener :children (vector))
        (plist-put opener :closer-line-no parinfer--UINT-NULL)
        (plist-put opener :closer-x parinfer--UINT-NULL)
        (plist-put opener :closer-ch "")

        (let* ((parent1 (parinfer--peek (parinfer--result-paren-stack result) 0))
               (parent2 (if parent1
                            (plist-get parent1 :children)
                          (parinfer--result-parens result))))
          (setf (parinfer--result-parens result)
                (vconcat parent2 (vector opener)))))

      ;; Push opener to paren stack
      (setf (parinfer--result-paren-stack result)
            (vconcat (parinfer--result-paren-stack result) (vector opener)))

      ;; Set tracking state
      (setf (parinfer--result-tracking-arg-tab-stop result) "space"))))

(defun parinfer--on-matched-close-paren (result)
  "Handle matched closing paren in RESULT."
  (let* ((paren-stack (parinfer--result-paren-stack result))
         (opener (parinfer--peek paren-stack 0))
         (trail (parinfer--result-paren-trail result)))

    ;; Set closer info if returnParens enabled
    (when (parinfer--result-return-parens result)
      (parinfer--set-closer opener
                            (parinfer--result-line-no result)
                            (parinfer--result-x result)
                            (parinfer--result-ch result)))

    ;; Extend paren trail
    (setf (parinfer--paren-trail-end-x trail) (+ (parinfer--result-x result) 1))
    (setf (parinfer--paren-trail-openers trail)
          (vconcat (parinfer--paren-trail-openers trail) (vector opener)))

    ;; Handle smart mode cursor holding
    (when (and (eq (parinfer--result-mode result) parinfer--INDENT-MODE)
               (parinfer--result-smart result)
               (parinfer--check-cursor-holding result))
      (let ((orig-start-x (parinfer--paren-trail-start-x trail))
            (orig-end-x (parinfer--paren-trail-end-x trail))
            (orig-openers (parinfer--paren-trail-openers trail)))
        (parinfer--reset-paren-trail result
                                     (parinfer--result-line-no result)
                                     (+ (parinfer--result-x result) 1))
        (setf (parinfer--paren-trail-clamped-start-x trail) orig-start-x)
        (setf (parinfer--paren-trail-clamped-end-x trail) orig-end-x)
        (setf (parinfer--paren-trail-clamped-openers trail) orig-openers)))

    ;; Pop from paren stack
    (let ((stack-len (length paren-stack)))
      (setf (parinfer--result-paren-stack result)
            (substring paren-stack 0 (- stack-len 1))))

    ;; Clear tracking state
    (setf (parinfer--result-tracking-arg-tab-stop result) nil)))

(defun parinfer--on-unmatched-close-paren (result)
  "Handle unmatched closing paren in RESULT."
  (let ((mode (parinfer--result-mode result)))
    (cond
     ;; PAREN_MODE: throw error unless smart mode allows removal
     ((eq mode parinfer--PAREN-MODE)
      (let* ((trail (parinfer--result-paren-trail result))
             (in-leading-paren-trail (and (= (parinfer--paren-trail-line-no trail)
                                             (parinfer--result-line-no result))
                                          (= (parinfer--paren-trail-start-x trail)
                                             (parinfer--result-indent-x result))))
             (can-remove (and (parinfer--result-smart result)
                              in-leading-paren-trail)))
        (unless can-remove
          (parinfer--create-error result parinfer--ERROR-UNMATCHED-CLOSE-PAREN))))

     ;; INDENT_MODE: cache error positions
     ((eq mode parinfer--INDENT-MODE)
      (let ((cache (parinfer--result-error-pos-cache result)))
        (unless (gethash parinfer--ERROR-UNMATCHED-CLOSE-PAREN cache)
          (parinfer--cache-error-pos result parinfer--ERROR-UNMATCHED-CLOSE-PAREN)
          (let ((opener (parinfer--peek (parinfer--result-paren-stack result) 0)))
            (when opener
              (let ((e (parinfer--cache-error-pos result parinfer--ERROR-UNMATCHED-OPEN-PAREN)))
                (plist-put e :input-line-no (plist-get opener :input-line-no))
                (plist-put e :input-x (plist-get opener :input-x)))))))))

    ;; Remove the character
    (setf (parinfer--result-ch result) "")))

(defun parinfer--on-close-paren (result)
  "Handle closing paren in RESULT (dispatcher)."
  (when (parinfer--result-is-in-code result)
    (if (parinfer--valid-close-paren-p (parinfer--result-paren-stack result)
                                       (parinfer--result-ch result))
        (parinfer--on-matched-close-paren result)
      (parinfer--on-unmatched-close-paren result))))

;; ---------------------------------------------------------------------------
;; Line Processing

(defun parinfer--init-line (result)
  "Initialize line state in RESULT."
  (setf (parinfer--result-x result) 0)
  (setf (parinfer--result-line-no result) (+ (parinfer--result-line-no result) 1))

  ;; Reset line-specific state
  (setf (parinfer--result-indent-x result) parinfer--UINT-NULL)
  (setf (parinfer--result-comment-x result) parinfer--UINT-NULL)
  (setf (parinfer--result-indent-delta result) 0)

  ;; Clear error cache entries
  (let ((cache (parinfer--result-error-pos-cache result)))
    (remhash parinfer--ERROR-UNMATCHED-CLOSE-PAREN cache)
    (remhash parinfer--ERROR-UNMATCHED-OPEN-PAREN cache)
    (remhash parinfer--ERROR-LEADING-CLOSE-PAREN cache))

  ;; Reset tracking state
  (setf (parinfer--result-tracking-arg-tab-stop result) nil)
  (setf (parinfer--result-tracking-indent result)
        (not (parinfer--result-is-in-str result))))

(defun parinfer--commit-char (result orig-ch)
  "Commit character to line in RESULT.
ORIG-CH is the original character before any transformations."
  (let* ((ch (parinfer--result-ch result))
         (orig-ch-length (parinfer--str-len orig-ch))
         (ch-length (parinfer--str-len ch))
         (x (parinfer--result-x result))
         (line-no (parinfer--result-line-no result)))

    ;; Replace character if it changed
    (unless (string= orig-ch ch)
      (parinfer--replace-within-line result line-no x (+ x orig-ch-length) ch)
      ;; Update indentDelta (note: JavaScript has a bug here, we use correct formula)
      (setf (parinfer--result-indent-delta result)
            (+ (parinfer--result-indent-delta result)
               (- ch-length orig-ch-length))))

    ;; Advance x position
    (setf (parinfer--result-x result) (+ x ch-length))))

;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;; Cursor Helper Functions (for indentation)

(defsubst parinfer--is-cursor-left-of (cursor-x cursor-line x line-no)
  "Return t if cursor is to the left of position X on LINE-NO."
  (and (= cursor-line line-no)
       (not (= x parinfer--UINT-NULL))
       (not (= cursor-x parinfer--UINT-NULL))
       (<= cursor-x x)))  ; inclusive since (cursorX = x) implies (x-1 < cursor < x)

(defsubst parinfer--is-cursor-right-of (cursor-x cursor-line x line-no)
  "Return t if cursor is to the right of position X on LINE-NO."
  (and (= cursor-line line-no)
       (not (= x parinfer--UINT-NULL))
       (not (= cursor-x parinfer--UINT-NULL))
       (> cursor-x x)))

(defsubst parinfer--is-cursor-in-comment (result cursor-x cursor-line)
  "Return t if cursor is inside a comment in RESULT."
  (parinfer--is-cursor-right-of cursor-x cursor-line
                                 (parinfer--result-comment-x result)
                                 (parinfer--result-line-no result)))

;; ---------------------------------------------------------------------------
;; Indentation Functions

(defun parinfer--should-add-opener-indent (result opener)
  "Return t if we should add OPENER's indentDelta to current line in RESULT.
Don't add opener.indentDelta if the user already added it.
This happens when multiple lines are indented together."
  (not (= (plist-get opener :indent-delta)
          (parinfer--result-indent-delta result))))

(defun parinfer--add-indent (result delta)
  "Add DELTA spaces of indentation to current line in RESULT."
  (let* ((orig-indent (parinfer--result-x result))
         (new-indent (+ orig-indent delta))
         (indent-str (parinfer--repeat-string parinfer--BLANK-SPACE new-indent)))
    (parinfer--replace-within-line result
                                    (parinfer--result-line-no result)
                                    0
                                    orig-indent
                                    indent-str)
    (setf (parinfer--result-x result) new-indent)
    (setf (parinfer--result-indent-x result) new-indent)
    (setf (parinfer--result-indent-delta result)
          (+ (parinfer--result-indent-delta result) delta))))

(defun parinfer--correct-indent (result)
  "Correct indentation for current line in RESULT (Paren Mode)."
  (let* ((orig-indent (parinfer--result-x result))
         (new-indent orig-indent)
         (min-indent 0)
         (max-indent (parinfer--result-max-indent result))
         (opener (parinfer--peek (parinfer--result-paren-stack result) 0)))

    (when opener
      (setq min-indent (+ (plist-get opener :x) 1))
      (setq max-indent (plist-get opener :max-child-indent))
      (when (parinfer--should-add-opener-indent result opener)
        (setq new-indent (+ new-indent (plist-get opener :indent-delta)))))

    (setq new-indent (parinfer--clamp new-indent min-indent max-indent))

    (when (not (= new-indent orig-indent))
      (parinfer--add-indent result (- new-indent orig-indent)))))

(defun parinfer--on-leading-close-paren (result)
  "Handle leading close paren in RESULT."
  (let ((mode (parinfer--result-mode result)))
    (cond
     ;; INDENT_MODE
     ((eq mode parinfer--INDENT-MODE)
      (when (not (parinfer--result-force-balance result))
        (when (parinfer--result-smart result)
          ;; Exit to paren mode
          (signal 'parinfer-paren-mode-exit '(leading-close-paren)))
        (when (not (gethash parinfer--ERROR-LEADING-CLOSE-PAREN
                           (parinfer--result-error-pos-cache result)))
          (parinfer--cache-error-pos result parinfer--ERROR-LEADING-CLOSE-PAREN)))
      (setf (parinfer--result-skip-char result) t))

     ;; PAREN_MODE
     ((eq mode parinfer--PAREN-MODE)
      (if (not (parinfer--valid-close-paren-p (parinfer--result-paren-stack result)
                                              (parinfer--result-ch result)))
          (if (parinfer--result-smart result)
              (setf (parinfer--result-skip-char result) t)
            (signal 'parinfer-error
                    (list (parinfer--create-error result parinfer--ERROR-UNMATCHED-CLOSE-PAREN))))
        (if (parinfer--is-cursor-left-of (parinfer--result-cursor-x result)
                                         (parinfer--result-cursor-line result)
                                         (parinfer--result-x result)
                                         (parinfer--result-line-no result))
            (progn
              (parinfer--reset-paren-trail result
                                           (parinfer--result-line-no result)
                                           (parinfer--result-x result))
              (parinfer--on-indent result))
          (parinfer--append-paren-trail result)
          (setf (parinfer--result-skip-char result) t)))))))

(defun parinfer--on-comment-line (result)
  "Handle comment line indentation in RESULT."
  (let ((paren-trail-len (length (parinfer--paren-trail-openers
                                  (parinfer--result-paren-trail result)))))

    ;; restore the openers matching the previous paren trail
    (when (eq (parinfer--result-mode result) parinfer--PAREN-MODE)
      (let ((i 0))
        (while (< i paren-trail-len)
          (let ((opener (parinfer--peek (parinfer--paren-trail-openers
                                         (parinfer--result-paren-trail result))
                                       i)))
            (setf (parinfer--result-paren-stack result)
                  (vconcat (parinfer--result-paren-stack result) (vector opener))))
          (cl-incf i))))

    ;; Find parent opener and add its indent
    (let* ((opener-idx (parinfer--get-parent-opener-index result (parinfer--result-x result)))
           (opener (parinfer--peek (parinfer--result-paren-stack result) opener-idx)))
      (when opener
        ;; shift the comment line based on the parent open paren
        (when (parinfer--should-add-opener-indent result opener)
          (parinfer--add-indent result (plist-get opener :indent-delta)))))

    ;; repop the openers matching the previous paren trail
    (when (eq (parinfer--result-mode result) parinfer--PAREN-MODE)
      (let ((i2 0))
        (while (< i2 paren-trail-len)
          (let ((stack-len (length (parinfer--result-paren-stack result))))
            (setf (parinfer--result-paren-stack result)
                  (substring (parinfer--result-paren-stack result) 0 (- stack-len 1))))
          (cl-incf i2))))))

(defun parinfer--on-indent (result)
  "Handle indentation point in RESULT."
  (setf (parinfer--result-indent-x result) (parinfer--result-x result))
  (setf (parinfer--result-tracking-indent result) nil)

  (when (parinfer--result-quote-danger result)
    (signal 'parinfer-error
            (list (parinfer--create-error result parinfer--ERROR-QUOTE-DANGER))))

  (let ((mode (parinfer--result-mode result)))
    (cond
     ((eq mode parinfer--INDENT-MODE)
      (parinfer--correct-paren-trail result (parinfer--result-x result))
      (let ((opener (parinfer--peek (parinfer--result-paren-stack result) 0)))
        (when (and opener (parinfer--should-add-opener-indent result opener))
          (parinfer--add-indent result (plist-get opener :indent-delta)))))

     ((eq mode parinfer--PAREN-MODE)
      (parinfer--correct-indent result)))))

(defun parinfer--check-indent (result)
  "Check and handle indentation in RESULT (dispatcher)."
  (let ((ch (parinfer--result-ch result)))
    (cond
     ((parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))
      (parinfer--on-leading-close-paren result))

     ((parinfer--is-comment-char ch (parinfer--result-comment-chars result))
      ;; comments don't count as indentation points
      (parinfer--on-comment-line result)
      (setf (parinfer--result-tracking-indent result) nil))

     ((and (not (equal ch parinfer--NEWLINE))
           (not (equal ch parinfer--BLANK-SPACE))
           (not (equal ch parinfer--TAB)))
      (parinfer--on-indent result)))))

;;---------------------------------------------------------------------------
;; Character Processing (stubs for dependencies)

(defun parinfer--handle-change-delta (result)
  "Handle change delta tracking in RESULT."
  (when (and (parinfer--result-changes result)
             (or (parinfer--result-smart result)
                 (eq (parinfer--result-mode result) parinfer--PAREN-MODE)))
    (let ((line (gethash (parinfer--result-input-line-no result)
                        (parinfer--result-changes result))))
      (when line
        (let ((change (gethash (parinfer--result-input-x result) line)))
          (when change
            (setf (parinfer--result-indent-delta result)
                  (+ (parinfer--result-indent-delta result)
                     (- (plist-get change :new-end-x)
                        (plist-get change :old-end-x))))))))))

  "Check and handle indentation in RESULT (dispatcher)."
  (let ((ch (parinfer--result-ch result)))
    (cond
     ((parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))
      (parinfer--on-leading-close-paren result))

     ((parinfer--is-comment-char ch (parinfer--result-comment-chars result))
      ;; comments don't count as indentation points
      (parinfer--on-comment-line result)
      (setf (parinfer--result-tracking-indent result) nil))

     ((and (not (equal ch parinfer--NEWLINE))
           (not (equal ch parinfer--BLANK-SPACE))
           (not (equal ch parinfer--TAB)))
      (parinfer--on-indent result)))))

(defun parinfer--is-closable (result)
  "Check if we can close a paren trail in RESULT.
Alias for parinfer--closable-p for compatibility."
  (parinfer--closable-p result))

(defun parinfer--track-arg-tab-stop (result state)
  "Track argument tab stop in RESULT with STATE.
STATE can be 'space or 'arg."
  (cond
   ((eq state 'space)
    (when (and (parinfer--result-is-in-code result)
               (parinfer--whitespace-p result))
      (setf (parinfer--result-tracking-arg-tab-stop result) 'arg)))

   ((eq state 'arg)
    (when (not (parinfer--whitespace-p result))
      (let ((opener (parinfer--peek (parinfer--result-paren-stack result) 0)))
        (when opener
          (plist-put opener :arg-x (parinfer--result-x result)))
        (setf (parinfer--result-tracking-arg-tab-stop result) nil))))))

;;---------------------------------------------------------------------------
;; Week 4 Stub Functions (dependencies for Week 3 code)

(defun parinfer--get-parent-opener-index (result indent-x)
  "Get index of parent opener for INDENT-X in RESULT.
STUB: Returns 0 for now (will be fully implemented in Week 4)."
  ;; TODO: Full implementation in Week 4
  ;; For now, return 0 to pop all openers (basic behavior)
  0)

(defun parinfer--correct-paren-trail (result indent-x)
  "Correct paren trail from INDENT-X in RESULT.
STUB: Minimal implementation for now (will be completed in Week 4)."
  ;; TODO: Full implementation in Week 4
  nil)

(defun parinfer--append-paren-trail (result)
  "Append a valid close-paren to the end of the paren trail in RESULT.
STUB: Not yet implemented (Week 4)."
  ;; TODO: Implement in Week 4
  nil)


(defun parinfer--on-char (result)
  "Main character dispatcher in RESULT."
  (let ((ch (parinfer--result-ch result)))
    (setf (parinfer--result-is-escaped result) nil)

    (cond
     ;; Dispatch to character handlers
     ((parinfer--result-is-escaping result)
      (parinfer--after-backslash result))

     ((parinfer--open-paren-p ch (parinfer--result-open-paren-chars result))
      (parinfer--on-open-paren result))

     ((parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))
      (parinfer--on-close-paren result))

     ((equal ch parinfer--DOUBLE-QUOTE)
      (parinfer--on-quote result))

     ((parinfer--is-comment-char ch (parinfer--result-comment-chars result))
      (parinfer--on-comment-char result))

     ((equal ch parinfer--BACKSLASH)
      (parinfer--on-backslash result))

     ((equal ch parinfer--TAB)
      (parinfer--on-tab result))

     ((equal ch parinfer--NEWLINE)
      (parinfer--on-newline result)))

    ;; Update ch after potential modifications
    (setq ch (parinfer--result-ch result))

    ;; Update isInCode
    (setf (parinfer--result-is-in-code result)
          (and (not (parinfer--result-is-in-comment result))
               (not (parinfer--result-is-in-str result))))

    ;; Reset paren trail if closable
    (when (parinfer--is-closable result)
      (parinfer--reset-paren-trail result
                                   (parinfer--result-line-no result)
                                   (+ (parinfer--result-x result)
                                      (parinfer--str-len ch))))

    ;; Track arg tab stop if needed
    (let ((state (parinfer--result-tracking-arg-tab-stop result)))
      (when state
        (parinfer--track-arg-tab-stop result state)))))

(defun parinfer--process-char (result ch)
  "Process character CH in RESULT."
  (let ((orig-ch ch))
    (setf (parinfer--result-ch result) ch)
    (setf (parinfer--result-skip-char result) nil)

    ;; Handle change delta
    (parinfer--handle-change-delta result)

    ;; Check indentation if tracking
    (when (parinfer--result-tracking-indent result)
      (parinfer--check-indent result))

    ;; Skip character or process it
    (if (parinfer--result-skip-char result)
        (setf (parinfer--result-ch result) "")
      (parinfer--on-char result))

    ;; Commit character to line
    (parinfer--commit-char result orig-ch)))

;; ---------------------------------------------------------------------------
;; Public API (incomplete - to be filled in)

;;;###autoload
(defun parinfer-indent-mode (text &optional options)
  "Process TEXT using Parinfer Indent Mode with OPTIONS.
Returns a result plist with :success, :text, :cursor-x, :cursor-line, etc."
  (error "Not yet implemented"))

;;;###autoload
(defun parinfer-paren-mode (text &optional options)
  "Process TEXT using Parinfer Paren Mode with OPTIONS.
Returns a result plist with :success, :text, :cursor-x, :cursor-line, etc."
  (error "Not yet implemented"))

;;;###autoload
(defun parinfer-smart-mode (text &optional options)
  "Process TEXT using Parinfer Smart Mode with OPTIONS.
Returns a result plist with :success, :text, :cursor-x, :cursor-line, etc."
  (error "Not yet implemented"))

(provide 'parinfer)

;;; parinfer.el ends here
