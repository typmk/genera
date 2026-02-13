;;;; demo.lisp - Demo Common Lisp file for SLIME testing

(defpackage :demo
  (:use :cl)
  (:export #:greet
           #:fibonacci
           #:list-squares))

(in-package :demo)

(defun greet (name)
  "Greet someone by name."
  (format nil "Hello, ~A! Welcome to Common Lisp with SLIME." name))

(defun fibonacci (n)
  "Calculate the nth Fibonacci number."
  (cond
    ((<= n 0) 0)
    ((= n 1) 1)
    (t (+ (fibonacci (- n 1))
          (fibonacci (- n 2))))))

(defun list-squares (n)
  "Return a list of squares from 1 to n."
  (loop for i from 1 to n
        collect (* i i)))

;; Demo usage
(format t "~%Demo functions loaded!~%")
(format t "Greeting: ~A~%" (greet "Claude"))
(format t "Fibonacci(10): ~A~%" (fibonacci 10))
(format t "Squares 1-5: ~A~%" (list-squares 5))
