;; Minimal IIFE test - no runtime dependencies

;; Test let in expression context (IIFE)
(php/echo (php/strval (+ 1 (let [x 2] x))))
(php/echo "\n")

;; Test nested let
(php/echo (php/strval (+ (let [a 1] a) (let [b 2] b))))
(php/echo "\n")

;; Test do in expression context
(php/echo (php/strval (+ 10 (do (def temp 5) temp))))
(php/echo "\n")

(php/echo "Done\n")
