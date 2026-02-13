(ns test-interop-spec)

;; array_slice: min 2, max 4
(php/array_slice [])             ;; WARN: Expected 2-4 got 1
(php/array_slice [] 1)           ;; OK
(php/array_slice [] 1 1)         ;; OK
(php/array_slice [] 1 1 true)    ;; OK
(php/array_slice [] 1 1 true 1)  ;; WARN: Expected 2-4 got 5

;; variadic: array_merge
(php/array_merge)                ;; WARN? Depends on spec. Usually at least 1?
(php/array_merge [])             ;; OK
