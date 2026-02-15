;; 07-bitmap.clj — Bitmap intern + multi-dimensional queries
;;
;; Source bytes → structural bitmasks. No AST. No tree walk.
;; Same idea as clojure-fast/parser_bitmap.c but in Clojure.
;;
;; Core insight: intern IS parse. Characters → bit positions.
;; Query = AND + POPCOUNT over u64 words. O(n/64) per word.
;;
;; Bitmask layers (each a u64[] over source positions):
;;   BM_OPEN     — ( [ {
;;   BM_CLOSE    — ) ] }
;;   BM_WS       — space, tab, newline, comma
;;   BM_DIGIT    — 0-9
;;   BM_ALPHA    — a-z A-Z _ - + * / < > = ! ? . & ^
;;   BM_QUOTE    — " ' ` ~ @ #
;;   BM_SEMI     — ;
;;   BM_NL       — newline (\n)
;;
;; Derived (computed from base layers):
;;   BM_STRUCT   — BM_OPEN | BM_CLOSE  (structural characters)
;;   BM_TOKEN    — ~(BM_WS | BM_STRUCT | BM_QUOTE | BM_SEMI) (token chars)

;; ================================================================
;; Constants
;; ================================================================

;; Character codes
(def CH_LPAREN    40)
(def CH_RPAREN    41)
(def CH_LBRACKET  91)
(def CH_RBRACKET  93)
(def CH_LBRACE   123)
(def CH_RBRACE   125)
(def CH_SPACE     32)
(def CH_TAB        9)
(def CH_NL        10)
(def CH_CR        13)
(def CH_COMMA     44)
(def CH_SEMI      59)
(def CH_DQUOTE    34)
(def CH_SQUOTE    39)
(def CH_BACKTICK  96)
(def CH_TILDE    126)
(def CH_AT        64)
(def CH_HASH      35)

;; Layer indices (offset into bitmap memory as layer * nwords * 8)
(def BM_OPEN   0)
(def BM_CLOSE  1)
(def BM_WS     2)
(def BM_DIGIT  3)
(def BM_ALPHA  4)
(def BM_QUOTE  5)
(def BM_SEMI   6)
(def BM_NL     7)
(def BM_STRUCT 8)
(def BM_TOKEN  9)
(def BM_NLAYERS 10)

;; Bitmap index layout: [base nw len]
;;   base = pointer to memory (BM_NLAYERS * nw * 8 bytes)
;;   nw   = number of u64 words per layer
;;   len  = source length in bytes
(def BM_IDX_BASE 0)
(def BM_IDX_NW   1)
(def BM_IDX_LEN  2)

;; ================================================================
;; Bitmap index: create, intern, query
;; ================================================================

;; bm-create: allocate bitmap index for source of `len` bytes
;; Returns [base nw len]
(defn bm-create [len]
  (let [nw (+ (/ len 64) (if (= (mod len 64) 0) 0 1))
        nbytes (* BM_NLAYERS nw 8)
        mem (mem-new! nbytes)]
    (mem-fill! mem 0 0 nbytes)
    [(mem-base mem) nw len]))

;; bm-word-ptr: pointer to word `w` of layer `layer`
(defn bm-word-ptr [bm layer w]
  (+ (nth bm BM_IDX_BASE) (* (+ (* layer (nth bm BM_IDX_NW)) w) 8)))

;; bm-set-bit!: set bit `pos` in layer
(defn bm-set-bit! [bm layer pos]
  (let [w (/ pos 64)
        b (mod pos 64)
        ptr (bm-word-ptr bm layer w)
        old (load64 ptr)]
    (store64 ptr (bit-or old (bit-shift-left 1 b)))))

;; bm-classify: classify one byte into a layer (or -1)
(defn bm-classify [ch]
  (cond
    (or (= ch CH_LPAREN) (= ch CH_LBRACKET) (= ch CH_LBRACE))
      BM_OPEN
    (or (= ch CH_RPAREN) (= ch CH_RBRACKET) (= ch CH_RBRACE))
      BM_CLOSE
    (or (= ch CH_SPACE) (= ch CH_TAB) (= ch CH_NL) (= ch CH_CR) (= ch CH_COMMA))
      BM_WS
    (and (>= ch 48) (<= ch 57))
      BM_DIGIT
    (or (and (>= ch 97) (<= ch 122))
        (and (>= ch 65) (<= ch 90))
        (= ch 95) (= ch 45) (= ch 43)
        (= ch 42) (= ch 47) (= ch 60)
        (= ch 62) (= ch 61) (= ch 33)
        (= ch 63) (= ch 46) (= ch 38)
        (= ch 94))
      BM_ALPHA
    (or (= ch CH_DQUOTE) (= ch CH_SQUOTE) (= ch CH_BACKTICK)
        (= ch CH_TILDE) (= ch CH_AT) (= ch CH_HASH))
      BM_QUOTE
    (= ch CH_SEMI)
      BM_SEMI
    1 -1))

;; bm-intern!: scan source bytes, set bits in base layers, derive layers
(defn bm-intern! [bm src-ptr src-len]
  (loop [i 0]
    (when (< i src-len)
      (let [ch (load8 (+ src-ptr i))
            layer (bm-classify ch)]
        (when (>= layer 0)
          (bm-set-bit! bm layer i))
        (when (= ch CH_NL)
          (bm-set-bit! bm BM_NL i)))
      (recur (inc i))))
  ;; Derive structural and token layers
  (let [nw (nth bm BM_IDX_NW)]
    (loop [w 0]
      (when (< w nw)
        (let [open  (load64 (bm-word-ptr bm BM_OPEN w))
              close (load64 (bm-word-ptr bm BM_CLOSE w))
              ws    (load64 (bm-word-ptr bm BM_WS w))
              quote (load64 (bm-word-ptr bm BM_QUOTE w))
              semi  (load64 (bm-word-ptr bm BM_SEMI w))
              struc (bit-or open close)]
          (store64 (bm-word-ptr bm BM_STRUCT w) struc)
          (store64 (bm-word-ptr bm BM_TOKEN w)
                   (bit-not (bit-or (bit-or ws struc) (bit-or quote semi)))))
        (recur (inc w))))))

;; bm-intern-gram!: intern the current gram's source
(defn bm-intern-gram! []
  (let [bm (bm-create (gram-src-len))]
    (bm-intern! bm (gram-src-ptr) (gram-src-len))
    bm))

;; ================================================================
;; Queries — all O(nwords), each word O(1)
;; ================================================================

;; bm-count: popcount all words of a layer → total set bits
(defn bm-count [bm layer]
  (let [nw (nth bm BM_IDX_NW)]
    (loop [w 0 c 0]
      (if (= w nw) c
        (recur (inc w)
          (+ c (popcount (load64 (bm-word-ptr bm layer w)))))))))

;; bm-and-count: popcount(layer1 AND layer2) → co-occurrence
(defn bm-and-count [bm l1 l2]
  (let [nw (nth bm BM_IDX_NW)]
    (loop [w 0 c 0]
      (if (= w nw) c
        (recur (inc w)
          (+ c (popcount (bit-and
                  (load64 (bm-word-ptr bm l1 w))
                  (load64 (bm-word-ptr bm l2 w))))))))))

;; bm-depth-at: nesting depth at position pos
;; = popcount(opens before pos) - popcount(closes before pos)
(defn bm-depth-at [bm pos]
  (let [w (/ pos 64)
        b (mod pos 64)
        full (loop [i 0 opens 0 closes 0]
               (if (= i w)
                 (- opens closes)
                 (recur (inc i)
                   (+ opens (popcount (load64 (bm-word-ptr bm BM_OPEN i))))
                   (+ closes (popcount (load64 (bm-word-ptr bm BM_CLOSE i)))))))
        mask (- (bit-shift-left 1 (inc b)) 1)
        ow (bit-and (load64 (bm-word-ptr bm BM_OPEN w)) mask)
        cw (bit-and (load64 (bm-word-ptr bm BM_CLOSE w)) mask)]
    (+ full (- (popcount ow) (popcount cw)))))

;; bm-max-depth: maximum nesting depth in the source
(defn bm-max-depth [bm]
  (let [len (nth bm BM_IDX_LEN)]
    (loop [i 0 d 0 mx 0]
      (if (= i len) mx
        (let [ch (load8 (+ (gram-src-ptr) i))
              nd (cond
                   (or (= ch CH_LPAREN) (= ch CH_LBRACKET) (= ch CH_LBRACE))
                     (inc d)
                   (or (= ch CH_RPAREN) (= ch CH_RBRACKET) (= ch CH_RBRACE))
                     (dec d)
                   1 d)]
          (recur (inc i) nd (if (> nd mx) nd mx)))))))

;; bm-form-count: number of top-level forms (opens at depth 0)
(defn bm-form-count [bm]
  (let [len (nth bm BM_IDX_LEN)]
    (loop [i 0 d 0 c 0]
      (if (= i len) c
        (let [ch (load8 (+ (gram-src-ptr) i))]
          (cond
            (or (= ch CH_LPAREN) (= ch CH_LBRACKET) (= ch CH_LBRACE))
              (recur (inc i) (inc d) (if (= d 0) (inc c) c))
            (or (= ch CH_RPAREN) (= ch CH_RBRACKET) (= ch CH_RBRACE))
              (recur (inc i) (dec d) c)
            1 (recur (inc i) d c)))))))

;; bm-line-count: number of newlines + 1
(defn bm-line-count [bm]
  (inc (bm-count bm BM_NL)))

;; bm-summary: print a summary of the bitmap index
(defn bm-summary [bm]
  (println "Bitmap index:" (nth bm BM_IDX_LEN) "bytes," (nth bm BM_IDX_NW) "words")
  (println "  opens:  " (bm-count bm BM_OPEN))
  (println "  closes: " (bm-count bm BM_CLOSE))
  (println "  ws:     " (bm-count bm BM_WS))
  (println "  digits: " (bm-count bm BM_DIGIT))
  (println "  alpha:  " (bm-count bm BM_ALPHA))
  (println "  quotes: " (bm-count bm BM_QUOTE))
  (println "  struct: " (bm-count bm BM_STRUCT))
  (println "  token:  " (bm-count bm BM_TOKEN))
  (println "  lines:  " (bm-line-count bm))
  (println "  forms:  " (bm-form-count bm))
  (println "  depth:  " (bm-max-depth bm)))
