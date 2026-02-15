;; plt/platform_php.clj — Clojure → PHP platform mappings
;;
;; Two mapping tables:
;;   1. moss-map: Clojure names → moss\coll function names
;;   2. native-map: Clojure names → PHP native function names
;;
;; Uses str-intern for StrId-based lookup (integer compare, no string alloc).

;; ============================================================================
;; 1. Moss runtime functions (namespace: moss\coll)
;;    These map 1:1 by name. We track them to emit `use function` imports.
;; ============================================================================

;; Persistent data structures
(def SYM_ASSOC    (str-intern "assoc"))
(def SYM_DISSOC   (str-intern "dissoc"))
(def SYM_GET      (str-intern "get"))
(def SYM_UPDATE   (str-intern "update"))
(def SYM_CONJ     (str-intern "conj"))
(def SYM_INTO     (str-intern "into"))
(def SYM_MERGE    (str-intern "merge"))
(def SYM_KEYS     (str-intern "keys"))
(def SYM_VALS     (str-intern "vals"))
(def SYM_CONTAINS (str-intern "contains?"))
(def SYM_PMAP     (str-intern "pmap"))
(def SYM_PVEC     (str-intern "pvec"))
(def SYM_COUNT    (str-intern "count"))
(def SYM_EMPTY    (str-intern "empty?"))

;; Sequences
(def SYM_FIRST    (str-intern "first"))
(def SYM_REST     (str-intern "rest"))
(def SYM_SEQ      (str-intern "seq"))
(def SYM_CONS     (str-intern "cons"))
(def SYM_NTH      (str-intern "nth"))
(def SYM_TAKE     (str-intern "take"))
(def SYM_DROP     (str-intern "drop"))
(def SYM_MAP      (str-intern "map"))
(def SYM_FILTER   (str-intern "filter"))
(def SYM_REDUCE   (str-intern "reduce"))
(def SYM_SOME     (str-intern "some"))
(def SYM_EVERY    (str-intern "every?"))

;; Atoms
(def SYM_ATOM     (str-intern "atom"))
(def SYM_DEREF    (str-intern "deref"))
(def SYM_SWAP     (str-intern "swap"))
(def SYM_RESET    (str-intern "reset"))

;; Higher-order
(def SYM_COMP     (str-intern "comp"))
(def SYM_PARTIAL  (str-intern "partial"))
(def SYM_IDENTITY (str-intern "identity"))
(def SYM_APPLY    (str-intern "apply"))
(def SYM_JUXT     (str-intern "juxt"))

;; sig/dispatch
(def SYM_SEND     (str-intern "send"))
(def SYM_ON       (str-intern "on"))
(def SYM_OFF      (str-intern "off"))
(def SYM_TAP      (str-intern "tap"))
(def SYM_BUS      (str-intern "bus"))
(def SYM_SIG      (str-intern "sig"))

;; String
(def SYM_STR      (str-intern "str"))
(def SYM_PR_STR   (str-intern "pr-str"))

;; Image
(def SYM_IMAGE    (str-intern "image"))
(def SYM_DEFINE   (str-intern "define"))

;; Vectors (pvec ops)
(def SYM_V_GET    (str-intern "v-get"))
(def SYM_V_APPEND (str-intern "v-append"))
(def SYM_V_PUT    (str-intern "v-put"))
(def SYM_V_POP    (str-intern "v-pop"))
(def SYM_V_COUNT  (str-intern "v-count"))
(def SYM_V_FIRST  (str-intern "v-first"))
(def SYM_V_LAST   (str-intern "v-last"))

;; pmap ops (aliased: put=assoc, del=dissoc, has=contains?)
(def SYM_PUT      (str-intern "put"))
(def SYM_DEL      (str-intern "del"))
(def SYM_HAS      (str-intern "has?"))

;; Moss function name lookup: returns PHP name string or nil
;; Most map 1:1 (assoc→assoc), some remap (contains?→has, every?→every)
(defn moss-fn [sym]
  (cond
    ;; Persistent data
    (= sym SYM_ASSOC)    "assoc"
    (= sym SYM_DISSOC)   "dissoc"
    (= sym SYM_GET)      "get"
    (= sym SYM_UPDATE)   "update"
    (= sym SYM_CONJ)     "conj"
    (= sym SYM_INTO)     "into"
    (= sym SYM_MERGE)    "merge"
    (= sym SYM_KEYS)     "keys"
    (= sym SYM_VALS)     "vals"
    (= sym SYM_CONTAINS) "has"
    (= sym SYM_PMAP)     "pmap"
    (= sym SYM_PVEC)     "pvec"
    (= sym SYM_COUNT)    "count"
    (= sym SYM_EMPTY)    "empty_p"
    (= sym SYM_PUT)      "put"
    (= sym SYM_DEL)      "del"
    (= sym SYM_HAS)      "has"
    ;; Sequences
    (= sym SYM_FIRST)    "first"
    (= sym SYM_REST)     "rest"
    (= sym SYM_SEQ)      "seq"
    (= sym SYM_CONS)     "cons"
    (= sym SYM_NTH)      "nth"
    (= sym SYM_TAKE)     "take"
    (= sym SYM_DROP)     "drop"
    (= sym SYM_MAP)      "map"
    (= sym SYM_FILTER)   "filter"
    (= sym SYM_REDUCE)   "reduce"
    (= sym SYM_SOME)     "some"
    (= sym SYM_EVERY)    "every"
    ;; Atoms
    (= sym SYM_ATOM)     "atom"
    (= sym SYM_DEREF)    "deref"
    (= sym SYM_SWAP)     "swap"
    (= sym SYM_RESET)    "reset"
    ;; Higher-order
    (= sym SYM_COMP)     "comp"
    (= sym SYM_PARTIAL)  "partial"
    (= sym SYM_IDENTITY) "identity"
    (= sym SYM_APPLY)    "apply"
    (= sym SYM_JUXT)     "juxt"
    ;; sig
    (= sym SYM_SEND)     "send"
    (= sym SYM_ON)       "on"
    (= sym SYM_OFF)      "off"
    (= sym SYM_TAP)      "tap"
    (= sym SYM_BUS)      "bus"
    (= sym SYM_SIG)      "sig"
    ;; String
    (= sym SYM_STR)      "str"
    (= sym SYM_PR_STR)   "pr_str"
    ;; Image
    (= sym SYM_IMAGE)    "image"
    (= sym SYM_DEFINE)   "define"
    ;; Vectors
    (= sym SYM_V_GET)    "v_get"
    (= sym SYM_V_APPEND) "v_append"
    (= sym SYM_V_PUT)    "v_put"
    (= sym SYM_V_POP)    "v_pop"
    (= sym SYM_V_COUNT)  "v_count"
    (= sym SYM_V_FIRST)  "v_first"
    (= sym SYM_V_LAST)   "v_last"
    :else nil))

;; ============================================================================
;; 2. PHP native functions (no namespace needed)
;;    Clojure idiom → direct PHP builtin
;; ============================================================================

;; Type predicates
(def SYM_NILQ     (str-intern "nil?"))
(def SYM_STRINGQ  (str-intern "string?"))
(def SYM_NUMBERQ  (str-intern "number?"))
(def SYM_INTQ     (str-intern "int?"))
(def SYM_FLOATQ   (str-intern "float?"))
(def SYM_ARRAYQ   (str-intern "array?"))
(def SYM_FNQ      (str-intern "fn?"))

;; String operations
(def SYM_UPPER    (str-intern "upper-case"))
(def SYM_LOWER    (str-intern "lower-case"))
(def SYM_TRIM     (str-intern "trim"))
(def SYM_STRLEN   (str-intern "str-length"))
(def SYM_SUBSTR   (str-intern "subs"))
(def SYM_INDEXOF  (str-intern "index-of"))
(def SYM_STARTW   (str-intern "starts-with?"))
(def SYM_ENDW     (str-intern "ends-with?"))
(def SYM_INCLW    (str-intern "includes?"))
(def SYM_SPLIT    (str-intern "split"))
(def SYM_JOIN     (str-intern "join"))
(def SYM_REPLACE  (str-intern "replace"))
(def SYM_STRREV   (str-intern "reverse"))
(def SYM_REPEAT   (str-intern "str-repeat"))

;; Math
(def SYM_ABS      (str-intern "abs"))
(def SYM_CEIL     (str-intern "ceil"))
(def SYM_FLOOR    (str-intern "floor"))
(def SYM_ROUND    (str-intern "round"))
(def SYM_SQRT     (str-intern "sqrt"))
(def SYM_POW      (str-intern "pow"))
(def SYM_MAXFN    (str-intern "max"))
(def SYM_MINFN    (str-intern "min"))

;; JSON
(def SYM_JSONENC  (str-intern "json-encode"))
(def SYM_JSONDEC  (str-intern "json-decode"))

;; Regex
(def SYM_REMATCH  (str-intern "re-match"))
(def SYM_REMALL   (str-intern "re-match-all"))
(def SYM_REREPL   (str-intern "re-replace"))
(def SYM_RESPLIT  (str-intern "re-split"))

;; IO
(def SYM_SLURP    (str-intern "slurp"))
(def SYM_SPIT     (str-intern "spit"))
(def SYM_FEXISTS  (str-intern "file-exists?"))

;; PHP native lookup: returns PHP function name string or nil
(defn native-fn [sym]
  (cond
    ;; Type predicates
    (= sym SYM_NILQ)     "is_null"
    (= sym SYM_STRINGQ)  "is_string"
    (= sym SYM_NUMBERQ)  "is_numeric"
    (= sym SYM_INTQ)     "is_int"
    (= sym SYM_FLOATQ)   "is_float"
    (= sym SYM_ARRAYQ)   "is_array"
    (= sym SYM_FNQ)      "is_callable"
    ;; String
    (= sym SYM_UPPER)    "strtoupper"
    (= sym SYM_LOWER)    "strtolower"
    (= sym SYM_TRIM)     "trim"
    (= sym SYM_STRLEN)   "strlen"
    (= sym SYM_SUBSTR)   "substr"
    (= sym SYM_INDEXOF)  "strpos"
    (= sym SYM_STARTW)   "str_starts_with"
    (= sym SYM_ENDW)     "str_ends_with"
    (= sym SYM_INCLW)    "str_contains"
    (= sym SYM_SPLIT)    "explode"
    (= sym SYM_JOIN)     "implode"
    (= sym SYM_REPLACE)  "str_replace"
    (= sym SYM_STRREV)   "strrev"
    (= sym SYM_REPEAT)   "str_repeat"
    ;; Math
    (= sym SYM_ABS)      "abs"
    (= sym SYM_CEIL)     "ceil"
    (= sym SYM_FLOOR)    "floor"
    (= sym SYM_ROUND)    "round"
    (= sym SYM_SQRT)     "sqrt"
    (= sym SYM_POW)      "pow"
    (= sym SYM_MAXFN)    "max"
    (= sym SYM_MINFN)    "min"
    ;; JSON
    (= sym SYM_JSONENC)  "json_encode"
    (= sym SYM_JSONDEC)  "json_decode"
    ;; Regex
    (= sym SYM_REMATCH)  "preg_match"
    (= sym SYM_REMALL)   "preg_match_all"
    (= sym SYM_REREPL)   "preg_replace"
    (= sym SYM_RESPLIT)  "preg_split"
    ;; IO
    (= sym SYM_SLURP)    "file_get_contents"
    (= sym SYM_SPIT)     "file_put_contents"
    (= sym SYM_FEXISTS)  "file_exists"
    :else nil))

;; ============================================================================
;; 3. Unified lookup + use-function header
;; ============================================================================

;; Look up a sym for platform mapping. Returns [kind php-name] or nil.
;; kind: "moss" or "native"
(defn platform-lookup [sym]
  (let [m (moss-fn sym)]
    (if m
      (list "moss" m)
      (let [n (native-fn sym)]
        (if n
          (list "native" n)
          nil)))))

;; Emit the use-function imports for moss\coll
(defn emit-php-imports []
  (out-emit "use function moss\coll\{")
  (out-nl)
  (out-emit "    pmap, pvec, atom, deref, swap, reset,")
  (out-nl)
  (out-emit "    get, put, del, has, assoc, dissoc, update, merge, keys, vals, count,")
  (out-nl)
  (out-emit "    each, to_array, puts,")
  (out-nl)
  (out-emit "    v_get, v_append, v_put, v_pop, v_count, v_first, v_last, v_to_array,")
  (out-nl)
  (out-emit "    sig, on, off, send, tap, bus,")
  (out-nl)
  (out-emit "    image")
  (out-nl)
  (out-emit "};")
  (out-nl)
  (out-nl))
