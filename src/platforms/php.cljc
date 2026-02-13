(ns platforms.php
  "PHP platform mappings.

   Maps abstract Clojure operations to PHP equivalents.
   Used by the PHP emitter to translate portable code to PHP calls.

   Example:
     (string/upper-case s)  ; abstract operation
     strtoupper($s)         ; emitted PHP")

;; ============================================================================
;; Type Predicates
;; ============================================================================

(def type-mappings
  {:string?     'is_string
   :number?     'is_numeric
   :int?        'is_int
   :float?      'is_float
   :bool?       'is_bool
   :nil?        'is_null
   :array?      'is_array
   :fn?         'is_callable
   :object?     'is_object})

;; ============================================================================
;; IO Operations
;; ============================================================================

(def io-mappings
  {:read-file       'file_get_contents
   :write-file      'file_put_contents
   :file-exists?    'file_exists
   :delete-file     'unlink
   :rename-file     'rename
   :make-dir        'mkdir
   :list-dir        'scandir
   :current-dir     'getcwd
   :temp-file       'tempnam})

;; ============================================================================
;; String Operations
;; ============================================================================

(def string-mappings
  {:upper-case      'strtoupper
   :lower-case      'strtolower
   :trim            'trim
   :ltrim           'ltrim
   :rtrim           'rtrim
   :split           'explode
   :join            'implode
   :replace         'str_replace
   :index-of        'strpos
   :last-index-of   'strrpos
   :substring       'substr
   :length          'strlen
   :starts-with?    'str_starts_with
   :ends-with?      'str_ends_with
   :contains?       'str_contains
   :reverse         'strrev
   :repeat          'str_repeat
   :pad-left        'str_pad    ; with STR_PAD_LEFT
   :pad-right       'str_pad})  ; with STR_PAD_RIGHT

;; ============================================================================
;; Math Operations
;; ============================================================================

(def math-mappings
  {:abs             'abs
   :ceil            'ceil
   :floor           'floor
   :round           'round
   :sqrt            'sqrt
   :pow             'pow
   :log             'log
   :exp             'exp
   :sin             'sin
   :cos             'cos
   :tan             'tan
   :min             'min
   :max             'max
   :rand            'mt_rand
   :rand-int        'random_int})

;; ============================================================================
;; Array Operations (for HAMT nodes)
;; ============================================================================

(def array-mappings
  {:make            'array
   :length          'count
   :slice           'array_slice
   :concat          'array_merge
   :push            'array_push
   :pop             'array_pop
   :map             'array_map
   :filter          'array_filter
   :reduce          'array_reduce
   :keys            'array_keys
   :values          'array_values
   :key-exists?     'array_key_exists
   :in-array?       'in_array
   :flip            'array_flip
   :reverse         'array_reverse
   :sort            'sort
   :unique          'array_unique})

;; ============================================================================
;; Time Operations
;; ============================================================================

(def time-mappings
  {:now             'time
   :now-ms          'microtime   ; with true arg → float
   :sleep           'sleep
   :sleep-ms        'usleep      ; microseconds
   :date            'date
   :strtotime       'strtotime
   :mktime          'mktime})

;; ============================================================================
;; System Operations
;; ============================================================================

(def system-mappings
  {:exit            'exit
   :getenv          'getenv
   :setenv          'putenv
   :exec            'exec
   :shell-exec      'shell_exec
   :system          'system
   :passthru        'passthru})

;; ============================================================================
;; Hash Operations
;; ============================================================================

(def hash-mappings
  {:hash            'hash
   :md5             'md5
   :sha1            'sha1
   :sha256          '(fn [s] (hash "sha256" s))
   :crc32           'crc32
   :spl-object-hash 'spl_object_hash})

;; ============================================================================
;; JSON Operations
;; ============================================================================

(def json-mappings
  {:encode          'json_encode
   :decode          'json_decode})

;; ============================================================================
;; Regex Operations
;; ============================================================================

(def regex-mappings
  {:match           'preg_match
   :match-all       'preg_match_all
   :replace         'preg_replace
   :split           'preg_split})

;; ============================================================================
;; Output Operations
;; ============================================================================

(def output-mappings
  {:print           'echo
   :println         'echo       ; emitter adds "\n"
   :pr              'print_r
   :prn             'var_export})

;; ============================================================================
;; Combined Mappings
;; ============================================================================

(def mappings
  "All platform mappings combined.
   Structured as :category/operation → PHP function symbol."
  (merge
   (into {} (map (fn [[k v]] [(keyword "type" (name k)) v]) type-mappings))
   (into {} (map (fn [[k v]] [(keyword "io" (name k)) v]) io-mappings))
   (into {} (map (fn [[k v]] [(keyword "string" (name k)) v]) string-mappings))
   (into {} (map (fn [[k v]] [(keyword "math" (name k)) v]) math-mappings))
   (into {} (map (fn [[k v]] [(keyword "array" (name k)) v]) array-mappings))
   (into {} (map (fn [[k v]] [(keyword "time" (name k)) v]) time-mappings))
   (into {} (map (fn [[k v]] [(keyword "system" (name k)) v]) system-mappings))
   (into {} (map (fn [[k v]] [(keyword "hash" (name k)) v]) hash-mappings))
   (into {} (map (fn [[k v]] [(keyword "json" (name k)) v]) json-mappings))
   (into {} (map (fn [[k v]] [(keyword "regex" (name k)) v]) regex-mappings))
   (into {} (map (fn [[k v]] [(keyword "output" (name k)) v]) output-mappings))))

;; ============================================================================
;; Reverse Mappings (for cross-platform remapping)
;; ============================================================================

(def reverse-mappings
  "Maps PHP function symbols back to abstract operations.
   Used for cross-platform code remapping."
  (into {} (map (fn [[k v]] [v k]) mappings)))

;; ============================================================================
;; Lookup Functions
;; ============================================================================

(defn get-mapping
  "Get the PHP function for an abstract operation.

   (get-mapping :string/upper-case) => 'strtoupper"
  [op]
  (get mappings op))

(defn get-reverse
  "Get the abstract operation for a PHP function.

   (get-reverse 'strtoupper) => :string/upper-case"
  [php-fn]
  (get reverse-mappings php-fn))
