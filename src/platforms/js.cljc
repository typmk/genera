(ns platforms.js
  "JavaScript platform mappings.

   Maps abstract Clojure operations to JavaScript equivalents.
   Used by the JS emitter to translate portable code to JS calls.

   Note: Some JS operations are methods (.method) rather than functions.
   The emitter handles this distinction.

   Example:
     (string/upper-case s)  ; abstract operation
     s.toUpperCase()        ; emitted JS")

;; ============================================================================
;; Type Predicates
;; ============================================================================

(def type-mappings
  {:string?     '(fn [x] (=== (typeof x) "string"))
   :number?     '(fn [x] (=== (typeof x) "number"))
   :int?        'Number.isInteger
   :float?      '(fn [x] (and (=== (typeof x) "number") (not (Number.isInteger x))))
   :bool?       '(fn [x] (=== (typeof x) "boolean"))
   :nil?        '(fn [x] (=== x null))
   :array?      'Array.isArray
   :fn?         '(fn [x] (=== (typeof x) "function"))
   :object?     '(fn [x] (=== (typeof x) "object"))})

;; ============================================================================
;; IO Operations (Node.js)
;; ============================================================================

(def io-mappings
  {:read-file       'fs.readFileSync
   :write-file      'fs.writeFileSync
   :file-exists?    'fs.existsSync
   :delete-file     'fs.unlinkSync
   :rename-file     'fs.renameSync
   :make-dir        'fs.mkdirSync
   :list-dir        'fs.readdirSync
   :current-dir     'process.cwd
   :temp-file       'os.tmpdir})

;; ============================================================================
;; String Operations (methods - emitter handles .method syntax)
;; ============================================================================

(def string-mappings
  {:upper-case      '.toUpperCase
   :lower-case      '.toLowerCase
   :trim            '.trim
   :ltrim           '.trimStart
   :rtrim           '.trimEnd
   :split           '.split
   :join            '.join        ; on array
   :replace         '.replace
   :replace-all     '.replaceAll
   :index-of        '.indexOf
   :last-index-of   '.lastIndexOf
   :substring       '.substring
   :slice           '.slice
   :length          '.-length     ; property
   :starts-with?    '.startsWith
   :ends-with?      '.endsWith
   :includes?       '.includes
   :repeat          '.repeat
   :pad-start       '.padStart
   :pad-end         '.padEnd
   :char-at         '.charAt
   :char-code-at    '.charCodeAt})

;; ============================================================================
;; Math Operations
;; ============================================================================

(def math-mappings
  {:abs             'Math.abs
   :ceil            'Math.ceil
   :floor           'Math.floor
   :round           'Math.round
   :sqrt            'Math.sqrt
   :pow             'Math.pow
   :log             'Math.log
   :exp             'Math.exp
   :sin             'Math.sin
   :cos             'Math.cos
   :tan             'Math.tan
   :min             'Math.min
   :max             'Math.max
   :rand            'Math.random
   :rand-int        '(fn [n] (Math.floor (* (Math.random) n)))})

;; ============================================================================
;; Array Operations
;; ============================================================================

(def array-mappings
  {:make            'Array
   :length          '.-length
   :slice           '.slice
   :concat          '.concat
   :push            '.push
   :pop             '.pop
   :shift           '.shift
   :unshift         '.unshift
   :map             '.map
   :filter          '.filter
   :reduce          '.reduce
   :find            '.find
   :find-index      '.findIndex
   :every           '.every
   :some            '.some
   :includes?       '.includes
   :index-of        '.indexOf
   :reverse         '.reverse
   :sort            '.sort
   :flat            '.flat
   :flat-map        '.flatMap})

;; ============================================================================
;; Time Operations
;; ============================================================================

(def time-mappings
  {:now             '(fn [] (Math.floor (/ (Date.now) 1000)))
   :now-ms          'Date.now
   :sleep           'setTimeout        ; async
   :sleep-ms        'setTimeout})

;; ============================================================================
;; System Operations (Node.js)
;; ============================================================================

(def system-mappings
  {:exit            'process.exit
   :getenv          '(fn [k] (aget process.env k))
   :setenv          '(fn [k v] (aset process.env k v))
   :exec            'child_process.execSync
   :spawn           'child_process.spawn})

;; ============================================================================
;; Hash Operations (crypto module)
;; ============================================================================

(def hash-mappings
  {:hash            '(fn [alg s] (.digest (.update (crypto.createHash alg) s) "hex"))
   :md5             '(fn [s] (.digest (.update (crypto.createHash "md5") s) "hex"))
   :sha1            '(fn [s] (.digest (.update (crypto.createHash "sha1") s) "hex"))
   :sha256          '(fn [s] (.digest (.update (crypto.createHash "sha256") s) "hex"))})

;; ============================================================================
;; JSON Operations
;; ============================================================================

(def json-mappings
  {:encode          'JSON.stringify
   :decode          'JSON.parse})

;; ============================================================================
;; Regex Operations (methods)
;; ============================================================================

(def regex-mappings
  {:match           '.match
   :match-all       '.matchAll
   :replace         '.replace
   :split           '.split
   :test            '.test})

;; ============================================================================
;; Output Operations
;; ============================================================================

(def output-mappings
  {:print           'console.log
   :println         'console.log
   :pr              'console.log
   :prn             'console.dir})

;; ============================================================================
;; Combined Mappings
;; ============================================================================

(def mappings
  "All platform mappings combined.
   Structured as :category/operation → JS function/method symbol."
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
;; Method vs Function Helpers
;; ============================================================================

(defn method?
  "Returns true if the mapping is a method (starts with . or .-)"
  [mapping]
  (when (symbol? mapping)
    (let [n (name mapping)]
      (or (.startsWith n ".")
          (.startsWith n ".-")))))

(defn get-mapping
  "Get the JS function/method for an abstract operation.

   (get-mapping :string/upper-case) => '.toUpperCase"
  [op]
  (get mappings op))
