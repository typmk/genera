(ns clojure.doc
  "Portable documentation lookup and browsing.

   This namespace provides platform-independent documentation access.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.doc namespaces.

   Platform examples:
     clojure.java.javadoc - Java API docs
     clojure.php.phpdoc   - PHP documentation
     clojure.js.mdn       - MDN Web Docs")

;;; ====================================================================
;;; Protocols - backends must implement these
;;; ====================================================================

(defprotocol DocProvider
  "Provide documentation URLs for symbols and types.
   Backends implement this for their platform's documentation."
  (-doc-url [provider x]
    "Return documentation URL for x (symbol, class, var, etc.), or nil if unknown.")
  (-doc-name [provider]
    "Return the name of this doc provider (e.g. \"Javadoc\", \"PHPDoc\")"))

;;; ====================================================================
;;; Dynamic vars
;;; ====================================================================

;; Registered doc providers (vector, searched in order)
(def ^:dynamic *doc-providers* [])

;; Web search fallback URL pattern (use %s for query)
(def ^:dynamic *feeling-lucky-url*
  "http://www.google.com/search?btnI=I%27m%20Feeling%20Lucky&q=allinurl:")

;; Whether to fall back to web search
(def ^:dynamic *feeling-lucky* true)

;;; ====================================================================
;;; Provider registry
;;; ====================================================================

(defn register-doc-provider!
  "Register a doc provider. Providers are searched in registration order."
  [provider]
  (alter-var-root #'*doc-providers* conj provider))

(defn clear-doc-providers!
  "Clear all registered doc providers."
  []
  (alter-var-root #'*doc-providers* (constantly [])))

;;; ====================================================================
;;; Portable API
;;; ====================================================================

(defn doc-url
  "Return documentation URL for x, or nil if not found.
   Searches registered providers in order."
  [x]
  (some #(-doc-url % x) *doc-providers*))

(defn browse-doc
  "Open documentation for x in the system browser.

   Searches registered doc providers for a URL. If none found and
   *feeling-lucky* is true, falls back to a web search.

   Returns true on success."
  [x]
  (require 'clojure.browse)
  (let [browse-url (resolve 'clojure.browse/browse-url)
        url (or (doc-url x)
                (when *feeling-lucky*
                  (str *feeling-lucky-url* (str x))))]
    (if url
      (do (browse-url url) true)
      (do (println "No documentation found for:" x) false))))

;;; ====================================================================
;;; Convenience macros (like clojure.java.javadoc/javadoc)
;;; ====================================================================

(defmacro browse
  "Open documentation for the given symbol in the browser.
   Symbol is not evaluated.

   Usage: (clojure.doc/browse some-fn)
          (clojure.doc/browse SomeClass)"
  [sym]
  `(browse-doc '~sym))
