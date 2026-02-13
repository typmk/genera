(ns clojure.browse
  "Portable URL opening in system browser.

   This namespace provides platform-independent browser operations.
   Each platform (JVM, PHP, JS, etc.) implements the protocols
   defined here via clojure.{platform}.browse namespaces.")

;;; ====================================================================
;;; Protocols - backends must implement these
;;; ====================================================================

(defprotocol Browser
  "Open URLs in the system browser.
   Backends must provide an implementation of this protocol."
  (-browse-url [browser url]
    "Open url in the system's default browser.
     Returns true on success, false or throws on failure."))

;;; ====================================================================
;;; Dynamic vars
;;; ====================================================================

;; Platform implementations register their browser here
(def ^:dynamic *browser* nil)

;; Optional: script/command to open URLs (platform-specific)
(def ^:dynamic *open-url-script* nil)

;;; ====================================================================
;;; Portable API
;;; ====================================================================

(defn browse-url
  "Open url in the system's default browser.

   On JVM: Uses java.awt.Desktop or platform commands (open, xdg-open)
   On PHP: Uses platform commands or browser headers
   On JS:  Uses window.open or child_process

   Returns true on success."
  [url]
  (cond
    *browser*
    (-browse-url *browser* url)

    *open-url-script*
    (do
      (require 'clojure.shell)
      (let [sh (resolve 'clojure.shell/sh)
            {:keys [exit]} (sh *open-url-script* (str url))]
        (zero? exit)))

    :else
    (throw (ex-info "No browser registered. Platform must set *browser* or *open-url-script*."
                    {:url url}))))
