(ns observatory.app
  "System Observatory - Main application entry point.

   A unified Task Manager + Hardware Observatory with progressive disclosure.
   Click anything to drill deeper into the system."
  (:require [reagent.dom :as rdom]
            [re-frame.core :as rf]
            [observatory.state :as state]
            [observatory.views.shell :as shell]))

;; =============================================================================
;; App Initialization
;; =============================================================================

(defn ^:dev/after-load mount-root
  "Mount the root component. Called on hot reload."
  []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [shell/main-shell] root-el)))

(defn init
  "Initialize the application."
  []
  (js/console.log "System Observatory starting...")

  ;; Initialize Re-frame
  (rf/dispatch-sync [::state/initialize])

  ;; Load sample data for development
  (rf/dispatch [::state/load-sample-data])

  ;; Mount the UI
  (mount-root)

  (js/console.log "System Observatory ready. Click anything to inspect."))
