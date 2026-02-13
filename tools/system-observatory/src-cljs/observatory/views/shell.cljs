(ns observatory.views.shell
  "Main application shell - sidebar, content area, inspector panel.

   The shell provides the structure for progressive disclosure:
   - Sidebar: top-level navigation (Processes, Network, Cache, Timeline)
   - Content: current view based on tab and drill path
   - Inspector: SLIME-style panel for deep inspection
   - Breadcrumb: shows current drill path, click to navigate back"
  (:require [re-frame.core :as rf]
            [observatory.state :as state]
            [observatory.views.processes :as processes]
            [observatory.views.cache :as cache-view]
            [observatory.views.inspector :as inspector]
            [observatory.views.timeline :as timeline]))

;; =============================================================================
;; Sidebar
;; =============================================================================

(defn sidebar-item
  "A single sidebar navigation item."
  [{:keys [id icon label active?]}]
  [:button.sidebar-item
   {:class (when active? "active")
    :on-click #(rf/dispatch [::state/set-tab id])}
   [:span.icon icon]
   [:span.label label]])

(defn sidebar
  "Main navigation sidebar."
  []
  (let [current-tab @(rf/subscribe [::state/current-tab])
        open? @(rf/subscribe [::state/sidebar-open?])]
    [:nav.sidebar {:class (when-not open? "collapsed")}
     [:div.sidebar-header
      [:h1.logo "Observatory"]
      [:button.collapse-btn
       {:on-click #(rf/dispatch [::state/toggle-sidebar])}
       (if open? "◀" "▶")]]

     [:div.sidebar-nav
      [sidebar-item {:id :processes
                     :icon "⚙"
                     :label "Processes"
                     :active? (= current-tab :processes)}]
      [sidebar-item {:id :network
                     :icon "🌐"
                     :label "Network"
                     :active? (= current-tab :network)}]
      [sidebar-item {:id :cache
                     :icon "📦"
                     :label "Cache"
                     :active? (= current-tab :cache)}]
      [sidebar-item {:id :memory
                     :icon "🧠"
                     :label "Memory"
                     :active? (= current-tab :memory)}]
      [sidebar-item {:id :disk
                     :icon "💾"
                     :label "Disk"
                     :active? (= current-tab :disk)}]
      [sidebar-item {:id :timeline
                     :icon "⏱"
                     :label "Timeline"
                     :active? (= current-tab :timeline)}]]

     [:div.sidebar-footer
      [:div.mode-indicator
       [:span.dot.simulation] "Simulation Mode"]]]))

;; =============================================================================
;; Breadcrumb Navigation
;; =============================================================================

(defn breadcrumb
  "Shows current drill path and allows navigation back up."
  []
  (let [drill-path @(rf/subscribe [::state/drill-path])]
    [:div.breadcrumb
     [:button.crumb.root
      {:on-click #(rf/dispatch [::state/drill-reset])}
      "🏠 Root"]
     (for [[idx segment] (map-indexed vector drill-path)]
       ^{:key idx}
       [:span.crumb-separator " › "])
     (for [[idx segment] (map-indexed vector drill-path)]
       ^{:key (str "seg-" idx)}
       [:button.crumb
        {:on-click #(rf/dispatch [::state/drill-up])}
        (str segment)])]))

;; =============================================================================
;; Main Content Area
;; =============================================================================

(defn content-panel
  "Main content panel - switches based on current tab."
  []
  (let [current-tab @(rf/subscribe [::state/current-tab])]
    [:main.content-panel
     [breadcrumb]
     [:div.content-body
      (case current-tab
        :processes [processes/processes-view]
        :network   [:div.placeholder "Network connections - coming soon"]
        :cache     [cache-view/cache-view]
        :memory    [:div.placeholder "Memory view - coming soon"]
        :disk      [:div.placeholder "Disk view - coming soon"]
        :timeline  [timeline/timeline-view]
        [:div.placeholder "Select a view from the sidebar"])]]))

;; =============================================================================
;; Status Bar
;; =============================================================================

(defn status-bar
  "Bottom status bar with system stats."
  []
  (let [processes @(rf/subscribe [::state/processes])
        trace-summary @(rf/subscribe [::state/trace-summary])]
    [:footer.status-bar
     [:div.stat
      [:span.label "Processes:"]
      [:span.value (count processes)]]
     [:div.stat
      [:span.label "Events:"]
      [:span.value (or (:total trace-summary) 0)]]
     (when-let [hit-rate (:hit-rate trace-summary)]
       [:div.stat
        [:span.label "Cache Hit Rate:"]
        [:span.value (str (int (* 100 (:hit-rate hit-rate))) "%")]])
     [:div.spacer]
     [:div.stat
      [:button.btn-small
       {:on-click #(rf/dispatch [::state/toggle-inspector])}
       "Toggle Inspector"]]]))

;; =============================================================================
;; Main Shell
;; =============================================================================

(defn main-shell
  "Root component - assembles the full application layout."
  []
  (let [inspector-open? @(rf/subscribe [::state/inspector-open?])]
    [:div.app-shell
     [sidebar]
     [:div.main-area
      [content-panel]
      [status-bar]]
     (when inspector-open?
       [inspector/inspector-panel])]))
