(ns observatory.views.processes
  "Process list view - the Task Manager surface.

   Shows processes in a sortable table. Click any process to:
   - Open inspector with process details
   - Drill into memory regions, cache behavior, etc.

   This is the entry point for progressive disclosure."
  (:require [re-frame.core :as rf]
            [observatory.state :as state]))

;; =============================================================================
;; Formatting Helpers
;; =============================================================================

(defn format-bytes
  "Format bytes as human-readable string."
  [bytes]
  (cond
    (nil? bytes) "-"
    (< bytes 1024) (str bytes " B")
    (< bytes (* 1024 1024)) (str (int (/ bytes 1024)) " KB")
    (< bytes (* 1024 1024 1024)) (str (int (/ bytes 1024 1024)) " MB")
    :else (str (.toFixed (/ bytes 1024 1024 1024) 1) " GB")))

(defn format-percent
  "Format percentage with one decimal."
  [pct]
  (if pct
    (str (.toFixed pct 1) "%")
    "-"))

;; =============================================================================
;; Process Row
;; =============================================================================

(defn process-row
  "A single process row. Clickable for inspection."
  [{:keys [pid name cpu-percent memory-bytes threads state] :as process}]
  [:tr.process-row
   {:on-click #(rf/dispatch [::state/select {:type :process :data process}])
    :class (when (= state :stopped) "stopped")}

   ;; PID - clickable to drill into
   [:td.pid
    [:button.drill-link
     {:on-click (fn [e]
                  (.stopPropagation e)
                  (rf/dispatch [::state/select {:type :process :data process}]))}
     pid]]

   ;; Name
   [:td.name name]

   ;; CPU - with visual bar
   [:td.cpu
    [:div.metric-cell
     [:div.bar-bg
      [:div.bar-fill.cpu {:style {:width (str (min cpu-percent 100) "%")}}]]
     [:span.value (format-percent cpu-percent)]]]

   ;; Memory - with visual bar
   [:td.memory
    [:div.metric-cell
     [:div.bar-bg
      [:div.bar-fill.memory {:style {:width (str (min (* (/ memory-bytes (* 4 1024 1024 1024)) 100) 100) "%")}}]]
     [:span.value (format-bytes memory-bytes)]]]

   ;; Threads
   [:td.threads threads]

   ;; State indicator
   [:td.state
    [:span.state-badge {:class (cljs.core/name (or state :running))}
     (or state :running)]]])

;; =============================================================================
;; Process Table
;; =============================================================================

(defn sort-indicator
  "Shows sort direction arrow."
  [current-sort column]
  (when (= current-sort column)
    [:span.sort-arrow "▼"]))

(defn process-table
  "Main process table with sortable columns."
  []
  (let [processes @(rf/subscribe [::state/processes-by-cpu])
        ;; Local state for sorting would go here
        current-sort :cpu-percent]
    [:div.process-table-container
     [:table.process-table
      [:thead
       [:tr
        [:th.pid "PID"]
        [:th.name "Name"]
        [:th.cpu {:class "sortable"}
         "CPU" [sort-indicator current-sort :cpu-percent]]
        [:th.memory {:class "sortable"}
         "Memory" [sort-indicator current-sort :memory-bytes]]
        [:th.threads "Threads"]
        [:th.state "State"]]]
      [:tbody
       (for [process processes]
         ^{:key (:pid process)}
         [process-row process])]]]))

;; =============================================================================
;; Summary Stats
;; =============================================================================

(defn summary-stats
  "Overview stats for all processes."
  []
  (let [processes @(rf/subscribe [::state/processes])]
    (when (seq processes)
      (let [total-cpu (reduce + 0 (map :cpu-percent processes))
            total-memory (reduce + 0 (map :memory-bytes processes))
            total-threads (reduce + 0 (map :threads processes))]
        [:div.summary-stats
         [:div.stat-card
          [:div.stat-value (count processes)]
          [:div.stat-label "Processes"]]
         [:div.stat-card
          [:div.stat-value (format-percent total-cpu)]
          [:div.stat-label "Total CPU"]]
         [:div.stat-card
          [:div.stat-value (format-bytes total-memory)]
          [:div.stat-label "Total Memory"]]
         [:div.stat-card
          [:div.stat-value total-threads]
          [:div.stat-label "Total Threads"]]]))))

;; =============================================================================
;; Quick Actions
;; =============================================================================

(defn quick-actions
  "Action bar for process operations."
  []
  [:div.quick-actions
   [:input.search-input
    {:type "text"
     :placeholder "Filter processes..."}]
   [:div.action-buttons
    [:button.btn "Refresh"]
    [:button.btn "Tree View"]]])

;; =============================================================================
;; Main View
;; =============================================================================

(defn processes-view
  "Complete processes view - Task Manager style."
  []
  [:div.processes-view
   [:div.view-header
    [:h2 "Processes"]
    [:p.hint "Click any process to inspect. Drill into memory, cache, I/O."]]
   [quick-actions]
   [summary-stats]
   [process-table]])
