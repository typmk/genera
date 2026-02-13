(ns observatory.views.inspector
  "SLIME-style inspector panel.

   The inspector shows the currently selected object with:
   - Summary header
   - Expandable fields (click to drill deeper)
   - Raw data view
   - Actions (view in cache, track accesses, etc.)

   Everything is data. Click any field to make it the new selection."
  (:require [re-frame.core :as rf]
            [observatory.state :as state]
            [clojure.string :as str]))

;; =============================================================================
;; Formatting
;; =============================================================================

(defn format-value
  "Format a value for display based on its type."
  [v]
  (cond
    (nil? v) [:span.nil "nil"]
    (boolean? v) [:span.boolean (str v)]
    (number? v) (if (> v 0xFFFF)
                  [:span.hex (str "0x" (.toString v 16))]
                  [:span.number (str v)])
    (string? v) [:span.string (pr-str v)]
    (keyword? v) [:span.keyword (str v)]
    (vector? v) [:span.collection (str "[" (count v) " items]")]
    (map? v) [:span.collection (str "{" (count v) " keys}")]
    (seq? v) [:span.collection (str "(" (count v) " items)")]
    :else [:span.unknown (pr-str v)]))

(defn truncate
  "Truncate string to max length."
  [s max-len]
  (if (> (count s) max-len)
    (str (subs s 0 (- max-len 3)) "...")
    s))

;; =============================================================================
;; Field Rendering
;; =============================================================================

(defn inspectable?
  "Check if a value can be drilled into."
  [v]
  (or (map? v) (vector? v) (seq? v)))

(defn field-row
  "A single field in the inspector."
  [{:keys [key value depth on-expand]}]
  (let [can-expand? (inspectable? value)
        indent (* depth 16)]
    [:div.field-row {:style {:padding-left (str indent "px")}}
     [:span.field-key (str (name key))]
     [:span.field-sep ": "]
     (if can-expand?
       [:button.field-value.expandable
        {:on-click #(when on-expand (on-expand key value))}
        [format-value value]
        [:span.expand-hint " ▶"]]
       [:span.field-value [format-value value]])]))

(defn map-fields
  "Render all fields of a map."
  [{:keys [data depth on-select max-fields]
    :or {depth 0 max-fields 50}}]
  (let [entries (take max-fields (seq data))
        remaining (- (count data) max-fields)]
    [:div.map-fields
     (for [[k v] entries]
       ^{:key (str k)}
       [field-row {:key k
                   :value v
                   :depth depth
                   :on-expand (fn [k v]
                                (when on-select
                                  (on-select {:type (cond
                                                      (map? v) :map
                                                      (vector? v) :vector
                                                      :else :value)
                                              :key k
                                              :data v})))}])
     (when (pos? remaining)
       [:div.truncated {:style {:padding-left (str (* depth 16) "px")}}
        (str "... and " remaining " more fields")])]))

;; =============================================================================
;; Type-Specific Inspectors
;; =============================================================================

(defmulti inspect-object
  "Render inspector content based on selection type."
  (fn [selection] (:type selection)))

(defmethod inspect-object :process
  [{:keys [data]}]
  [:div.inspector-content
   [:div.inspector-header
    [:h3.object-type "Process"]
    [:h2.object-title (:name data)]
    [:div.object-id "PID: " (:pid data)]]

   [:div.inspector-section
    [:h4 "Overview"]
    [:div.overview-grid
     [:div.overview-item
      [:span.label "CPU"]
      [:span.value (.toFixed (:cpu-percent data) 1) "%"]]
     [:div.overview-item
      [:span.label "Memory"]
      [:span.value (int (/ (:memory-bytes data) 1024 1024)) " MB"]]
     [:div.overview-item
      [:span.label "Threads"]
      [:span.value (:threads data)]]
     [:div.overview-item
      [:span.label "State"]
      [:span.value (str (:state data))]]]]

   [:div.inspector-section
    [:h4 "All Fields"]
    [map-fields {:data data}]]

   [:div.inspector-section
    [:h4 "Actions"]
    [:div.action-buttons
     [:button.action-btn "View Memory Regions"]
     [:button.action-btn "View Cache Behavior"]
     [:button.action-btn "View I/O Activity"]
     [:button.action-btn "Attach Trace"]]]])

(defmethod inspect-object :cache-line
  [{:keys [data]}]
  [:div.inspector-content
   [:div.inspector-header
    [:h3.object-type "Cache Line"]
    [:h2.object-title (str "0x" (.toString (:tag data) 16))]
    [:div.object-id "Set " (:set-index data) ", Way " (:way-index data)]]

   [:div.inspector-section
    [:h4 "State"]
    [:div.cache-line-state
     [:span.mesi-badge {:class (name (:state data))}
      (str/upper-case (name (:state data)))]
     [:span.lru "LRU: " (:lru-position data)]]]

   [:div.inspector-section
    [:h4 "Data (64 bytes)"]
    [:div.hex-view
     ;; Would render hex dump here
     [:pre.hex-dump "00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F\n10 11 12 13 14 15 16 17  18 19 1A 1B 1C 1D 1E 1F\n..."]]]

   [:div.inspector-section
    [:h4 "Actions"]
    [:div.action-buttons
     [:button.action-btn "View in RAM"]
     [:button.action-btn "Track Accesses"]
     [:button.action-btn "Decode as Instructions"]]]])

(defmethod inspect-object :map
  [{:keys [key data]}]
  [:div.inspector-content
   [:div.inspector-header
    [:h3.object-type "Map"]
    [:h2.object-title (str key)]
    [:div.object-id (count data) " keys"]]

   [:div.inspector-section
    [:h4 "Fields"]
    [map-fields {:data data}]]])

(defmethod inspect-object :vector
  [{:keys [key data]}]
  [:div.inspector-content
   [:div.inspector-header
    [:h3.object-type "Vector"]
    [:h2.object-title (str key)]
    [:div.object-id (count data) " items"]]

   [:div.inspector-section
    [:h4 "Items"]
    [:div.vector-items
     (for [[idx item] (take 100 (map-indexed vector data))]
       ^{:key idx}
       [:div.vector-item
        [:span.index idx]
        [:span.value [format-value item]]])]]])

(defmethod inspect-object :default
  [{:keys [type data]}]
  [:div.inspector-content
   [:div.inspector-header
    [:h3.object-type (str type)]
    [:h2.object-title "Object"]]

   [:div.inspector-section
    [:h4 "Data"]
    [:pre.raw-data (pr-str data)]]])

;; =============================================================================
;; Inspector Panel
;; =============================================================================

(defn inspector-panel
  "The main inspector panel - slides in from the right."
  []
  (let [selection @(rf/subscribe [::state/selection])]
    [:aside.inspector-panel
     [:div.inspector-toolbar
      [:button.close-btn
       {:on-click #(rf/dispatch [::state/toggle-inspector])}
       "✕"]
      [:button.back-btn
       {:on-click #(rf/dispatch [::state/drill-up])}
       "← Back"]
      [:span.toolbar-title "Inspector"]]

     (if selection
       [inspect-object selection]
       [:div.no-selection
        [:p "Click any object to inspect it."]
        [:p.hint "Processes, cache lines, memory regions - everything is explorable."]])]))
