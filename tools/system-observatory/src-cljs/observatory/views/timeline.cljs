(ns observatory.views.timeline
  "Timeline view for trace playback.

   Shows events over time with:
   - Scrubber to move through time
   - Event lanes (CPU, L1, L2, L3, RAM, Disk)
   - Speed control (slow down nanoseconds to seconds)
   - Click events to inspect"
  (:require [re-frame.core :as rf]
            [observatory.state :as state]
            [observatory.trace :as trace]))

;; =============================================================================
;; Time Formatting
;; =============================================================================

(defn format-time
  "Format nanoseconds as human-readable time."
  [ns]
  (cond
    (< ns 1000) (str ns "ns")
    (< ns 1000000) (str (.toFixed (/ ns 1000) 1) "μs")
    (< ns 1000000000) (str (.toFixed (/ ns 1000000) 2) "ms")
    :else (str (.toFixed (/ ns 1000000000) 3) "s")))

;; =============================================================================
;; Playback Controls
;; =============================================================================

(defn playback-controls
  "Transport controls for timeline playback."
  []
  (let [playback @(rf/subscribe [::state/playback])
        metadata @(rf/subscribe [::state/trace-metadata])]
    [:div.playback-controls
     [:div.transport
      [:button.transport-btn {:on-click #(rf/dispatch [::state/set-playback-position 0])}
       "⏮"]
      [:button.transport-btn "⏪"]
      [:button.transport-btn.play
       {:on-click #(rf/dispatch [::state/toggle-playback])}
       (if (:playing? playback) "⏸" "▶")]
      [:button.transport-btn "⏩"]
      [:button.transport-btn
       {:on-click #(rf/dispatch [::state/set-playback-position
                                 (:duration-ns metadata 0)])}
       "⏭"]]

     [:div.time-display
      [:span.current (format-time (:position playback 0))]
      [:span.sep " / "]
      [:span.total (format-time (:duration-ns metadata 0))]]

     [:div.speed-control
      [:span.label "Speed:"]
      [:select.speed-select
       {:value (:speed playback)
        :on-change #(rf/dispatch [::state/set-playback-speed
                                  (js/parseFloat (-> % .-target .-value))])}
       [:option {:value 0.001} "0.001x (ns→ms)"]
       [:option {:value 0.01} "0.01x"]
       [:option {:value 0.1} "0.1x"]
       [:option {:value 1} "1x (real-time)"]
       [:option {:value 10} "10x"]]]]))

;; =============================================================================
;; Scrubber
;; =============================================================================

(defn scrubber
  "Timeline scrubber - drag to move through time."
  []
  (let [playback @(rf/subscribe [::state/playback])
        metadata @(rf/subscribe [::state/trace-metadata])
        position (:position playback 0)
        duration (:duration-ns metadata 1)
        pct (* 100 (/ position duration))]
    [:div.scrubber
     [:div.scrubber-track
      {:on-click (fn [e]
                   (let [rect (-> e .-target .getBoundingClientRect)
                         x (- (.-clientX e) (.-left rect))
                         width (.-width rect)
                         new-pct (/ x width)
                         new-pos (* new-pct duration)]
                     (rf/dispatch [::state/set-playback-position (int new-pos)])))}
      [:div.scrubber-fill {:style {:width (str pct "%")}}]
      [:div.scrubber-handle {:style {:left (str pct "%")}}]]

     [:div.time-markers
      [:span.marker "0"]
      [:span.marker (format-time (/ duration 4))]
      [:span.marker (format-time (/ duration 2))]
      [:span.marker (format-time (* 3 (/ duration 4)))]
      [:span.marker (format-time duration)]]]))

;; =============================================================================
;; Event Lanes
;; =============================================================================

(defn event-marker
  "A single event marker on a lane."
  [{:keys [event position duration on-click]}]
  (let [pct (* 100 (/ (:timestamp event) duration))]
    [:div.event-marker
     {:class [(name (:type event))
              (when (:hit? event) "hit")
              (when-not (:hit? event) "miss")]
      :style {:left (str pct "%")}
      :on-click #(on-click event)
      :title (str (:type event) " @ " (format-time (:timestamp event)))}]))

(defn event-lane
  "A single event lane (e.g., CPU, L1, L2)."
  [{:keys [label events duration on-event-click]}]
  [:div.event-lane
   [:div.lane-label label]
   [:div.lane-track
    (for [[idx event] (map-indexed vector events)]
      ^{:key idx}
      [event-marker {:event event
                     :position (:timestamp event)
                     :duration duration
                     :on-click on-event-click}])]])

(defn event-lanes
  "All event lanes stacked vertically."
  []
  (let [events @(rf/subscribe [::state/trace-events])
        metadata @(rf/subscribe [::state/trace-metadata])
        duration (:duration-ns metadata 1)

        ;; Group events by type/level
        cpu-events (filter #(#{:instruction-sample :context-switch} (:type %)) events)
        l1-events (filter #(and (#{:cache-hit :cache-miss} (:type %))
                                (= :l1 (:level %))) events)
        l2-events (filter #(and (#{:cache-hit :cache-miss} (:type %))
                                (= :l2 (:level %))) events)
        l3-events (filter #(and (#{:cache-hit :cache-miss} (:type %))
                                (= :l3 (:level %))) events)
        ram-events (filter #(#{:page-fault} (:type %)) events)
        disk-events (filter #(#{:disk-read :disk-write} (:type %)) events)

        handle-click (fn [event]
                       (rf/dispatch [::state/select {:type :trace-event :data event}]))]

    [:div.event-lanes
     [event-lane {:label "CPU" :events cpu-events :duration duration
                  :on-event-click handle-click}]
     [event-lane {:label "L1" :events l1-events :duration duration
                  :on-event-click handle-click}]
     [event-lane {:label "L2" :events l2-events :duration duration
                  :on-event-click handle-click}]
     [event-lane {:label "L3" :events l3-events :duration duration
                  :on-event-click handle-click}]
     [event-lane {:label "RAM" :events ram-events :duration duration
                  :on-event-click handle-click}]
     [event-lane {:label "Disk" :events disk-events :duration duration
                  :on-event-click handle-click}]]))

;; =============================================================================
;; Current Event Info
;; =============================================================================

(defn current-event-info
  "Shows info about event at current playback position."
  []
  (let [events @(rf/subscribe [::state/trace-events])
        playback @(rf/subscribe [::state/playback])
        position (:position playback 0)

        ;; Find event nearest to current position
        nearest (->> events
                     (filter #(<= (:timestamp %) position))
                     (sort-by :timestamp >)
                     first)]
    [:div.current-event-info
     (if nearest
       [:div.event-detail
        [:span.event-type (str (:type nearest))]
        [:span.event-time (format-time (:timestamp nearest))]
        (when (:address nearest)
          [:span.event-addr (str "@ 0x" (.toString (:address nearest) 16))])
        (when (:level nearest)
          [:span.event-level (str (:level nearest))])]
       [:div.no-event "No event at this position"])]))

;; =============================================================================
;; Trace Summary
;; =============================================================================

(defn trace-summary
  "Summary statistics for the current trace."
  []
  (let [summary @(rf/subscribe [::state/trace-summary])]
    (when summary
      [:div.trace-summary
       [:div.summary-stat
        [:span.value (:total summary)]
        [:span.label "Events"]]

       (for [[event-type count] (:by-type summary)]
         ^{:key event-type}
         [:div.summary-stat
          [:span.value count]
          [:span.label (name event-type)]])

       (when-let [hit-rate (:hit-rate summary)]
         [:div.summary-stat.highlight
          [:span.value (str (int (* 100 (:hit-rate hit-rate))) "%")]
          [:span.label "Cache Hit Rate"]])])))

;; =============================================================================
;; Main Timeline View
;; =============================================================================

(defn timeline-view
  "Complete timeline view with all components."
  []
  [:div.timeline-view
   [:div.view-header
    [:h2 "Timeline"]
    [:p.hint "Scrub through trace events. Slow down time to see cache behavior."]]

   [trace-summary]

   [:div.timeline-main
    [playback-controls]
    [scrubber]
    [event-lanes]
    [current-event-info]]])
