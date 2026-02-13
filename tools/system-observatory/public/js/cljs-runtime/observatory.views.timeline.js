goog.provide('observatory.views.timeline');
/**
 * Format nanoseconds as human-readable time.
 */
observatory.views.timeline.format_time = (function observatory$views$timeline$format_time(ns){
if((ns < (1000))){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(ns),"ns"].join('');
} else {
if((ns < (1000000))){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1((ns / (1000)).toFixed((1))),"\u03BCs"].join('');
} else {
if((ns < (1000000000))){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1((ns / (1000000)).toFixed((2))),"ms"].join('');
} else {
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1((ns / (1000000000)).toFixed((3))),"s"].join('');

}
}
}
});
/**
 * Transport controls for timeline playback.
 */
observatory.views.timeline.playback_controls = (function observatory$views$timeline$playback_controls(){
var playback = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","playback","observatory.state/playback",120493633)], null)));
var metadata = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-metadata","observatory.state/trace-metadata",1553110044)], null)));
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.playback-controls","div.playback-controls",356648064),new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.transport","div.transport",1683568747),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.transport-btn","button.transport-btn",-1563384452),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","set-playback-position","observatory.state/set-playback-position",720664023),(0)], null));
})], null),"\u23EE"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.transport-btn","button.transport-btn",-1563384452),"\u23EA"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.transport-btn.play","button.transport-btn.play",809184695),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","toggle-playback","observatory.state/toggle-playback",1627720313)], null));
})], null),(cljs.core.truth_(new cljs.core.Keyword(null,"playing?","playing?",-1884542863).cljs$core$IFn$_invoke$arity$1(playback))?"\u23F8":"\u25B6")], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.transport-btn","button.transport-btn",-1563384452),"\u23E9"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.transport-btn","button.transport-btn",-1563384452),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","set-playback-position","observatory.state/set-playback-position",720664023),new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911).cljs$core$IFn$_invoke$arity$2(metadata,(0))], null));
})], null),"\u23ED"], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.time-display","div.time-display",-1533822626),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.current","span.current",530473873),observatory.views.timeline.format_time(new cljs.core.Keyword(null,"position","position",-2011731912).cljs$core$IFn$_invoke$arity$2(playback,(0)))], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.sep","span.sep",-303070340)," / "], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.total","span.total",614879921),observatory.views.timeline.format_time(new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911).cljs$core$IFn$_invoke$arity$2(metadata,(0)))], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.speed-control","div.speed-control",944976301),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Speed:"], null),new cljs.core.PersistentVector(null, 7, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"select.speed-select","select.speed-select",-692684148),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"value","value",305978217),new cljs.core.Keyword(null,"speed","speed",1257663751).cljs$core$IFn$_invoke$arity$1(playback),new cljs.core.Keyword(null,"on-change","on-change",-732046149),(function (p1__12926_SHARP_){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","set-playback-speed","observatory.state/set-playback-speed",-1990522384),parseFloat(p1__12926_SHARP_.target.value)], null));
})], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"value","value",305978217),0.001], null),"0.001x (ns\u2192ms)"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"value","value",305978217),0.01], null),"0.01x"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"value","value",305978217),0.1], null),"0.1x"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"value","value",305978217),(1)], null),"1x (real-time)"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"value","value",305978217),(10)], null),"10x"], null)], null)], null)], null);
});
/**
 * Timeline scrubber - drag to move through time.
 */
observatory.views.timeline.scrubber = (function observatory$views$timeline$scrubber(){
var playback = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","playback","observatory.state/playback",120493633)], null)));
var metadata = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-metadata","observatory.state/trace-metadata",1553110044)], null)));
var position = new cljs.core.Keyword(null,"position","position",-2011731912).cljs$core$IFn$_invoke$arity$2(playback,(0));
var duration = new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911).cljs$core$IFn$_invoke$arity$2(metadata,(1));
var pct = ((100) * (position / duration));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.scrubber","div.scrubber",803704190),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.scrubber-track","div.scrubber-track",-406360063),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (e){
var rect = e.target.getBoundingClientRect();
var x = (e.clientX - rect.left);
var width = rect.width;
var new_pct = (x / width);
var new_pos = (new_pct * duration);
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","set-playback-position","observatory.state/set-playback-position",720664023),(new_pos | (0))], null));
})], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.scrubber-fill","div.scrubber-fill",-972567435),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"width","width",-384071477),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(pct),"%"].join('')], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.scrubber-handle","div.scrubber-handle",-1605840461),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"left","left",-399115937),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(pct),"%"].join('')], null)], null)], null)], null),new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.time-markers","div.time-markers",-1743007944),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.marker","span.marker",-1293851844),"0"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.marker","span.marker",-1293851844),observatory.views.timeline.format_time((duration / (4)))], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.marker","span.marker",-1293851844),observatory.views.timeline.format_time((duration / (2)))], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.marker","span.marker",-1293851844),observatory.views.timeline.format_time(((3) * (duration / (4))))], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.marker","span.marker",-1293851844),observatory.views.timeline.format_time(duration)], null)], null)], null);
});
/**
 * A single event marker on a lane.
 */
observatory.views.timeline.event_marker = (function observatory$views$timeline$event_marker(p__12927){
var map__12928 = p__12927;
var map__12928__$1 = cljs.core.__destructure_map(map__12928);
var event = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12928__$1,new cljs.core.Keyword(null,"event","event",301435442));
var position = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12928__$1,new cljs.core.Keyword(null,"position","position",-2011731912));
var duration = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12928__$1,new cljs.core.Keyword(null,"duration","duration",1444101068));
var on_click = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12928__$1,new cljs.core.Keyword(null,"on-click","on-click",1632826543));
var pct = ((100) * (new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(event) / duration));
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.event-marker","div.event-marker",-48739539),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"class","class",-2030961996),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.name(new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(event)),(cljs.core.truth_(new cljs.core.Keyword(null,"hit?","hit?",-364237458).cljs$core$IFn$_invoke$arity$1(event))?"hit":null),(cljs.core.truth_(new cljs.core.Keyword(null,"hit?","hit?",-364237458).cljs$core$IFn$_invoke$arity$1(event))?null:"miss")], null),new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"left","left",-399115937),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(pct),"%"].join('')], null),new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return (on_click.cljs$core$IFn$_invoke$arity$1 ? on_click.cljs$core$IFn$_invoke$arity$1(event) : on_click.call(null,event));
}),new cljs.core.Keyword(null,"title","title",636505583),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(event))," @ ",observatory.views.timeline.format_time(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(event))].join('')], null)], null);
});
/**
 * A single event lane (e.g., CPU, L1, L2).
 */
observatory.views.timeline.event_lane = (function observatory$views$timeline$event_lane(p__12929){
var map__12930 = p__12929;
var map__12930__$1 = cljs.core.__destructure_map(map__12930);
var label = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12930__$1,new cljs.core.Keyword(null,"label","label",1718410804));
var events = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12930__$1,new cljs.core.Keyword(null,"events","events",1792552201));
var duration = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12930__$1,new cljs.core.Keyword(null,"duration","duration",1444101068));
var on_event_click = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12930__$1,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.event-lane","div.event-lane",951791407),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.lane-label","div.lane-label",1178900693),label], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.lane-track","div.lane-track",738713531),(function (){var iter__5480__auto__ = (function observatory$views$timeline$event_lane_$_iter__12931(s__12932){
return (new cljs.core.LazySeq(null,(function (){
var s__12932__$1 = s__12932;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12932__$1);
if(temp__5804__auto__){
var s__12932__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12932__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12932__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12934 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12933 = (0);
while(true){
if((i__12933 < size__5479__auto__)){
var vec__12935 = cljs.core._nth(c__5478__auto__,i__12933);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12935,(0),null);
var event = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12935,(1),null);
cljs.core.chunk_append(b__12934,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_marker,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"event","event",301435442),event,new cljs.core.Keyword(null,"position","position",-2011731912),new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(event),new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-click","on-click",1632826543),on_event_click], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)));

var G__12970 = (i__12933 + (1));
i__12933 = G__12970;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12934),observatory$views$timeline$event_lane_$_iter__12931(cljs.core.chunk_rest(s__12932__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12934),null);
}
} else {
var vec__12938 = cljs.core.first(s__12932__$2);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12938,(0),null);
var event = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12938,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_marker,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"event","event",301435442),event,new cljs.core.Keyword(null,"position","position",-2011731912),new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(event),new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-click","on-click",1632826543),on_event_click], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)),observatory$views$timeline$event_lane_$_iter__12931(cljs.core.rest(s__12932__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2(cljs.core.vector,events));
})()], null)], null);
});
/**
 * All event lanes stacked vertically.
 */
observatory.views.timeline.event_lanes = (function observatory$views$timeline$event_lanes(){
var events = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-events","observatory.state/trace-events",446112557)], null)));
var metadata = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-metadata","observatory.state/trace-metadata",1553110044)], null)));
var duration = new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911).cljs$core$IFn$_invoke$arity$2(metadata,(1));
var cpu_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12941_SHARP_){
var G__12948 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12941_SHARP_);
var fexpr__12947 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"context-switch","context-switch",-1077083135),null,new cljs.core.Keyword(null,"instruction-sample","instruction-sample",-1459445273),null], null), null);
return (fexpr__12947.cljs$core$IFn$_invoke$arity$1 ? fexpr__12947.cljs$core$IFn$_invoke$arity$1(G__12948) : fexpr__12947.call(null,G__12948));
}),events);
var l1_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12942_SHARP_){
var and__5000__auto__ = (function (){var G__12950 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12942_SHARP_);
var fexpr__12949 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),null,new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),null], null), null);
return (fexpr__12949.cljs$core$IFn$_invoke$arity$1 ? fexpr__12949.cljs$core$IFn$_invoke$arity$1(G__12950) : fexpr__12949.call(null,G__12950));
})();
if(cljs.core.truth_(and__5000__auto__)){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(p1__12942_SHARP_));
} else {
return and__5000__auto__;
}
}),events);
var l2_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12943_SHARP_){
var and__5000__auto__ = (function (){var G__12952 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12943_SHARP_);
var fexpr__12951 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),null,new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),null], null), null);
return (fexpr__12951.cljs$core$IFn$_invoke$arity$1 ? fexpr__12951.cljs$core$IFn$_invoke$arity$1(G__12952) : fexpr__12951.call(null,G__12952));
})();
if(cljs.core.truth_(and__5000__auto__)){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"l2","l2",-462426027),new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(p1__12943_SHARP_));
} else {
return and__5000__auto__;
}
}),events);
var l3_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12944_SHARP_){
var and__5000__auto__ = (function (){var G__12954 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12944_SHARP_);
var fexpr__12953 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),null,new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),null], null), null);
return (fexpr__12953.cljs$core$IFn$_invoke$arity$1 ? fexpr__12953.cljs$core$IFn$_invoke$arity$1(G__12954) : fexpr__12953.call(null,G__12954));
})();
if(cljs.core.truth_(and__5000__auto__)){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"l3","l3",1415952440),new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(p1__12944_SHARP_));
} else {
return and__5000__auto__;
}
}),events);
var ram_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12945_SHARP_){
var G__12956 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12945_SHARP_);
var fexpr__12955 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"page-fault","page-fault",-1895673594),null], null), null);
return (fexpr__12955.cljs$core$IFn$_invoke$arity$1 ? fexpr__12955.cljs$core$IFn$_invoke$arity$1(G__12956) : fexpr__12955.call(null,G__12956));
}),events);
var disk_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12946_SHARP_){
var G__12958 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12946_SHARP_);
var fexpr__12957 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651),null,new cljs.core.Keyword(null,"disk-write","disk-write",33697799),null], null), null);
return (fexpr__12957.cljs$core$IFn$_invoke$arity$1 ? fexpr__12957.cljs$core$IFn$_invoke$arity$1(G__12958) : fexpr__12957.call(null,G__12958));
}),events);
var handle_click = (function (event){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","select","observatory.state/select",-638815980),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"trace-event","trace-event",-546884369),new cljs.core.Keyword(null,"data","data",-232669377),event], null)], null));
});
return new cljs.core.PersistentVector(null, 7, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.event-lanes","div.event-lanes",620635138),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lane,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"label","label",1718410804),"CPU",new cljs.core.Keyword(null,"events","events",1792552201),cpu_events,new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583),handle_click], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lane,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"label","label",1718410804),"L1",new cljs.core.Keyword(null,"events","events",1792552201),l1_events,new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583),handle_click], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lane,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"label","label",1718410804),"L2",new cljs.core.Keyword(null,"events","events",1792552201),l2_events,new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583),handle_click], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lane,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"label","label",1718410804),"L3",new cljs.core.Keyword(null,"events","events",1792552201),l3_events,new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583),handle_click], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lane,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"label","label",1718410804),"RAM",new cljs.core.Keyword(null,"events","events",1792552201),ram_events,new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583),handle_click], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lane,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"label","label",1718410804),"Disk",new cljs.core.Keyword(null,"events","events",1792552201),disk_events,new cljs.core.Keyword(null,"duration","duration",1444101068),duration,new cljs.core.Keyword(null,"on-event-click","on-event-click",1620373583),handle_click], null)], null)], null);
});
/**
 * Shows info about event at current playback position.
 */
observatory.views.timeline.current_event_info = (function observatory$views$timeline$current_event_info(){
var events = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-events","observatory.state/trace-events",446112557)], null)));
var playback = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","playback","observatory.state/playback",120493633)], null)));
var position = new cljs.core.Keyword(null,"position","position",-2011731912).cljs$core$IFn$_invoke$arity$2(playback,(0));
var nearest = cljs.core.first(cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"timestamp","timestamp",579478971),cljs.core._GT_,cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12959_SHARP_){
return (new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(p1__12959_SHARP_) <= position);
}),events)));
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.current-event-info","div.current-event-info",-1248707908),(cljs.core.truth_(nearest)?new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.event-detail","div.event-detail",-972902930),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.event-type","span.event-type",1811695998),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(nearest))], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.event-time","span.event-time",1080475017),observatory.views.timeline.format_time(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(nearest))], null),(cljs.core.truth_(new cljs.core.Keyword(null,"address","address",559499426).cljs$core$IFn$_invoke$arity$1(nearest))?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.event-addr","span.event-addr",-1566051390),["@ 0x",cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"address","address",559499426).cljs$core$IFn$_invoke$arity$1(nearest).toString((16)))].join('')], null):null),(cljs.core.truth_(new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(nearest))?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.event-level","span.event-level",-636819819),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(nearest))], null):null)], null):new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.no-event","div.no-event",-326639016),"No event at this position"], null))], null);
});
/**
 * Summary statistics for the current trace.
 */
observatory.views.timeline.trace_summary = (function observatory$views$timeline$trace_summary(){
var summary = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-summary","observatory.state/trace-summary",-1106632419)], null)));
if(cljs.core.truth_(summary)){
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.trace-summary","div.trace-summary",-1370384960),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.summary-stat","div.summary-stat",-966440128),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.Keyword(null,"total","total",1916810418).cljs$core$IFn$_invoke$arity$1(summary)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Events"], null)], null),(function (){var iter__5480__auto__ = (function observatory$views$timeline$trace_summary_$_iter__12960(s__12961){
return (new cljs.core.LazySeq(null,(function (){
var s__12961__$1 = s__12961;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12961__$1);
if(temp__5804__auto__){
var s__12961__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12961__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12961__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12963 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12962 = (0);
while(true){
if((i__12962 < size__5479__auto__)){
var vec__12964 = cljs.core._nth(c__5478__auto__,i__12962);
var event_type = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12964,(0),null);
var count = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12964,(1),null);
cljs.core.chunk_append(b__12963,cljs.core.with_meta(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.summary-stat","div.summary-stat",-966440128),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),count], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),cljs.core.name(event_type)], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),event_type], null)));

var G__12971 = (i__12962 + (1));
i__12962 = G__12971;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12963),observatory$views$timeline$trace_summary_$_iter__12960(cljs.core.chunk_rest(s__12961__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12963),null);
}
} else {
var vec__12967 = cljs.core.first(s__12961__$2);
var event_type = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12967,(0),null);
var count = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12967,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.summary-stat","div.summary-stat",-966440128),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),count], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),cljs.core.name(event_type)], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),event_type], null)),observatory$views$timeline$trace_summary_$_iter__12960(cljs.core.rest(s__12961__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(new cljs.core.Keyword(null,"by-type","by-type",460217689).cljs$core$IFn$_invoke$arity$1(summary));
})(),(function (){var temp__5804__auto__ = new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451).cljs$core$IFn$_invoke$arity$1(summary);
if(cljs.core.truth_(temp__5804__auto__)){
var hit_rate = temp__5804__auto__;
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.summary-stat.highlight","div.summary-stat.highlight",1943515282),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),[cljs.core.str.cljs$core$IFn$_invoke$arity$1((((100) * new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451).cljs$core$IFn$_invoke$arity$1(hit_rate)) | (0))),"%"].join('')], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Cache Hit Rate"], null)], null);
} else {
return null;
}
})()], null);
} else {
return null;
}
});
/**
 * Complete timeline view with all components.
 */
observatory.views.timeline.timeline_view = (function observatory$views$timeline$timeline_view(){
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.timeline-view","div.timeline-view",-1071709410),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.view-header","div.view-header",1229518712),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2","h2",-372662728),"Timeline"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"p.hint","p.hint",-144164893),"Scrub through trace events. Slow down time to see cache behavior."], null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.trace_summary], null),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.timeline-main","div.timeline-main",1821290694),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.playback_controls], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.scrubber], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.event_lanes], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.current_event_info], null)], null)], null);
});

//# sourceMappingURL=observatory.views.timeline.js.map
