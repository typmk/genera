goog.provide('observatory.trace');
/**
 * Create a new empty trace with metadata
 */
observatory.trace.make_trace = (function observatory$trace$make_trace(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12701 = arguments.length;
var i__5727__auto___12702 = (0);
while(true){
if((i__5727__auto___12702 < len__5726__auto___12701)){
args__5732__auto__.push((arguments[i__5727__auto___12702]));

var G__12703 = (i__5727__auto___12702 + (1));
i__5727__auto___12702 = G__12703;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((0) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((0)),(0),null)):null);
return observatory.trace.make_trace.cljs$core$IFn$_invoke$arity$variadic(argseq__5733__auto__);
});

(observatory.trace.make_trace.cljs$core$IFn$_invoke$arity$variadic = (function (p__12618){
var map__12619 = p__12618;
var map__12619__$1 = cljs.core.__destructure_map(map__12619);
var target_pid = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12619__$1,new cljs.core.Keyword(null,"target-pid","target-pid",1279085739),null);
var cpu_topology = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12619__$1,new cljs.core.Keyword(null,"cpu-topology","cpu-topology",340623578));
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"metadata","metadata",1799301597),new cljs.core.PersistentArrayMap(null, 7, [new cljs.core.Keyword(null,"start-time","start-time",814801386),cljs.core.str.cljs$core$IFn$_invoke$arity$1((new Date())),new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911),(0),new cljs.core.Keyword(null,"event-count","event-count",1113474312),(0),new cljs.core.Keyword(null,"target-pid","target-pid",1279085739),target_pid,new cljs.core.Keyword(null,"cpu-topology","cpu-topology",340623578),cpu_topology,new cljs.core.Keyword(null,"ram-topology","ram-topology",-249728294),null,new cljs.core.Keyword(null,"disks","disks",345654709),cljs.core.PersistentVector.EMPTY], null),new cljs.core.Keyword(null,"events","events",1792552201),cljs.core.PersistentVector.EMPTY], null);
}));

(observatory.trace.make_trace.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(observatory.trace.make_trace.cljs$lang$applyTo = (function (seq12617){
var self__5712__auto__ = this;
return self__5712__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq(seq12617));
}));

/**
 * Add an event to a trace. Returns new trace.
 */
observatory.trace.add_event = (function observatory$trace$add_event(trace,event){
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$4(cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(cljs.core.update.cljs$core$IFn$_invoke$arity$4(trace,new cljs.core.Keyword(null,"events","events",1792552201),cljs.core.conj,event),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"metadata","metadata",1799301597),new cljs.core.Keyword(null,"event-count","event-count",1113474312)], null),cljs.core.inc),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"metadata","metadata",1799301597),new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911)], null),cljs.core.max,new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(event));
});
/**
 * Add multiple events to a trace
 */
observatory.trace.add_events = (function observatory$trace$add_events(trace,events){
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(observatory.trace.add_event,trace,events);
});
/**
 * Predicate: filter by event type(s)
 */
observatory.trace.by_type = (function observatory$trace$by_type(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12704 = arguments.length;
var i__5727__auto___12705 = (0);
while(true){
if((i__5727__auto___12705 < len__5726__auto___12704)){
args__5732__auto__.push((arguments[i__5727__auto___12705]));

var G__12706 = (i__5727__auto___12705 + (1));
i__5727__auto___12705 = G__12706;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((0) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((0)),(0),null)):null);
return observatory.trace.by_type.cljs$core$IFn$_invoke$arity$variadic(argseq__5733__auto__);
});

(observatory.trace.by_type.cljs$core$IFn$_invoke$arity$variadic = (function (types){
var type_set = cljs.core.set(types);
return (function (event){
return cljs.core.contains_QMARK_(type_set,new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(event));
});
}));

(observatory.trace.by_type.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(observatory.trace.by_type.cljs$lang$applyTo = (function (seq12620){
var self__5712__auto__ = this;
return self__5712__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq(seq12620));
}));

/**
 * Predicate: filter by process ID
 */
observatory.trace.by_pid = (function observatory$trace$by_pid(pid){
return (function (event){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(pid,new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(event));
});
});
/**
 * Predicate: filter by thread ID
 */
observatory.trace.by_tid = (function observatory$trace$by_tid(tid){
return (function (event){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(tid,new cljs.core.Keyword(null,"tid","tid",-901350880).cljs$core$IFn$_invoke$arity$1(event));
});
});
/**
 * Predicate: filter by timestamp range [start, end)
 */
observatory.trace.by_time_range = (function observatory$trace$by_time_range(start_ns,end_ns){
return (function (event){
var ts = new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(event);
return (((ts >= start_ns)) && ((ts < end_ns)));
});
});
/**
 * Predicate: filter by address range (for memory events)
 */
observatory.trace.by_address_range = (function observatory$trace$by_address_range(start_addr,end_addr){
return (function (event){
var temp__5804__auto__ = new cljs.core.Keyword(null,"address","address",559499426).cljs$core$IFn$_invoke$arity$1(event);
if(cljs.core.truth_(temp__5804__auto__)){
var addr = temp__5804__auto__;
return (((addr >= start_addr)) && ((addr < end_addr)));
} else {
return null;
}
});
});
/**
 * Predicate: filter cache events by level
 */
observatory.trace.by_level = (function observatory$trace$by_level(level){
return (function (event){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(level,new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(event));
});
});
/**
 * Query events from trace with predicates.
 * 
 * Usage:
 * (query trace
 *        (by-type :cache-miss)
 *        (by-pid 1234)
 *        (by-time-range 0 1000000))
 */
observatory.trace.query = (function observatory$trace$query(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12707 = arguments.length;
var i__5727__auto___12708 = (0);
while(true){
if((i__5727__auto___12708 < len__5726__auto___12707)){
args__5732__auto__.push((arguments[i__5727__auto___12708]));

var G__12709 = (i__5727__auto___12708 + (1));
i__5727__auto___12708 = G__12709;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.query.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.query.cljs$core$IFn$_invoke$arity$variadic = (function (trace,predicates){
var pred = cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.every_pred,predicates);
return cljs.core.filter.cljs$core$IFn$_invoke$arity$2(pred,new cljs.core.Keyword(null,"events","events",1792552201).cljs$core$IFn$_invoke$arity$1(trace));
}));

(observatory.trace.query.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.query.cljs$lang$applyTo = (function (seq12621){
var G__12622 = cljs.core.first(seq12621);
var seq12621__$1 = cljs.core.next(seq12621);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12622,seq12621__$1);
}));

/**
 * Get first event matching predicates
 */
observatory.trace.query_first = (function observatory$trace$query_first(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12710 = arguments.length;
var i__5727__auto___12711 = (0);
while(true){
if((i__5727__auto___12711 < len__5726__auto___12710)){
args__5732__auto__.push((arguments[i__5727__auto___12711]));

var G__12712 = (i__5727__auto___12711 + (1));
i__5727__auto___12711 = G__12712;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.query_first.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.query_first.cljs$core$IFn$_invoke$arity$variadic = (function (trace,predicates){
return cljs.core.first(cljs.core.apply.cljs$core$IFn$_invoke$arity$3(observatory.trace.query,trace,predicates));
}));

(observatory.trace.query_first.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.query_first.cljs$lang$applyTo = (function (seq12623){
var G__12624 = cljs.core.first(seq12623);
var seq12623__$1 = cljs.core.next(seq12623);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12624,seq12623__$1);
}));

/**
 * Count events matching predicates
 */
observatory.trace.query_count = (function observatory$trace$query_count(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12713 = arguments.length;
var i__5727__auto___12714 = (0);
while(true){
if((i__5727__auto___12714 < len__5726__auto___12713)){
args__5732__auto__.push((arguments[i__5727__auto___12714]));

var G__12715 = (i__5727__auto___12714 + (1));
i__5727__auto___12714 = G__12715;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.query_count.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.query_count.cljs$core$IFn$_invoke$arity$variadic = (function (trace,predicates){
return cljs.core.count(cljs.core.apply.cljs$core$IFn$_invoke$arity$3(observatory.trace.query,trace,predicates));
}));

(observatory.trace.query_count.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.query_count.cljs$lang$applyTo = (function (seq12625){
var G__12626 = cljs.core.first(seq12625);
var seq12625__$1 = cljs.core.next(seq12625);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12626,seq12625__$1);
}));

/**
 * Group events by type
 */
observatory.trace.group_by_type = (function observatory$trace$group_by_type(events){
return cljs.core.group_by(new cljs.core.Keyword(null,"type","type",1174270348),events);
});
/**
 * Group events by process ID
 */
observatory.trace.group_by_pid = (function observatory$trace$group_by_pid(events){
return cljs.core.group_by(new cljs.core.Keyword(null,"pid","pid",1018387698),events);
});
/**
 * Group memory events by cache-line-aligned address
 */
observatory.trace.group_by_address = (function observatory$trace$group_by_address(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12716 = arguments.length;
var i__5727__auto___12717 = (0);
while(true){
if((i__5727__auto___12717 < len__5726__auto___12716)){
args__5732__auto__.push((arguments[i__5727__auto___12717]));

var G__12718 = (i__5727__auto___12717 + (1));
i__5727__auto___12717 = G__12718;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.group_by_address.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.group_by_address.cljs$core$IFn$_invoke$arity$variadic = (function (events,p__12630){
var map__12631 = p__12630;
var map__12631__$1 = cljs.core.__destructure_map(map__12631);
var line_size = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12631__$1,new cljs.core.Keyword(null,"line-size","line-size",8517417),(64));
var mask = (~ (line_size - (1)));
return cljs.core.group_by((function (p1__12627_SHARP_){
return ((function (){var or__5002__auto__ = new cljs.core.Keyword(null,"address","address",559499426).cljs$core$IFn$_invoke$arity$1(p1__12627_SHARP_);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return (0);
}
})() & mask);
}),events);
}));

(observatory.trace.group_by_address.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.group_by_address.cljs$lang$applyTo = (function (seq12628){
var G__12629 = cljs.core.first(seq12628);
var seq12628__$1 = cljs.core.next(seq12628);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12629,seq12628__$1);
}));

/**
 * Create histogram of event field values
 */
observatory.trace.histogram = (function observatory$trace$histogram(events,field){
return cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(cljs.core.val,cljs.core._GT_,cljs.core.frequencies(cljs.core.filter.cljs$core$IFn$_invoke$arity$2(cljs.core.some_QMARK_,cljs.core.map.cljs$core$IFn$_invoke$arity$2(field,events))));
});
/**
 * Bucket events by time intervals
 */
observatory.trace.timeline_buckets = (function observatory$trace$timeline_buckets(events,bucket_ns){
return cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p__12633){
var vec__12634 = p__12633;
var bucket = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12634,(0),null);
var evts = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12634,(1),null);
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"bucket","bucket",1126218366),bucket,new cljs.core.Keyword(null,"start-ns","start-ns",-432803573),(bucket * bucket_ns),new cljs.core.Keyword(null,"end-ns","end-ns",-1366705982),((bucket + (1)) * bucket_ns),new cljs.core.Keyword(null,"count","count",2139924085),cljs.core.count(evts),new cljs.core.Keyword(null,"events","events",1792552201),evts], null);
}),cljs.core.sort_by.cljs$core$IFn$_invoke$arity$2(cljs.core.key,cljs.core.group_by((function (p1__12632_SHARP_){
return cljs.core.quot(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(p1__12632_SHARP_),bucket_ns);
}),events)));
});
/**
 * Calculate cache hit rate from events
 */
observatory.trace.cache_hit_rate = (function observatory$trace$cache_hit_rate(events){
var cache_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12637_SHARP_){
var G__12640 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12637_SHARP_);
var fexpr__12639 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),null,new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),null], null), null);
return (fexpr__12639.cljs$core$IFn$_invoke$arity$1 ? fexpr__12639.cljs$core$IFn$_invoke$arity$1(G__12640) : fexpr__12639.call(null,G__12640));
}),events);
var hits = cljs.core.count(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12638_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12638_SHARP_));
}),cache_events));
var total = cljs.core.count(cache_events);
if((total === (0))){
return null;
} else {
return new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"hits","hits",-2120002930),hits,new cljs.core.Keyword(null,"misses","misses",-1275188323),(total - hits),new cljs.core.Keyword(null,"total","total",1916810418),total,new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451),(hits / total)], null);
}
});
/**
 * Calculate hit rate per cache level
 */
observatory.trace.cache_hit_rate_by_level = (function observatory$trace$cache_hit_rate_by_level(events){
return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p__12641){
var vec__12642 = p__12641;
var level = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12642,(0),null);
var evts = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12642,(1),null);
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [level,observatory.trace.cache_hit_rate(evts)], null);
}),cljs.core.group_by(new cljs.core.Keyword(null,"level","level",1290497552),cljs.core.filter.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"level","level",1290497552),events))));
});
/**
 * Find most frequently accessed addresses
 */
observatory.trace.hot_addresses = (function observatory$trace$hot_addresses(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12719 = arguments.length;
var i__5727__auto___12720 = (0);
while(true){
if((i__5727__auto___12720 < len__5726__auto___12719)){
args__5732__auto__.push((arguments[i__5727__auto___12720]));

var G__12721 = (i__5727__auto___12720 + (1));
i__5727__auto___12720 = G__12721;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.hot_addresses.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.hot_addresses.cljs$core$IFn$_invoke$arity$variadic = (function (events,p__12648){
var map__12649 = p__12648;
var map__12649__$1 = cljs.core.__destructure_map(map__12649);
var top_n = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12649__$1,new cljs.core.Keyword(null,"top-n","top-n",1854609254),(20));
var line_size = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12649__$1,new cljs.core.Keyword(null,"line-size","line-size",8517417),(64));
var mask = (~ (line_size - (1)));
return cljs.core.take.cljs$core$IFn$_invoke$arity$2(top_n,cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(cljs.core.val,cljs.core._GT_,cljs.core.frequencies(cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__12645_SHARP_){
return (p1__12645_SHARP_ & mask);
}),cljs.core.keep.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"address","address",559499426),events)))));
}));

(observatory.trace.hot_addresses.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.hot_addresses.cljs$lang$applyTo = (function (seq12646){
var G__12647 = cljs.core.first(seq12646);
var seq12646__$1 = cljs.core.next(seq12646);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12647,seq12646__$1);
}));

/**
 * Summarize page fault events
 */
observatory.trace.page_fault_summary = (function observatory$trace$page_fault_summary(events){
var faults = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12650_SHARP_){
var G__12654 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12650_SHARP_);
var fexpr__12653 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"page-fault","page-fault",-1895673594),null,new cljs.core.Keyword(null,"page-fault-hard","page-fault-hard",489885262),null], null), null);
return (fexpr__12653.cljs$core$IFn$_invoke$arity$1 ? fexpr__12653.cljs$core$IFn$_invoke$arity$1(G__12654) : fexpr__12653.call(null,G__12654));
}),events);
var hard = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12651_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"page-fault-hard","page-fault-hard",489885262),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12651_SHARP_));
}),faults);
var soft = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12652_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"page-fault","page-fault",-1895673594),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12652_SHARP_));
}),faults);
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"total","total",1916810418),cljs.core.count(faults),new cljs.core.Keyword(null,"hard-faults","hard-faults",1601738338),cljs.core.count(hard),new cljs.core.Keyword(null,"soft-faults","soft-faults",103639672),cljs.core.count(soft),new cljs.core.Keyword(null,"hard-fault-addresses","hard-fault-addresses",-1318012934),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"address","address",559499426),hard),new cljs.core.Keyword(null,"by-pid","by-pid",1199701227),observatory.trace.histogram(faults,new cljs.core.Keyword(null,"pid","pid",1018387698))], null);
});
/**
 * Summarize disk I/O events
 */
observatory.trace.io_summary = (function observatory$trace$io_summary(events){
var io_events = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12655_SHARP_){
var G__12659 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12655_SHARP_);
var fexpr__12658 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651),null,new cljs.core.Keyword(null,"disk-write","disk-write",33697799),null], null), null);
return (fexpr__12658.cljs$core$IFn$_invoke$arity$1 ? fexpr__12658.cljs$core$IFn$_invoke$arity$1(G__12659) : fexpr__12658.call(null,G__12659));
}),events);
var reads = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12656_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12656_SHARP_));
}),io_events);
var writes = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12657_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"disk-write","disk-write",33697799),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12657_SHARP_));
}),io_events);
return new cljs.core.PersistentArrayMap(null, 7, [new cljs.core.Keyword(null,"read-count","read-count",977086476),cljs.core.count(reads),new cljs.core.Keyword(null,"write-count","write-count",1277724566),cljs.core.count(writes),new cljs.core.Keyword(null,"read-bytes","read-bytes",-1280034403),cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"size-bytes","size-bytes",2109643324),reads)),new cljs.core.Keyword(null,"write-bytes","write-bytes",-1890264825),cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"size-bytes","size-bytes",2109643324),writes)),new cljs.core.Keyword(null,"avg-read-latency-ns","avg-read-latency-ns",941095349),((cljs.core.seq(reads))?(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517),reads)) / cljs.core.count(reads)):null),new cljs.core.Keyword(null,"avg-write-latency-ns","avg-write-latency-ns",1892207728),((cljs.core.seq(writes))?(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517),writes)) / cljs.core.count(writes)):null),new cljs.core.Keyword(null,"max-queue-depth","max-queue-depth",728549869),cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core.max,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"queue-depth","queue-depth",1749450332),io_events))], null);
});
/**
 * Calculate event rate over time
 */
observatory.trace.events_per_second = (function observatory$trace$events_per_second(events){
var ns_per_sec = (1000000000);
var buckets = observatory.trace.timeline_buckets(events,ns_per_sec);
return cljs.core.mapv.cljs$core$IFn$_invoke$arity$2((function (p__12660){
var map__12661 = p__12660;
var map__12661__$1 = cljs.core.__destructure_map(map__12661);
var bucket = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12661__$1,new cljs.core.Keyword(null,"bucket","bucket",1126218366));
var count = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12661__$1,new cljs.core.Keyword(null,"count","count",2139924085));
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"second","second",-444702010),bucket,new cljs.core.Keyword(null,"events-per-sec","events-per-sec",-1486160466),count], null);
}),buckets);
});
/**
 * Calculate rolling cache hit rate with window
 */
observatory.trace.rolling_hit_rate = (function observatory$trace$rolling_hit_rate(events,window_size){
return cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (window){
var hits = cljs.core.count(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12663_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12663_SHARP_));
}),window));
var ts = new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(cljs.core.last(window));
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"timestamp","timestamp",579478971),ts,new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451),(hits / window_size)], null);
}),cljs.core.partition.cljs$core$IFn$_invoke$arity$3(window_size,(1),cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12662_SHARP_){
var G__12665 = new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(p1__12662_SHARP_);
var fexpr__12664 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),null,new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),null], null), null);
return (fexpr__12664.cljs$core$IFn$_invoke$arity$1 ? fexpr__12664.cljs$core$IFn$_invoke$arity$1(G__12665) : fexpr__12664.call(null,G__12665));
}),events)));
});
/**
 * Find page faults that follow cache misses (potential causation)
 */
observatory.trace.correlate_miss_to_fault = (function observatory$trace$correlate_miss_to_fault(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12722 = arguments.length;
var i__5727__auto___12723 = (0);
while(true){
if((i__5727__auto___12723 < len__5726__auto___12722)){
args__5732__auto__.push((arguments[i__5727__auto___12723]));

var G__12724 = (i__5727__auto___12723 + (1));
i__5727__auto___12723 = G__12724;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.correlate_miss_to_fault.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.correlate_miss_to_fault.cljs$core$IFn$_invoke$arity$variadic = (function (events,p__12668){
var map__12669 = p__12668;
var map__12669__$1 = cljs.core.__destructure_map(map__12669);
var window_ns = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12669__$1,new cljs.core.Keyword(null,"window-ns","window-ns",-632333101),(10000));
var sorted = cljs.core.sort_by.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"timestamp","timestamp",579478971),events);
var iter__5480__auto__ = (function observatory$trace$iter__12670(s__12671){
return (new cljs.core.LazySeq(null,(function (){
var s__12671__$1 = s__12671;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12671__$1);
if(temp__5804__auto__){
var s__12671__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12671__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12671__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12673 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12672 = (0);
while(true){
if((i__12672 < size__5479__auto__)){
var vec__12674 = cljs.core._nth(c__5478__auto__,i__12672);
var miss = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12674,(0),null);
var fault = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12674,(1),null);
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(miss))) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"page-fault-hard","page-fault-hard",489885262),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(fault))) && (((new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(miss)) < window_ns)))))){
cljs.core.chunk_append(b__12673,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"miss","miss",1465931270),miss,new cljs.core.Keyword(null,"fault","fault",-1838508432),fault,new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517),(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(miss))], null));

var G__12725 = (i__12672 + (1));
i__12672 = G__12725;
continue;
} else {
var G__12726 = (i__12672 + (1));
i__12672 = G__12726;
continue;
}
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12673),observatory$trace$iter__12670(cljs.core.chunk_rest(s__12671__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12673),null);
}
} else {
var vec__12677 = cljs.core.first(s__12671__$2);
var miss = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12677,(0),null);
var fault = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12677,(1),null);
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(miss))) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"page-fault-hard","page-fault-hard",489885262),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(fault))) && (((new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(miss)) < window_ns)))))){
return cljs.core.cons(new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"miss","miss",1465931270),miss,new cljs.core.Keyword(null,"fault","fault",-1838508432),fault,new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517),(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(miss))], null),observatory$trace$iter__12670(cljs.core.rest(s__12671__$2)));
} else {
var G__12727 = cljs.core.rest(s__12671__$2);
s__12671__$1 = G__12727;
continue;
}
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.partition.cljs$core$IFn$_invoke$arity$3((2),(1),sorted));
}));

(observatory.trace.correlate_miss_to_fault.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.correlate_miss_to_fault.cljs$lang$applyTo = (function (seq12666){
var G__12667 = cljs.core.first(seq12666);
var seq12666__$1 = cljs.core.next(seq12666);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12667,seq12666__$1);
}));

/**
 * Find disk I/O following hard page faults
 */
observatory.trace.correlate_io_to_fault = (function observatory$trace$correlate_io_to_fault(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12728 = arguments.length;
var i__5727__auto___12729 = (0);
while(true){
if((i__5727__auto___12729 < len__5726__auto___12728)){
args__5732__auto__.push((arguments[i__5727__auto___12729]));

var G__12730 = (i__5727__auto___12729 + (1));
i__5727__auto___12729 = G__12730;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return observatory.trace.correlate_io_to_fault.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(observatory.trace.correlate_io_to_fault.cljs$core$IFn$_invoke$arity$variadic = (function (events,p__12682){
var map__12683 = p__12682;
var map__12683__$1 = cljs.core.__destructure_map(map__12683);
var window_ns = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12683__$1,new cljs.core.Keyword(null,"window-ns","window-ns",-632333101),(1000000));
var sorted = cljs.core.sort_by.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"timestamp","timestamp",579478971),events);
var iter__5480__auto__ = (function observatory$trace$iter__12684(s__12685){
return (new cljs.core.LazySeq(null,(function (){
var s__12685__$1 = s__12685;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12685__$1);
if(temp__5804__auto__){
var s__12685__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12685__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12685__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12687 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12686 = (0);
while(true){
if((i__12686 < size__5479__auto__)){
var vec__12688 = cljs.core._nth(c__5478__auto__,i__12686);
var fault = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12688,(0),null);
var io = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12688,(1),null);
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"page-fault-hard","page-fault-hard",489885262),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(fault))) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(io))) && (((new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(io) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault)) < window_ns)))))){
cljs.core.chunk_append(b__12687,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"fault","fault",-1838508432),fault,new cljs.core.Keyword(null,"io","io",-307341917),io,new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517),(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(io) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault))], null));

var G__12731 = (i__12686 + (1));
i__12686 = G__12731;
continue;
} else {
var G__12732 = (i__12686 + (1));
i__12686 = G__12732;
continue;
}
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12687),observatory$trace$iter__12684(cljs.core.chunk_rest(s__12685__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12687),null);
}
} else {
var vec__12691 = cljs.core.first(s__12685__$2);
var fault = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12691,(0),null);
var io = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12691,(1),null);
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"page-fault-hard","page-fault-hard",489885262),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(fault))) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(io))) && (((new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(io) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault)) < window_ns)))))){
return cljs.core.cons(new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"fault","fault",-1838508432),fault,new cljs.core.Keyword(null,"io","io",-307341917),io,new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517),(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(io) - new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(fault))], null),observatory$trace$iter__12684(cljs.core.rest(s__12685__$2)));
} else {
var G__12733 = cljs.core.rest(s__12685__$2);
s__12685__$1 = G__12733;
continue;
}
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.partition.cljs$core$IFn$_invoke$arity$3((2),(1),sorted));
}));

(observatory.trace.correlate_io_to_fault.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(observatory.trace.correlate_io_to_fault.cljs$lang$applyTo = (function (seq12680){
var G__12681 = cljs.core.first(seq12680);
var seq12680__$1 = cljs.core.next(seq12680);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12681,seq12680__$1);
}));

/**
 * Export trace to EDN string
 */
observatory.trace.trace__GT_edn = (function observatory$trace$trace__GT_edn(trace){
return cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([trace], 0));
});
/**
 * Import trace from EDN string
 */
observatory.trace.edn__GT_trace = (function observatory$trace$edn__GT_trace(edn_str){
return cljs.reader.read_string.cljs$core$IFn$_invoke$arity$1(edn_str);
});
/**
 * Format number as hex string
 */
observatory.trace.format_hex = (function observatory$trace$format_hex(n){
return ["0x",cljs.core.str.cljs$core$IFn$_invoke$arity$1(n.toString((16)))].join('');
});
/**
 * Export events to CSV format
 */
observatory.trace.events__GT_csv = (function observatory$trace$events__GT_csv(events){
var headers = new cljs.core.PersistentVector(null, 8, 5, cljs.core.PersistentVector.EMPTY_NODE, ["timestamp","type","pid","tid","address","level","hit","latency"], null);
var rows = (function (){var iter__5480__auto__ = (function observatory$trace$events__GT_csv_$_iter__12695(s__12696){
return (new cljs.core.LazySeq(null,(function (){
var s__12696__$1 = s__12696;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12696__$1);
if(temp__5804__auto__){
var s__12696__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12696__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12696__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12698 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12697 = (0);
while(true){
if((i__12697 < size__5479__auto__)){
var e = cljs.core._nth(c__5478__auto__,i__12697);
cljs.core.chunk_append(b__12698,new cljs.core.PersistentVector(null, 8, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.name(new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"tid","tid",-901350880).cljs$core$IFn$_invoke$arity$1(e)),(function (){var temp__5804__auto____$1 = new cljs.core.Keyword(null,"address","address",559499426).cljs$core$IFn$_invoke$arity$1(e);
if(cljs.core.truth_(temp__5804__auto____$1)){
var a = temp__5804__auto____$1;
return observatory.trace.format_hex(a);
} else {
return null;
}
})(),(function (){var G__12699 = new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(e);
if((G__12699 == null)){
return null;
} else {
return cljs.core.name(G__12699);
}
})(),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"hit?","hit?",-364237458).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517).cljs$core$IFn$_invoke$arity$1(e))], null));

var G__12734 = (i__12697 + (1));
i__12697 = G__12734;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12698),observatory$trace$events__GT_csv_$_iter__12695(cljs.core.chunk_rest(s__12696__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12698),null);
}
} else {
var e = cljs.core.first(s__12696__$2);
return cljs.core.cons(new cljs.core.PersistentVector(null, 8, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"timestamp","timestamp",579478971).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.name(new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"tid","tid",-901350880).cljs$core$IFn$_invoke$arity$1(e)),(function (){var temp__5804__auto____$1 = new cljs.core.Keyword(null,"address","address",559499426).cljs$core$IFn$_invoke$arity$1(e);
if(cljs.core.truth_(temp__5804__auto____$1)){
var a = temp__5804__auto____$1;
return observatory.trace.format_hex(a);
} else {
return null;
}
})(),(function (){var G__12700 = new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(e);
if((G__12700 == null)){
return null;
} else {
return cljs.core.name(G__12700);
}
})(),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"hit?","hit?",-364237458).cljs$core$IFn$_invoke$arity$1(e)),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"latency-ns","latency-ns",-336384517).cljs$core$IFn$_invoke$arity$1(e))], null),observatory$trace$events__GT_csv_$_iter__12695(cljs.core.rest(s__12696__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(events);
})();
return clojure.string.join.cljs$core$IFn$_invoke$arity$2("\n",cljs.core.cons(clojure.string.join.cljs$core$IFn$_invoke$arity$2(",",headers),cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__12694_SHARP_){
return clojure.string.join.cljs$core$IFn$_invoke$arity$2(",",p1__12694_SHARP_);
}),rows)));
});

//# sourceMappingURL=observatory.trace.js.map
