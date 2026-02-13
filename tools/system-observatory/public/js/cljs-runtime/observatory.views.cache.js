goog.provide('observatory.views.cache');
/**
 * A single cache line cell. Clickable to inspect.
 */
observatory.views.cache.line_cell = (function observatory$views$cache$line_cell(p__12821){
var map__12822 = p__12821;
var map__12822__$1 = cljs.core.__destructure_map(map__12822);
var set_index = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12822__$1,new cljs.core.Keyword(null,"set-index","set-index",797648137));
var way_index = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12822__$1,new cljs.core.Keyword(null,"way-index","way-index",1268630527));
var line = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12822__$1,new cljs.core.Keyword(null,"line","line",212345235));
var config = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12822__$1,new cljs.core.Keyword(null,"config","config",994861415));
var has_data_QMARK_ = (!((line == null)));
var state = new cljs.core.Keyword(null,"state","state",-1988618099).cljs$core$IFn$_invoke$arity$1(line);
var recent_QMARK_ = (function (){var and__5000__auto__ = line;
if(cljs.core.truth_(and__5000__auto__)){
return ((Date.now() - new cljs.core.Keyword(null,"last-access","last-access",1110221580).cljs$core$IFn$_invoke$arity$2(line,(0))) < (1000));
} else {
return and__5000__auto__;
}
})();
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.cache-line","div.cache-line",981857174),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"class","class",-2030961996),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [((has_data_QMARK_)?null:"empty"),((has_data_QMARK_)?cljs.core.name(state):null),(cljs.core.truth_(recent_QMARK_)?"recent":null)], null),new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (e){
e.stopPropagation();

if(has_data_QMARK_){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","select","observatory.state/select",-638815980),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"cache-line","cache-line",596120169),new cljs.core.Keyword(null,"data","data",-232669377),cljs.core.assoc.cljs$core$IFn$_invoke$arity$variadic(line,new cljs.core.Keyword(null,"set-index","set-index",797648137),set_index,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"way-index","way-index",1268630527),way_index], 0))], null)], null));
} else {
return null;
}
})], null),((has_data_QMARK_)?new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.line-content","div.line-content",1681264632),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.tag","div.tag",1088822530),["0x",cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"tag","tag",-1290361223).cljs$core$IFn$_invoke$arity$1(line).toString((16)))].join('')], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.state-indicator","div.state-indicator",-100091230),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),cljs.core.name(state)], null)], null)], null):null)], null);
});
/**
 * A single set (row) showing all its ways.
 */
observatory.views.cache.cache_set_row = (function observatory$views$cache$cache_set_row(p__12823){
var map__12824 = p__12823;
var map__12824__$1 = cljs.core.__destructure_map(map__12824);
var set_index = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12824__$1,new cljs.core.Keyword(null,"set-index","set-index",797648137));
var ways = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12824__$1,new cljs.core.Keyword(null,"ways","ways",1426688978));
var config = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12824__$1,new cljs.core.Keyword(null,"config","config",994861415));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.cache-set","div.cache-set",1075704306),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.set-index","div.set-index",-382424329),["Set 0x",cljs.core.str.cljs$core$IFn$_invoke$arity$1(set_index.toString((16)))].join('')], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.ways","div.ways",-75685000),(function (){var iter__5480__auto__ = (function observatory$views$cache$cache_set_row_$_iter__12825(s__12826){
return (new cljs.core.LazySeq(null,(function (){
var s__12826__$1 = s__12826;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12826__$1);
if(temp__5804__auto__){
var s__12826__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12826__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12826__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12828 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12827 = (0);
while(true){
if((i__12827 < size__5479__auto__)){
var vec__12829 = cljs.core._nth(c__5478__auto__,i__12827);
var way_index = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12829,(0),null);
var way = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12829,(1),null);
cljs.core.chunk_append(b__12828,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.line_cell,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"set-index","set-index",797648137),set_index,new cljs.core.Keyword(null,"way-index","way-index",1268630527),way_index,new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(way),new cljs.core.Keyword(null,"config","config",994861415),config], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),way_index], null)));

var G__12860 = (i__12827 + (1));
i__12827 = G__12860;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12828),observatory$views$cache$cache_set_row_$_iter__12825(cljs.core.chunk_rest(s__12826__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12828),null);
}
} else {
var vec__12832 = cljs.core.first(s__12826__$2);
var way_index = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12832,(0),null);
var way = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12832,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.line_cell,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"set-index","set-index",797648137),set_index,new cljs.core.Keyword(null,"way-index","way-index",1268630527),way_index,new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(way),new cljs.core.Keyword(null,"config","config",994861415),config], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),way_index], null)),observatory$views$cache$cache_set_row_$_iter__12825(cljs.core.rest(s__12826__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2(cljs.core.vector,ways));
})()], null)], null);
});
/**
 * View for a single cache level (L1/L2/L3).
 */
observatory.views.cache.cache_level_view = (function observatory$views$cache$cache_level_view(p__12835){
var map__12836 = p__12835;
var map__12836__$1 = cljs.core.__destructure_map(map__12836);
var level = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12836__$1,new cljs.core.Keyword(null,"level","level",1290497552));
var cache = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12836__$1,new cljs.core.Keyword(null,"cache","cache",-1237023054));
var config = new cljs.core.Keyword(null,"config","config",994861415).cljs$core$IFn$_invoke$arity$1(cache);
var sets = new cljs.core.Keyword(null,"sets","sets",400955582).cljs$core$IFn$_invoke$arity$1(cache);
var stats = new cljs.core.Keyword(null,"stats","stats",-85643011).cljs$core$IFn$_invoke$arity$1(cache);
var visible_sets = cljs.core.take.cljs$core$IFn$_invoke$arity$2((32),sets);
var total_sets = cljs.core.count(sets);
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.cache-level-view","div.cache-level-view",765964478),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.level-header","div.level-header",-387330636),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3.level-name","h3.level-name",1561382265),[cljs.core.name(level)," Cache"].join('')], null),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.level-stats","div.level-stats",-1170543718),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.stat","span.stat",-1585470148),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"size-kb","size-kb",-1863311670).cljs$core$IFn$_invoke$arity$1(config)),"KB"].join('')], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.stat","span.stat",-1585470148),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"associativity","associativity",835945425).cljs$core$IFn$_invoke$arity$1(config)),"-way"].join('')], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.stat","span.stat",-1585470148),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.count(sets))," sets"].join('')], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.stat","span.stat",-1585470148),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"line-size","line-size",8517417).cljs$core$IFn$_invoke$arity$2(config,(64))),"B lines"].join('')], null)], null)], null),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.level-metrics","div.level-metrics",-1067234520),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.metric","div.metric",2048277572),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Hits"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.Keyword(null,"hits","hits",-2120002930).cljs$core$IFn$_invoke$arity$2(stats,(0))], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.metric","div.metric",2048277572),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Misses"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.Keyword(null,"misses","misses",-1275188323).cljs$core$IFn$_invoke$arity$2(stats,(0))], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.metric","div.metric",2048277572),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Hit Rate"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),((((new cljs.core.Keyword(null,"hits","hits",-2120002930).cljs$core$IFn$_invoke$arity$2(stats,(0)) + new cljs.core.Keyword(null,"misses","misses",-1275188323).cljs$core$IFn$_invoke$arity$2(stats,(0))) > (0)))?[cljs.core.str.cljs$core$IFn$_invoke$arity$1((((100) * (new cljs.core.Keyword(null,"hits","hits",-2120002930).cljs$core$IFn$_invoke$arity$2(stats,(0)) / (new cljs.core.Keyword(null,"hits","hits",-2120002930).cljs$core$IFn$_invoke$arity$2(stats,(0)) + new cljs.core.Keyword(null,"misses","misses",-1275188323).cljs$core$IFn$_invoke$arity$2(stats,(0))))) | (0))),"%"].join(''):"-")], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.metric","div.metric",2048277572),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Evictions"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.Keyword(null,"evictions","evictions",-1315226615).cljs$core$IFn$_invoke$arity$2(stats,(0))], null)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.way-headers","div.way-headers",-1968483208),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.set-index-header","div.set-index-header",512584165),"Set"], null),(function (){var iter__5480__auto__ = (function observatory$views$cache$cache_level_view_$_iter__12837(s__12838){
return (new cljs.core.LazySeq(null,(function (){
var s__12838__$1 = s__12838;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12838__$1);
if(temp__5804__auto__){
var s__12838__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12838__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12838__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12840 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12839 = (0);
while(true){
if((i__12839 < size__5479__auto__)){
var w = cljs.core._nth(c__5478__auto__,i__12839);
cljs.core.chunk_append(b__12840,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.way-header","div.way-header",820494121),["Way ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(w)].join('')], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),w], null)));

var G__12861 = (i__12839 + (1));
i__12839 = G__12861;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12840),observatory$views$cache$cache_level_view_$_iter__12837(cljs.core.chunk_rest(s__12838__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12840),null);
}
} else {
var w = cljs.core.first(s__12838__$2);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.way-header","div.way-header",820494121),["Way ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(w)].join('')], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),w], null)),observatory$views$cache$cache_level_view_$_iter__12837(cljs.core.rest(s__12838__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.range.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"associativity","associativity",835945425).cljs$core$IFn$_invoke$arity$1(config)));
})()], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.sets-container","div.sets-container",-1236186559),(function (){var iter__5480__auto__ = (function observatory$views$cache$cache_level_view_$_iter__12841(s__12842){
return (new cljs.core.LazySeq(null,(function (){
var s__12842__$1 = s__12842;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12842__$1);
if(temp__5804__auto__){
var s__12842__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12842__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12842__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12844 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12843 = (0);
while(true){
if((i__12843 < size__5479__auto__)){
var vec__12845 = cljs.core._nth(c__5478__auto__,i__12843);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12845,(0),null);
var cache_set = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12845,(1),null);
cljs.core.chunk_append(b__12844,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.cache_set_row,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"set-index","set-index",797648137),new cljs.core.Keyword(null,"index","index",-1531685915).cljs$core$IFn$_invoke$arity$1(cache_set),new cljs.core.Keyword(null,"ways","ways",1426688978),new cljs.core.Keyword(null,"ways","ways",1426688978).cljs$core$IFn$_invoke$arity$1(cache_set),new cljs.core.Keyword(null,"config","config",994861415),config], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)));

var G__12862 = (i__12843 + (1));
i__12843 = G__12862;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12844),observatory$views$cache$cache_level_view_$_iter__12841(cljs.core.chunk_rest(s__12842__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12844),null);
}
} else {
var vec__12848 = cljs.core.first(s__12842__$2);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12848,(0),null);
var cache_set = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12848,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.cache_set_row,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"set-index","set-index",797648137),new cljs.core.Keyword(null,"index","index",-1531685915).cljs$core$IFn$_invoke$arity$1(cache_set),new cljs.core.Keyword(null,"ways","ways",1426688978),new cljs.core.Keyword(null,"ways","ways",1426688978).cljs$core$IFn$_invoke$arity$1(cache_set),new cljs.core.Keyword(null,"config","config",994861415),config], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)),observatory$views$cache$cache_level_view_$_iter__12841(cljs.core.rest(s__12842__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2(cljs.core.vector,visible_sets));
})(),(((total_sets > cljs.core.count(visible_sets)))?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.truncation-notice","div.truncation-notice",1192447922),["Showing ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.count(visible_sets))," of ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(total_sets)," sets. ","Scroll or filter to see more."].join('')], null):null)], null)], null);
});
/**
 * Visual overview of the full cache hierarchy.
 */
observatory.views.cache.hierarchy_overview = (function observatory$views$cache$hierarchy_overview(){
var hierarchy = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","cache-hierarchy","observatory.state/cache-hierarchy",1611142670)], null)));
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.hierarchy-overview","div.hierarchy-overview",-635983032),new cljs.core.PersistentVector(null, 11, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"svg.hierarchy-diagram","svg.hierarchy-diagram",-1334908651),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"viewBox","viewBox",-469489477),"0 0 400 200"], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"g.cpu","g.cpu",2002263273),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"transform","transform",1381301764),"translate(175, 10)"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"rect","rect",-108902628),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"width","width",-384071477),(50),new cljs.core.Keyword(null,"height","height",1025178622),(30),new cljs.core.Keyword(null,"rx","rx",1627208482),(4),new cljs.core.Keyword(null,"class","class",-2030961996),"cpu-block"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"text","text",-1790561697),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"x","x",2099068185),(25),new cljs.core.Keyword(null,"y","y",-1757859776),(20),new cljs.core.Keyword(null,"text-anchor","text-anchor",585613696),"middle"], null),"CPU"], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"g.l1","g.l1",-446787652),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"transform","transform",1381301764),"translate(150, 60)"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"rect","rect",-108902628),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"width","width",-384071477),(100),new cljs.core.Keyword(null,"height","height",1025178622),(25),new cljs.core.Keyword(null,"rx","rx",1627208482),(3),new cljs.core.Keyword(null,"class","class",-2030961996),"cache-block l1"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"text","text",-1790561697),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"x","x",2099068185),(50),new cljs.core.Keyword(null,"y","y",-1757859776),(17),new cljs.core.Keyword(null,"text-anchor","text-anchor",585613696),"middle"], null),"L1 (32KB)"], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"g.l2","g.l2",1762813989),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"transform","transform",1381301764),"translate(125, 100)"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"rect","rect",-108902628),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"width","width",-384071477),(150),new cljs.core.Keyword(null,"height","height",1025178622),(25),new cljs.core.Keyword(null,"rx","rx",1627208482),(3),new cljs.core.Keyword(null,"class","class",-2030961996),"cache-block l2"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"text","text",-1790561697),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"x","x",2099068185),(75),new cljs.core.Keyword(null,"y","y",-1757859776),(17),new cljs.core.Keyword(null,"text-anchor","text-anchor",585613696),"middle"], null),"L2 (256KB)"], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"g.l3","g.l3",177685666),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"transform","transform",1381301764),"translate(75, 140)"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"rect","rect",-108902628),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"width","width",-384071477),(250),new cljs.core.Keyword(null,"height","height",1025178622),(25),new cljs.core.Keyword(null,"rx","rx",1627208482),(3),new cljs.core.Keyword(null,"class","class",-2030961996),"cache-block l3"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"text","text",-1790561697),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"x","x",2099068185),(125),new cljs.core.Keyword(null,"y","y",-1757859776),(17),new cljs.core.Keyword(null,"text-anchor","text-anchor",585613696),"middle"], null),"L3 (8MB)"], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"g.ram","g.ram",707373729),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"transform","transform",1381301764),"translate(25, 180)"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"rect","rect",-108902628),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"width","width",-384071477),(350),new cljs.core.Keyword(null,"height","height",1025178622),(15),new cljs.core.Keyword(null,"rx","rx",1627208482),(2),new cljs.core.Keyword(null,"class","class",-2030961996),"ram-block"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"text","text",-1790561697),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"x","x",2099068185),(175),new cljs.core.Keyword(null,"y","y",-1757859776),(12),new cljs.core.Keyword(null,"text-anchor","text-anchor",585613696),"middle"], null),"RAM"], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"path.connection","path.connection",577537285),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"d","d",1972142424),"M200,40 L200,60"], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"path.connection","path.connection",577537285),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"d","d",1972142424),"M200,85 L200,100"], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"path.connection","path.connection",577537285),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"d","d",1972142424),"M200,125 L200,140"], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"path.connection","path.connection",577537285),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"d","d",1972142424),"M200,165 L200,180"], null)], null)], null)], null);
});
/**
 * Controls for cache simulation mode.
 */
observatory.views.cache.simulation_controls = (function observatory$views$cache$simulation_controls(){
var address_input = cljs.core.atom.cljs$core$IFn$_invoke$arity$1("0x7ff612340000");
return (function (){
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.simulation-controls","div.simulation-controls",1540354349),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Simulate Access"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.input-row","div.input-row",-1792565831),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input.address-input","input.address-input",-768071330),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"type","type",1174270348),"text",new cljs.core.Keyword(null,"value","value",305978217),cljs.core.deref(address_input),new cljs.core.Keyword(null,"on-change","on-change",-732046149),(function (p1__12851_SHARP_){
return cljs.core.reset_BANG_(address_input,p1__12851_SHARP_.target.value);
}),new cljs.core.Keyword(null,"placeholder","placeholder",-104873083),"Enter address (hex)"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.simulate-btn","button.simulate-btn",-553066524),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
var addr = parseInt(cljs.core.deref(address_input),(16));
if(cljs.core.truth_(isNaN(addr))){
return null;
} else {
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","simulate-access","observatory.state/simulate-access",-1793672144),addr], null));
}
})], null),"Access"], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.quick-patterns","div.quick-patterns",1194274649),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Quick patterns:"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.pattern-btn","button.pattern-btn",26885899),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
var seq__12852 = cljs.core.seq(cljs.core.range.cljs$core$IFn$_invoke$arity$1((100)));
var chunk__12853 = null;
var count__12854 = (0);
var i__12855 = (0);
while(true){
if((i__12855 < count__12854)){
var i = chunk__12853.cljs$core$IIndexed$_nth$arity$2(null,i__12855);
re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","simulate-access","observatory.state/simulate-access",-1793672144),((4096) + (i * (64)))], null));


var G__12863 = seq__12852;
var G__12864 = chunk__12853;
var G__12865 = count__12854;
var G__12866 = (i__12855 + (1));
seq__12852 = G__12863;
chunk__12853 = G__12864;
count__12854 = G__12865;
i__12855 = G__12866;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__12852);
if(temp__5804__auto__){
var seq__12852__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__12852__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__12852__$1);
var G__12867 = cljs.core.chunk_rest(seq__12852__$1);
var G__12868 = c__5525__auto__;
var G__12869 = cljs.core.count(c__5525__auto__);
var G__12870 = (0);
seq__12852 = G__12867;
chunk__12853 = G__12868;
count__12854 = G__12869;
i__12855 = G__12870;
continue;
} else {
var i = cljs.core.first(seq__12852__$1);
re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","simulate-access","observatory.state/simulate-access",-1793672144),((4096) + (i * (64)))], null));


var G__12871 = cljs.core.next(seq__12852__$1);
var G__12872 = null;
var G__12873 = (0);
var G__12874 = (0);
seq__12852 = G__12871;
chunk__12853 = G__12872;
count__12854 = G__12873;
i__12855 = G__12874;
continue;
}
} else {
return null;
}
}
break;
}
})], null),"Sequential"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.pattern-btn","button.pattern-btn",26885899),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
var seq__12856 = cljs.core.seq(cljs.core.range.cljs$core$IFn$_invoke$arity$1((100)));
var chunk__12857 = null;
var count__12858 = (0);
var i__12859 = (0);
while(true){
if((i__12859 < count__12858)){
var _ = chunk__12857.cljs$core$IIndexed$_nth$arity$2(null,i__12859);
re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","simulate-access","observatory.state/simulate-access",-1793672144),((4096) + (cljs.core.rand_int((1000)) * (64)))], null));


var G__12875 = seq__12856;
var G__12876 = chunk__12857;
var G__12877 = count__12858;
var G__12878 = (i__12859 + (1));
seq__12856 = G__12875;
chunk__12857 = G__12876;
count__12858 = G__12877;
i__12859 = G__12878;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__12856);
if(temp__5804__auto__){
var seq__12856__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__12856__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__12856__$1);
var G__12879 = cljs.core.chunk_rest(seq__12856__$1);
var G__12880 = c__5525__auto__;
var G__12881 = cljs.core.count(c__5525__auto__);
var G__12882 = (0);
seq__12856 = G__12879;
chunk__12857 = G__12880;
count__12858 = G__12881;
i__12859 = G__12882;
continue;
} else {
var _ = cljs.core.first(seq__12856__$1);
re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","simulate-access","observatory.state/simulate-access",-1793672144),((4096) + (cljs.core.rand_int((1000)) * (64)))], null));


var G__12883 = cljs.core.next(seq__12856__$1);
var G__12884 = null;
var G__12885 = (0);
var G__12886 = (0);
seq__12856 = G__12883;
chunk__12857 = G__12884;
count__12858 = G__12885;
i__12859 = G__12886;
continue;
}
} else {
return null;
}
}
break;
}
})], null),"Random"], null)], null)], null);
});
});
/**
 * Complete cache visualization view.
 */
observatory.views.cache.cache_view = (function observatory$views$cache$cache_view(){
var cache_state = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","cache-state","observatory.state/cache-state",1632104742)], null)));
var active_level = re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","current-tab","observatory.state/current-tab",507392906)], null));
return new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.cache-view","div.cache-view",411358113),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.view-header","div.view-header",1229518712),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2","h2",-372662728),"Cache Hierarchy"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"p.hint","p.hint",-144164893),"Click any cache line to inspect. Colors show MESI state."], null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.hierarchy_overview], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.simulation_controls], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.cache-tabs","div.cache-tabs",-1934384322),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.tab","button.tab",-638246460),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),"active"], null),"L1 Data"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.tab","button.tab",-638246460),"L2"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.tab","button.tab",-638246460),"L3"], null)], null),(cljs.core.truth_(new cljs.core.Keyword(null,"l1d","l1d",223086801).cljs$core$IFn$_invoke$arity$1(cache_state))?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.cache_level_view,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"l1d","l1d",223086801),new cljs.core.Keyword(null,"cache","cache",-1237023054),new cljs.core.Keyword(null,"l1d","l1d",223086801).cljs$core$IFn$_invoke$arity$1(cache_state)], null)], null):null)], null);
});

//# sourceMappingURL=observatory.views.cache.js.map
