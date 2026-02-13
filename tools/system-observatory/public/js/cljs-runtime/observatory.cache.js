goog.provide('observatory.cache');
/**
 * Decompose address into tag, set index, and offset.
 * 
 * For a cache with S sets and B byte lines:
 * - Offset bits: log2(B) - identifies byte within line
 * - Index bits: log2(S) - identifies which set
 * - Tag bits: remaining high bits - identifies which line
 * 
 * Example for 32KB, 8-way, 64B lines:
 * - 32KB / 8 ways = 4KB per way
 * - 4KB / 64B = 64 sets
 * - Offset: 6 bits (64 bytes)
 * - Index: 6 bits (64 sets)
 * - Tag: 52 bits
 */
observatory.cache.address_parts = (function observatory$cache$address_parts(address,p__12153){
var map__12154 = p__12153;
var map__12154__$1 = cljs.core.__destructure_map(map__12154);
var sets = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12154__$1,new cljs.core.Keyword(null,"sets","sets",400955582));
var line_size = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12154__$1,new cljs.core.Keyword(null,"line-size","line-size",8517417));
var offset_bits = (Math.ceil((Math.log(line_size) / Math.log((2)))) | (0));
var index_bits = (Math.ceil((Math.log(sets) / Math.log((2)))) | (0));
var offset_mask = (((1) << offset_bits) - (1));
var index_mask = (((1) << index_bits) - (1));
return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"offset","offset",296498311),(address & offset_mask),new cljs.core.Keyword(null,"set-index","set-index",797648137),((address >> offset_bits) & index_mask),new cljs.core.Keyword(null,"tag","tag",-1290361223),(address >> (offset_bits + index_bits))], null);
});
/**
 * Get the cache-line-aligned address (clear offset bits)
 */
observatory.cache.line_address = (function observatory$cache$line_address(address,line_size){
var offset_bits = (Math.ceil((Math.log(line_size) / Math.log((2)))) | (0));
return (address & (~ (((1) << offset_bits) - (1))));
});
/**
 * Create an empty cache way
 */
observatory.cache.make_empty_way = (function observatory$cache$make_empty_way(index){
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"index","index",-1531685915),index,new cljs.core.Keyword(null,"line","line",212345235),null], null);
});
/**
 * Create an empty cache set with N ways
 */
observatory.cache.make_empty_set = (function observatory$cache$make_empty_set(index,associativity){
return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"index","index",-1531685915),index,new cljs.core.Keyword(null,"ways","ways",1426688978),cljs.core.mapv.cljs$core$IFn$_invoke$arity$2(observatory.cache.make_empty_way,cljs.core.range.cljs$core$IFn$_invoke$arity$1(associativity)),new cljs.core.Keyword(null,"lru-order","lru-order",-1890066848),cljs.core.vec(cljs.core.range.cljs$core$IFn$_invoke$arity$1(associativity))], null);
});
/**
 * Create a new empty cache from config.
 * 
 * Config should have:
 * - :level (:l1, :l2, :l3)
 * - :type (:data, :instruction, :unified)
 * - :size-kb
 * - :line-size (default 64)
 * - :associativity
 */
observatory.cache.make_cache = (function observatory$cache$make_cache(p__12158){
var map__12159 = p__12158;
var map__12159__$1 = cljs.core.__destructure_map(map__12159);
var config = map__12159__$1;
var level = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12159__$1,new cljs.core.Keyword(null,"level","level",1290497552));
var type = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12159__$1,new cljs.core.Keyword(null,"type","type",1174270348));
var size_kb = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12159__$1,new cljs.core.Keyword(null,"size-kb","size-kb",-1863311670));
var line_size = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12159__$1,new cljs.core.Keyword(null,"line-size","line-size",8517417),(64));
var associativity = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12159__$1,new cljs.core.Keyword(null,"associativity","associativity",835945425));
var size_bytes = (size_kb * (1024));
var lines = cljs.core.quot(size_bytes,line_size);
var sets = cljs.core.quot(lines,associativity);
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"level","level",1290497552),level,new cljs.core.Keyword(null,"type","type",1174270348),type,new cljs.core.Keyword(null,"config","config",994861415),cljs.core.assoc.cljs$core$IFn$_invoke$arity$variadic(config,new cljs.core.Keyword(null,"sets","sets",400955582),sets,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"line-size","line-size",8517417),line_size], 0)),new cljs.core.Keyword(null,"sets","sets",400955582),cljs.core.mapv.cljs$core$IFn$_invoke$arity$2((function (p1__12157_SHARP_){
return observatory.cache.make_empty_set(p1__12157_SHARP_,associativity);
}),cljs.core.range.cljs$core$IFn$_invoke$arity$1(sets)),new cljs.core.Keyword(null,"stats","stats",-85643011),new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"hits","hits",-2120002930),(0),new cljs.core.Keyword(null,"misses","misses",-1275188323),(0),new cljs.core.Keyword(null,"evictions","evictions",-1315226615),(0),new cljs.core.Keyword(null,"reads","reads",-1215067361),(0),new cljs.core.Keyword(null,"writes","writes",-102226269),(0)], null)], null);
});
/**
 * Move a way to MRU position (end of LRU order).
 * Returns updated lru-order vector.
 */
observatory.cache.touch_way = (function observatory$cache$touch_way(lru_order,way_index){
var filtered = cljs.core.filterv((function (p1__12161_SHARP_){
return cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(p1__12161_SHARP_,way_index);
}),lru_order);
return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(filtered,way_index);
});
/**
 * Get the LRU way index (first in order)
 */
observatory.cache.lru_victim = (function observatory$cache$lru_victim(lru_order){
return cljs.core.first(lru_order);
});
/**
 * Find a cache line by tag in a set.
 * Returns [way-index line] or nil.
 */
observatory.cache.find_line_in_set = (function observatory$cache$find_line_in_set(cache_set,tag){
return cljs.core.first(cljs.core.filter.cljs$core$IFn$_invoke$arity$2(cljs.core.some_QMARK_,cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2((function (i,way){
if(cljs.core.truth_((function (){var and__5000__auto__ = new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(way);
if(cljs.core.truth_(and__5000__auto__)){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"tag","tag",-1290361223).cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(way)),tag);
} else {
return and__5000__auto__;
}
})())){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [i,new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(way)], null);
} else {
return null;
}
}),new cljs.core.Keyword(null,"ways","ways",1426688978).cljs$core$IFn$_invoke$arity$1(cache_set))));
});
/**
 * Find an empty way in a set. Returns way-index or nil.
 */
observatory.cache.find_empty_way = (function observatory$cache$find_empty_way(cache_set){
return cljs.core.first(cljs.core.filter.cljs$core$IFn$_invoke$arity$2(cljs.core.some_QMARK_,cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2((function (i,way){
if((new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(way) == null)){
return i;
} else {
return null;
}
}),new cljs.core.Keyword(null,"ways","ways",1426688978).cljs$core$IFn$_invoke$arity$1(cache_set))));
});
/**
 * Update a specific way in a set with a new line
 */
observatory.cache.update_way = (function observatory$cache$update_way(cache_set,way_index,line,timestamp){
return cljs.core.update.cljs$core$IFn$_invoke$arity$4(cljs.core.assoc_in(cache_set,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ways","ways",1426688978),way_index,new cljs.core.Keyword(null,"line","line",212345235)], null),cljs.core.assoc.cljs$core$IFn$_invoke$arity$variadic(line,new cljs.core.Keyword(null,"last-access","last-access",1110221580),timestamp,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"access-count","access-count",169899798),(cljs.core.get.cljs$core$IFn$_invoke$arity$3(line,new cljs.core.Keyword(null,"access-count","access-count",169899798),(0)) + (1))], 0))),new cljs.core.Keyword(null,"lru-order","lru-order",-1890066848),observatory.cache.touch_way,way_index);
});
/**
 * Evict LRU line and insert new one.
 * Returns [new-set evicted-line]
 */
observatory.cache.evict_and_insert = (function observatory$cache$evict_and_insert(cache_set,new_line,timestamp){
var victim_way = observatory.cache.lru_victim(new cljs.core.Keyword(null,"lru-order","lru-order",-1890066848).cljs$core$IFn$_invoke$arity$1(cache_set));
var evicted = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(cache_set,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ways","ways",1426688978),victim_way,new cljs.core.Keyword(null,"line","line",212345235)], null));
var new_set = observatory.cache.update_way(cache_set,victim_way,new_line,timestamp);
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new_set,evicted], null);
});
/**
 * Perform a cache access. Pure function.
 * 
 * Arguments:
 * - cache: current cache state
 * - address: memory address being accessed
 * - timestamp: current timestamp (for LRU tracking)
 * - opts: {:write? false, :data nil}
 * 
 * Returns:
 * {:cache new-cache-state
 *  :result {:hit? bool
 *           :level :l1/:l2/:l3
 *           :set-index int
 *           :way-index int (or nil for miss)
 *           :evicted nil or {:address :data :dirty?}
 *           :latency-cycles int}}
 */
observatory.cache.access = (function observatory$cache$access(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12289 = arguments.length;
var i__5727__auto___12290 = (0);
while(true){
if((i__5727__auto___12290 < len__5726__auto___12289)){
args__5732__auto__.push((arguments[i__5727__auto___12290]));

var G__12291 = (i__5727__auto___12290 + (1));
i__5727__auto___12290 = G__12291;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((3) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((3)),(0),null)):null);
return observatory.cache.access.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__5733__auto__);
});

(observatory.cache.access.cljs$core$IFn$_invoke$arity$variadic = (function (cache,address,timestamp,p__12168){
var vec__12169 = p__12168;
var map__12172 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12169,(0),null);
var map__12172__$1 = cljs.core.__destructure_map(map__12172);
var write_QMARK_ = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12172__$1,new cljs.core.Keyword(null,"write?","write?",673505583),false);
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12172__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var map__12173 = cache;
var map__12173__$1 = cljs.core.__destructure_map(map__12173);
var config = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12173__$1,new cljs.core.Keyword(null,"config","config",994861415));
var sets = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12173__$1,new cljs.core.Keyword(null,"sets","sets",400955582));
var map__12174 = observatory.cache.address_parts(address,config);
var map__12174__$1 = cljs.core.__destructure_map(map__12174);
var set_index = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12174__$1,new cljs.core.Keyword(null,"set-index","set-index",797648137));
var tag = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12174__$1,new cljs.core.Keyword(null,"tag","tag",-1290361223));
var cache_set = cljs.core.get.cljs$core$IFn$_invoke$arity$2(sets,set_index);
var vec__12175 = observatory.cache.find_line_in_set(cache_set,tag);
var way_index = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12175,(0),null);
var existing_line = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12175,(1),null);
if(cljs.core.truth_(existing_line)){
var updated_line = (function (){var G__12178 = existing_line;
var G__12178__$1 = (cljs.core.truth_(write_QMARK_)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__12178,new cljs.core.Keyword(null,"state","state",-1988618099),new cljs.core.Keyword(null,"modified","modified",-2134587826)):G__12178);
if(cljs.core.truth_(data)){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__12178__$1,new cljs.core.Keyword(null,"data","data",-232669377),data);
} else {
return G__12178__$1;
}
})();
var new_set = observatory.cache.update_way(cache_set,way_index,updated_line,timestamp);
var new_cache = cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(cljs.core.assoc_in(cache,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"sets","sets",400955582),set_index], null),new_set),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"stats","stats",-85643011),new cljs.core.Keyword(null,"hits","hits",-2120002930)], null),cljs.core.inc),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"stats","stats",-85643011),(cljs.core.truth_(write_QMARK_)?new cljs.core.Keyword(null,"writes","writes",-102226269):new cljs.core.Keyword(null,"reads","reads",-1215067361))], null),cljs.core.inc);
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache","cache",-1237023054),new_cache,new cljs.core.Keyword(null,"result","result",1415092211),new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"hit?","hit?",-364237458),true,new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(cache),new cljs.core.Keyword(null,"set-index","set-index",797648137),set_index,new cljs.core.Keyword(null,"way-index","way-index",1268630527),way_index,new cljs.core.Keyword(null,"evicted","evicted",1052280659),null,new cljs.core.Keyword(null,"latency-cycles","latency-cycles",-682475027),(function (){var G__12181 = new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(cache);
var G__12181__$1 = (((G__12181 instanceof cljs.core.Keyword))?G__12181.fqn:null);
switch (G__12181__$1) {
case "l1":
return (4);

break;
case "l2":
return (12);

break;
case "l3":
return (36);

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__12181__$1)].join('')));

}
})()], null)], null);
} else {
var new_line = new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"tag","tag",-1290361223),tag,new cljs.core.Keyword(null,"data","data",-232669377),(function (){var or__5002__auto__ = data;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return cljs.core.vec(cljs.core.repeat.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"line-size","line-size",8517417).cljs$core$IFn$_invoke$arity$1(config),(0)));
}
})(),new cljs.core.Keyword(null,"state","state",-1988618099),(cljs.core.truth_(write_QMARK_)?new cljs.core.Keyword(null,"modified","modified",-2134587826):new cljs.core.Keyword(null,"exclusive","exclusive",-1507998718)),new cljs.core.Keyword(null,"last-access","last-access",1110221580),timestamp,new cljs.core.Keyword(null,"access-count","access-count",169899798),(1)], null);
var empty_way = observatory.cache.find_empty_way(cache_set);
var vec__12198 = (cljs.core.truth_(empty_way)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.cache.update_way(cache_set,empty_way,new_line,timestamp),null], null):observatory.cache.evict_and_insert(cache_set,new_line,timestamp));
var new_set = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12198,(0),null);
var evicted = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12198,(1),null);
var new_cache = (function (){var G__12201 = cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(cljs.core.assoc_in(cache,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"sets","sets",400955582),set_index], null),new_set),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"stats","stats",-85643011),new cljs.core.Keyword(null,"misses","misses",-1275188323)], null),cljs.core.inc),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"stats","stats",-85643011),(cljs.core.truth_(write_QMARK_)?new cljs.core.Keyword(null,"writes","writes",-102226269):new cljs.core.Keyword(null,"reads","reads",-1215067361))], null),cljs.core.inc);
if(cljs.core.truth_(evicted)){
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(G__12201,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"stats","stats",-85643011),new cljs.core.Keyword(null,"evictions","evictions",-1315226615)], null),cljs.core.inc);
} else {
return G__12201;
}
})();
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"cache","cache",-1237023054),new_cache,new cljs.core.Keyword(null,"result","result",1415092211),new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"hit?","hit?",-364237458),false,new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(cache),new cljs.core.Keyword(null,"set-index","set-index",797648137),set_index,new cljs.core.Keyword(null,"way-index","way-index",1268630527),(function (){var or__5002__auto__ = empty_way;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return observatory.cache.lru_victim(new cljs.core.Keyword(null,"lru-order","lru-order",-1890066848).cljs$core$IFn$_invoke$arity$1(cache_set));
}
})(),new cljs.core.Keyword(null,"evicted","evicted",1052280659),(cljs.core.truth_(evicted)?new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"address","address",559499426),(new cljs.core.Keyword(null,"tag","tag",-1290361223).cljs$core$IFn$_invoke$arity$1(evicted) << ((Math.ceil((Math.log(new cljs.core.Keyword(null,"sets","sets",400955582).cljs$core$IFn$_invoke$arity$1(config)) / Math.log((2)))) | (0)) + (Math.ceil((Math.log(new cljs.core.Keyword(null,"line-size","line-size",8517417).cljs$core$IFn$_invoke$arity$1(config)) / Math.log((2)))) | (0)))),new cljs.core.Keyword(null,"data","data",-232669377),new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(evicted),new cljs.core.Keyword(null,"dirty?","dirty?",-2059845846),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"modified","modified",-2134587826),new cljs.core.Keyword(null,"state","state",-1988618099).cljs$core$IFn$_invoke$arity$1(evicted))], null):null),new cljs.core.Keyword(null,"latency-cycles","latency-cycles",-682475027),(function (){var G__12209 = new cljs.core.Keyword(null,"level","level",1290497552).cljs$core$IFn$_invoke$arity$1(cache);
var G__12209__$1 = (((G__12209 instanceof cljs.core.Keyword))?G__12209.fqn:null);
switch (G__12209__$1) {
case "l1":
return (4);

break;
case "l2":
return (12);

break;
case "l3":
return (36);

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__12209__$1)].join('')));

}
})()], null)], null);
}
}));

(observatory.cache.access.cljs$lang$maxFixedArity = (3));

/** @this {Function} */
(observatory.cache.access.cljs$lang$applyTo = (function (seq12162){
var G__12163 = cljs.core.first(seq12162);
var seq12162__$1 = cljs.core.next(seq12162);
var G__12164 = cljs.core.first(seq12162__$1);
var seq12162__$2 = cljs.core.next(seq12162__$1);
var G__12165 = cljs.core.first(seq12162__$2);
var seq12162__$3 = cljs.core.next(seq12162__$2);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12163,G__12164,G__12165,seq12162__$3);
}));

/**
 * Create a multi-level cache hierarchy.
 * 
 * Example config:
 * {:l1d {:size-kb 32 :associativity 8 :type :data}
 *  :l1i {:size-kb 32 :associativity 8 :type :instruction}
 *  :l2  {:size-kb 256 :associativity 4 :type :unified}
 *  :l3  {:size-kb 8192 :associativity 16 :type :unified}}
 */
observatory.cache.make_cache_hierarchy = (function observatory$cache$make_cache_hierarchy(config){
return new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"l1d","l1d",223086801),(cljs.core.truth_(new cljs.core.Keyword(null,"l1d","l1d",223086801).cljs$core$IFn$_invoke$arity$1(config))?observatory.cache.make_cache(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"l1d","l1d",223086801).cljs$core$IFn$_invoke$arity$1(config),new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"l1","l1",1864175779))):null),new cljs.core.Keyword(null,"l1i","l1i",1449379658),(cljs.core.truth_(new cljs.core.Keyword(null,"l1i","l1i",1449379658).cljs$core$IFn$_invoke$arity$1(config))?observatory.cache.make_cache(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"l1i","l1i",1449379658).cljs$core$IFn$_invoke$arity$1(config),new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"l1","l1",1864175779))):null),new cljs.core.Keyword(null,"l2","l2",-462426027),(cljs.core.truth_(new cljs.core.Keyword(null,"l2","l2",-462426027).cljs$core$IFn$_invoke$arity$1(config))?observatory.cache.make_cache(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"l2","l2",-462426027).cljs$core$IFn$_invoke$arity$1(config),new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"l2","l2",-462426027))):null),new cljs.core.Keyword(null,"l3","l3",1415952440),(cljs.core.truth_(new cljs.core.Keyword(null,"l3","l3",1415952440).cljs$core$IFn$_invoke$arity$1(config))?observatory.cache.make_cache(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"l3","l3",1415952440).cljs$core$IFn$_invoke$arity$1(config),new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"l3","l3",1415952440))):null)], null);
});
/**
 * Access through cache hierarchy.
 * 
 * Checks L1 -> L2 -> L3 -> RAM.
 * On miss, fills all levels.
 * 
 * Returns:
 * {:hierarchy new-hierarchy
 *  :results [{:level :l1 :hit? true/false ...} ...]
 *  :total-latency int
 *  :final-level :l1/:l2/:l3/:ram}
 */
observatory.cache.hierarchy_access = (function observatory$cache$hierarchy_access(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12305 = arguments.length;
var i__5727__auto___12306 = (0);
while(true){
if((i__5727__auto___12306 < len__5726__auto___12305)){
args__5732__auto__.push((arguments[i__5727__auto___12306]));

var G__12308 = (i__5727__auto___12306 + (1));
i__5727__auto___12306 = G__12308;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((3) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((3)),(0),null)):null);
return observatory.cache.hierarchy_access.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__5733__auto__);
});

(observatory.cache.hierarchy_access.cljs$core$IFn$_invoke$arity$variadic = (function (hierarchy,address,timestamp,p__12226){
var vec__12227 = p__12226;
var map__12230 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12227,(0),null);
var map__12230__$1 = cljs.core.__destructure_map(map__12230);
var opts = map__12230__$1;
var write_QMARK_ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12230__$1,new cljs.core.Keyword(null,"write?","write?",673505583));
var instruction_QMARK_ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12230__$1,new cljs.core.Keyword(null,"instruction?","instruction?",1170992380));
var l1_cache = (cljs.core.truth_(instruction_QMARK_)?new cljs.core.Keyword(null,"l1i","l1i",1449379658).cljs$core$IFn$_invoke$arity$1(hierarchy):new cljs.core.Keyword(null,"l1d","l1d",223086801).cljs$core$IFn$_invoke$arity$1(hierarchy));
var levels = (function (){var G__12231 = cljs.core.PersistentVector.EMPTY;
var G__12231__$1 = (cljs.core.truth_(l1_cache)?cljs.core.conj.cljs$core$IFn$_invoke$arity$2(G__12231,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"l1","l1",1864175779),l1_cache,(cljs.core.truth_(instruction_QMARK_)?new cljs.core.Keyword(null,"l1i","l1i",1449379658):new cljs.core.Keyword(null,"l1d","l1d",223086801))], null)):G__12231);
var G__12231__$2 = (cljs.core.truth_(new cljs.core.Keyword(null,"l2","l2",-462426027).cljs$core$IFn$_invoke$arity$1(hierarchy))?cljs.core.conj.cljs$core$IFn$_invoke$arity$2(G__12231__$1,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"l2","l2",-462426027),new cljs.core.Keyword(null,"l2","l2",-462426027).cljs$core$IFn$_invoke$arity$1(hierarchy),new cljs.core.Keyword(null,"l2","l2",-462426027)], null)):G__12231__$1);
if(cljs.core.truth_(new cljs.core.Keyword(null,"l3","l3",1415952440).cljs$core$IFn$_invoke$arity$1(hierarchy))){
return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(G__12231__$2,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"l3","l3",1415952440),new cljs.core.Keyword(null,"l3","l3",1415952440).cljs$core$IFn$_invoke$arity$1(hierarchy),new cljs.core.Keyword(null,"l3","l3",1415952440)], null));
} else {
return G__12231__$2;
}
})();
var remaining = levels;
var current_hierarchy = hierarchy;
var results = cljs.core.PersistentVector.EMPTY;
var found_at = null;
while(true){
if(cljs.core.truth_((function (){var or__5002__auto__ = cljs.core.empty_QMARK_(remaining);
if(or__5002__auto__){
return or__5002__auto__;
} else {
return found_at;
}
})())){
var total_latency = (cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"latency-cycles","latency-cycles",-682475027),results)) + (cljs.core.truth_(found_at)?(0):(100)));
return new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"hierarchy","hierarchy",-1053470341),current_hierarchy,new cljs.core.Keyword(null,"results","results",-1134170113),results,new cljs.core.Keyword(null,"total-latency","total-latency",166706795),total_latency,new cljs.core.Keyword(null,"final-level","final-level",493390115),(function (){var or__5002__auto__ = found_at;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return new cljs.core.Keyword(null,"ram","ram",-1633008146);
}
})()], null);
} else {
var vec__12240 = cljs.core.first(remaining);
var level = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12240,(0),null);
var cache = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12240,(1),null);
var key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12240,(2),null);
var map__12243 = observatory.cache.access.cljs$core$IFn$_invoke$arity$variadic(cache,address,timestamp,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([opts], 0));
var map__12243__$1 = cljs.core.__destructure_map(map__12243);
var cache__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12243__$1,new cljs.core.Keyword(null,"cache","cache",-1237023054));
var result = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12243__$1,new cljs.core.Keyword(null,"result","result",1415092211));
var new_hierarchy = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(current_hierarchy,key,cache__$1);
var new_results = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(results,result);
if(cljs.core.truth_(new cljs.core.Keyword(null,"hit?","hit?",-364237458).cljs$core$IFn$_invoke$arity$1(result))){
var G__12315 = cljs.core.PersistentVector.EMPTY;
var G__12316 = new_hierarchy;
var G__12317 = new_results;
var G__12318 = level;
remaining = G__12315;
current_hierarchy = G__12316;
results = G__12317;
found_at = G__12318;
continue;
} else {
var G__12319 = cljs.core.rest(remaining);
var G__12320 = new_hierarchy;
var G__12321 = new_results;
var G__12322 = null;
remaining = G__12319;
current_hierarchy = G__12320;
results = G__12321;
found_at = G__12322;
continue;
}
}
break;
}
}));

(observatory.cache.hierarchy_access.cljs$lang$maxFixedArity = (3));

/** @this {Function} */
(observatory.cache.hierarchy_access.cljs$lang$applyTo = (function (seq12217){
var G__12218 = cljs.core.first(seq12217);
var seq12217__$1 = cljs.core.next(seq12217);
var G__12219 = cljs.core.first(seq12217__$1);
var seq12217__$2 = cljs.core.next(seq12217__$1);
var G__12220 = cljs.core.first(seq12217__$2);
var seq12217__$3 = cljs.core.next(seq12217__$2);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12218,G__12219,G__12220,seq12217__$3);
}));

/**
 * Replay a sequence of memory accesses through cache hierarchy.
 * 
 * accesses: [{:address 0x... :timestamp 0 :write? false} ...]
 * 
 * Returns sequence of access results with running state.
 */
observatory.cache.replay_accesses = (function observatory$cache$replay_accesses(hierarchy,accesses){
return cljs.core.reductions.cljs$core$IFn$_invoke$arity$3((function (p__12250,p__12251){
var map__12252 = p__12250;
var map__12252__$1 = cljs.core.__destructure_map(map__12252);
var hierarchy__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12252__$1,new cljs.core.Keyword(null,"hierarchy","hierarchy",-1053470341));
var map__12253 = p__12251;
var map__12253__$1 = cljs.core.__destructure_map(map__12253);
var access_info = map__12253__$1;
var address = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12253__$1,new cljs.core.Keyword(null,"address","address",559499426));
var timestamp = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12253__$1,new cljs.core.Keyword(null,"timestamp","timestamp",579478971));
var result = observatory.cache.hierarchy_access.cljs$core$IFn$_invoke$arity$variadic(hierarchy__$1,address,timestamp,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([access_info], 0));
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(result,new cljs.core.Keyword(null,"access","access",2027349272),access_info);
}),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"hierarchy","hierarchy",-1053470341),hierarchy,new cljs.core.Keyword(null,"results","results",-1134170113),cljs.core.PersistentVector.EMPTY,new cljs.core.Keyword(null,"total-latency","total-latency",166706795),(0),new cljs.core.Keyword(null,"final-level","final-level",493390115),null], null),accesses);
});
/**
 * Summarize results of replay-accesses
 */
observatory.cache.summarize_replay = (function observatory$cache$summarize_replay(replay_results){
var results = cljs.core.rest(replay_results);
return new cljs.core.PersistentArrayMap(null, 7, [new cljs.core.Keyword(null,"total-accesses","total-accesses",-1913751768),cljs.core.count(results),new cljs.core.Keyword(null,"l1-hits","l1-hits",1170871182),cljs.core.count(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12254_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"final-level","final-level",493390115).cljs$core$IFn$_invoke$arity$1(p1__12254_SHARP_));
}),results)),new cljs.core.Keyword(null,"l2-hits","l2-hits",-153675918),cljs.core.count(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12255_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"l2","l2",-462426027),new cljs.core.Keyword(null,"final-level","final-level",493390115).cljs$core$IFn$_invoke$arity$1(p1__12255_SHARP_));
}),results)),new cljs.core.Keyword(null,"l3-hits","l3-hits",-1218584748),cljs.core.count(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12256_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"l3","l3",1415952440),new cljs.core.Keyword(null,"final-level","final-level",493390115).cljs$core$IFn$_invoke$arity$1(p1__12256_SHARP_));
}),results)),new cljs.core.Keyword(null,"ram-accesses","ram-accesses",890357625),cljs.core.count(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12257_SHARP_){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"ram","ram",-1633008146),new cljs.core.Keyword(null,"final-level","final-level",493390115).cljs$core$IFn$_invoke$arity$1(p1__12257_SHARP_));
}),results)),new cljs.core.Keyword(null,"total-cycles","total-cycles",-1331413246),cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"total-latency","total-latency",166706795),results)),new cljs.core.Keyword(null,"avg-latency","avg-latency",519741513),(cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,0.0,cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"total-latency","total-latency",166706795),results)) / (function (){var x__5087__auto__ = (1);
var y__5088__auto__ = cljs.core.count(results);
return ((x__5087__auto__ > y__5088__auto__) ? x__5087__auto__ : y__5088__auto__);
})())], null);
});

//# sourceMappingURL=observatory.cache.js.map
