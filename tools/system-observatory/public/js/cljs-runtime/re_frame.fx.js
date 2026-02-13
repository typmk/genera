goog.provide('re_frame.fx');
re_frame.fx.kind = new cljs.core.Keyword(null,"fx","fx",-1237829572);
if(cljs.core.truth_((re_frame.registrar.kinds.cljs$core$IFn$_invoke$arity$1 ? re_frame.registrar.kinds.cljs$core$IFn$_invoke$arity$1(re_frame.fx.kind) : re_frame.registrar.kinds.call(null,re_frame.fx.kind)))){
} else {
throw (new Error("Assert failed: (re-frame.registrar/kinds kind)"));
}
re_frame.fx.reg_fx = (function re_frame$fx$reg_fx(id,handler){
return re_frame.registrar.register_handler(re_frame.fx.kind,id,handler);
});
/**
 * An interceptor whose `:after` actions the contents of `:effects`. As a result,
 *   this interceptor is Domino 3.
 * 
 *   This interceptor is silently added (by reg-event-db etc) to the front of
 *   interceptor chains for all events.
 * 
 *   For each key in `:effects` (a map), it calls the registered `effects handler`
 *   (see `reg-fx` for registration of effect handlers).
 * 
 *   So, if `:effects` was:
 *    {:dispatch  [:hello 42]
 *     :db        {...}
 *     :undo      "set flag"}
 * 
 *   it will call the registered effect handlers for each of the map's keys:
 *   `:dispatch`, `:undo` and `:db`. When calling each handler, provides the map
 *   value for that key - so in the example above the effect handler for :dispatch
 *   will be given one arg `[:hello 42]`.
 * 
 *   You cannot rely on the ordering in which effects are executed, other than that
 *   `:db` is guaranteed to be executed first.
 */
re_frame.fx.do_fx = re_frame.interceptor.__GT_interceptor.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"do-fx","do-fx",1194163050),new cljs.core.Keyword(null,"after","after",594996914),(function re_frame$fx$do_fx_after(context){
if(re_frame.trace.is_trace_enabled_QMARK_()){
var _STAR_current_trace_STAR__orig_val__13244 = re_frame.trace._STAR_current_trace_STAR_;
var _STAR_current_trace_STAR__temp_val__13245 = re_frame.trace.start_trace(new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"op-type","op-type",-1636141668),new cljs.core.Keyword("event","do-fx","event/do-fx",1357330452)], null));
(re_frame.trace._STAR_current_trace_STAR_ = _STAR_current_trace_STAR__temp_val__13245);

try{try{var effects = new cljs.core.Keyword(null,"effects","effects",-282369292).cljs$core$IFn$_invoke$arity$1(context);
var effects_without_db = cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(effects,new cljs.core.Keyword(null,"db","db",993250759));
var temp__5804__auto___13337 = new cljs.core.Keyword(null,"db","db",993250759).cljs$core$IFn$_invoke$arity$1(effects);
if(cljs.core.truth_(temp__5804__auto___13337)){
var new_db_13338 = temp__5804__auto___13337;
var fexpr__13247_13339 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,new cljs.core.Keyword(null,"db","db",993250759),false);
(fexpr__13247_13339.cljs$core$IFn$_invoke$arity$1 ? fexpr__13247_13339.cljs$core$IFn$_invoke$arity$1(new_db_13338) : fexpr__13247_13339.call(null,new_db_13338));
} else {
}

var seq__13248 = cljs.core.seq(effects_without_db);
var chunk__13249 = null;
var count__13250 = (0);
var i__13251 = (0);
while(true){
if((i__13251 < count__13250)){
var vec__13262 = chunk__13249.cljs$core$IIndexed$_nth$arity$2(null,i__13251);
var effect_key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13262,(0),null);
var effect_value = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13262,(1),null);
var temp__5802__auto___13340 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,effect_key,false);
if(cljs.core.truth_(temp__5802__auto___13340)){
var effect_fn_13341 = temp__5802__auto___13340;
(effect_fn_13341.cljs$core$IFn$_invoke$arity$1 ? effect_fn_13341.cljs$core$IFn$_invoke$arity$1(effect_value) : effect_fn_13341.call(null,effect_value));
} else {
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: no handler registered for effect:",effect_key,". Ignoring.",((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"event","event",301435442),effect_key))?["You may be trying to return a coeffect map from an event-fx handler. ","See https://day8.github.io/re-frame/use-cofx-as-fx/"].join(''):null)], 0));
}


var G__13342 = seq__13248;
var G__13343 = chunk__13249;
var G__13344 = count__13250;
var G__13345 = (i__13251 + (1));
seq__13248 = G__13342;
chunk__13249 = G__13343;
count__13250 = G__13344;
i__13251 = G__13345;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__13248);
if(temp__5804__auto__){
var seq__13248__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__13248__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__13248__$1);
var G__13346 = cljs.core.chunk_rest(seq__13248__$1);
var G__13347 = c__5525__auto__;
var G__13348 = cljs.core.count(c__5525__auto__);
var G__13349 = (0);
seq__13248 = G__13346;
chunk__13249 = G__13347;
count__13250 = G__13348;
i__13251 = G__13349;
continue;
} else {
var vec__13265 = cljs.core.first(seq__13248__$1);
var effect_key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13265,(0),null);
var effect_value = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13265,(1),null);
var temp__5802__auto___13350 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,effect_key,false);
if(cljs.core.truth_(temp__5802__auto___13350)){
var effect_fn_13351 = temp__5802__auto___13350;
(effect_fn_13351.cljs$core$IFn$_invoke$arity$1 ? effect_fn_13351.cljs$core$IFn$_invoke$arity$1(effect_value) : effect_fn_13351.call(null,effect_value));
} else {
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: no handler registered for effect:",effect_key,". Ignoring.",((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"event","event",301435442),effect_key))?["You may be trying to return a coeffect map from an event-fx handler. ","See https://day8.github.io/re-frame/use-cofx-as-fx/"].join(''):null)], 0));
}


var G__13352 = cljs.core.next(seq__13248__$1);
var G__13353 = null;
var G__13354 = (0);
var G__13355 = (0);
seq__13248 = G__13352;
chunk__13249 = G__13353;
count__13250 = G__13354;
i__13251 = G__13355;
continue;
}
} else {
return null;
}
}
break;
}
}finally {if(re_frame.trace.is_trace_enabled_QMARK_()){
var end__12936__auto___13356 = re_frame.interop.now();
var duration__12937__auto___13357 = (end__12936__auto___13356 - new cljs.core.Keyword(null,"start","start",-355208981).cljs$core$IFn$_invoke$arity$1(re_frame.trace._STAR_current_trace_STAR_));
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(re_frame.trace.traces,cljs.core.conj,cljs.core.assoc.cljs$core$IFn$_invoke$arity$variadic(re_frame.trace._STAR_current_trace_STAR_,new cljs.core.Keyword(null,"duration","duration",1444101068),duration__12937__auto___13357,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"end","end",-268185958),re_frame.interop.now()], 0)));

re_frame.trace.run_tracing_callbacks_BANG_(end__12936__auto___13356);
} else {
}
}}finally {(re_frame.trace._STAR_current_trace_STAR_ = _STAR_current_trace_STAR__orig_val__13244);
}} else {
var effects = new cljs.core.Keyword(null,"effects","effects",-282369292).cljs$core$IFn$_invoke$arity$1(context);
var effects_without_db = cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(effects,new cljs.core.Keyword(null,"db","db",993250759));
var temp__5804__auto___13358 = new cljs.core.Keyword(null,"db","db",993250759).cljs$core$IFn$_invoke$arity$1(effects);
if(cljs.core.truth_(temp__5804__auto___13358)){
var new_db_13359 = temp__5804__auto___13358;
var fexpr__13269_13360 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,new cljs.core.Keyword(null,"db","db",993250759),false);
(fexpr__13269_13360.cljs$core$IFn$_invoke$arity$1 ? fexpr__13269_13360.cljs$core$IFn$_invoke$arity$1(new_db_13359) : fexpr__13269_13360.call(null,new_db_13359));
} else {
}

var seq__13272 = cljs.core.seq(effects_without_db);
var chunk__13273 = null;
var count__13274 = (0);
var i__13275 = (0);
while(true){
if((i__13275 < count__13274)){
var vec__13283 = chunk__13273.cljs$core$IIndexed$_nth$arity$2(null,i__13275);
var effect_key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13283,(0),null);
var effect_value = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13283,(1),null);
var temp__5802__auto___13361 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,effect_key,false);
if(cljs.core.truth_(temp__5802__auto___13361)){
var effect_fn_13362 = temp__5802__auto___13361;
(effect_fn_13362.cljs$core$IFn$_invoke$arity$1 ? effect_fn_13362.cljs$core$IFn$_invoke$arity$1(effect_value) : effect_fn_13362.call(null,effect_value));
} else {
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: no handler registered for effect:",effect_key,". Ignoring.",((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"event","event",301435442),effect_key))?["You may be trying to return a coeffect map from an event-fx handler. ","See https://day8.github.io/re-frame/use-cofx-as-fx/"].join(''):null)], 0));
}


var G__13363 = seq__13272;
var G__13364 = chunk__13273;
var G__13365 = count__13274;
var G__13366 = (i__13275 + (1));
seq__13272 = G__13363;
chunk__13273 = G__13364;
count__13274 = G__13365;
i__13275 = G__13366;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__13272);
if(temp__5804__auto__){
var seq__13272__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__13272__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__13272__$1);
var G__13367 = cljs.core.chunk_rest(seq__13272__$1);
var G__13368 = c__5525__auto__;
var G__13369 = cljs.core.count(c__5525__auto__);
var G__13370 = (0);
seq__13272 = G__13367;
chunk__13273 = G__13368;
count__13274 = G__13369;
i__13275 = G__13370;
continue;
} else {
var vec__13298 = cljs.core.first(seq__13272__$1);
var effect_key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13298,(0),null);
var effect_value = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13298,(1),null);
var temp__5802__auto___13371 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,effect_key,false);
if(cljs.core.truth_(temp__5802__auto___13371)){
var effect_fn_13372 = temp__5802__auto___13371;
(effect_fn_13372.cljs$core$IFn$_invoke$arity$1 ? effect_fn_13372.cljs$core$IFn$_invoke$arity$1(effect_value) : effect_fn_13372.call(null,effect_value));
} else {
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: no handler registered for effect:",effect_key,". Ignoring.",((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"event","event",301435442),effect_key))?["You may be trying to return a coeffect map from an event-fx handler. ","See https://day8.github.io/re-frame/use-cofx-as-fx/"].join(''):null)], 0));
}


var G__13373 = cljs.core.next(seq__13272__$1);
var G__13374 = null;
var G__13375 = (0);
var G__13376 = (0);
seq__13272 = G__13373;
chunk__13273 = G__13374;
count__13274 = G__13375;
i__13275 = G__13376;
continue;
}
} else {
return null;
}
}
break;
}
}
})], 0));
re_frame.fx.dispatch_later = (function re_frame$fx$dispatch_later(p__13305){
var map__13306 = p__13305;
var map__13306__$1 = cljs.core.__destructure_map(map__13306);
var effect = map__13306__$1;
var ms = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__13306__$1,new cljs.core.Keyword(null,"ms","ms",-1152709733));
var dispatch = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__13306__$1,new cljs.core.Keyword(null,"dispatch","dispatch",1319337009));
if(((cljs.core.empty_QMARK_(dispatch)) || ((!(typeof ms === 'number'))))){
return re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"error","error",-978969032),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: ignoring bad :dispatch-later value:",effect], 0));
} else {
return re_frame.interop.set_timeout_BANG_((function (){
return re_frame.router.dispatch(dispatch);
}),ms);
}
});
re_frame.fx.reg_fx(new cljs.core.Keyword(null,"dispatch-later","dispatch-later",291951390),(function (value){
if(cljs.core.map_QMARK_(value)){
return re_frame.fx.dispatch_later(value);
} else {
var seq__13307 = cljs.core.seq(cljs.core.remove.cljs$core$IFn$_invoke$arity$2(cljs.core.nil_QMARK_,value));
var chunk__13308 = null;
var count__13309 = (0);
var i__13310 = (0);
while(true){
if((i__13310 < count__13309)){
var effect = chunk__13308.cljs$core$IIndexed$_nth$arity$2(null,i__13310);
re_frame.fx.dispatch_later(effect);


var G__13377 = seq__13307;
var G__13378 = chunk__13308;
var G__13379 = count__13309;
var G__13380 = (i__13310 + (1));
seq__13307 = G__13377;
chunk__13308 = G__13378;
count__13309 = G__13379;
i__13310 = G__13380;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__13307);
if(temp__5804__auto__){
var seq__13307__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__13307__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__13307__$1);
var G__13381 = cljs.core.chunk_rest(seq__13307__$1);
var G__13382 = c__5525__auto__;
var G__13383 = cljs.core.count(c__5525__auto__);
var G__13384 = (0);
seq__13307 = G__13381;
chunk__13308 = G__13382;
count__13309 = G__13383;
i__13310 = G__13384;
continue;
} else {
var effect = cljs.core.first(seq__13307__$1);
re_frame.fx.dispatch_later(effect);


var G__13385 = cljs.core.next(seq__13307__$1);
var G__13386 = null;
var G__13387 = (0);
var G__13388 = (0);
seq__13307 = G__13385;
chunk__13308 = G__13386;
count__13309 = G__13387;
i__13310 = G__13388;
continue;
}
} else {
return null;
}
}
break;
}
}
}));
re_frame.fx.reg_fx(new cljs.core.Keyword(null,"fx","fx",-1237829572),(function (seq_of_effects){
if((!(cljs.core.sequential_QMARK_(seq_of_effects)))){
return re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: \":fx\" effect expects a seq, but was given ",cljs.core.type(seq_of_effects)], 0));
} else {
var seq__13311 = cljs.core.seq(cljs.core.remove.cljs$core$IFn$_invoke$arity$2(cljs.core.nil_QMARK_,seq_of_effects));
var chunk__13312 = null;
var count__13313 = (0);
var i__13314 = (0);
while(true){
if((i__13314 < count__13313)){
var vec__13321 = chunk__13312.cljs$core$IIndexed$_nth$arity$2(null,i__13314);
var effect_key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13321,(0),null);
var effect_value = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13321,(1),null);
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"db","db",993250759),effect_key)){
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: \":fx\" effect should not contain a :db effect"], 0));
} else {
}

var temp__5802__auto___13389 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,effect_key,false);
if(cljs.core.truth_(temp__5802__auto___13389)){
var effect_fn_13390 = temp__5802__auto___13389;
(effect_fn_13390.cljs$core$IFn$_invoke$arity$1 ? effect_fn_13390.cljs$core$IFn$_invoke$arity$1(effect_value) : effect_fn_13390.call(null,effect_value));
} else {
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: in \":fx\" effect found ",effect_key," which has no associated handler. Ignoring."], 0));
}


var G__13391 = seq__13311;
var G__13392 = chunk__13312;
var G__13393 = count__13313;
var G__13394 = (i__13314 + (1));
seq__13311 = G__13391;
chunk__13312 = G__13392;
count__13313 = G__13393;
i__13314 = G__13394;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__13311);
if(temp__5804__auto__){
var seq__13311__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__13311__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__13311__$1);
var G__13395 = cljs.core.chunk_rest(seq__13311__$1);
var G__13396 = c__5525__auto__;
var G__13397 = cljs.core.count(c__5525__auto__);
var G__13398 = (0);
seq__13311 = G__13395;
chunk__13312 = G__13396;
count__13313 = G__13397;
i__13314 = G__13398;
continue;
} else {
var vec__13324 = cljs.core.first(seq__13311__$1);
var effect_key = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13324,(0),null);
var effect_value = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13324,(1),null);
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"db","db",993250759),effect_key)){
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: \":fx\" effect should not contain a :db effect"], 0));
} else {
}

var temp__5802__auto___13399 = re_frame.registrar.get_handler.cljs$core$IFn$_invoke$arity$3(re_frame.fx.kind,effect_key,false);
if(cljs.core.truth_(temp__5802__auto___13399)){
var effect_fn_13400 = temp__5802__auto___13399;
(effect_fn_13400.cljs$core$IFn$_invoke$arity$1 ? effect_fn_13400.cljs$core$IFn$_invoke$arity$1(effect_value) : effect_fn_13400.call(null,effect_value));
} else {
re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"warn","warn",-436710552),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: in \":fx\" effect found ",effect_key," which has no associated handler. Ignoring."], 0));
}


var G__13401 = cljs.core.next(seq__13311__$1);
var G__13402 = null;
var G__13403 = (0);
var G__13404 = (0);
seq__13311 = G__13401;
chunk__13312 = G__13402;
count__13313 = G__13403;
i__13314 = G__13404;
continue;
}
} else {
return null;
}
}
break;
}
}
}));
re_frame.fx.reg_fx(new cljs.core.Keyword(null,"dispatch","dispatch",1319337009),(function (value){
if((!(cljs.core.vector_QMARK_(value)))){
return re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"error","error",-978969032),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: ignoring bad :dispatch value. Expected a vector, but got:",value], 0));
} else {
return re_frame.router.dispatch(value);
}
}));
re_frame.fx.reg_fx(new cljs.core.Keyword(null,"dispatch-n","dispatch-n",-504469236),(function (value){
if((!(cljs.core.sequential_QMARK_(value)))){
return re_frame.loggers.console.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword(null,"error","error",-978969032),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["re-frame: ignoring bad :dispatch-n value. Expected a collection, but got:",value], 0));
} else {
var seq__13327 = cljs.core.seq(cljs.core.remove.cljs$core$IFn$_invoke$arity$2(cljs.core.nil_QMARK_,value));
var chunk__13328 = null;
var count__13329 = (0);
var i__13330 = (0);
while(true){
if((i__13330 < count__13329)){
var event = chunk__13328.cljs$core$IIndexed$_nth$arity$2(null,i__13330);
re_frame.router.dispatch(event);


var G__13405 = seq__13327;
var G__13406 = chunk__13328;
var G__13407 = count__13329;
var G__13408 = (i__13330 + (1));
seq__13327 = G__13405;
chunk__13328 = G__13406;
count__13329 = G__13407;
i__13330 = G__13408;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__13327);
if(temp__5804__auto__){
var seq__13327__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__13327__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__13327__$1);
var G__13409 = cljs.core.chunk_rest(seq__13327__$1);
var G__13410 = c__5525__auto__;
var G__13411 = cljs.core.count(c__5525__auto__);
var G__13412 = (0);
seq__13327 = G__13409;
chunk__13328 = G__13410;
count__13329 = G__13411;
i__13330 = G__13412;
continue;
} else {
var event = cljs.core.first(seq__13327__$1);
re_frame.router.dispatch(event);


var G__13413 = cljs.core.next(seq__13327__$1);
var G__13414 = null;
var G__13415 = (0);
var G__13416 = (0);
seq__13327 = G__13413;
chunk__13328 = G__13414;
count__13329 = G__13415;
i__13330 = G__13416;
continue;
}
} else {
return null;
}
}
break;
}
}
}));
re_frame.fx.reg_fx(new cljs.core.Keyword(null,"deregister-event-handler","deregister-event-handler",-1096518994),(function (value){
var clear_event = cljs.core.partial.cljs$core$IFn$_invoke$arity$2(re_frame.registrar.clear_handlers,re_frame.events.kind);
if(cljs.core.sequential_QMARK_(value)){
var seq__13331 = cljs.core.seq(value);
var chunk__13332 = null;
var count__13333 = (0);
var i__13334 = (0);
while(true){
if((i__13334 < count__13333)){
var event = chunk__13332.cljs$core$IIndexed$_nth$arity$2(null,i__13334);
clear_event(event);


var G__13417 = seq__13331;
var G__13418 = chunk__13332;
var G__13419 = count__13333;
var G__13420 = (i__13334 + (1));
seq__13331 = G__13417;
chunk__13332 = G__13418;
count__13333 = G__13419;
i__13334 = G__13420;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__13331);
if(temp__5804__auto__){
var seq__13331__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__13331__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__13331__$1);
var G__13421 = cljs.core.chunk_rest(seq__13331__$1);
var G__13422 = c__5525__auto__;
var G__13423 = cljs.core.count(c__5525__auto__);
var G__13424 = (0);
seq__13331 = G__13421;
chunk__13332 = G__13422;
count__13333 = G__13423;
i__13334 = G__13424;
continue;
} else {
var event = cljs.core.first(seq__13331__$1);
clear_event(event);


var G__13425 = cljs.core.next(seq__13331__$1);
var G__13426 = null;
var G__13427 = (0);
var G__13428 = (0);
seq__13331 = G__13425;
chunk__13332 = G__13426;
count__13333 = G__13427;
i__13334 = G__13428;
continue;
}
} else {
return null;
}
}
break;
}
} else {
return clear_event(value);
}
}));
re_frame.fx.reg_fx(new cljs.core.Keyword(null,"db","db",993250759),(function (value){
if((!((cljs.core.deref(re_frame.db.app_db) === value)))){
return cljs.core.reset_BANG_(re_frame.db.app_db,value);
} else {
if(re_frame.trace.is_trace_enabled_QMARK_()){
var _STAR_current_trace_STAR__orig_val__13335 = re_frame.trace._STAR_current_trace_STAR_;
var _STAR_current_trace_STAR__temp_val__13336 = re_frame.trace.start_trace(new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"op-type","op-type",-1636141668),new cljs.core.Keyword("reagent","quiescent","reagent/quiescent",-16138681)], null));
(re_frame.trace._STAR_current_trace_STAR_ = _STAR_current_trace_STAR__temp_val__13336);

try{try{return null;
}finally {if(re_frame.trace.is_trace_enabled_QMARK_()){
var end__12936__auto___13429 = re_frame.interop.now();
var duration__12937__auto___13430 = (end__12936__auto___13429 - new cljs.core.Keyword(null,"start","start",-355208981).cljs$core$IFn$_invoke$arity$1(re_frame.trace._STAR_current_trace_STAR_));
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(re_frame.trace.traces,cljs.core.conj,cljs.core.assoc.cljs$core$IFn$_invoke$arity$variadic(re_frame.trace._STAR_current_trace_STAR_,new cljs.core.Keyword(null,"duration","duration",1444101068),duration__12937__auto___13430,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"end","end",-268185958),re_frame.interop.now()], 0)));

re_frame.trace.run_tracing_callbacks_BANG_(end__12936__auto___13429);
} else {
}
}}finally {(re_frame.trace._STAR_current_trace_STAR_ = _STAR_current_trace_STAR__orig_val__13335);
}} else {
return null;
}
}
}));

//# sourceMappingURL=re_frame.fx.js.map
