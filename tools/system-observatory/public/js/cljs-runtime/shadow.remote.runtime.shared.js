goog.provide('shadow.remote.runtime.shared');
shadow.remote.runtime.shared.init_state = (function shadow$remote$runtime$shared$init_state(client_info){
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"extensions","extensions",-1103629196),cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"ops","ops",1237330063),cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"client-info","client-info",1958982504),client_info,new cljs.core.Keyword(null,"call-id-seq","call-id-seq",-1679248218),(0),new cljs.core.Keyword(null,"call-handlers","call-handlers",386605551),cljs.core.PersistentArrayMap.EMPTY], null);
});
shadow.remote.runtime.shared.now = (function shadow$remote$runtime$shared$now(){
return Date.now();
});
shadow.remote.runtime.shared.get_client_id = (function shadow$remote$runtime$shared$get_client_id(p__14811){
var map__14812 = p__14811;
var map__14812__$1 = cljs.core.__destructure_map(map__14812);
var runtime = map__14812__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14812__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
var or__5002__auto__ = new cljs.core.Keyword(null,"client-id","client-id",-464622140).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(state_ref));
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
throw cljs.core.ex_info.cljs$core$IFn$_invoke$arity$2("runtime has no assigned runtime-id",new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"runtime","runtime",-1331573996),runtime], null));
}
});
shadow.remote.runtime.shared.relay_msg = (function shadow$remote$runtime$shared$relay_msg(runtime,msg){
var self_id_15018 = shadow.remote.runtime.shared.get_client_id(runtime);
if(cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"to","to",192099007).cljs$core$IFn$_invoke$arity$1(msg),self_id_15018)){
shadow.remote.runtime.api.relay_msg(runtime,msg);
} else {
Promise.resolve((1)).then((function (){
var G__14817 = runtime;
var G__14818 = cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(msg,new cljs.core.Keyword(null,"from","from",1815293044),self_id_15018);
return (shadow.remote.runtime.shared.process.cljs$core$IFn$_invoke$arity$2 ? shadow.remote.runtime.shared.process.cljs$core$IFn$_invoke$arity$2(G__14817,G__14818) : shadow.remote.runtime.shared.process.call(null,G__14817,G__14818));
}));
}

return msg;
});
shadow.remote.runtime.shared.reply = (function shadow$remote$runtime$shared$reply(runtime,p__14823,res){
var map__14824 = p__14823;
var map__14824__$1 = cljs.core.__destructure_map(map__14824);
var call_id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14824__$1,new cljs.core.Keyword(null,"call-id","call-id",1043012968));
var from = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14824__$1,new cljs.core.Keyword(null,"from","from",1815293044));
var res__$1 = (function (){var G__14825 = res;
var G__14825__$1 = (cljs.core.truth_(call_id)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__14825,new cljs.core.Keyword(null,"call-id","call-id",1043012968),call_id):G__14825);
if(cljs.core.truth_(from)){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__14825__$1,new cljs.core.Keyword(null,"to","to",192099007),from);
} else {
return G__14825__$1;
}
})();
return shadow.remote.runtime.api.relay_msg(runtime,res__$1);
});
shadow.remote.runtime.shared.call = (function shadow$remote$runtime$shared$call(var_args){
var G__14827 = arguments.length;
switch (G__14827) {
case 3:
return shadow.remote.runtime.shared.call.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
case 4:
return shadow.remote.runtime.shared.call.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.remote.runtime.shared.call.cljs$core$IFn$_invoke$arity$3 = (function (runtime,msg,handlers){
return shadow.remote.runtime.shared.call.cljs$core$IFn$_invoke$arity$4(runtime,msg,handlers,(0));
}));

(shadow.remote.runtime.shared.call.cljs$core$IFn$_invoke$arity$4 = (function (p__14828,msg,handlers,timeout_after_ms){
var map__14829 = p__14828;
var map__14829__$1 = cljs.core.__destructure_map(map__14829);
var runtime = map__14829__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14829__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
if(cljs.core.map_QMARK_(msg)){
} else {
throw (new Error("Assert failed: (map? msg)"));
}

if(cljs.core.map_QMARK_(handlers)){
} else {
throw (new Error("Assert failed: (map? handlers)"));
}

if(cljs.core.nat_int_QMARK_(timeout_after_ms)){
} else {
throw (new Error("Assert failed: (nat-int? timeout-after-ms)"));
}

var call_id = new cljs.core.Keyword(null,"call-id-seq","call-id-seq",-1679248218).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(state_ref));
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(state_ref,cljs.core.update,new cljs.core.Keyword(null,"call-id-seq","call-id-seq",-1679248218),cljs.core.inc);

cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(state_ref,cljs.core.assoc_in,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"call-handlers","call-handlers",386605551),call_id], null),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"handlers","handlers",79528781),handlers,new cljs.core.Keyword(null,"called-at","called-at",607081160),shadow.remote.runtime.shared.now(),new cljs.core.Keyword(null,"msg","msg",-1386103444),msg,new cljs.core.Keyword(null,"timeout","timeout",-318625318),timeout_after_ms], null));

return shadow.remote.runtime.api.relay_msg(runtime,cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(msg,new cljs.core.Keyword(null,"call-id","call-id",1043012968),call_id));
}));

(shadow.remote.runtime.shared.call.cljs$lang$maxFixedArity = 4);

shadow.remote.runtime.shared.trigger_BANG_ = (function shadow$remote$runtime$shared$trigger_BANG_(var_args){
var args__5732__auto__ = [];
var len__5726__auto___15028 = arguments.length;
var i__5727__auto___15029 = (0);
while(true){
if((i__5727__auto___15029 < len__5726__auto___15028)){
args__5732__auto__.push((arguments[i__5727__auto___15029]));

var G__15030 = (i__5727__auto___15029 + (1));
i__5727__auto___15029 = G__15030;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((2) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((2)),(0),null)):null);
return shadow.remote.runtime.shared.trigger_BANG_.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__5733__auto__);
});

(shadow.remote.runtime.shared.trigger_BANG_.cljs$core$IFn$_invoke$arity$variadic = (function (p__14835,ev,args){
var map__14836 = p__14835;
var map__14836__$1 = cljs.core.__destructure_map(map__14836);
var runtime = map__14836__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14836__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
var seq__14837 = cljs.core.seq(cljs.core.vals(new cljs.core.Keyword(null,"extensions","extensions",-1103629196).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(state_ref))));
var chunk__14840 = null;
var count__14841 = (0);
var i__14842 = (0);
while(true){
if((i__14842 < count__14841)){
var ext = chunk__14840.cljs$core$IIndexed$_nth$arity$2(null,i__14842);
var ev_fn = cljs.core.get.cljs$core$IFn$_invoke$arity$2(ext,ev);
if(cljs.core.truth_(ev_fn)){
cljs.core.apply.cljs$core$IFn$_invoke$arity$2(ev_fn,args);


var G__15036 = seq__14837;
var G__15037 = chunk__14840;
var G__15038 = count__14841;
var G__15039 = (i__14842 + (1));
seq__14837 = G__15036;
chunk__14840 = G__15037;
count__14841 = G__15038;
i__14842 = G__15039;
continue;
} else {
var G__15040 = seq__14837;
var G__15041 = chunk__14840;
var G__15042 = count__14841;
var G__15043 = (i__14842 + (1));
seq__14837 = G__15040;
chunk__14840 = G__15041;
count__14841 = G__15042;
i__14842 = G__15043;
continue;
}
} else {
var temp__5804__auto__ = cljs.core.seq(seq__14837);
if(temp__5804__auto__){
var seq__14837__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14837__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__14837__$1);
var G__15045 = cljs.core.chunk_rest(seq__14837__$1);
var G__15046 = c__5525__auto__;
var G__15047 = cljs.core.count(c__5525__auto__);
var G__15048 = (0);
seq__14837 = G__15045;
chunk__14840 = G__15046;
count__14841 = G__15047;
i__14842 = G__15048;
continue;
} else {
var ext = cljs.core.first(seq__14837__$1);
var ev_fn = cljs.core.get.cljs$core$IFn$_invoke$arity$2(ext,ev);
if(cljs.core.truth_(ev_fn)){
cljs.core.apply.cljs$core$IFn$_invoke$arity$2(ev_fn,args);


var G__15049 = cljs.core.next(seq__14837__$1);
var G__15050 = null;
var G__15051 = (0);
var G__15052 = (0);
seq__14837 = G__15049;
chunk__14840 = G__15050;
count__14841 = G__15051;
i__14842 = G__15052;
continue;
} else {
var G__15053 = cljs.core.next(seq__14837__$1);
var G__15054 = null;
var G__15055 = (0);
var G__15056 = (0);
seq__14837 = G__15053;
chunk__14840 = G__15054;
count__14841 = G__15055;
i__14842 = G__15056;
continue;
}
}
} else {
return null;
}
}
break;
}
}));

(shadow.remote.runtime.shared.trigger_BANG_.cljs$lang$maxFixedArity = (2));

/** @this {Function} */
(shadow.remote.runtime.shared.trigger_BANG_.cljs$lang$applyTo = (function (seq14832){
var G__14833 = cljs.core.first(seq14832);
var seq14832__$1 = cljs.core.next(seq14832);
var G__14834 = cljs.core.first(seq14832__$1);
var seq14832__$2 = cljs.core.next(seq14832__$1);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__14833,G__14834,seq14832__$2);
}));

shadow.remote.runtime.shared.welcome = (function shadow$remote$runtime$shared$welcome(p__14863,p__14864){
var map__14865 = p__14863;
var map__14865__$1 = cljs.core.__destructure_map(map__14865);
var runtime = map__14865__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14865__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
var map__14866 = p__14864;
var map__14866__$1 = cljs.core.__destructure_map(map__14866);
var msg = map__14866__$1;
var client_id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14866__$1,new cljs.core.Keyword(null,"client-id","client-id",-464622140));
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$variadic(state_ref,cljs.core.assoc,new cljs.core.Keyword(null,"client-id","client-id",-464622140),client_id,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"welcome","welcome",-578152123),true], 0));

var map__14867 = cljs.core.deref(state_ref);
var map__14867__$1 = cljs.core.__destructure_map(map__14867);
var client_info = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14867__$1,new cljs.core.Keyword(null,"client-info","client-info",1958982504));
var extensions = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14867__$1,new cljs.core.Keyword(null,"extensions","extensions",-1103629196));
shadow.remote.runtime.shared.relay_msg(runtime,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"hello","hello",-245025397),new cljs.core.Keyword(null,"client-info","client-info",1958982504),client_info], null));

return shadow.remote.runtime.shared.trigger_BANG_(runtime,new cljs.core.Keyword(null,"on-welcome","on-welcome",1895317125));
});
shadow.remote.runtime.shared.ping = (function shadow$remote$runtime$shared$ping(runtime,msg){
return shadow.remote.runtime.shared.reply(runtime,msg,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"pong","pong",-172484958)], null));
});
shadow.remote.runtime.shared.request_supported_ops = (function shadow$remote$runtime$shared$request_supported_ops(p__14877,msg){
var map__14878 = p__14877;
var map__14878__$1 = cljs.core.__destructure_map(map__14878);
var runtime = map__14878__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14878__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
return shadow.remote.runtime.shared.reply(runtime,msg,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"supported-ops","supported-ops",337914702),new cljs.core.Keyword(null,"ops","ops",1237330063),cljs.core.disj.cljs$core$IFn$_invoke$arity$variadic(cljs.core.set(cljs.core.keys(new cljs.core.Keyword(null,"ops","ops",1237330063).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(state_ref)))),new cljs.core.Keyword(null,"welcome","welcome",-578152123),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"unknown-relay-op","unknown-relay-op",170832753),new cljs.core.Keyword(null,"unknown-op","unknown-op",1900385996),new cljs.core.Keyword(null,"request-supported-ops","request-supported-ops",-1034994502),new cljs.core.Keyword(null,"tool-disconnect","tool-disconnect",189103996)], 0))], null));
});
shadow.remote.runtime.shared.unknown_relay_op = (function shadow$remote$runtime$shared$unknown_relay_op(msg){
return console.warn("unknown-relay-op",msg);
});
shadow.remote.runtime.shared.unknown_op = (function shadow$remote$runtime$shared$unknown_op(msg){
return console.warn("unknown-op",msg);
});
shadow.remote.runtime.shared.add_extension_STAR_ = (function shadow$remote$runtime$shared$add_extension_STAR_(p__14891,key,p__14892){
var map__14894 = p__14891;
var map__14894__$1 = cljs.core.__destructure_map(map__14894);
var state = map__14894__$1;
var extensions = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14894__$1,new cljs.core.Keyword(null,"extensions","extensions",-1103629196));
var map__14895 = p__14892;
var map__14895__$1 = cljs.core.__destructure_map(map__14895);
var spec = map__14895__$1;
var ops = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14895__$1,new cljs.core.Keyword(null,"ops","ops",1237330063));
var transit_write_handlers = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14895__$1,new cljs.core.Keyword(null,"transit-write-handlers","transit-write-handlers",1886308716));
if(cljs.core.contains_QMARK_(extensions,key)){
throw cljs.core.ex_info.cljs$core$IFn$_invoke$arity$2("extension already registered",new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"key","key",-1516042587),key,new cljs.core.Keyword(null,"spec","spec",347520401),spec], null));
} else {
}

return cljs.core.reduce_kv((function (state__$1,op_kw,op_handler){
if(cljs.core.truth_(cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(state__$1,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ops","ops",1237330063),op_kw], null)))){
throw cljs.core.ex_info.cljs$core$IFn$_invoke$arity$2("op already registered",new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"key","key",-1516042587),key,new cljs.core.Keyword(null,"op","op",-1882987955),op_kw], null));
} else {
}

return cljs.core.assoc_in(state__$1,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ops","ops",1237330063),op_kw], null),op_handler);
}),cljs.core.assoc_in(state,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"extensions","extensions",-1103629196),key], null),spec),ops);
});
shadow.remote.runtime.shared.add_extension = (function shadow$remote$runtime$shared$add_extension(p__14902,key,spec){
var map__14904 = p__14902;
var map__14904__$1 = cljs.core.__destructure_map(map__14904);
var runtime = map__14904__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14904__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(state_ref,shadow.remote.runtime.shared.add_extension_STAR_,key,spec);

var temp__5808__auto___15070 = new cljs.core.Keyword(null,"on-welcome","on-welcome",1895317125).cljs$core$IFn$_invoke$arity$1(spec);
if((temp__5808__auto___15070 == null)){
} else {
var on_welcome_15071 = temp__5808__auto___15070;
if(cljs.core.truth_(new cljs.core.Keyword(null,"welcome","welcome",-578152123).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(state_ref)))){
(on_welcome_15071.cljs$core$IFn$_invoke$arity$0 ? on_welcome_15071.cljs$core$IFn$_invoke$arity$0() : on_welcome_15071.call(null));
} else {
}
}

return runtime;
});
shadow.remote.runtime.shared.add_defaults = (function shadow$remote$runtime$shared$add_defaults(runtime){
return shadow.remote.runtime.shared.add_extension(runtime,new cljs.core.Keyword("shadow.remote.runtime.shared","defaults","shadow.remote.runtime.shared/defaults",-1821257543),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"ops","ops",1237330063),new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"welcome","welcome",-578152123),(function (p1__14907_SHARP_){
return shadow.remote.runtime.shared.welcome(runtime,p1__14907_SHARP_);
}),new cljs.core.Keyword(null,"unknown-relay-op","unknown-relay-op",170832753),(function (p1__14908_SHARP_){
return shadow.remote.runtime.shared.unknown_relay_op(p1__14908_SHARP_);
}),new cljs.core.Keyword(null,"unknown-op","unknown-op",1900385996),(function (p1__14909_SHARP_){
return shadow.remote.runtime.shared.unknown_op(p1__14909_SHARP_);
}),new cljs.core.Keyword(null,"ping","ping",-1670114784),(function (p1__14910_SHARP_){
return shadow.remote.runtime.shared.ping(runtime,p1__14910_SHARP_);
}),new cljs.core.Keyword(null,"request-supported-ops","request-supported-ops",-1034994502),(function (p1__14911_SHARP_){
return shadow.remote.runtime.shared.request_supported_ops(runtime,p1__14911_SHARP_);
})], null)], null));
});
shadow.remote.runtime.shared.del_extension_STAR_ = (function shadow$remote$runtime$shared$del_extension_STAR_(state,key){
var ext = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(state,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"extensions","extensions",-1103629196),key], null));
if(cljs.core.not(ext)){
return state;
} else {
return cljs.core.reduce_kv((function (state__$1,op_kw,op_handler){
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$4(state__$1,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ops","ops",1237330063)], null),cljs.core.dissoc,op_kw);
}),cljs.core.update.cljs$core$IFn$_invoke$arity$4(state,new cljs.core.Keyword(null,"extensions","extensions",-1103629196),cljs.core.dissoc,key),new cljs.core.Keyword(null,"ops","ops",1237330063).cljs$core$IFn$_invoke$arity$1(ext));
}
});
shadow.remote.runtime.shared.del_extension = (function shadow$remote$runtime$shared$del_extension(p__14919,key){
var map__14920 = p__14919;
var map__14920__$1 = cljs.core.__destructure_map(map__14920);
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14920__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(state_ref,shadow.remote.runtime.shared.del_extension_STAR_,key);
});
shadow.remote.runtime.shared.unhandled_call_result = (function shadow$remote$runtime$shared$unhandled_call_result(call_config,msg){
return console.warn("unhandled call result",msg,call_config);
});
shadow.remote.runtime.shared.unhandled_client_not_found = (function shadow$remote$runtime$shared$unhandled_client_not_found(p__14926,msg){
var map__14927 = p__14926;
var map__14927__$1 = cljs.core.__destructure_map(map__14927);
var runtime = map__14927__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14927__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
return shadow.remote.runtime.shared.trigger_BANG_.cljs$core$IFn$_invoke$arity$variadic(runtime,new cljs.core.Keyword(null,"on-client-not-found","on-client-not-found",-642452849),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([msg], 0));
});
shadow.remote.runtime.shared.reply_unknown_op = (function shadow$remote$runtime$shared$reply_unknown_op(runtime,msg){
return shadow.remote.runtime.shared.reply(runtime,msg,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"unknown-op","unknown-op",1900385996),new cljs.core.Keyword(null,"msg","msg",-1386103444),msg], null));
});
shadow.remote.runtime.shared.process = (function shadow$remote$runtime$shared$process(p__14940,p__14941){
var map__14944 = p__14940;
var map__14944__$1 = cljs.core.__destructure_map(map__14944);
var runtime = map__14944__$1;
var state_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14944__$1,new cljs.core.Keyword(null,"state-ref","state-ref",2127874952));
var map__14945 = p__14941;
var map__14945__$1 = cljs.core.__destructure_map(map__14945);
var msg = map__14945__$1;
var op = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14945__$1,new cljs.core.Keyword(null,"op","op",-1882987955));
var call_id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14945__$1,new cljs.core.Keyword(null,"call-id","call-id",1043012968));
var state = cljs.core.deref(state_ref);
var op_handler = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(state,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ops","ops",1237330063),op], null));
if(cljs.core.truth_(call_id)){
var cfg = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(state,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"call-handlers","call-handlers",386605551),call_id], null));
var call_handler = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(cfg,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"handlers","handlers",79528781),op], null));
if(cljs.core.truth_(call_handler)){
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$variadic(state_ref,cljs.core.update,new cljs.core.Keyword(null,"call-handlers","call-handlers",386605551),cljs.core.dissoc,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([call_id], 0));

return (call_handler.cljs$core$IFn$_invoke$arity$1 ? call_handler.cljs$core$IFn$_invoke$arity$1(msg) : call_handler.call(null,msg));
} else {
if(cljs.core.truth_(op_handler)){
return (op_handler.cljs$core$IFn$_invoke$arity$1 ? op_handler.cljs$core$IFn$_invoke$arity$1(msg) : op_handler.call(null,msg));
} else {
return shadow.remote.runtime.shared.unhandled_call_result(cfg,msg);

}
}
} else {
if(cljs.core.truth_(op_handler)){
return (op_handler.cljs$core$IFn$_invoke$arity$1 ? op_handler.cljs$core$IFn$_invoke$arity$1(msg) : op_handler.call(null,msg));
} else {
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"client-not-found","client-not-found",-1754042614),op)){
return shadow.remote.runtime.shared.unhandled_client_not_found(runtime,msg);
} else {
return shadow.remote.runtime.shared.reply_unknown_op(runtime,msg);

}
}
}
});
shadow.remote.runtime.shared.run_on_idle = (function shadow$remote$runtime$shared$run_on_idle(state_ref){
var seq__14957 = cljs.core.seq(cljs.core.vals(new cljs.core.Keyword(null,"extensions","extensions",-1103629196).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(state_ref))));
var chunk__14959 = null;
var count__14960 = (0);
var i__14961 = (0);
while(true){
if((i__14961 < count__14960)){
var map__14977 = chunk__14959.cljs$core$IIndexed$_nth$arity$2(null,i__14961);
var map__14977__$1 = cljs.core.__destructure_map(map__14977);
var on_idle = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14977__$1,new cljs.core.Keyword(null,"on-idle","on-idle",2044706602));
if(cljs.core.truth_(on_idle)){
(on_idle.cljs$core$IFn$_invoke$arity$0 ? on_idle.cljs$core$IFn$_invoke$arity$0() : on_idle.call(null));


var G__15074 = seq__14957;
var G__15075 = chunk__14959;
var G__15076 = count__14960;
var G__15077 = (i__14961 + (1));
seq__14957 = G__15074;
chunk__14959 = G__15075;
count__14960 = G__15076;
i__14961 = G__15077;
continue;
} else {
var G__15078 = seq__14957;
var G__15079 = chunk__14959;
var G__15080 = count__14960;
var G__15081 = (i__14961 + (1));
seq__14957 = G__15078;
chunk__14959 = G__15079;
count__14960 = G__15080;
i__14961 = G__15081;
continue;
}
} else {
var temp__5804__auto__ = cljs.core.seq(seq__14957);
if(temp__5804__auto__){
var seq__14957__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14957__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__14957__$1);
var G__15084 = cljs.core.chunk_rest(seq__14957__$1);
var G__15085 = c__5525__auto__;
var G__15086 = cljs.core.count(c__5525__auto__);
var G__15087 = (0);
seq__14957 = G__15084;
chunk__14959 = G__15085;
count__14960 = G__15086;
i__14961 = G__15087;
continue;
} else {
var map__14983 = cljs.core.first(seq__14957__$1);
var map__14983__$1 = cljs.core.__destructure_map(map__14983);
var on_idle = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__14983__$1,new cljs.core.Keyword(null,"on-idle","on-idle",2044706602));
if(cljs.core.truth_(on_idle)){
(on_idle.cljs$core$IFn$_invoke$arity$0 ? on_idle.cljs$core$IFn$_invoke$arity$0() : on_idle.call(null));


var G__15089 = cljs.core.next(seq__14957__$1);
var G__15090 = null;
var G__15091 = (0);
var G__15092 = (0);
seq__14957 = G__15089;
chunk__14959 = G__15090;
count__14960 = G__15091;
i__14961 = G__15092;
continue;
} else {
var G__15093 = cljs.core.next(seq__14957__$1);
var G__15094 = null;
var G__15095 = (0);
var G__15096 = (0);
seq__14957 = G__15093;
chunk__14959 = G__15094;
count__14960 = G__15095;
i__14961 = G__15096;
continue;
}
}
} else {
return null;
}
}
break;
}
});

//# sourceMappingURL=shadow.remote.runtime.shared.js.map
