goog.provide('shadow.remote.runtime.tap_support');
shadow.remote.runtime.tap_support.tap_subscribe = (function shadow$remote$runtime$tap_support$tap_subscribe(p__16699,p__16700){
var map__16701 = p__16699;
var map__16701__$1 = cljs.core.__destructure_map(map__16701);
var svc = map__16701__$1;
var subs_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16701__$1,new cljs.core.Keyword(null,"subs-ref","subs-ref",-1355989911));
var obj_support = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16701__$1,new cljs.core.Keyword(null,"obj-support","obj-support",1522559229));
var runtime = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16701__$1,new cljs.core.Keyword(null,"runtime","runtime",-1331573996));
var map__16702 = p__16700;
var map__16702__$1 = cljs.core.__destructure_map(map__16702);
var msg = map__16702__$1;
var from = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16702__$1,new cljs.core.Keyword(null,"from","from",1815293044));
var summary = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16702__$1,new cljs.core.Keyword(null,"summary","summary",380847952));
var history__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16702__$1,new cljs.core.Keyword(null,"history","history",-247395220));
var num = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__16702__$1,new cljs.core.Keyword(null,"num","num",1985240673),(10));
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(subs_ref,cljs.core.assoc,from,msg);

if(cljs.core.truth_(history__$1)){
return shadow.remote.runtime.shared.reply(runtime,msg,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"tap-subscribed","tap-subscribed",-1882247432),new cljs.core.Keyword(null,"history","history",-247395220),cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY,cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (oid){
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"oid","oid",-768692334),oid,new cljs.core.Keyword(null,"summary","summary",380847952),shadow.remote.runtime.obj_support.obj_describe_STAR_(obj_support,oid)], null);
}),shadow.remote.runtime.obj_support.get_tap_history(obj_support,num)))], null));
} else {
return null;
}
});
shadow.remote.runtime.tap_support.tap_unsubscribe = (function shadow$remote$runtime$tap_support$tap_unsubscribe(p__16703,p__16704){
var map__16705 = p__16703;
var map__16705__$1 = cljs.core.__destructure_map(map__16705);
var subs_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16705__$1,new cljs.core.Keyword(null,"subs-ref","subs-ref",-1355989911));
var map__16706 = p__16704;
var map__16706__$1 = cljs.core.__destructure_map(map__16706);
var from = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16706__$1,new cljs.core.Keyword(null,"from","from",1815293044));
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(subs_ref,cljs.core.dissoc,from);
});
shadow.remote.runtime.tap_support.request_tap_history = (function shadow$remote$runtime$tap_support$request_tap_history(p__16707,p__16708){
var map__16709 = p__16707;
var map__16709__$1 = cljs.core.__destructure_map(map__16709);
var obj_support = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16709__$1,new cljs.core.Keyword(null,"obj-support","obj-support",1522559229));
var runtime = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16709__$1,new cljs.core.Keyword(null,"runtime","runtime",-1331573996));
var map__16710 = p__16708;
var map__16710__$1 = cljs.core.__destructure_map(map__16710);
var msg = map__16710__$1;
var num = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__16710__$1,new cljs.core.Keyword(null,"num","num",1985240673),(10));
var tap_ids = shadow.remote.runtime.obj_support.get_tap_history(obj_support,num);
return shadow.remote.runtime.shared.reply(runtime,msg,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"tap-history","tap-history",-282803347),new cljs.core.Keyword(null,"oids","oids",-1580877688),tap_ids], null));
});
shadow.remote.runtime.tap_support.tool_disconnect = (function shadow$remote$runtime$tap_support$tool_disconnect(p__16711,tid){
var map__16712 = p__16711;
var map__16712__$1 = cljs.core.__destructure_map(map__16712);
var svc = map__16712__$1;
var subs_ref = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16712__$1,new cljs.core.Keyword(null,"subs-ref","subs-ref",-1355989911));
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(subs_ref,cljs.core.dissoc,tid);
});
shadow.remote.runtime.tap_support.start = (function shadow$remote$runtime$tap_support$start(runtime,obj_support){
var subs_ref = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var tap_fn = (function shadow$remote$runtime$tap_support$start_$_runtime_tap(obj){
if((!((obj == null)))){
var oid = shadow.remote.runtime.obj_support.register(obj_support,obj,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"from","from",1815293044),new cljs.core.Keyword(null,"tap","tap",-1086702463)], null));
var seq__16717 = cljs.core.seq(cljs.core.deref(subs_ref));
var chunk__16718 = null;
var count__16719 = (0);
var i__16720 = (0);
while(true){
if((i__16720 < count__16719)){
var vec__16727 = chunk__16718.cljs$core$IIndexed$_nth$arity$2(null,i__16720);
var tid = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16727,(0),null);
var tap_config = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16727,(1),null);
shadow.remote.runtime.api.relay_msg(runtime,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"tap","tap",-1086702463),new cljs.core.Keyword(null,"to","to",192099007),tid,new cljs.core.Keyword(null,"oid","oid",-768692334),oid], null));


var G__16736 = seq__16717;
var G__16737 = chunk__16718;
var G__16738 = count__16719;
var G__16739 = (i__16720 + (1));
seq__16717 = G__16736;
chunk__16718 = G__16737;
count__16719 = G__16738;
i__16720 = G__16739;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__16717);
if(temp__5804__auto__){
var seq__16717__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__16717__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__16717__$1);
var G__16740 = cljs.core.chunk_rest(seq__16717__$1);
var G__16741 = c__5525__auto__;
var G__16742 = cljs.core.count(c__5525__auto__);
var G__16743 = (0);
seq__16717 = G__16740;
chunk__16718 = G__16741;
count__16719 = G__16742;
i__16720 = G__16743;
continue;
} else {
var vec__16730 = cljs.core.first(seq__16717__$1);
var tid = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16730,(0),null);
var tap_config = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16730,(1),null);
shadow.remote.runtime.api.relay_msg(runtime,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"tap","tap",-1086702463),new cljs.core.Keyword(null,"to","to",192099007),tid,new cljs.core.Keyword(null,"oid","oid",-768692334),oid], null));


var G__16744 = cljs.core.next(seq__16717__$1);
var G__16745 = null;
var G__16746 = (0);
var G__16747 = (0);
seq__16717 = G__16744;
chunk__16718 = G__16745;
count__16719 = G__16746;
i__16720 = G__16747;
continue;
}
} else {
return null;
}
}
break;
}
} else {
return null;
}
});
var svc = new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"runtime","runtime",-1331573996),runtime,new cljs.core.Keyword(null,"obj-support","obj-support",1522559229),obj_support,new cljs.core.Keyword(null,"tap-fn","tap-fn",1573556461),tap_fn,new cljs.core.Keyword(null,"subs-ref","subs-ref",-1355989911),subs_ref], null);
shadow.remote.runtime.api.add_extension(runtime,new cljs.core.Keyword("shadow.remote.runtime.tap-support","ext","shadow.remote.runtime.tap-support/ext",1019069674),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"ops","ops",1237330063),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"tap-subscribe","tap-subscribe",411179050),(function (p1__16713_SHARP_){
return shadow.remote.runtime.tap_support.tap_subscribe(svc,p1__16713_SHARP_);
}),new cljs.core.Keyword(null,"tap-unsubscribe","tap-unsubscribe",1183890755),(function (p1__16714_SHARP_){
return shadow.remote.runtime.tap_support.tap_unsubscribe(svc,p1__16714_SHARP_);
}),new cljs.core.Keyword(null,"request-tap-history","request-tap-history",-670837812),(function (p1__16715_SHARP_){
return shadow.remote.runtime.tap_support.request_tap_history(svc,p1__16715_SHARP_);
})], null),new cljs.core.Keyword(null,"on-tool-disconnect","on-tool-disconnect",693464366),(function (p1__16716_SHARP_){
return shadow.remote.runtime.tap_support.tool_disconnect(svc,p1__16716_SHARP_);
})], null));

cljs.core.add_tap(tap_fn);

return svc;
});
shadow.remote.runtime.tap_support.stop = (function shadow$remote$runtime$tap_support$stop(p__16734){
var map__16735 = p__16734;
var map__16735__$1 = cljs.core.__destructure_map(map__16735);
var svc = map__16735__$1;
var tap_fn = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16735__$1,new cljs.core.Keyword(null,"tap-fn","tap-fn",1573556461));
var runtime = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16735__$1,new cljs.core.Keyword(null,"runtime","runtime",-1331573996));
cljs.core.remove_tap(tap_fn);

return shadow.remote.runtime.api.del_extension(runtime,new cljs.core.Keyword("shadow.remote.runtime.tap-support","ext","shadow.remote.runtime.tap-support/ext",1019069674));
});

//# sourceMappingURL=shadow.remote.runtime.tap_support.js.map
