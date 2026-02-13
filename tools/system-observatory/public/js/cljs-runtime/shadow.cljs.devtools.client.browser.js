goog.provide('shadow.cljs.devtools.client.browser');
shadow.cljs.devtools.client.browser.devtools_msg = (function shadow$cljs$devtools$client$browser$devtools_msg(var_args){
var args__5732__auto__ = [];
var len__5726__auto___17233 = arguments.length;
var i__5727__auto___17234 = (0);
while(true){
if((i__5727__auto___17234 < len__5726__auto___17233)){
args__5732__auto__.push((arguments[i__5727__auto___17234]));

var G__17235 = (i__5727__auto___17234 + (1));
i__5727__auto___17234 = G__17235;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic = (function (msg,args){
if(shadow.cljs.devtools.client.env.log){
if(cljs.core.seq(shadow.cljs.devtools.client.env.log_style)){
return console.log.apply(console,cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(cljs.core.into.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [["%cshadow-cljs: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(msg)].join(''),shadow.cljs.devtools.client.env.log_style], null),args)));
} else {
return console.log.apply(console,cljs.core.into_array.cljs$core$IFn$_invoke$arity$1(cljs.core.into.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [["shadow-cljs: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(msg)].join('')], null),args)));
}
} else {
return null;
}
}));

(shadow.cljs.devtools.client.browser.devtools_msg.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(shadow.cljs.devtools.client.browser.devtools_msg.cljs$lang$applyTo = (function (seq16949){
var G__16950 = cljs.core.first(seq16949);
var seq16949__$1 = cljs.core.next(seq16949);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__16950,seq16949__$1);
}));

shadow.cljs.devtools.client.browser.script_eval = (function shadow$cljs$devtools$client$browser$script_eval(code){
return goog.globalEval(code);
});
shadow.cljs.devtools.client.browser.do_js_load = (function shadow$cljs$devtools$client$browser$do_js_load(sources){
var seq__16951 = cljs.core.seq(sources);
var chunk__16952 = null;
var count__16953 = (0);
var i__16954 = (0);
while(true){
if((i__16954 < count__16953)){
var map__16959 = chunk__16952.cljs$core$IIndexed$_nth$arity$2(null,i__16954);
var map__16959__$1 = cljs.core.__destructure_map(map__16959);
var src = map__16959__$1;
var resource_id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16959__$1,new cljs.core.Keyword(null,"resource-id","resource-id",-1308422582));
var output_name = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16959__$1,new cljs.core.Keyword(null,"output-name","output-name",-1769107767));
var resource_name = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16959__$1,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100));
var js = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16959__$1,new cljs.core.Keyword(null,"js","js",1768080579));
$CLJS.SHADOW_ENV.setLoaded(output_name);

shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("load JS",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([resource_name], 0));

shadow.cljs.devtools.client.env.before_load_src(src);

try{shadow.cljs.devtools.client.browser.script_eval([cljs.core.str.cljs$core$IFn$_invoke$arity$1(js),"\n//# sourceURL=",cljs.core.str.cljs$core$IFn$_invoke$arity$1($CLJS.SHADOW_ENV.scriptBase),cljs.core.str.cljs$core$IFn$_invoke$arity$1(output_name)].join(''));
}catch (e16960){var e_17236 = e16960;
if(shadow.cljs.devtools.client.env.log){
console.error(["Failed to load ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(resource_name)].join(''),e_17236);
} else {
}

throw (new Error(["Failed to load ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(resource_name),": ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(e_17236.message)].join('')));
}

var G__17237 = seq__16951;
var G__17238 = chunk__16952;
var G__17239 = count__16953;
var G__17240 = (i__16954 + (1));
seq__16951 = G__17237;
chunk__16952 = G__17238;
count__16953 = G__17239;
i__16954 = G__17240;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__16951);
if(temp__5804__auto__){
var seq__16951__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__16951__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__16951__$1);
var G__17241 = cljs.core.chunk_rest(seq__16951__$1);
var G__17242 = c__5525__auto__;
var G__17243 = cljs.core.count(c__5525__auto__);
var G__17244 = (0);
seq__16951 = G__17241;
chunk__16952 = G__17242;
count__16953 = G__17243;
i__16954 = G__17244;
continue;
} else {
var map__16961 = cljs.core.first(seq__16951__$1);
var map__16961__$1 = cljs.core.__destructure_map(map__16961);
var src = map__16961__$1;
var resource_id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16961__$1,new cljs.core.Keyword(null,"resource-id","resource-id",-1308422582));
var output_name = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16961__$1,new cljs.core.Keyword(null,"output-name","output-name",-1769107767));
var resource_name = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16961__$1,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100));
var js = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16961__$1,new cljs.core.Keyword(null,"js","js",1768080579));
$CLJS.SHADOW_ENV.setLoaded(output_name);

shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("load JS",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([resource_name], 0));

shadow.cljs.devtools.client.env.before_load_src(src);

try{shadow.cljs.devtools.client.browser.script_eval([cljs.core.str.cljs$core$IFn$_invoke$arity$1(js),"\n//# sourceURL=",cljs.core.str.cljs$core$IFn$_invoke$arity$1($CLJS.SHADOW_ENV.scriptBase),cljs.core.str.cljs$core$IFn$_invoke$arity$1(output_name)].join(''));
}catch (e16962){var e_17245 = e16962;
if(shadow.cljs.devtools.client.env.log){
console.error(["Failed to load ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(resource_name)].join(''),e_17245);
} else {
}

throw (new Error(["Failed to load ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(resource_name),": ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(e_17245.message)].join('')));
}

var G__17246 = cljs.core.next(seq__16951__$1);
var G__17247 = null;
var G__17248 = (0);
var G__17249 = (0);
seq__16951 = G__17246;
chunk__16952 = G__17247;
count__16953 = G__17248;
i__16954 = G__17249;
continue;
}
} else {
return null;
}
}
break;
}
});
shadow.cljs.devtools.client.browser.do_js_reload = (function shadow$cljs$devtools$client$browser$do_js_reload(msg,sources,complete_fn,failure_fn){
return shadow.cljs.devtools.client.env.do_js_reload.cljs$core$IFn$_invoke$arity$4(cljs.core.assoc.cljs$core$IFn$_invoke$arity$variadic(msg,new cljs.core.Keyword(null,"log-missing-fn","log-missing-fn",732676765),(function (fn_sym){
return null;
}),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"log-call-async","log-call-async",183826192),(function (fn_sym){
return shadow.cljs.devtools.client.browser.devtools_msg(["call async ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym)].join(''));
}),new cljs.core.Keyword(null,"log-call","log-call",412404391),(function (fn_sym){
return shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym)].join(''));
})], 0)),(function (next){
shadow.cljs.devtools.client.browser.do_js_load(sources);

return (next.cljs$core$IFn$_invoke$arity$0 ? next.cljs$core$IFn$_invoke$arity$0() : next.call(null));
}),complete_fn,failure_fn);
});
/**
 * when (require '["some-str" :as x]) is done at the REPL we need to manually call the shadow.js.require for it
 * since the file only adds the shadow$provide. only need to do this for shadow-js.
 */
shadow.cljs.devtools.client.browser.do_js_requires = (function shadow$cljs$devtools$client$browser$do_js_requires(js_requires){
var seq__16963 = cljs.core.seq(js_requires);
var chunk__16964 = null;
var count__16965 = (0);
var i__16966 = (0);
while(true){
if((i__16966 < count__16965)){
var js_ns = chunk__16964.cljs$core$IIndexed$_nth$arity$2(null,i__16966);
var require_str_17250 = ["var ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(js_ns)," = shadow.js.require(\"",cljs.core.str.cljs$core$IFn$_invoke$arity$1(js_ns),"\");"].join('');
shadow.cljs.devtools.client.browser.script_eval(require_str_17250);


var G__17251 = seq__16963;
var G__17252 = chunk__16964;
var G__17253 = count__16965;
var G__17254 = (i__16966 + (1));
seq__16963 = G__17251;
chunk__16964 = G__17252;
count__16965 = G__17253;
i__16966 = G__17254;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__16963);
if(temp__5804__auto__){
var seq__16963__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__16963__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__16963__$1);
var G__17255 = cljs.core.chunk_rest(seq__16963__$1);
var G__17256 = c__5525__auto__;
var G__17257 = cljs.core.count(c__5525__auto__);
var G__17258 = (0);
seq__16963 = G__17255;
chunk__16964 = G__17256;
count__16965 = G__17257;
i__16966 = G__17258;
continue;
} else {
var js_ns = cljs.core.first(seq__16963__$1);
var require_str_17259 = ["var ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(js_ns)," = shadow.js.require(\"",cljs.core.str.cljs$core$IFn$_invoke$arity$1(js_ns),"\");"].join('');
shadow.cljs.devtools.client.browser.script_eval(require_str_17259);


var G__17260 = cljs.core.next(seq__16963__$1);
var G__17261 = null;
var G__17262 = (0);
var G__17263 = (0);
seq__16963 = G__17260;
chunk__16964 = G__17261;
count__16965 = G__17262;
i__16966 = G__17263;
continue;
}
} else {
return null;
}
}
break;
}
});
shadow.cljs.devtools.client.browser.handle_build_complete = (function shadow$cljs$devtools$client$browser$handle_build_complete(runtime,p__16968){
var map__16969 = p__16968;
var map__16969__$1 = cljs.core.__destructure_map(map__16969);
var msg = map__16969__$1;
var info = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16969__$1,new cljs.core.Keyword(null,"info","info",-317069002));
var reload_info = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16969__$1,new cljs.core.Keyword(null,"reload-info","reload-info",1648088086));
var warnings = cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY,cljs.core.distinct.cljs$core$IFn$_invoke$arity$1((function (){var iter__5480__auto__ = (function shadow$cljs$devtools$client$browser$handle_build_complete_$_iter__16970(s__16971){
return (new cljs.core.LazySeq(null,(function (){
var s__16971__$1 = s__16971;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__16971__$1);
if(temp__5804__auto__){
var xs__6360__auto__ = temp__5804__auto__;
var map__16976 = cljs.core.first(xs__6360__auto__);
var map__16976__$1 = cljs.core.__destructure_map(map__16976);
var src = map__16976__$1;
var resource_name = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16976__$1,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100));
var warnings = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16976__$1,new cljs.core.Keyword(null,"warnings","warnings",-735437651));
if(cljs.core.not(new cljs.core.Keyword(null,"from-jar","from-jar",1050932827).cljs$core$IFn$_invoke$arity$1(src))){
var iterys__5476__auto__ = ((function (s__16971__$1,map__16976,map__16976__$1,src,resource_name,warnings,xs__6360__auto__,temp__5804__auto__,map__16969,map__16969__$1,msg,info,reload_info){
return (function shadow$cljs$devtools$client$browser$handle_build_complete_$_iter__16970_$_iter__16972(s__16973){
return (new cljs.core.LazySeq(null,((function (s__16971__$1,map__16976,map__16976__$1,src,resource_name,warnings,xs__6360__auto__,temp__5804__auto__,map__16969,map__16969__$1,msg,info,reload_info){
return (function (){
var s__16973__$1 = s__16973;
while(true){
var temp__5804__auto____$1 = cljs.core.seq(s__16973__$1);
if(temp__5804__auto____$1){
var s__16973__$2 = temp__5804__auto____$1;
if(cljs.core.chunked_seq_QMARK_(s__16973__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__16973__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__16975 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__16974 = (0);
while(true){
if((i__16974 < size__5479__auto__)){
var warning = cljs.core._nth(c__5478__auto__,i__16974);
cljs.core.chunk_append(b__16975,cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(warning,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100),resource_name));

var G__17264 = (i__16974 + (1));
i__16974 = G__17264;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__16975),shadow$cljs$devtools$client$browser$handle_build_complete_$_iter__16970_$_iter__16972(cljs.core.chunk_rest(s__16973__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__16975),null);
}
} else {
var warning = cljs.core.first(s__16973__$2);
return cljs.core.cons(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(warning,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100),resource_name),shadow$cljs$devtools$client$browser$handle_build_complete_$_iter__16970_$_iter__16972(cljs.core.rest(s__16973__$2)));
}
} else {
return null;
}
break;
}
});})(s__16971__$1,map__16976,map__16976__$1,src,resource_name,warnings,xs__6360__auto__,temp__5804__auto__,map__16969,map__16969__$1,msg,info,reload_info))
,null,null));
});})(s__16971__$1,map__16976,map__16976__$1,src,resource_name,warnings,xs__6360__auto__,temp__5804__auto__,map__16969,map__16969__$1,msg,info,reload_info))
;
var fs__5477__auto__ = cljs.core.seq(iterys__5476__auto__(warnings));
if(fs__5477__auto__){
return cljs.core.concat.cljs$core$IFn$_invoke$arity$2(fs__5477__auto__,shadow$cljs$devtools$client$browser$handle_build_complete_$_iter__16970(cljs.core.rest(s__16971__$1)));
} else {
var G__17265 = cljs.core.rest(s__16971__$1);
s__16971__$1 = G__17265;
continue;
}
} else {
var G__17266 = cljs.core.rest(s__16971__$1);
s__16971__$1 = G__17266;
continue;
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(new cljs.core.Keyword(null,"sources","sources",-321166424).cljs$core$IFn$_invoke$arity$1(info));
})()));
if(shadow.cljs.devtools.client.env.log){
var seq__16977_17267 = cljs.core.seq(warnings);
var chunk__16978_17268 = null;
var count__16979_17269 = (0);
var i__16980_17270 = (0);
while(true){
if((i__16980_17270 < count__16979_17269)){
var map__16983_17271 = chunk__16978_17268.cljs$core$IIndexed$_nth$arity$2(null,i__16980_17270);
var map__16983_17272__$1 = cljs.core.__destructure_map(map__16983_17271);
var w_17273 = map__16983_17272__$1;
var msg_17274__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16983_17272__$1,new cljs.core.Keyword(null,"msg","msg",-1386103444));
var line_17275 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16983_17272__$1,new cljs.core.Keyword(null,"line","line",212345235));
var column_17276 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16983_17272__$1,new cljs.core.Keyword(null,"column","column",2078222095));
var resource_name_17277 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16983_17272__$1,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100));
console.warn(["BUILD-WARNING in ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(resource_name_17277)," at [",cljs.core.str.cljs$core$IFn$_invoke$arity$1(line_17275),":",cljs.core.str.cljs$core$IFn$_invoke$arity$1(column_17276),"]\n\t",cljs.core.str.cljs$core$IFn$_invoke$arity$1(msg_17274__$1)].join(''));


var G__17278 = seq__16977_17267;
var G__17279 = chunk__16978_17268;
var G__17280 = count__16979_17269;
var G__17281 = (i__16980_17270 + (1));
seq__16977_17267 = G__17278;
chunk__16978_17268 = G__17279;
count__16979_17269 = G__17280;
i__16980_17270 = G__17281;
continue;
} else {
var temp__5804__auto___17282 = cljs.core.seq(seq__16977_17267);
if(temp__5804__auto___17282){
var seq__16977_17283__$1 = temp__5804__auto___17282;
if(cljs.core.chunked_seq_QMARK_(seq__16977_17283__$1)){
var c__5525__auto___17284 = cljs.core.chunk_first(seq__16977_17283__$1);
var G__17285 = cljs.core.chunk_rest(seq__16977_17283__$1);
var G__17286 = c__5525__auto___17284;
var G__17287 = cljs.core.count(c__5525__auto___17284);
var G__17288 = (0);
seq__16977_17267 = G__17285;
chunk__16978_17268 = G__17286;
count__16979_17269 = G__17287;
i__16980_17270 = G__17288;
continue;
} else {
var map__16984_17289 = cljs.core.first(seq__16977_17283__$1);
var map__16984_17290__$1 = cljs.core.__destructure_map(map__16984_17289);
var w_17291 = map__16984_17290__$1;
var msg_17292__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16984_17290__$1,new cljs.core.Keyword(null,"msg","msg",-1386103444));
var line_17293 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16984_17290__$1,new cljs.core.Keyword(null,"line","line",212345235));
var column_17294 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16984_17290__$1,new cljs.core.Keyword(null,"column","column",2078222095));
var resource_name_17295 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16984_17290__$1,new cljs.core.Keyword(null,"resource-name","resource-name",2001617100));
console.warn(["BUILD-WARNING in ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(resource_name_17295)," at [",cljs.core.str.cljs$core$IFn$_invoke$arity$1(line_17293),":",cljs.core.str.cljs$core$IFn$_invoke$arity$1(column_17294),"]\n\t",cljs.core.str.cljs$core$IFn$_invoke$arity$1(msg_17292__$1)].join(''));


var G__17296 = cljs.core.next(seq__16977_17283__$1);
var G__17297 = null;
var G__17298 = (0);
var G__17299 = (0);
seq__16977_17267 = G__17296;
chunk__16978_17268 = G__17297;
count__16979_17269 = G__17298;
i__16980_17270 = G__17299;
continue;
}
} else {
}
}
break;
}
} else {
}

if((!(shadow.cljs.devtools.client.env.autoload))){
return shadow.cljs.devtools.client.hud.load_end_success();
} else {
if(((cljs.core.empty_QMARK_(warnings)) || (shadow.cljs.devtools.client.env.ignore_warnings))){
var sources_to_get = shadow.cljs.devtools.client.env.filter_reload_sources(info,reload_info);
if(cljs.core.not(cljs.core.seq(sources_to_get))){
return shadow.cljs.devtools.client.hud.load_end_success();
} else {
if(cljs.core.seq(cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(msg,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"reload-info","reload-info",1648088086),new cljs.core.Keyword(null,"after-load","after-load",-1278503285)], null)))){
} else {
shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("reloading code but no :after-load hooks are configured!",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["https://shadow-cljs.github.io/docs/UsersGuide.html#_lifecycle_hooks"], 0));
}

return shadow.cljs.devtools.client.shared.load_sources(runtime,sources_to_get,(function (p1__16967_SHARP_){
return shadow.cljs.devtools.client.browser.do_js_reload(msg,p1__16967_SHARP_,shadow.cljs.devtools.client.hud.load_end_success,shadow.cljs.devtools.client.hud.load_failure);
}));
}
} else {
return null;
}
}
});
shadow.cljs.devtools.client.browser.page_load_uri = (cljs.core.truth_(goog.global.document)?goog.Uri.parse(document.location.href):null);
shadow.cljs.devtools.client.browser.match_paths = (function shadow$cljs$devtools$client$browser$match_paths(old,new$){
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2("file",shadow.cljs.devtools.client.browser.page_load_uri.getScheme())){
var rel_new = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(new$,(1));
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(old,rel_new)) || (clojure.string.starts_with_QMARK_(old,[rel_new,"?"].join(''))))){
return rel_new;
} else {
return null;
}
} else {
var node_uri = goog.Uri.parse(old);
var node_uri_resolved = shadow.cljs.devtools.client.browser.page_load_uri.resolve(node_uri);
var node_abs = node_uri_resolved.getPath();
var and__5000__auto__ = ((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$1(shadow.cljs.devtools.client.browser.page_load_uri.hasSameDomainAs(node_uri))) || (cljs.core.not(node_uri.hasDomain())));
if(and__5000__auto__){
var and__5000__auto____$1 = cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(node_abs,new$);
if(and__5000__auto____$1){
return cljs.core.str.cljs$core$IFn$_invoke$arity$1((function (){var G__16986 = node_uri;
G__16986.setQuery(null);

G__16986.setPath(new$);

return G__16986;
})());
} else {
return and__5000__auto____$1;
}
} else {
return and__5000__auto__;
}
}
});
shadow.cljs.devtools.client.browser.handle_asset_update = (function shadow$cljs$devtools$client$browser$handle_asset_update(p__16987){
var map__16988 = p__16987;
var map__16988__$1 = cljs.core.__destructure_map(map__16988);
var msg = map__16988__$1;
var updates = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16988__$1,new cljs.core.Keyword(null,"updates","updates",2013983452));
var reload_info = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16988__$1,new cljs.core.Keyword(null,"reload-info","reload-info",1648088086));
var seq__16989 = cljs.core.seq(updates);
var chunk__16991 = null;
var count__16992 = (0);
var i__16993 = (0);
while(true){
if((i__16993 < count__16992)){
var path = chunk__16991.cljs$core$IIndexed$_nth$arity$2(null,i__16993);
if(clojure.string.ends_with_QMARK_(path,"css")){
var seq__17103_17300 = cljs.core.seq(cljs.core.array_seq.cljs$core$IFn$_invoke$arity$1(document.querySelectorAll("link[rel=\"stylesheet\"]")));
var chunk__17107_17301 = null;
var count__17108_17302 = (0);
var i__17109_17303 = (0);
while(true){
if((i__17109_17303 < count__17108_17302)){
var node_17304 = chunk__17107_17301.cljs$core$IIndexed$_nth$arity$2(null,i__17109_17303);
if(cljs.core.not(node_17304.shadow$old)){
var path_match_17305 = shadow.cljs.devtools.client.browser.match_paths(node_17304.getAttribute("href"),path);
if(cljs.core.truth_(path_match_17305)){
var new_link_17306 = (function (){var G__17135 = node_17304.cloneNode(true);
G__17135.setAttribute("href",[cljs.core.str.cljs$core$IFn$_invoke$arity$1(path_match_17305),"?r=",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.rand.cljs$core$IFn$_invoke$arity$0())].join(''));

return G__17135;
})();
(node_17304.shadow$old = true);

(new_link_17306.onload = ((function (seq__17103_17300,chunk__17107_17301,count__17108_17302,i__17109_17303,seq__16989,chunk__16991,count__16992,i__16993,new_link_17306,path_match_17305,node_17304,path,map__16988,map__16988__$1,msg,updates,reload_info){
return (function (e){
var seq__17136_17307 = cljs.core.seq(cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(msg,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"reload-info","reload-info",1648088086),new cljs.core.Keyword(null,"asset-load","asset-load",-1925902322)], null)));
var chunk__17138_17308 = null;
var count__17139_17309 = (0);
var i__17140_17310 = (0);
while(true){
if((i__17140_17310 < count__17139_17309)){
var map__17144_17311 = chunk__17138_17308.cljs$core$IIndexed$_nth$arity$2(null,i__17140_17310);
var map__17144_17312__$1 = cljs.core.__destructure_map(map__17144_17311);
var task_17313 = map__17144_17312__$1;
var fn_str_17314 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17144_17312__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17315 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17144_17312__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17316 = goog.getObjectByName(fn_str_17314,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17315)].join(''));

(fn_obj_17316.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17316.cljs$core$IFn$_invoke$arity$2(path,new_link_17306) : fn_obj_17316.call(null,path,new_link_17306));


var G__17317 = seq__17136_17307;
var G__17318 = chunk__17138_17308;
var G__17319 = count__17139_17309;
var G__17320 = (i__17140_17310 + (1));
seq__17136_17307 = G__17317;
chunk__17138_17308 = G__17318;
count__17139_17309 = G__17319;
i__17140_17310 = G__17320;
continue;
} else {
var temp__5804__auto___17321 = cljs.core.seq(seq__17136_17307);
if(temp__5804__auto___17321){
var seq__17136_17322__$1 = temp__5804__auto___17321;
if(cljs.core.chunked_seq_QMARK_(seq__17136_17322__$1)){
var c__5525__auto___17323 = cljs.core.chunk_first(seq__17136_17322__$1);
var G__17324 = cljs.core.chunk_rest(seq__17136_17322__$1);
var G__17325 = c__5525__auto___17323;
var G__17326 = cljs.core.count(c__5525__auto___17323);
var G__17327 = (0);
seq__17136_17307 = G__17324;
chunk__17138_17308 = G__17325;
count__17139_17309 = G__17326;
i__17140_17310 = G__17327;
continue;
} else {
var map__17145_17328 = cljs.core.first(seq__17136_17322__$1);
var map__17145_17329__$1 = cljs.core.__destructure_map(map__17145_17328);
var task_17330 = map__17145_17329__$1;
var fn_str_17331 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17145_17329__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17332 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17145_17329__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17333 = goog.getObjectByName(fn_str_17331,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17332)].join(''));

(fn_obj_17333.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17333.cljs$core$IFn$_invoke$arity$2(path,new_link_17306) : fn_obj_17333.call(null,path,new_link_17306));


var G__17334 = cljs.core.next(seq__17136_17322__$1);
var G__17335 = null;
var G__17336 = (0);
var G__17337 = (0);
seq__17136_17307 = G__17334;
chunk__17138_17308 = G__17335;
count__17139_17309 = G__17336;
i__17140_17310 = G__17337;
continue;
}
} else {
}
}
break;
}

return goog.dom.removeNode(node_17304);
});})(seq__17103_17300,chunk__17107_17301,count__17108_17302,i__17109_17303,seq__16989,chunk__16991,count__16992,i__16993,new_link_17306,path_match_17305,node_17304,path,map__16988,map__16988__$1,msg,updates,reload_info))
);

shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("load CSS",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([path_match_17305], 0));

goog.dom.insertSiblingAfter(new_link_17306,node_17304);


var G__17338 = seq__17103_17300;
var G__17339 = chunk__17107_17301;
var G__17340 = count__17108_17302;
var G__17341 = (i__17109_17303 + (1));
seq__17103_17300 = G__17338;
chunk__17107_17301 = G__17339;
count__17108_17302 = G__17340;
i__17109_17303 = G__17341;
continue;
} else {
var G__17342 = seq__17103_17300;
var G__17343 = chunk__17107_17301;
var G__17344 = count__17108_17302;
var G__17345 = (i__17109_17303 + (1));
seq__17103_17300 = G__17342;
chunk__17107_17301 = G__17343;
count__17108_17302 = G__17344;
i__17109_17303 = G__17345;
continue;
}
} else {
var G__17346 = seq__17103_17300;
var G__17347 = chunk__17107_17301;
var G__17348 = count__17108_17302;
var G__17349 = (i__17109_17303 + (1));
seq__17103_17300 = G__17346;
chunk__17107_17301 = G__17347;
count__17108_17302 = G__17348;
i__17109_17303 = G__17349;
continue;
}
} else {
var temp__5804__auto___17350 = cljs.core.seq(seq__17103_17300);
if(temp__5804__auto___17350){
var seq__17103_17351__$1 = temp__5804__auto___17350;
if(cljs.core.chunked_seq_QMARK_(seq__17103_17351__$1)){
var c__5525__auto___17352 = cljs.core.chunk_first(seq__17103_17351__$1);
var G__17353 = cljs.core.chunk_rest(seq__17103_17351__$1);
var G__17354 = c__5525__auto___17352;
var G__17355 = cljs.core.count(c__5525__auto___17352);
var G__17356 = (0);
seq__17103_17300 = G__17353;
chunk__17107_17301 = G__17354;
count__17108_17302 = G__17355;
i__17109_17303 = G__17356;
continue;
} else {
var node_17357 = cljs.core.first(seq__17103_17351__$1);
if(cljs.core.not(node_17357.shadow$old)){
var path_match_17358 = shadow.cljs.devtools.client.browser.match_paths(node_17357.getAttribute("href"),path);
if(cljs.core.truth_(path_match_17358)){
var new_link_17359 = (function (){var G__17146 = node_17357.cloneNode(true);
G__17146.setAttribute("href",[cljs.core.str.cljs$core$IFn$_invoke$arity$1(path_match_17358),"?r=",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.rand.cljs$core$IFn$_invoke$arity$0())].join(''));

return G__17146;
})();
(node_17357.shadow$old = true);

(new_link_17359.onload = ((function (seq__17103_17300,chunk__17107_17301,count__17108_17302,i__17109_17303,seq__16989,chunk__16991,count__16992,i__16993,new_link_17359,path_match_17358,node_17357,seq__17103_17351__$1,temp__5804__auto___17350,path,map__16988,map__16988__$1,msg,updates,reload_info){
return (function (e){
var seq__17147_17360 = cljs.core.seq(cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(msg,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"reload-info","reload-info",1648088086),new cljs.core.Keyword(null,"asset-load","asset-load",-1925902322)], null)));
var chunk__17149_17361 = null;
var count__17150_17362 = (0);
var i__17151_17363 = (0);
while(true){
if((i__17151_17363 < count__17150_17362)){
var map__17155_17364 = chunk__17149_17361.cljs$core$IIndexed$_nth$arity$2(null,i__17151_17363);
var map__17155_17365__$1 = cljs.core.__destructure_map(map__17155_17364);
var task_17366 = map__17155_17365__$1;
var fn_str_17367 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17155_17365__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17368 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17155_17365__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17369 = goog.getObjectByName(fn_str_17367,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17368)].join(''));

(fn_obj_17369.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17369.cljs$core$IFn$_invoke$arity$2(path,new_link_17359) : fn_obj_17369.call(null,path,new_link_17359));


var G__17370 = seq__17147_17360;
var G__17371 = chunk__17149_17361;
var G__17372 = count__17150_17362;
var G__17373 = (i__17151_17363 + (1));
seq__17147_17360 = G__17370;
chunk__17149_17361 = G__17371;
count__17150_17362 = G__17372;
i__17151_17363 = G__17373;
continue;
} else {
var temp__5804__auto___17374__$1 = cljs.core.seq(seq__17147_17360);
if(temp__5804__auto___17374__$1){
var seq__17147_17375__$1 = temp__5804__auto___17374__$1;
if(cljs.core.chunked_seq_QMARK_(seq__17147_17375__$1)){
var c__5525__auto___17376 = cljs.core.chunk_first(seq__17147_17375__$1);
var G__17377 = cljs.core.chunk_rest(seq__17147_17375__$1);
var G__17378 = c__5525__auto___17376;
var G__17379 = cljs.core.count(c__5525__auto___17376);
var G__17380 = (0);
seq__17147_17360 = G__17377;
chunk__17149_17361 = G__17378;
count__17150_17362 = G__17379;
i__17151_17363 = G__17380;
continue;
} else {
var map__17156_17381 = cljs.core.first(seq__17147_17375__$1);
var map__17156_17382__$1 = cljs.core.__destructure_map(map__17156_17381);
var task_17383 = map__17156_17382__$1;
var fn_str_17384 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17156_17382__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17385 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17156_17382__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17386 = goog.getObjectByName(fn_str_17384,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17385)].join(''));

(fn_obj_17386.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17386.cljs$core$IFn$_invoke$arity$2(path,new_link_17359) : fn_obj_17386.call(null,path,new_link_17359));


var G__17387 = cljs.core.next(seq__17147_17375__$1);
var G__17388 = null;
var G__17389 = (0);
var G__17390 = (0);
seq__17147_17360 = G__17387;
chunk__17149_17361 = G__17388;
count__17150_17362 = G__17389;
i__17151_17363 = G__17390;
continue;
}
} else {
}
}
break;
}

return goog.dom.removeNode(node_17357);
});})(seq__17103_17300,chunk__17107_17301,count__17108_17302,i__17109_17303,seq__16989,chunk__16991,count__16992,i__16993,new_link_17359,path_match_17358,node_17357,seq__17103_17351__$1,temp__5804__auto___17350,path,map__16988,map__16988__$1,msg,updates,reload_info))
);

shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("load CSS",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([path_match_17358], 0));

goog.dom.insertSiblingAfter(new_link_17359,node_17357);


var G__17391 = cljs.core.next(seq__17103_17351__$1);
var G__17392 = null;
var G__17393 = (0);
var G__17394 = (0);
seq__17103_17300 = G__17391;
chunk__17107_17301 = G__17392;
count__17108_17302 = G__17393;
i__17109_17303 = G__17394;
continue;
} else {
var G__17395 = cljs.core.next(seq__17103_17351__$1);
var G__17396 = null;
var G__17397 = (0);
var G__17398 = (0);
seq__17103_17300 = G__17395;
chunk__17107_17301 = G__17396;
count__17108_17302 = G__17397;
i__17109_17303 = G__17398;
continue;
}
} else {
var G__17399 = cljs.core.next(seq__17103_17351__$1);
var G__17400 = null;
var G__17401 = (0);
var G__17402 = (0);
seq__17103_17300 = G__17399;
chunk__17107_17301 = G__17400;
count__17108_17302 = G__17401;
i__17109_17303 = G__17402;
continue;
}
}
} else {
}
}
break;
}


var G__17403 = seq__16989;
var G__17404 = chunk__16991;
var G__17405 = count__16992;
var G__17406 = (i__16993 + (1));
seq__16989 = G__17403;
chunk__16991 = G__17404;
count__16992 = G__17405;
i__16993 = G__17406;
continue;
} else {
var G__17407 = seq__16989;
var G__17408 = chunk__16991;
var G__17409 = count__16992;
var G__17410 = (i__16993 + (1));
seq__16989 = G__17407;
chunk__16991 = G__17408;
count__16992 = G__17409;
i__16993 = G__17410;
continue;
}
} else {
var temp__5804__auto__ = cljs.core.seq(seq__16989);
if(temp__5804__auto__){
var seq__16989__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__16989__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__16989__$1);
var G__17411 = cljs.core.chunk_rest(seq__16989__$1);
var G__17412 = c__5525__auto__;
var G__17413 = cljs.core.count(c__5525__auto__);
var G__17414 = (0);
seq__16989 = G__17411;
chunk__16991 = G__17412;
count__16992 = G__17413;
i__16993 = G__17414;
continue;
} else {
var path = cljs.core.first(seq__16989__$1);
if(clojure.string.ends_with_QMARK_(path,"css")){
var seq__17157_17415 = cljs.core.seq(cljs.core.array_seq.cljs$core$IFn$_invoke$arity$1(document.querySelectorAll("link[rel=\"stylesheet\"]")));
var chunk__17161_17416 = null;
var count__17162_17417 = (0);
var i__17163_17418 = (0);
while(true){
if((i__17163_17418 < count__17162_17417)){
var node_17419 = chunk__17161_17416.cljs$core$IIndexed$_nth$arity$2(null,i__17163_17418);
if(cljs.core.not(node_17419.shadow$old)){
var path_match_17420 = shadow.cljs.devtools.client.browser.match_paths(node_17419.getAttribute("href"),path);
if(cljs.core.truth_(path_match_17420)){
var new_link_17421 = (function (){var G__17189 = node_17419.cloneNode(true);
G__17189.setAttribute("href",[cljs.core.str.cljs$core$IFn$_invoke$arity$1(path_match_17420),"?r=",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.rand.cljs$core$IFn$_invoke$arity$0())].join(''));

return G__17189;
})();
(node_17419.shadow$old = true);

(new_link_17421.onload = ((function (seq__17157_17415,chunk__17161_17416,count__17162_17417,i__17163_17418,seq__16989,chunk__16991,count__16992,i__16993,new_link_17421,path_match_17420,node_17419,path,seq__16989__$1,temp__5804__auto__,map__16988,map__16988__$1,msg,updates,reload_info){
return (function (e){
var seq__17190_17422 = cljs.core.seq(cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(msg,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"reload-info","reload-info",1648088086),new cljs.core.Keyword(null,"asset-load","asset-load",-1925902322)], null)));
var chunk__17192_17423 = null;
var count__17193_17424 = (0);
var i__17194_17425 = (0);
while(true){
if((i__17194_17425 < count__17193_17424)){
var map__17198_17426 = chunk__17192_17423.cljs$core$IIndexed$_nth$arity$2(null,i__17194_17425);
var map__17198_17427__$1 = cljs.core.__destructure_map(map__17198_17426);
var task_17428 = map__17198_17427__$1;
var fn_str_17429 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17198_17427__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17430 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17198_17427__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17431 = goog.getObjectByName(fn_str_17429,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17430)].join(''));

(fn_obj_17431.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17431.cljs$core$IFn$_invoke$arity$2(path,new_link_17421) : fn_obj_17431.call(null,path,new_link_17421));


var G__17432 = seq__17190_17422;
var G__17433 = chunk__17192_17423;
var G__17434 = count__17193_17424;
var G__17435 = (i__17194_17425 + (1));
seq__17190_17422 = G__17432;
chunk__17192_17423 = G__17433;
count__17193_17424 = G__17434;
i__17194_17425 = G__17435;
continue;
} else {
var temp__5804__auto___17436__$1 = cljs.core.seq(seq__17190_17422);
if(temp__5804__auto___17436__$1){
var seq__17190_17437__$1 = temp__5804__auto___17436__$1;
if(cljs.core.chunked_seq_QMARK_(seq__17190_17437__$1)){
var c__5525__auto___17438 = cljs.core.chunk_first(seq__17190_17437__$1);
var G__17439 = cljs.core.chunk_rest(seq__17190_17437__$1);
var G__17440 = c__5525__auto___17438;
var G__17441 = cljs.core.count(c__5525__auto___17438);
var G__17442 = (0);
seq__17190_17422 = G__17439;
chunk__17192_17423 = G__17440;
count__17193_17424 = G__17441;
i__17194_17425 = G__17442;
continue;
} else {
var map__17199_17443 = cljs.core.first(seq__17190_17437__$1);
var map__17199_17444__$1 = cljs.core.__destructure_map(map__17199_17443);
var task_17445 = map__17199_17444__$1;
var fn_str_17446 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17199_17444__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17447 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17199_17444__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17448 = goog.getObjectByName(fn_str_17446,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17447)].join(''));

(fn_obj_17448.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17448.cljs$core$IFn$_invoke$arity$2(path,new_link_17421) : fn_obj_17448.call(null,path,new_link_17421));


var G__17449 = cljs.core.next(seq__17190_17437__$1);
var G__17450 = null;
var G__17451 = (0);
var G__17452 = (0);
seq__17190_17422 = G__17449;
chunk__17192_17423 = G__17450;
count__17193_17424 = G__17451;
i__17194_17425 = G__17452;
continue;
}
} else {
}
}
break;
}

return goog.dom.removeNode(node_17419);
});})(seq__17157_17415,chunk__17161_17416,count__17162_17417,i__17163_17418,seq__16989,chunk__16991,count__16992,i__16993,new_link_17421,path_match_17420,node_17419,path,seq__16989__$1,temp__5804__auto__,map__16988,map__16988__$1,msg,updates,reload_info))
);

shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("load CSS",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([path_match_17420], 0));

goog.dom.insertSiblingAfter(new_link_17421,node_17419);


var G__17453 = seq__17157_17415;
var G__17454 = chunk__17161_17416;
var G__17455 = count__17162_17417;
var G__17456 = (i__17163_17418 + (1));
seq__17157_17415 = G__17453;
chunk__17161_17416 = G__17454;
count__17162_17417 = G__17455;
i__17163_17418 = G__17456;
continue;
} else {
var G__17457 = seq__17157_17415;
var G__17458 = chunk__17161_17416;
var G__17459 = count__17162_17417;
var G__17460 = (i__17163_17418 + (1));
seq__17157_17415 = G__17457;
chunk__17161_17416 = G__17458;
count__17162_17417 = G__17459;
i__17163_17418 = G__17460;
continue;
}
} else {
var G__17461 = seq__17157_17415;
var G__17462 = chunk__17161_17416;
var G__17463 = count__17162_17417;
var G__17464 = (i__17163_17418 + (1));
seq__17157_17415 = G__17461;
chunk__17161_17416 = G__17462;
count__17162_17417 = G__17463;
i__17163_17418 = G__17464;
continue;
}
} else {
var temp__5804__auto___17465__$1 = cljs.core.seq(seq__17157_17415);
if(temp__5804__auto___17465__$1){
var seq__17157_17466__$1 = temp__5804__auto___17465__$1;
if(cljs.core.chunked_seq_QMARK_(seq__17157_17466__$1)){
var c__5525__auto___17467 = cljs.core.chunk_first(seq__17157_17466__$1);
var G__17468 = cljs.core.chunk_rest(seq__17157_17466__$1);
var G__17469 = c__5525__auto___17467;
var G__17470 = cljs.core.count(c__5525__auto___17467);
var G__17471 = (0);
seq__17157_17415 = G__17468;
chunk__17161_17416 = G__17469;
count__17162_17417 = G__17470;
i__17163_17418 = G__17471;
continue;
} else {
var node_17472 = cljs.core.first(seq__17157_17466__$1);
if(cljs.core.not(node_17472.shadow$old)){
var path_match_17473 = shadow.cljs.devtools.client.browser.match_paths(node_17472.getAttribute("href"),path);
if(cljs.core.truth_(path_match_17473)){
var new_link_17474 = (function (){var G__17200 = node_17472.cloneNode(true);
G__17200.setAttribute("href",[cljs.core.str.cljs$core$IFn$_invoke$arity$1(path_match_17473),"?r=",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.rand.cljs$core$IFn$_invoke$arity$0())].join(''));

return G__17200;
})();
(node_17472.shadow$old = true);

(new_link_17474.onload = ((function (seq__17157_17415,chunk__17161_17416,count__17162_17417,i__17163_17418,seq__16989,chunk__16991,count__16992,i__16993,new_link_17474,path_match_17473,node_17472,seq__17157_17466__$1,temp__5804__auto___17465__$1,path,seq__16989__$1,temp__5804__auto__,map__16988,map__16988__$1,msg,updates,reload_info){
return (function (e){
var seq__17201_17475 = cljs.core.seq(cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(msg,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"reload-info","reload-info",1648088086),new cljs.core.Keyword(null,"asset-load","asset-load",-1925902322)], null)));
var chunk__17203_17476 = null;
var count__17204_17477 = (0);
var i__17205_17478 = (0);
while(true){
if((i__17205_17478 < count__17204_17477)){
var map__17209_17479 = chunk__17203_17476.cljs$core$IIndexed$_nth$arity$2(null,i__17205_17478);
var map__17209_17480__$1 = cljs.core.__destructure_map(map__17209_17479);
var task_17481 = map__17209_17480__$1;
var fn_str_17482 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17209_17480__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17483 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17209_17480__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17484 = goog.getObjectByName(fn_str_17482,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17483)].join(''));

(fn_obj_17484.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17484.cljs$core$IFn$_invoke$arity$2(path,new_link_17474) : fn_obj_17484.call(null,path,new_link_17474));


var G__17485 = seq__17201_17475;
var G__17486 = chunk__17203_17476;
var G__17487 = count__17204_17477;
var G__17488 = (i__17205_17478 + (1));
seq__17201_17475 = G__17485;
chunk__17203_17476 = G__17486;
count__17204_17477 = G__17487;
i__17205_17478 = G__17488;
continue;
} else {
var temp__5804__auto___17489__$2 = cljs.core.seq(seq__17201_17475);
if(temp__5804__auto___17489__$2){
var seq__17201_17490__$1 = temp__5804__auto___17489__$2;
if(cljs.core.chunked_seq_QMARK_(seq__17201_17490__$1)){
var c__5525__auto___17491 = cljs.core.chunk_first(seq__17201_17490__$1);
var G__17492 = cljs.core.chunk_rest(seq__17201_17490__$1);
var G__17493 = c__5525__auto___17491;
var G__17494 = cljs.core.count(c__5525__auto___17491);
var G__17495 = (0);
seq__17201_17475 = G__17492;
chunk__17203_17476 = G__17493;
count__17204_17477 = G__17494;
i__17205_17478 = G__17495;
continue;
} else {
var map__17210_17496 = cljs.core.first(seq__17201_17490__$1);
var map__17210_17497__$1 = cljs.core.__destructure_map(map__17210_17496);
var task_17498 = map__17210_17497__$1;
var fn_str_17499 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17210_17497__$1,new cljs.core.Keyword(null,"fn-str","fn-str",-1348506402));
var fn_sym_17500 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17210_17497__$1,new cljs.core.Keyword(null,"fn-sym","fn-sym",1423988510));
var fn_obj_17501 = goog.getObjectByName(fn_str_17499,$CLJS);
shadow.cljs.devtools.client.browser.devtools_msg(["call ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(fn_sym_17500)].join(''));

(fn_obj_17501.cljs$core$IFn$_invoke$arity$2 ? fn_obj_17501.cljs$core$IFn$_invoke$arity$2(path,new_link_17474) : fn_obj_17501.call(null,path,new_link_17474));


var G__17502 = cljs.core.next(seq__17201_17490__$1);
var G__17503 = null;
var G__17504 = (0);
var G__17505 = (0);
seq__17201_17475 = G__17502;
chunk__17203_17476 = G__17503;
count__17204_17477 = G__17504;
i__17205_17478 = G__17505;
continue;
}
} else {
}
}
break;
}

return goog.dom.removeNode(node_17472);
});})(seq__17157_17415,chunk__17161_17416,count__17162_17417,i__17163_17418,seq__16989,chunk__16991,count__16992,i__16993,new_link_17474,path_match_17473,node_17472,seq__17157_17466__$1,temp__5804__auto___17465__$1,path,seq__16989__$1,temp__5804__auto__,map__16988,map__16988__$1,msg,updates,reload_info))
);

shadow.cljs.devtools.client.browser.devtools_msg.cljs$core$IFn$_invoke$arity$variadic("load CSS",cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([path_match_17473], 0));

goog.dom.insertSiblingAfter(new_link_17474,node_17472);


var G__17506 = cljs.core.next(seq__17157_17466__$1);
var G__17507 = null;
var G__17508 = (0);
var G__17509 = (0);
seq__17157_17415 = G__17506;
chunk__17161_17416 = G__17507;
count__17162_17417 = G__17508;
i__17163_17418 = G__17509;
continue;
} else {
var G__17510 = cljs.core.next(seq__17157_17466__$1);
var G__17511 = null;
var G__17512 = (0);
var G__17513 = (0);
seq__17157_17415 = G__17510;
chunk__17161_17416 = G__17511;
count__17162_17417 = G__17512;
i__17163_17418 = G__17513;
continue;
}
} else {
var G__17514 = cljs.core.next(seq__17157_17466__$1);
var G__17515 = null;
var G__17516 = (0);
var G__17517 = (0);
seq__17157_17415 = G__17514;
chunk__17161_17416 = G__17515;
count__17162_17417 = G__17516;
i__17163_17418 = G__17517;
continue;
}
}
} else {
}
}
break;
}


var G__17518 = cljs.core.next(seq__16989__$1);
var G__17519 = null;
var G__17520 = (0);
var G__17521 = (0);
seq__16989 = G__17518;
chunk__16991 = G__17519;
count__16992 = G__17520;
i__16993 = G__17521;
continue;
} else {
var G__17522 = cljs.core.next(seq__16989__$1);
var G__17523 = null;
var G__17524 = (0);
var G__17525 = (0);
seq__16989 = G__17522;
chunk__16991 = G__17523;
count__16992 = G__17524;
i__16993 = G__17525;
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
shadow.cljs.devtools.client.browser.global_eval = (function shadow$cljs$devtools$client$browser$global_eval(js){
if(cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2("undefined",typeof(module))){
return eval(js);
} else {
return (0,eval)(js);;
}
});
shadow.cljs.devtools.client.browser.runtime_info = (((typeof SHADOW_CONFIG !== 'undefined'))?shadow.json.to_clj.cljs$core$IFn$_invoke$arity$1(SHADOW_CONFIG):null);
shadow.cljs.devtools.client.browser.client_info = cljs.core.merge.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([shadow.cljs.devtools.client.browser.runtime_info,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"host","host",-1558485167),(cljs.core.truth_(goog.global.document)?new cljs.core.Keyword(null,"browser","browser",828191719):new cljs.core.Keyword(null,"browser-worker","browser-worker",1638998282)),new cljs.core.Keyword(null,"user-agent","user-agent",1220426212),[(cljs.core.truth_(goog.userAgent.OPERA)?"Opera":(cljs.core.truth_(goog.userAgent.product.CHROME)?"Chrome":(cljs.core.truth_(goog.userAgent.IE)?"MSIE":(cljs.core.truth_(goog.userAgent.EDGE)?"Edge":(cljs.core.truth_(goog.userAgent.GECKO)?"Firefox":(cljs.core.truth_(goog.userAgent.SAFARI)?"Safari":(cljs.core.truth_(goog.userAgent.WEBKIT)?"Webkit":null)))))))," ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(goog.userAgent.VERSION)," [",cljs.core.str.cljs$core$IFn$_invoke$arity$1(goog.userAgent.PLATFORM),"]"].join(''),new cljs.core.Keyword(null,"dom","dom",-1236537922),(!((goog.global.document == null)))], null)], 0));
if((typeof shadow !== 'undefined') && (typeof shadow.cljs !== 'undefined') && (typeof shadow.cljs.devtools !== 'undefined') && (typeof shadow.cljs.devtools.client !== 'undefined') && (typeof shadow.cljs.devtools.client.browser !== 'undefined') && (typeof shadow.cljs.devtools.client.browser.ws_was_welcome_ref !== 'undefined')){
} else {
shadow.cljs.devtools.client.browser.ws_was_welcome_ref = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(false);
}
if(((shadow.cljs.devtools.client.env.enabled) && ((shadow.cljs.devtools.client.env.worker_client_id > (0))))){
(shadow.cljs.devtools.client.shared.Runtime.prototype.shadow$remote$runtime$api$IEvalJS$ = cljs.core.PROTOCOL_SENTINEL);

(shadow.cljs.devtools.client.shared.Runtime.prototype.shadow$remote$runtime$api$IEvalJS$_js_eval$arity$4 = (function (this$,code,success,fail){
var this$__$1 = this;
try{var G__17212 = shadow.cljs.devtools.client.browser.global_eval(code);
return (success.cljs$core$IFn$_invoke$arity$1 ? success.cljs$core$IFn$_invoke$arity$1(G__17212) : success.call(null,G__17212));
}catch (e17211){var e = e17211;
return (fail.cljs$core$IFn$_invoke$arity$1 ? fail.cljs$core$IFn$_invoke$arity$1(e) : fail.call(null,e));
}}));

(shadow.cljs.devtools.client.shared.Runtime.prototype.shadow$cljs$devtools$client$shared$IHostSpecific$ = cljs.core.PROTOCOL_SENTINEL);

(shadow.cljs.devtools.client.shared.Runtime.prototype.shadow$cljs$devtools$client$shared$IHostSpecific$do_invoke$arity$5 = (function (this$,ns,p__17213,success,fail){
var map__17214 = p__17213;
var map__17214__$1 = cljs.core.__destructure_map(map__17214);
var js = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17214__$1,new cljs.core.Keyword(null,"js","js",1768080579));
var this$__$1 = this;
try{var G__17216 = shadow.cljs.devtools.client.browser.global_eval(js);
return (success.cljs$core$IFn$_invoke$arity$1 ? success.cljs$core$IFn$_invoke$arity$1(G__17216) : success.call(null,G__17216));
}catch (e17215){var e = e17215;
return (fail.cljs$core$IFn$_invoke$arity$1 ? fail.cljs$core$IFn$_invoke$arity$1(e) : fail.call(null,e));
}}));

(shadow.cljs.devtools.client.shared.Runtime.prototype.shadow$cljs$devtools$client$shared$IHostSpecific$do_repl_init$arity$4 = (function (runtime,p__17217,done,error){
var map__17218 = p__17217;
var map__17218__$1 = cljs.core.__destructure_map(map__17218);
var repl_sources = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17218__$1,new cljs.core.Keyword(null,"repl-sources","repl-sources",723867535));
var runtime__$1 = this;
return shadow.cljs.devtools.client.shared.load_sources(runtime__$1,cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY,cljs.core.remove.cljs$core$IFn$_invoke$arity$2(shadow.cljs.devtools.client.env.src_is_loaded_QMARK_,repl_sources)),(function (sources){
shadow.cljs.devtools.client.browser.do_js_load(sources);

return (done.cljs$core$IFn$_invoke$arity$0 ? done.cljs$core$IFn$_invoke$arity$0() : done.call(null));
}));
}));

(shadow.cljs.devtools.client.shared.Runtime.prototype.shadow$cljs$devtools$client$shared$IHostSpecific$do_repl_require$arity$4 = (function (runtime,p__17219,done,error){
var map__17220 = p__17219;
var map__17220__$1 = cljs.core.__destructure_map(map__17220);
var msg = map__17220__$1;
var sources = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17220__$1,new cljs.core.Keyword(null,"sources","sources",-321166424));
var reload_namespaces = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17220__$1,new cljs.core.Keyword(null,"reload-namespaces","reload-namespaces",250210134));
var js_requires = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17220__$1,new cljs.core.Keyword(null,"js-requires","js-requires",-1311472051));
var runtime__$1 = this;
var sources_to_load = cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentVector.EMPTY,cljs.core.remove.cljs$core$IFn$_invoke$arity$2((function (p__17221){
var map__17222 = p__17221;
var map__17222__$1 = cljs.core.__destructure_map(map__17222);
var src = map__17222__$1;
var provides = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17222__$1,new cljs.core.Keyword(null,"provides","provides",-1634397992));
var and__5000__auto__ = shadow.cljs.devtools.client.env.src_is_loaded_QMARK_(src);
if(cljs.core.truth_(and__5000__auto__)){
return cljs.core.not(cljs.core.some(reload_namespaces,provides));
} else {
return and__5000__auto__;
}
}),sources));
if(cljs.core.not(cljs.core.seq(sources_to_load))){
var G__17223 = cljs.core.PersistentVector.EMPTY;
return (done.cljs$core$IFn$_invoke$arity$1 ? done.cljs$core$IFn$_invoke$arity$1(G__17223) : done.call(null,G__17223));
} else {
return shadow.remote.runtime.shared.call.cljs$core$IFn$_invoke$arity$3(runtime__$1,new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"op","op",-1882987955),new cljs.core.Keyword(null,"cljs-load-sources","cljs-load-sources",-1458295962),new cljs.core.Keyword(null,"to","to",192099007),shadow.cljs.devtools.client.env.worker_client_id,new cljs.core.Keyword(null,"sources","sources",-321166424),cljs.core.into.cljs$core$IFn$_invoke$arity$3(cljs.core.PersistentVector.EMPTY,cljs.core.map.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"resource-id","resource-id",-1308422582)),sources_to_load)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"cljs-sources","cljs-sources",31121610),(function (p__17224){
var map__17225 = p__17224;
var map__17225__$1 = cljs.core.__destructure_map(map__17225);
var msg__$1 = map__17225__$1;
var sources__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17225__$1,new cljs.core.Keyword(null,"sources","sources",-321166424));
try{shadow.cljs.devtools.client.browser.do_js_load(sources__$1);

if(cljs.core.seq(js_requires)){
shadow.cljs.devtools.client.browser.do_js_requires(js_requires);
} else {
}

return (done.cljs$core$IFn$_invoke$arity$1 ? done.cljs$core$IFn$_invoke$arity$1(sources_to_load) : done.call(null,sources_to_load));
}catch (e17226){var ex = e17226;
return (error.cljs$core$IFn$_invoke$arity$1 ? error.cljs$core$IFn$_invoke$arity$1(ex) : error.call(null,ex));
}})], null));
}
}));

shadow.cljs.devtools.client.shared.add_plugin_BANG_(new cljs.core.Keyword("shadow.cljs.devtools.client.browser","client","shadow.cljs.devtools.client.browser/client",-1461019282),cljs.core.PersistentHashSet.EMPTY,(function (p__17227){
var map__17228 = p__17227;
var map__17228__$1 = cljs.core.__destructure_map(map__17228);
var env = map__17228__$1;
var runtime = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17228__$1,new cljs.core.Keyword(null,"runtime","runtime",-1331573996));
var svc = new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"runtime","runtime",-1331573996),runtime], null);
shadow.remote.runtime.api.add_extension(runtime,new cljs.core.Keyword("shadow.cljs.devtools.client.browser","client","shadow.cljs.devtools.client.browser/client",-1461019282),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"on-welcome","on-welcome",1895317125),(function (){
cljs.core.reset_BANG_(shadow.cljs.devtools.client.browser.ws_was_welcome_ref,true);

shadow.cljs.devtools.client.hud.connection_error_clear_BANG_();

shadow.cljs.devtools.client.env.patch_goog_BANG_();

return shadow.cljs.devtools.client.browser.devtools_msg(["#",cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"client-id","client-id",-464622140).cljs$core$IFn$_invoke$arity$1(cljs.core.deref(new cljs.core.Keyword(null,"state-ref","state-ref",2127874952).cljs$core$IFn$_invoke$arity$1(runtime))))," ready!"].join(''));
}),new cljs.core.Keyword(null,"on-disconnect","on-disconnect",-809021814),(function (e){
if(cljs.core.truth_(cljs.core.deref(shadow.cljs.devtools.client.browser.ws_was_welcome_ref))){
shadow.cljs.devtools.client.hud.connection_error("The Websocket connection was closed!");

return cljs.core.reset_BANG_(shadow.cljs.devtools.client.browser.ws_was_welcome_ref,false);
} else {
return null;
}
}),new cljs.core.Keyword(null,"on-reconnect","on-reconnect",1239988702),(function (e){
return shadow.cljs.devtools.client.hud.connection_error("Reconnecting ...");
}),new cljs.core.Keyword(null,"ops","ops",1237330063),new cljs.core.PersistentArrayMap(null, 7, [new cljs.core.Keyword(null,"access-denied","access-denied",959449406),(function (msg){
cljs.core.reset_BANG_(shadow.cljs.devtools.client.browser.ws_was_welcome_ref,false);

return shadow.cljs.devtools.client.hud.connection_error(["Stale Output! Your loaded JS was not produced by the running shadow-cljs instance."," Is the watch for this build running?"].join(''));
}),new cljs.core.Keyword(null,"cljs-asset-update","cljs-asset-update",1224093028),(function (msg){
return shadow.cljs.devtools.client.browser.handle_asset_update(msg);
}),new cljs.core.Keyword(null,"cljs-build-configure","cljs-build-configure",-2089891268),(function (msg){
return null;
}),new cljs.core.Keyword(null,"cljs-build-start","cljs-build-start",-725781241),(function (msg){
shadow.cljs.devtools.client.hud.hud_hide();

shadow.cljs.devtools.client.hud.load_start();

return shadow.cljs.devtools.client.env.run_custom_notify_BANG_(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(msg,new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"build-start","build-start",-959649480)));
}),new cljs.core.Keyword(null,"cljs-build-complete","cljs-build-complete",273626153),(function (msg){
var msg__$1 = shadow.cljs.devtools.client.env.add_warnings_to_info(msg);
shadow.cljs.devtools.client.hud.connection_error_clear_BANG_();

shadow.cljs.devtools.client.hud.hud_warnings(msg__$1);

shadow.cljs.devtools.client.browser.handle_build_complete(runtime,msg__$1);

return shadow.cljs.devtools.client.env.run_custom_notify_BANG_(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(msg__$1,new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"build-complete","build-complete",-501868472)));
}),new cljs.core.Keyword(null,"cljs-build-failure","cljs-build-failure",1718154990),(function (msg){
shadow.cljs.devtools.client.hud.load_end();

shadow.cljs.devtools.client.hud.hud_error(msg);

return shadow.cljs.devtools.client.env.run_custom_notify_BANG_(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(msg,new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"build-failure","build-failure",-2107487466)));
}),new cljs.core.Keyword("shadow.cljs.devtools.client.env","worker-notify","shadow.cljs.devtools.client.env/worker-notify",-1456820670),(function (p__17229){
var map__17230 = p__17229;
var map__17230__$1 = cljs.core.__destructure_map(map__17230);
var event_op = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17230__$1,new cljs.core.Keyword(null,"event-op","event-op",200358057));
var client_id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17230__$1,new cljs.core.Keyword(null,"client-id","client-id",-464622140));
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"client-disconnect","client-disconnect",640227957),event_op)) && (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(client_id,shadow.cljs.devtools.client.env.worker_client_id)))){
shadow.cljs.devtools.client.hud.connection_error_clear_BANG_();

return shadow.cljs.devtools.client.hud.connection_error("The watch for this build was stopped!");
} else {
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"client-connect","client-connect",-1113973888),event_op)){
shadow.cljs.devtools.client.hud.connection_error_clear_BANG_();

return shadow.cljs.devtools.client.hud.connection_error("The watch for this build was restarted. Reload required!");
} else {
return null;
}
}
})], null)], null));

return svc;
}),(function (p__17231){
var map__17232 = p__17231;
var map__17232__$1 = cljs.core.__destructure_map(map__17232);
var svc = map__17232__$1;
var runtime = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__17232__$1,new cljs.core.Keyword(null,"runtime","runtime",-1331573996));
return shadow.remote.runtime.api.del_extension(runtime,new cljs.core.Keyword("shadow.cljs.devtools.client.browser","client","shadow.cljs.devtools.client.browser/client",-1461019282));
}));

shadow.cljs.devtools.client.shared.init_runtime_BANG_(shadow.cljs.devtools.client.browser.client_info,shadow.cljs.devtools.client.websocket.start,shadow.cljs.devtools.client.websocket.send,shadow.cljs.devtools.client.websocket.stop);
} else {
}

//# sourceMappingURL=shadow.cljs.devtools.client.browser.js.map
