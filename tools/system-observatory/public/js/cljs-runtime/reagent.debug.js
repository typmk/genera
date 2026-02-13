goog.provide('reagent.debug');
reagent.debug.has_console = (typeof console !== 'undefined');
reagent.debug.tracking = false;
if((typeof reagent !== 'undefined') && (typeof reagent.debug !== 'undefined') && (typeof reagent.debug.warnings !== 'undefined')){
} else {
reagent.debug.warnings = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(null);
}
if((typeof reagent !== 'undefined') && (typeof reagent.debug !== 'undefined') && (typeof reagent.debug.track_console !== 'undefined')){
} else {
reagent.debug.track_console = (function (){var o = ({});
(o.warn = (function() { 
var G__11896__delegate = function (args){
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$variadic(reagent.debug.warnings,cljs.core.update_in,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"warn","warn",-436710552)], null),cljs.core.conj,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.str,args)], 0));
};
var G__11896 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__11897__i = 0, G__11897__a = new Array(arguments.length -  0);
while (G__11897__i < G__11897__a.length) {G__11897__a[G__11897__i] = arguments[G__11897__i + 0]; ++G__11897__i;}
  args = new cljs.core.IndexedSeq(G__11897__a,0,null);
} 
return G__11896__delegate.call(this,args);};
G__11896.cljs$lang$maxFixedArity = 0;
G__11896.cljs$lang$applyTo = (function (arglist__11898){
var args = cljs.core.seq(arglist__11898);
return G__11896__delegate(args);
});
G__11896.cljs$core$IFn$_invoke$arity$variadic = G__11896__delegate;
return G__11896;
})()
);

(o.error = (function() { 
var G__11899__delegate = function (args){
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$variadic(reagent.debug.warnings,cljs.core.update_in,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"error","error",-978969032)], null),cljs.core.conj,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([cljs.core.apply.cljs$core$IFn$_invoke$arity$2(cljs.core.str,args)], 0));
};
var G__11899 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__11900__i = 0, G__11900__a = new Array(arguments.length -  0);
while (G__11900__i < G__11900__a.length) {G__11900__a[G__11900__i] = arguments[G__11900__i + 0]; ++G__11900__i;}
  args = new cljs.core.IndexedSeq(G__11900__a,0,null);
} 
return G__11899__delegate.call(this,args);};
G__11899.cljs$lang$maxFixedArity = 0;
G__11899.cljs$lang$applyTo = (function (arglist__11901){
var args = cljs.core.seq(arglist__11901);
return G__11899__delegate(args);
});
G__11899.cljs$core$IFn$_invoke$arity$variadic = G__11899__delegate;
return G__11899;
})()
);

return o;
})();
}
reagent.debug.track_warnings = (function reagent$debug$track_warnings(f){
(reagent.debug.tracking = true);

cljs.core.reset_BANG_(reagent.debug.warnings,null);

(f.cljs$core$IFn$_invoke$arity$0 ? f.cljs$core$IFn$_invoke$arity$0() : f.call(null));

var warns = cljs.core.deref(reagent.debug.warnings);
cljs.core.reset_BANG_(reagent.debug.warnings,null);

(reagent.debug.tracking = false);

return warns;
});

//# sourceMappingURL=reagent.debug.js.map
