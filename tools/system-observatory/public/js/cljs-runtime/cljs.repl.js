goog.provide('cljs.repl');
cljs.repl.print_doc = (function cljs$repl$print_doc(p__16243){
var map__16244 = p__16243;
var map__16244__$1 = cljs.core.__destructure_map(map__16244);
var m = map__16244__$1;
var n = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16244__$1,new cljs.core.Keyword(null,"ns","ns",441598760));
var nm = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16244__$1,new cljs.core.Keyword(null,"name","name",1843675177));
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["-------------------------"], 0));

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (){var or__5002__auto__ = new cljs.core.Keyword(null,"spec","spec",347520401).cljs$core$IFn$_invoke$arity$1(m);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return [(function (){var temp__5804__auto__ = new cljs.core.Keyword(null,"ns","ns",441598760).cljs$core$IFn$_invoke$arity$1(m);
if(cljs.core.truth_(temp__5804__auto__)){
var ns = temp__5804__auto__;
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(ns),"/"].join('');
} else {
return null;
}
})(),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"name","name",1843675177).cljs$core$IFn$_invoke$arity$1(m))].join('');
}
})()], 0));

if(cljs.core.truth_(new cljs.core.Keyword(null,"protocol","protocol",652470118).cljs$core$IFn$_invoke$arity$1(m))){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["Protocol"], 0));
} else {
}

if(cljs.core.truth_(new cljs.core.Keyword(null,"forms","forms",2045992350).cljs$core$IFn$_invoke$arity$1(m))){
var seq__16248_16450 = cljs.core.seq(new cljs.core.Keyword(null,"forms","forms",2045992350).cljs$core$IFn$_invoke$arity$1(m));
var chunk__16249_16451 = null;
var count__16250_16452 = (0);
var i__16251_16453 = (0);
while(true){
if((i__16251_16453 < count__16250_16452)){
var f_16454 = chunk__16249_16451.cljs$core$IIndexed$_nth$arity$2(null,i__16251_16453);
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["  ",f_16454], 0));


var G__16455 = seq__16248_16450;
var G__16456 = chunk__16249_16451;
var G__16457 = count__16250_16452;
var G__16458 = (i__16251_16453 + (1));
seq__16248_16450 = G__16455;
chunk__16249_16451 = G__16456;
count__16250_16452 = G__16457;
i__16251_16453 = G__16458;
continue;
} else {
var temp__5804__auto___16459 = cljs.core.seq(seq__16248_16450);
if(temp__5804__auto___16459){
var seq__16248_16460__$1 = temp__5804__auto___16459;
if(cljs.core.chunked_seq_QMARK_(seq__16248_16460__$1)){
var c__5525__auto___16462 = cljs.core.chunk_first(seq__16248_16460__$1);
var G__16463 = cljs.core.chunk_rest(seq__16248_16460__$1);
var G__16464 = c__5525__auto___16462;
var G__16465 = cljs.core.count(c__5525__auto___16462);
var G__16466 = (0);
seq__16248_16450 = G__16463;
chunk__16249_16451 = G__16464;
count__16250_16452 = G__16465;
i__16251_16453 = G__16466;
continue;
} else {
var f_16467 = cljs.core.first(seq__16248_16460__$1);
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["  ",f_16467], 0));


var G__16468 = cljs.core.next(seq__16248_16460__$1);
var G__16469 = null;
var G__16470 = (0);
var G__16471 = (0);
seq__16248_16450 = G__16468;
chunk__16249_16451 = G__16469;
count__16250_16452 = G__16470;
i__16251_16453 = G__16471;
continue;
}
} else {
}
}
break;
}
} else {
if(cljs.core.truth_(new cljs.core.Keyword(null,"arglists","arglists",1661989754).cljs$core$IFn$_invoke$arity$1(m))){
var arglists_16472 = new cljs.core.Keyword(null,"arglists","arglists",1661989754).cljs$core$IFn$_invoke$arity$1(m);
if(cljs.core.truth_((function (){var or__5002__auto__ = new cljs.core.Keyword(null,"macro","macro",-867863404).cljs$core$IFn$_invoke$arity$1(m);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return new cljs.core.Keyword(null,"repl-special-function","repl-special-function",1262603725).cljs$core$IFn$_invoke$arity$1(m);
}
})())){
cljs.core.prn.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([arglists_16472], 0));
} else {
cljs.core.prn.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(new cljs.core.Symbol(null,"quote","quote",1377916282,null),cljs.core.first(arglists_16472)))?cljs.core.second(arglists_16472):arglists_16472)], 0));
}
} else {
}
}

if(cljs.core.truth_(new cljs.core.Keyword(null,"special-form","special-form",-1326536374).cljs$core$IFn$_invoke$arity$1(m))){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["Special Form"], 0));

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",new cljs.core.Keyword(null,"doc","doc",1913296891).cljs$core$IFn$_invoke$arity$1(m)], 0));

if(cljs.core.contains_QMARK_(m,new cljs.core.Keyword(null,"url","url",276297046))){
if(cljs.core.truth_(new cljs.core.Keyword(null,"url","url",276297046).cljs$core$IFn$_invoke$arity$1(m))){
return cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([["\n  Please see http://clojure.org/",cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"url","url",276297046).cljs$core$IFn$_invoke$arity$1(m))].join('')], 0));
} else {
return null;
}
} else {
return cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([["\n  Please see http://clojure.org/special_forms#",cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"name","name",1843675177).cljs$core$IFn$_invoke$arity$1(m))].join('')], 0));
}
} else {
if(cljs.core.truth_(new cljs.core.Keyword(null,"macro","macro",-867863404).cljs$core$IFn$_invoke$arity$1(m))){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["Macro"], 0));
} else {
}

if(cljs.core.truth_(new cljs.core.Keyword(null,"spec","spec",347520401).cljs$core$IFn$_invoke$arity$1(m))){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["Spec"], 0));
} else {
}

if(cljs.core.truth_(new cljs.core.Keyword(null,"repl-special-function","repl-special-function",1262603725).cljs$core$IFn$_invoke$arity$1(m))){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["REPL Special Function"], 0));
} else {
}

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",new cljs.core.Keyword(null,"doc","doc",1913296891).cljs$core$IFn$_invoke$arity$1(m)], 0));

if(cljs.core.truth_(new cljs.core.Keyword(null,"protocol","protocol",652470118).cljs$core$IFn$_invoke$arity$1(m))){
var seq__16254_16473 = cljs.core.seq(new cljs.core.Keyword(null,"methods","methods",453930866).cljs$core$IFn$_invoke$arity$1(m));
var chunk__16255_16474 = null;
var count__16256_16475 = (0);
var i__16257_16476 = (0);
while(true){
if((i__16257_16476 < count__16256_16475)){
var vec__16266_16477 = chunk__16255_16474.cljs$core$IIndexed$_nth$arity$2(null,i__16257_16476);
var name_16478 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16266_16477,(0),null);
var map__16269_16479 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16266_16477,(1),null);
var map__16269_16480__$1 = cljs.core.__destructure_map(map__16269_16479);
var doc_16481 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16269_16480__$1,new cljs.core.Keyword(null,"doc","doc",1913296891));
var arglists_16482 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16269_16480__$1,new cljs.core.Keyword(null,"arglists","arglists",1661989754));
cljs.core.println();

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",name_16478], 0));

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",arglists_16482], 0));

if(cljs.core.truth_(doc_16481)){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",doc_16481], 0));
} else {
}


var G__16483 = seq__16254_16473;
var G__16484 = chunk__16255_16474;
var G__16485 = count__16256_16475;
var G__16486 = (i__16257_16476 + (1));
seq__16254_16473 = G__16483;
chunk__16255_16474 = G__16484;
count__16256_16475 = G__16485;
i__16257_16476 = G__16486;
continue;
} else {
var temp__5804__auto___16487 = cljs.core.seq(seq__16254_16473);
if(temp__5804__auto___16487){
var seq__16254_16488__$1 = temp__5804__auto___16487;
if(cljs.core.chunked_seq_QMARK_(seq__16254_16488__$1)){
var c__5525__auto___16489 = cljs.core.chunk_first(seq__16254_16488__$1);
var G__16490 = cljs.core.chunk_rest(seq__16254_16488__$1);
var G__16491 = c__5525__auto___16489;
var G__16492 = cljs.core.count(c__5525__auto___16489);
var G__16493 = (0);
seq__16254_16473 = G__16490;
chunk__16255_16474 = G__16491;
count__16256_16475 = G__16492;
i__16257_16476 = G__16493;
continue;
} else {
var vec__16291_16494 = cljs.core.first(seq__16254_16488__$1);
var name_16495 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16291_16494,(0),null);
var map__16297_16496 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16291_16494,(1),null);
var map__16297_16497__$1 = cljs.core.__destructure_map(map__16297_16496);
var doc_16498 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16297_16497__$1,new cljs.core.Keyword(null,"doc","doc",1913296891));
var arglists_16499 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16297_16497__$1,new cljs.core.Keyword(null,"arglists","arglists",1661989754));
cljs.core.println();

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",name_16495], 0));

cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",arglists_16499], 0));

if(cljs.core.truth_(doc_16498)){
cljs.core.println.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([" ",doc_16498], 0));
} else {
}


var G__16500 = cljs.core.next(seq__16254_16488__$1);
var G__16501 = null;
var G__16502 = (0);
var G__16503 = (0);
seq__16254_16473 = G__16500;
chunk__16255_16474 = G__16501;
count__16256_16475 = G__16502;
i__16257_16476 = G__16503;
continue;
}
} else {
}
}
break;
}
} else {
}

if(cljs.core.truth_(n)){
var temp__5804__auto__ = cljs.spec.alpha.get_spec(cljs.core.symbol.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.ns_name(n)),cljs.core.name(nm)));
if(cljs.core.truth_(temp__5804__auto__)){
var fnspec = temp__5804__auto__;
cljs.core.print.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2(["Spec"], 0));

var seq__16303 = cljs.core.seq(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"args","args",1315556576),new cljs.core.Keyword(null,"ret","ret",-468222814),new cljs.core.Keyword(null,"fn","fn",-1175266204)], null));
var chunk__16304 = null;
var count__16305 = (0);
var i__16306 = (0);
while(true){
if((i__16306 < count__16305)){
var role = chunk__16304.cljs$core$IIndexed$_nth$arity$2(null,i__16306);
var temp__5804__auto___16506__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(fnspec,role);
if(cljs.core.truth_(temp__5804__auto___16506__$1)){
var spec_16507 = temp__5804__auto___16506__$1;
cljs.core.print.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([["\n ",cljs.core.name(role),":"].join(''),cljs.spec.alpha.describe(spec_16507)], 0));
} else {
}


var G__16510 = seq__16303;
var G__16511 = chunk__16304;
var G__16512 = count__16305;
var G__16513 = (i__16306 + (1));
seq__16303 = G__16510;
chunk__16304 = G__16511;
count__16305 = G__16512;
i__16306 = G__16513;
continue;
} else {
var temp__5804__auto____$1 = cljs.core.seq(seq__16303);
if(temp__5804__auto____$1){
var seq__16303__$1 = temp__5804__auto____$1;
if(cljs.core.chunked_seq_QMARK_(seq__16303__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__16303__$1);
var G__16514 = cljs.core.chunk_rest(seq__16303__$1);
var G__16515 = c__5525__auto__;
var G__16516 = cljs.core.count(c__5525__auto__);
var G__16517 = (0);
seq__16303 = G__16514;
chunk__16304 = G__16515;
count__16305 = G__16516;
i__16306 = G__16517;
continue;
} else {
var role = cljs.core.first(seq__16303__$1);
var temp__5804__auto___16518__$2 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(fnspec,role);
if(cljs.core.truth_(temp__5804__auto___16518__$2)){
var spec_16519 = temp__5804__auto___16518__$2;
cljs.core.print.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([["\n ",cljs.core.name(role),":"].join(''),cljs.spec.alpha.describe(spec_16519)], 0));
} else {
}


var G__16520 = cljs.core.next(seq__16303__$1);
var G__16521 = null;
var G__16522 = (0);
var G__16523 = (0);
seq__16303 = G__16520;
chunk__16304 = G__16521;
count__16305 = G__16522;
i__16306 = G__16523;
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
} else {
return null;
}
}
});
/**
 * Constructs a data representation for a Error with keys:
 *  :cause - root cause message
 *  :phase - error phase
 *  :via - cause chain, with cause keys:
 *           :type - exception class symbol
 *           :message - exception message
 *           :data - ex-data
 *           :at - top stack element
 *  :trace - root cause stack elements
 */
cljs.repl.Error__GT_map = (function cljs$repl$Error__GT_map(o){
return cljs.core.Throwable__GT_map(o);
});
/**
 * Returns an analysis of the phase, error, cause, and location of an error that occurred
 *   based on Throwable data, as returned by Throwable->map. All attributes other than phase
 *   are optional:
 *  :clojure.error/phase - keyword phase indicator, one of:
 *    :read-source :compile-syntax-check :compilation :macro-syntax-check :macroexpansion
 *    :execution :read-eval-result :print-eval-result
 *  :clojure.error/source - file name (no path)
 *  :clojure.error/line - integer line number
 *  :clojure.error/column - integer column number
 *  :clojure.error/symbol - symbol being expanded/compiled/invoked
 *  :clojure.error/class - cause exception class symbol
 *  :clojure.error/cause - cause exception message
 *  :clojure.error/spec - explain-data for spec error
 */
cljs.repl.ex_triage = (function cljs$repl$ex_triage(datafied_throwable){
var map__16377 = datafied_throwable;
var map__16377__$1 = cljs.core.__destructure_map(map__16377);
var via = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16377__$1,new cljs.core.Keyword(null,"via","via",-1904457336));
var trace = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16377__$1,new cljs.core.Keyword(null,"trace","trace",-1082747415));
var phase = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__16377__$1,new cljs.core.Keyword(null,"phase","phase",575722892),new cljs.core.Keyword(null,"execution","execution",253283524));
var map__16378 = cljs.core.last(via);
var map__16378__$1 = cljs.core.__destructure_map(map__16378);
var type = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16378__$1,new cljs.core.Keyword(null,"type","type",1174270348));
var message = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16378__$1,new cljs.core.Keyword(null,"message","message",-406056002));
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16378__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var map__16379 = data;
var map__16379__$1 = cljs.core.__destructure_map(map__16379);
var problems = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16379__$1,new cljs.core.Keyword("cljs.spec.alpha","problems","cljs.spec.alpha/problems",447400814));
var fn = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16379__$1,new cljs.core.Keyword("cljs.spec.alpha","fn","cljs.spec.alpha/fn",408600443));
var caller = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16379__$1,new cljs.core.Keyword("cljs.spec.test.alpha","caller","cljs.spec.test.alpha/caller",-398302390));
var map__16380 = new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(cljs.core.first(via));
var map__16380__$1 = cljs.core.__destructure_map(map__16380);
var top_data = map__16380__$1;
var source = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16380__$1,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397));
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3((function (){var G__16382 = phase;
var G__16382__$1 = (((G__16382 instanceof cljs.core.Keyword))?G__16382.fqn:null);
switch (G__16382__$1) {
case "read-source":
var map__16384 = data;
var map__16384__$1 = cljs.core.__destructure_map(map__16384);
var line = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16384__$1,new cljs.core.Keyword("clojure.error","line","clojure.error/line",-1816287471));
var column = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16384__$1,new cljs.core.Keyword("clojure.error","column","clojure.error/column",304721553));
var G__16386 = cljs.core.merge.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"data","data",-232669377).cljs$core$IFn$_invoke$arity$1(cljs.core.second(via)),top_data], 0));
var G__16386__$1 = (cljs.core.truth_(source)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16386,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397),source):G__16386);
var G__16386__$2 = (cljs.core.truth_((function (){var fexpr__16387 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, ["NO_SOURCE_PATH",null,"NO_SOURCE_FILE",null], null), null);
return (fexpr__16387.cljs$core$IFn$_invoke$arity$1 ? fexpr__16387.cljs$core$IFn$_invoke$arity$1(source) : fexpr__16387.call(null,source));
})())?cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(G__16386__$1,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397)):G__16386__$1);
if(cljs.core.truth_(message)){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16386__$2,new cljs.core.Keyword("clojure.error","cause","clojure.error/cause",-1879175742),message);
} else {
return G__16386__$2;
}

break;
case "compile-syntax-check":
case "compilation":
case "macro-syntax-check":
case "macroexpansion":
var G__16388 = top_data;
var G__16388__$1 = (cljs.core.truth_(source)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16388,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397),source):G__16388);
var G__16388__$2 = (cljs.core.truth_((function (){var fexpr__16389 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, ["NO_SOURCE_PATH",null,"NO_SOURCE_FILE",null], null), null);
return (fexpr__16389.cljs$core$IFn$_invoke$arity$1 ? fexpr__16389.cljs$core$IFn$_invoke$arity$1(source) : fexpr__16389.call(null,source));
})())?cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(G__16388__$1,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397)):G__16388__$1);
var G__16388__$3 = (cljs.core.truth_(type)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16388__$2,new cljs.core.Keyword("clojure.error","class","clojure.error/class",278435890),type):G__16388__$2);
var G__16388__$4 = (cljs.core.truth_(message)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16388__$3,new cljs.core.Keyword("clojure.error","cause","clojure.error/cause",-1879175742),message):G__16388__$3);
if(cljs.core.truth_(problems)){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16388__$4,new cljs.core.Keyword("clojure.error","spec","clojure.error/spec",2055032595),data);
} else {
return G__16388__$4;
}

break;
case "read-eval-result":
case "print-eval-result":
var vec__16391 = cljs.core.first(trace);
var source__$1 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16391,(0),null);
var method = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16391,(1),null);
var file = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16391,(2),null);
var line = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16391,(3),null);
var G__16394 = top_data;
var G__16394__$1 = (cljs.core.truth_(line)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16394,new cljs.core.Keyword("clojure.error","line","clojure.error/line",-1816287471),line):G__16394);
var G__16394__$2 = (cljs.core.truth_(file)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16394__$1,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397),file):G__16394__$1);
var G__16394__$3 = (cljs.core.truth_((function (){var and__5000__auto__ = source__$1;
if(cljs.core.truth_(and__5000__auto__)){
return method;
} else {
return and__5000__auto__;
}
})())?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16394__$2,new cljs.core.Keyword("clojure.error","symbol","clojure.error/symbol",1544821994),(new cljs.core.PersistentVector(null,2,(5),cljs.core.PersistentVector.EMPTY_NODE,[source__$1,method],null))):G__16394__$2);
var G__16394__$4 = (cljs.core.truth_(type)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16394__$3,new cljs.core.Keyword("clojure.error","class","clojure.error/class",278435890),type):G__16394__$3);
if(cljs.core.truth_(message)){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16394__$4,new cljs.core.Keyword("clojure.error","cause","clojure.error/cause",-1879175742),message);
} else {
return G__16394__$4;
}

break;
case "execution":
var vec__16399 = cljs.core.first(trace);
var source__$1 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16399,(0),null);
var method = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16399,(1),null);
var file = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16399,(2),null);
var line = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16399,(3),null);
var file__$1 = cljs.core.first(cljs.core.remove.cljs$core$IFn$_invoke$arity$2((function (p1__16376_SHARP_){
var or__5002__auto__ = (p1__16376_SHARP_ == null);
if(or__5002__auto__){
return or__5002__auto__;
} else {
var fexpr__16402 = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, ["NO_SOURCE_PATH",null,"NO_SOURCE_FILE",null], null), null);
return (fexpr__16402.cljs$core$IFn$_invoke$arity$1 ? fexpr__16402.cljs$core$IFn$_invoke$arity$1(p1__16376_SHARP_) : fexpr__16402.call(null,p1__16376_SHARP_));
}
}),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"file","file",-1269645878).cljs$core$IFn$_invoke$arity$1(caller),file], null)));
var err_line = (function (){var or__5002__auto__ = new cljs.core.Keyword(null,"line","line",212345235).cljs$core$IFn$_invoke$arity$1(caller);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return line;
}
})();
var G__16407 = new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword("clojure.error","class","clojure.error/class",278435890),type], null);
var G__16407__$1 = (cljs.core.truth_(err_line)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16407,new cljs.core.Keyword("clojure.error","line","clojure.error/line",-1816287471),err_line):G__16407);
var G__16407__$2 = (cljs.core.truth_(message)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16407__$1,new cljs.core.Keyword("clojure.error","cause","clojure.error/cause",-1879175742),message):G__16407__$1);
var G__16407__$3 = (cljs.core.truth_((function (){var or__5002__auto__ = fn;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
var and__5000__auto__ = source__$1;
if(cljs.core.truth_(and__5000__auto__)){
return method;
} else {
return and__5000__auto__;
}
}
})())?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16407__$2,new cljs.core.Keyword("clojure.error","symbol","clojure.error/symbol",1544821994),(function (){var or__5002__auto__ = fn;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return (new cljs.core.PersistentVector(null,2,(5),cljs.core.PersistentVector.EMPTY_NODE,[source__$1,method],null));
}
})()):G__16407__$2);
var G__16407__$4 = (cljs.core.truth_(file__$1)?cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16407__$3,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397),file__$1):G__16407__$3);
if(cljs.core.truth_(problems)){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(G__16407__$4,new cljs.core.Keyword("clojure.error","spec","clojure.error/spec",2055032595),data);
} else {
return G__16407__$4;
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__16382__$1)].join('')));

}
})(),new cljs.core.Keyword("clojure.error","phase","clojure.error/phase",275140358),phase);
});
/**
 * Returns a string from exception data, as produced by ex-triage.
 *   The first line summarizes the exception phase and location.
 *   The subsequent lines describe the cause.
 */
cljs.repl.ex_str = (function cljs$repl$ex_str(p__16410){
var map__16411 = p__16410;
var map__16411__$1 = cljs.core.__destructure_map(map__16411);
var triage_data = map__16411__$1;
var phase = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","phase","clojure.error/phase",275140358));
var source = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","source","clojure.error/source",-2011936397));
var line = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","line","clojure.error/line",-1816287471));
var column = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","column","clojure.error/column",304721553));
var symbol = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","symbol","clojure.error/symbol",1544821994));
var class$ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","class","clojure.error/class",278435890));
var cause = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","cause","clojure.error/cause",-1879175742));
var spec = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__16411__$1,new cljs.core.Keyword("clojure.error","spec","clojure.error/spec",2055032595));
var loc = [cljs.core.str.cljs$core$IFn$_invoke$arity$1((function (){var or__5002__auto__ = source;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return "<cljs repl>";
}
})()),":",cljs.core.str.cljs$core$IFn$_invoke$arity$1((function (){var or__5002__auto__ = line;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return (1);
}
})()),(cljs.core.truth_(column)?[":",cljs.core.str.cljs$core$IFn$_invoke$arity$1(column)].join(''):"")].join('');
var class_name = cljs.core.name((function (){var or__5002__auto__ = class$;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return "";
}
})());
var simple_class = class_name;
var cause_type = ((cljs.core.contains_QMARK_(new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, ["RuntimeException",null,"Exception",null], null), null),simple_class))?"":[" (",simple_class,")"].join(''));
var format = goog.string.format;
var G__16412 = phase;
var G__16412__$1 = (((G__16412 instanceof cljs.core.Keyword))?G__16412.fqn:null);
switch (G__16412__$1) {
case "read-source":
return (format.cljs$core$IFn$_invoke$arity$3 ? format.cljs$core$IFn$_invoke$arity$3("Syntax error reading source at (%s).\n%s\n",loc,cause) : format.call(null,"Syntax error reading source at (%s).\n%s\n",loc,cause));

break;
case "macro-syntax-check":
var G__16413 = "Syntax error macroexpanding %sat (%s).\n%s";
var G__16414 = (cljs.core.truth_(symbol)?[cljs.core.str.cljs$core$IFn$_invoke$arity$1(symbol)," "].join(''):"");
var G__16415 = loc;
var G__16416 = (cljs.core.truth_(spec)?(function (){var sb__5647__auto__ = (new goog.string.StringBuffer());
var _STAR_print_newline_STAR__orig_val__16417_16528 = cljs.core._STAR_print_newline_STAR_;
var _STAR_print_fn_STAR__orig_val__16418_16529 = cljs.core._STAR_print_fn_STAR_;
var _STAR_print_newline_STAR__temp_val__16419_16530 = true;
var _STAR_print_fn_STAR__temp_val__16420_16531 = (function (x__5648__auto__){
return sb__5647__auto__.append(x__5648__auto__);
});
(cljs.core._STAR_print_newline_STAR_ = _STAR_print_newline_STAR__temp_val__16419_16530);

(cljs.core._STAR_print_fn_STAR_ = _STAR_print_fn_STAR__temp_val__16420_16531);

try{cljs.spec.alpha.explain_out(cljs.core.update.cljs$core$IFn$_invoke$arity$3(spec,new cljs.core.Keyword("cljs.spec.alpha","problems","cljs.spec.alpha/problems",447400814),(function (probs){
return cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__16408_SHARP_){
return cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(p1__16408_SHARP_,new cljs.core.Keyword(null,"in","in",-1531184865));
}),probs);
}))
);
}finally {(cljs.core._STAR_print_fn_STAR_ = _STAR_print_fn_STAR__orig_val__16418_16529);

(cljs.core._STAR_print_newline_STAR_ = _STAR_print_newline_STAR__orig_val__16417_16528);
}
return cljs.core.str.cljs$core$IFn$_invoke$arity$1(sb__5647__auto__);
})():(format.cljs$core$IFn$_invoke$arity$2 ? format.cljs$core$IFn$_invoke$arity$2("%s\n",cause) : format.call(null,"%s\n",cause)));
return (format.cljs$core$IFn$_invoke$arity$4 ? format.cljs$core$IFn$_invoke$arity$4(G__16413,G__16414,G__16415,G__16416) : format.call(null,G__16413,G__16414,G__16415,G__16416));

break;
case "macroexpansion":
var G__16421 = "Unexpected error%s macroexpanding %sat (%s).\n%s\n";
var G__16422 = cause_type;
var G__16423 = (cljs.core.truth_(symbol)?[cljs.core.str.cljs$core$IFn$_invoke$arity$1(symbol)," "].join(''):"");
var G__16424 = loc;
var G__16425 = cause;
return (format.cljs$core$IFn$_invoke$arity$5 ? format.cljs$core$IFn$_invoke$arity$5(G__16421,G__16422,G__16423,G__16424,G__16425) : format.call(null,G__16421,G__16422,G__16423,G__16424,G__16425));

break;
case "compile-syntax-check":
var G__16426 = "Syntax error%s compiling %sat (%s).\n%s\n";
var G__16427 = cause_type;
var G__16428 = (cljs.core.truth_(symbol)?[cljs.core.str.cljs$core$IFn$_invoke$arity$1(symbol)," "].join(''):"");
var G__16429 = loc;
var G__16430 = cause;
return (format.cljs$core$IFn$_invoke$arity$5 ? format.cljs$core$IFn$_invoke$arity$5(G__16426,G__16427,G__16428,G__16429,G__16430) : format.call(null,G__16426,G__16427,G__16428,G__16429,G__16430));

break;
case "compilation":
var G__16431 = "Unexpected error%s compiling %sat (%s).\n%s\n";
var G__16432 = cause_type;
var G__16433 = (cljs.core.truth_(symbol)?[cljs.core.str.cljs$core$IFn$_invoke$arity$1(symbol)," "].join(''):"");
var G__16434 = loc;
var G__16435 = cause;
return (format.cljs$core$IFn$_invoke$arity$5 ? format.cljs$core$IFn$_invoke$arity$5(G__16431,G__16432,G__16433,G__16434,G__16435) : format.call(null,G__16431,G__16432,G__16433,G__16434,G__16435));

break;
case "read-eval-result":
return (format.cljs$core$IFn$_invoke$arity$5 ? format.cljs$core$IFn$_invoke$arity$5("Error reading eval result%s at %s (%s).\n%s\n",cause_type,symbol,loc,cause) : format.call(null,"Error reading eval result%s at %s (%s).\n%s\n",cause_type,symbol,loc,cause));

break;
case "print-eval-result":
return (format.cljs$core$IFn$_invoke$arity$5 ? format.cljs$core$IFn$_invoke$arity$5("Error printing return value%s at %s (%s).\n%s\n",cause_type,symbol,loc,cause) : format.call(null,"Error printing return value%s at %s (%s).\n%s\n",cause_type,symbol,loc,cause));

break;
case "execution":
if(cljs.core.truth_(spec)){
var G__16436 = "Execution error - invalid arguments to %s at (%s).\n%s";
var G__16437 = symbol;
var G__16438 = loc;
var G__16439 = (function (){var sb__5647__auto__ = (new goog.string.StringBuffer());
var _STAR_print_newline_STAR__orig_val__16440_16533 = cljs.core._STAR_print_newline_STAR_;
var _STAR_print_fn_STAR__orig_val__16441_16534 = cljs.core._STAR_print_fn_STAR_;
var _STAR_print_newline_STAR__temp_val__16442_16535 = true;
var _STAR_print_fn_STAR__temp_val__16443_16536 = (function (x__5648__auto__){
return sb__5647__auto__.append(x__5648__auto__);
});
(cljs.core._STAR_print_newline_STAR_ = _STAR_print_newline_STAR__temp_val__16442_16535);

(cljs.core._STAR_print_fn_STAR_ = _STAR_print_fn_STAR__temp_val__16443_16536);

try{cljs.spec.alpha.explain_out(cljs.core.update.cljs$core$IFn$_invoke$arity$3(spec,new cljs.core.Keyword("cljs.spec.alpha","problems","cljs.spec.alpha/problems",447400814),(function (probs){
return cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__16409_SHARP_){
return cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(p1__16409_SHARP_,new cljs.core.Keyword(null,"in","in",-1531184865));
}),probs);
}))
);
}finally {(cljs.core._STAR_print_fn_STAR_ = _STAR_print_fn_STAR__orig_val__16441_16534);

(cljs.core._STAR_print_newline_STAR_ = _STAR_print_newline_STAR__orig_val__16440_16533);
}
return cljs.core.str.cljs$core$IFn$_invoke$arity$1(sb__5647__auto__);
})();
return (format.cljs$core$IFn$_invoke$arity$4 ? format.cljs$core$IFn$_invoke$arity$4(G__16436,G__16437,G__16438,G__16439) : format.call(null,G__16436,G__16437,G__16438,G__16439));
} else {
var G__16445 = "Execution error%s at %s(%s).\n%s\n";
var G__16446 = cause_type;
var G__16447 = (cljs.core.truth_(symbol)?[cljs.core.str.cljs$core$IFn$_invoke$arity$1(symbol)," "].join(''):"");
var G__16448 = loc;
var G__16449 = cause;
return (format.cljs$core$IFn$_invoke$arity$5 ? format.cljs$core$IFn$_invoke$arity$5(G__16445,G__16446,G__16447,G__16448,G__16449) : format.call(null,G__16445,G__16446,G__16447,G__16448,G__16449));
}

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__16412__$1)].join('')));

}
});
cljs.repl.error__GT_str = (function cljs$repl$error__GT_str(error){
return cljs.repl.ex_str(cljs.repl.ex_triage(cljs.repl.Error__GT_map(error)));
});

//# sourceMappingURL=cljs.repl.js.map
