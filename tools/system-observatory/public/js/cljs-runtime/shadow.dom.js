goog.provide('shadow.dom');
shadow.dom.transition_supported_QMARK_ = true;

/**
 * @interface
 */
shadow.dom.IElement = function(){};

var shadow$dom$IElement$_to_dom$dyn_11065 = (function (this$){
var x__5350__auto__ = (((this$ == null))?null:this$);
var m__5351__auto__ = (shadow.dom._to_dom[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(this$) : m__5351__auto__.call(null,this$));
} else {
var m__5349__auto__ = (shadow.dom._to_dom["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(this$) : m__5349__auto__.call(null,this$));
} else {
throw cljs.core.missing_protocol("IElement.-to-dom",this$);
}
}
});
shadow.dom._to_dom = (function shadow$dom$_to_dom(this$){
if((((!((this$ == null)))) && ((!((this$.shadow$dom$IElement$_to_dom$arity$1 == null)))))){
return this$.shadow$dom$IElement$_to_dom$arity$1(this$);
} else {
return shadow$dom$IElement$_to_dom$dyn_11065(this$);
}
});


/**
 * @interface
 */
shadow.dom.SVGElement = function(){};

var shadow$dom$SVGElement$_to_svg$dyn_11066 = (function (this$){
var x__5350__auto__ = (((this$ == null))?null:this$);
var m__5351__auto__ = (shadow.dom._to_svg[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(this$) : m__5351__auto__.call(null,this$));
} else {
var m__5349__auto__ = (shadow.dom._to_svg["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(this$) : m__5349__auto__.call(null,this$));
} else {
throw cljs.core.missing_protocol("SVGElement.-to-svg",this$);
}
}
});
shadow.dom._to_svg = (function shadow$dom$_to_svg(this$){
if((((!((this$ == null)))) && ((!((this$.shadow$dom$SVGElement$_to_svg$arity$1 == null)))))){
return this$.shadow$dom$SVGElement$_to_svg$arity$1(this$);
} else {
return shadow$dom$SVGElement$_to_svg$dyn_11066(this$);
}
});

shadow.dom.lazy_native_coll_seq = (function shadow$dom$lazy_native_coll_seq(coll,idx){
if((idx < coll.length)){
return (new cljs.core.LazySeq(null,(function (){
return cljs.core.cons((coll[idx]),(function (){var G__10592 = coll;
var G__10593 = (idx + (1));
return (shadow.dom.lazy_native_coll_seq.cljs$core$IFn$_invoke$arity$2 ? shadow.dom.lazy_native_coll_seq.cljs$core$IFn$_invoke$arity$2(G__10592,G__10593) : shadow.dom.lazy_native_coll_seq.call(null,G__10592,G__10593));
})());
}),null,null));
} else {
return null;
}
});

/**
* @constructor
 * @implements {cljs.core.IIndexed}
 * @implements {cljs.core.ICounted}
 * @implements {cljs.core.ISeqable}
 * @implements {cljs.core.IDeref}
 * @implements {shadow.dom.IElement}
*/
shadow.dom.NativeColl = (function (coll){
this.coll = coll;
this.cljs$lang$protocol_mask$partition0$ = 8421394;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(shadow.dom.NativeColl.prototype.cljs$core$IDeref$_deref$arity$1 = (function (this$){
var self__ = this;
var this$__$1 = this;
return self__.coll;
}));

(shadow.dom.NativeColl.prototype.cljs$core$IIndexed$_nth$arity$2 = (function (this$,n){
var self__ = this;
var this$__$1 = this;
return (self__.coll[n]);
}));

(shadow.dom.NativeColl.prototype.cljs$core$IIndexed$_nth$arity$3 = (function (this$,n,not_found){
var self__ = this;
var this$__$1 = this;
var or__5002__auto__ = (self__.coll[n]);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return not_found;
}
}));

(shadow.dom.NativeColl.prototype.cljs$core$ICounted$_count$arity$1 = (function (this$){
var self__ = this;
var this$__$1 = this;
return self__.coll.length;
}));

(shadow.dom.NativeColl.prototype.cljs$core$ISeqable$_seq$arity$1 = (function (this$){
var self__ = this;
var this$__$1 = this;
return shadow.dom.lazy_native_coll_seq(self__.coll,(0));
}));

(shadow.dom.NativeColl.prototype.shadow$dom$IElement$ = cljs.core.PROTOCOL_SENTINEL);

(shadow.dom.NativeColl.prototype.shadow$dom$IElement$_to_dom$arity$1 = (function (this$){
var self__ = this;
var this$__$1 = this;
return self__.coll;
}));

(shadow.dom.NativeColl.getBasis = (function (){
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"coll","coll",-1006698606,null)], null);
}));

(shadow.dom.NativeColl.cljs$lang$type = true);

(shadow.dom.NativeColl.cljs$lang$ctorStr = "shadow.dom/NativeColl");

(shadow.dom.NativeColl.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"shadow.dom/NativeColl");
}));

/**
 * Positional factory function for shadow.dom/NativeColl.
 */
shadow.dom.__GT_NativeColl = (function shadow$dom$__GT_NativeColl(coll){
return (new shadow.dom.NativeColl(coll));
});

shadow.dom.native_coll = (function shadow$dom$native_coll(coll){
return (new shadow.dom.NativeColl(coll));
});
shadow.dom.dom_node = (function shadow$dom$dom_node(el){
if((el == null)){
return null;
} else {
if((((!((el == null))))?((((false) || ((cljs.core.PROTOCOL_SENTINEL === el.shadow$dom$IElement$))))?true:false):false)){
return el.shadow$dom$IElement$_to_dom$arity$1(null);
} else {
if(typeof el === 'string'){
return document.createTextNode(el);
} else {
if(typeof el === 'number'){
return document.createTextNode(cljs.core.str.cljs$core$IFn$_invoke$arity$1(el));
} else {
return el;

}
}
}
}
});
shadow.dom.query_one = (function shadow$dom$query_one(var_args){
var G__10611 = arguments.length;
switch (G__10611) {
case 1:
return shadow.dom.query_one.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.query_one.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.query_one.cljs$core$IFn$_invoke$arity$1 = (function (sel){
return document.querySelector(sel);
}));

(shadow.dom.query_one.cljs$core$IFn$_invoke$arity$2 = (function (sel,root){
return shadow.dom.dom_node(root).querySelector(sel);
}));

(shadow.dom.query_one.cljs$lang$maxFixedArity = 2);

shadow.dom.query = (function shadow$dom$query(var_args){
var G__10621 = arguments.length;
switch (G__10621) {
case 1:
return shadow.dom.query.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.query.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.query.cljs$core$IFn$_invoke$arity$1 = (function (sel){
return (new shadow.dom.NativeColl(document.querySelectorAll(sel)));
}));

(shadow.dom.query.cljs$core$IFn$_invoke$arity$2 = (function (sel,root){
return (new shadow.dom.NativeColl(shadow.dom.dom_node(root).querySelectorAll(sel)));
}));

(shadow.dom.query.cljs$lang$maxFixedArity = 2);

shadow.dom.by_id = (function shadow$dom$by_id(var_args){
var G__10627 = arguments.length;
switch (G__10627) {
case 2:
return shadow.dom.by_id.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 1:
return shadow.dom.by_id.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.by_id.cljs$core$IFn$_invoke$arity$2 = (function (id,el){
return shadow.dom.dom_node(el).getElementById(id);
}));

(shadow.dom.by_id.cljs$core$IFn$_invoke$arity$1 = (function (id){
return document.getElementById(id);
}));

(shadow.dom.by_id.cljs$lang$maxFixedArity = 2);

shadow.dom.build = shadow.dom.dom_node;
shadow.dom.ev_stop = (function shadow$dom$ev_stop(var_args){
var G__10631 = arguments.length;
switch (G__10631) {
case 1:
return shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 4:
return shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$1 = (function (e){
if(cljs.core.truth_(e.stopPropagation)){
e.stopPropagation();

e.preventDefault();
} else {
(e.cancelBubble = true);

(e.returnValue = false);
}

return e;
}));

(shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$2 = (function (e,el){
shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$1(e);

return el;
}));

(shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$4 = (function (e,el,scope,owner){
shadow.dom.ev_stop.cljs$core$IFn$_invoke$arity$1(e);

return el;
}));

(shadow.dom.ev_stop.cljs$lang$maxFixedArity = 4);

/**
 * check wether a parent node (or the document) contains the child
 */
shadow.dom.contains_QMARK_ = (function shadow$dom$contains_QMARK_(var_args){
var G__10637 = arguments.length;
switch (G__10637) {
case 1:
return shadow.dom.contains_QMARK_.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.contains_QMARK_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.contains_QMARK_.cljs$core$IFn$_invoke$arity$1 = (function (el){
return goog.dom.contains(document,shadow.dom.dom_node(el));
}));

(shadow.dom.contains_QMARK_.cljs$core$IFn$_invoke$arity$2 = (function (parent,el){
return goog.dom.contains(shadow.dom.dom_node(parent),shadow.dom.dom_node(el));
}));

(shadow.dom.contains_QMARK_.cljs$lang$maxFixedArity = 2);

shadow.dom.add_class = (function shadow$dom$add_class(el,cls){
return goog.dom.classlist.add(shadow.dom.dom_node(el),cls);
});
shadow.dom.remove_class = (function shadow$dom$remove_class(el,cls){
return goog.dom.classlist.remove(shadow.dom.dom_node(el),cls);
});
shadow.dom.toggle_class = (function shadow$dom$toggle_class(var_args){
var G__10640 = arguments.length;
switch (G__10640) {
case 2:
return shadow.dom.toggle_class.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return shadow.dom.toggle_class.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.toggle_class.cljs$core$IFn$_invoke$arity$2 = (function (el,cls){
return goog.dom.classlist.toggle(shadow.dom.dom_node(el),cls);
}));

(shadow.dom.toggle_class.cljs$core$IFn$_invoke$arity$3 = (function (el,cls,v){
if(cljs.core.truth_(v)){
return shadow.dom.add_class(el,cls);
} else {
return shadow.dom.remove_class(el,cls);
}
}));

(shadow.dom.toggle_class.cljs$lang$maxFixedArity = 3);

shadow.dom.dom_listen = (cljs.core.truth_((function (){var or__5002__auto__ = (!((typeof document !== 'undefined')));
if(or__5002__auto__){
return or__5002__auto__;
} else {
return document.addEventListener;
}
})())?(function shadow$dom$dom_listen_good(el,ev,handler){
return el.addEventListener(ev,handler,false);
}):(function shadow$dom$dom_listen_ie(el,ev,handler){
try{return el.attachEvent(["on",cljs.core.str.cljs$core$IFn$_invoke$arity$1(ev)].join(''),(function (e){
return (handler.cljs$core$IFn$_invoke$arity$2 ? handler.cljs$core$IFn$_invoke$arity$2(e,el) : handler.call(null,e,el));
}));
}catch (e10651){if((e10651 instanceof Object)){
var e = e10651;
return console.log("didnt support attachEvent",el,e);
} else {
throw e10651;

}
}}));
shadow.dom.dom_listen_remove = (cljs.core.truth_((function (){var or__5002__auto__ = (!((typeof document !== 'undefined')));
if(or__5002__auto__){
return or__5002__auto__;
} else {
return document.removeEventListener;
}
})())?(function shadow$dom$dom_listen_remove_good(el,ev,handler){
return el.removeEventListener(ev,handler,false);
}):(function shadow$dom$dom_listen_remove_ie(el,ev,handler){
return el.detachEvent(["on",cljs.core.str.cljs$core$IFn$_invoke$arity$1(ev)].join(''),handler);
}));
shadow.dom.on_query = (function shadow$dom$on_query(root_el,ev,selector,handler){
var seq__10664 = cljs.core.seq(shadow.dom.query.cljs$core$IFn$_invoke$arity$2(selector,root_el));
var chunk__10665 = null;
var count__10666 = (0);
var i__10667 = (0);
while(true){
if((i__10667 < count__10666)){
var el = chunk__10665.cljs$core$IIndexed$_nth$arity$2(null,i__10667);
var handler_11073__$1 = ((function (seq__10664,chunk__10665,count__10666,i__10667,el){
return (function (e){
return (handler.cljs$core$IFn$_invoke$arity$2 ? handler.cljs$core$IFn$_invoke$arity$2(e,el) : handler.call(null,e,el));
});})(seq__10664,chunk__10665,count__10666,i__10667,el))
;
shadow.dom.dom_listen(el,cljs.core.name(ev),handler_11073__$1);


var G__11074 = seq__10664;
var G__11075 = chunk__10665;
var G__11076 = count__10666;
var G__11077 = (i__10667 + (1));
seq__10664 = G__11074;
chunk__10665 = G__11075;
count__10666 = G__11076;
i__10667 = G__11077;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__10664);
if(temp__5804__auto__){
var seq__10664__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__10664__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__10664__$1);
var G__11078 = cljs.core.chunk_rest(seq__10664__$1);
var G__11079 = c__5525__auto__;
var G__11080 = cljs.core.count(c__5525__auto__);
var G__11081 = (0);
seq__10664 = G__11078;
chunk__10665 = G__11079;
count__10666 = G__11080;
i__10667 = G__11081;
continue;
} else {
var el = cljs.core.first(seq__10664__$1);
var handler_11082__$1 = ((function (seq__10664,chunk__10665,count__10666,i__10667,el,seq__10664__$1,temp__5804__auto__){
return (function (e){
return (handler.cljs$core$IFn$_invoke$arity$2 ? handler.cljs$core$IFn$_invoke$arity$2(e,el) : handler.call(null,e,el));
});})(seq__10664,chunk__10665,count__10666,i__10667,el,seq__10664__$1,temp__5804__auto__))
;
shadow.dom.dom_listen(el,cljs.core.name(ev),handler_11082__$1);


var G__11083 = cljs.core.next(seq__10664__$1);
var G__11084 = null;
var G__11085 = (0);
var G__11086 = (0);
seq__10664 = G__11083;
chunk__10665 = G__11084;
count__10666 = G__11085;
i__10667 = G__11086;
continue;
}
} else {
return null;
}
}
break;
}
});
shadow.dom.on = (function shadow$dom$on(var_args){
var G__10683 = arguments.length;
switch (G__10683) {
case 3:
return shadow.dom.on.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
case 4:
return shadow.dom.on.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.on.cljs$core$IFn$_invoke$arity$3 = (function (el,ev,handler){
return shadow.dom.on.cljs$core$IFn$_invoke$arity$4(el,ev,handler,false);
}));

(shadow.dom.on.cljs$core$IFn$_invoke$arity$4 = (function (el,ev,handler,capture){
if(cljs.core.vector_QMARK_(ev)){
return shadow.dom.on_query(el,cljs.core.first(ev),cljs.core.second(ev),handler);
} else {
var handler__$1 = (function (e){
return (handler.cljs$core$IFn$_invoke$arity$2 ? handler.cljs$core$IFn$_invoke$arity$2(e,el) : handler.call(null,e,el));
});
return shadow.dom.dom_listen(shadow.dom.dom_node(el),cljs.core.name(ev),handler__$1);
}
}));

(shadow.dom.on.cljs$lang$maxFixedArity = 4);

shadow.dom.remove_event_handler = (function shadow$dom$remove_event_handler(el,ev,handler){
return shadow.dom.dom_listen_remove(shadow.dom.dom_node(el),cljs.core.name(ev),handler);
});
shadow.dom.add_event_listeners = (function shadow$dom$add_event_listeners(el,events){
var seq__10685 = cljs.core.seq(events);
var chunk__10686 = null;
var count__10687 = (0);
var i__10688 = (0);
while(true){
if((i__10688 < count__10687)){
var vec__10707 = chunk__10686.cljs$core$IIndexed$_nth$arity$2(null,i__10688);
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10707,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10707,(1),null);
shadow.dom.on.cljs$core$IFn$_invoke$arity$3(el,k,v);


var G__11088 = seq__10685;
var G__11089 = chunk__10686;
var G__11090 = count__10687;
var G__11091 = (i__10688 + (1));
seq__10685 = G__11088;
chunk__10686 = G__11089;
count__10687 = G__11090;
i__10688 = G__11091;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__10685);
if(temp__5804__auto__){
var seq__10685__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__10685__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__10685__$1);
var G__11092 = cljs.core.chunk_rest(seq__10685__$1);
var G__11093 = c__5525__auto__;
var G__11094 = cljs.core.count(c__5525__auto__);
var G__11095 = (0);
seq__10685 = G__11092;
chunk__10686 = G__11093;
count__10687 = G__11094;
i__10688 = G__11095;
continue;
} else {
var vec__10712 = cljs.core.first(seq__10685__$1);
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10712,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10712,(1),null);
shadow.dom.on.cljs$core$IFn$_invoke$arity$3(el,k,v);


var G__11096 = cljs.core.next(seq__10685__$1);
var G__11097 = null;
var G__11098 = (0);
var G__11099 = (0);
seq__10685 = G__11096;
chunk__10686 = G__11097;
count__10687 = G__11098;
i__10688 = G__11099;
continue;
}
} else {
return null;
}
}
break;
}
});
shadow.dom.set_style = (function shadow$dom$set_style(el,styles){
var dom = shadow.dom.dom_node(el);
var seq__10729 = cljs.core.seq(styles);
var chunk__10730 = null;
var count__10731 = (0);
var i__10732 = (0);
while(true){
if((i__10732 < count__10731)){
var vec__10759 = chunk__10730.cljs$core$IIndexed$_nth$arity$2(null,i__10732);
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10759,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10759,(1),null);
goog.style.setStyle(dom,cljs.core.name(k),(((v == null))?"":v));


var G__11100 = seq__10729;
var G__11101 = chunk__10730;
var G__11102 = count__10731;
var G__11103 = (i__10732 + (1));
seq__10729 = G__11100;
chunk__10730 = G__11101;
count__10731 = G__11102;
i__10732 = G__11103;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__10729);
if(temp__5804__auto__){
var seq__10729__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__10729__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__10729__$1);
var G__11104 = cljs.core.chunk_rest(seq__10729__$1);
var G__11105 = c__5525__auto__;
var G__11106 = cljs.core.count(c__5525__auto__);
var G__11107 = (0);
seq__10729 = G__11104;
chunk__10730 = G__11105;
count__10731 = G__11106;
i__10732 = G__11107;
continue;
} else {
var vec__10770 = cljs.core.first(seq__10729__$1);
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10770,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10770,(1),null);
goog.style.setStyle(dom,cljs.core.name(k),(((v == null))?"":v));


var G__11108 = cljs.core.next(seq__10729__$1);
var G__11109 = null;
var G__11110 = (0);
var G__11111 = (0);
seq__10729 = G__11108;
chunk__10730 = G__11109;
count__10731 = G__11110;
i__10732 = G__11111;
continue;
}
} else {
return null;
}
}
break;
}
});
shadow.dom.set_attr_STAR_ = (function shadow$dom$set_attr_STAR_(el,key,value){
var G__10781_11112 = key;
var G__10781_11113__$1 = (((G__10781_11112 instanceof cljs.core.Keyword))?G__10781_11112.fqn:null);
switch (G__10781_11113__$1) {
case "id":
(el.id = cljs.core.str.cljs$core$IFn$_invoke$arity$1(value));

break;
case "class":
(el.className = cljs.core.str.cljs$core$IFn$_invoke$arity$1(value));

break;
case "for":
(el.htmlFor = value);

break;
case "cellpadding":
el.setAttribute("cellPadding",value);

break;
case "cellspacing":
el.setAttribute("cellSpacing",value);

break;
case "colspan":
el.setAttribute("colSpan",value);

break;
case "frameborder":
el.setAttribute("frameBorder",value);

break;
case "height":
el.setAttribute("height",value);

break;
case "maxlength":
el.setAttribute("maxLength",value);

break;
case "role":
el.setAttribute("role",value);

break;
case "rowspan":
el.setAttribute("rowSpan",value);

break;
case "type":
el.setAttribute("type",value);

break;
case "usemap":
el.setAttribute("useMap",value);

break;
case "valign":
el.setAttribute("vAlign",value);

break;
case "width":
el.setAttribute("width",value);

break;
case "on":
shadow.dom.add_event_listeners(el,value);

break;
case "style":
if((value == null)){
} else {
if(typeof value === 'string'){
el.setAttribute("style",value);
} else {
if(cljs.core.map_QMARK_(value)){
shadow.dom.set_style(el,value);
} else {
goog.style.setStyle(el,value);

}
}
}

break;
default:
var ks_11115 = cljs.core.name(key);
if(cljs.core.truth_((function (){var or__5002__auto__ = goog.string.startsWith(ks_11115,"data-");
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return goog.string.startsWith(ks_11115,"aria-");
}
})())){
el.setAttribute(ks_11115,value);
} else {
(el[ks_11115] = value);
}

}

return el;
});
shadow.dom.set_attrs = (function shadow$dom$set_attrs(el,attrs){
return cljs.core.reduce_kv((function (el__$1,key,value){
shadow.dom.set_attr_STAR_(el__$1,key,value);

return el__$1;
}),shadow.dom.dom_node(el),attrs);
});
shadow.dom.set_attr = (function shadow$dom$set_attr(el,key,value){
return shadow.dom.set_attr_STAR_(shadow.dom.dom_node(el),key,value);
});
shadow.dom.has_class_QMARK_ = (function shadow$dom$has_class_QMARK_(el,cls){
return goog.dom.classlist.contains(shadow.dom.dom_node(el),cls);
});
shadow.dom.merge_class_string = (function shadow$dom$merge_class_string(current,extra_class){
if(cljs.core.seq(current)){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(current)," ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(extra_class)].join('');
} else {
return extra_class;
}
});
shadow.dom.parse_tag = (function shadow$dom$parse_tag(spec){
var spec__$1 = cljs.core.name(spec);
var fdot = spec__$1.indexOf(".");
var fhash = spec__$1.indexOf("#");
if(((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2((-1),fdot)) && (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2((-1),fhash)))){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [spec__$1,null,null], null);
} else {
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2((-1),fhash)){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [spec__$1.substring((0),fdot),null,clojure.string.replace(spec__$1.substring((fdot + (1))),/\./," ")], null);
} else {
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2((-1),fdot)){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [spec__$1.substring((0),fhash),spec__$1.substring((fhash + (1))),null], null);
} else {
if((fhash > fdot)){
throw ["cant have id after class?",spec__$1].join('');
} else {
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [spec__$1.substring((0),fhash),spec__$1.substring((fhash + (1)),fdot),clojure.string.replace(spec__$1.substring((fdot + (1))),/\./," ")], null);

}
}
}
}
});
shadow.dom.create_dom_node = (function shadow$dom$create_dom_node(tag_def,p__10792){
var map__10793 = p__10792;
var map__10793__$1 = cljs.core.__destructure_map(map__10793);
var props = map__10793__$1;
var class$ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__10793__$1,new cljs.core.Keyword(null,"class","class",-2030961996));
var tag_props = ({});
var vec__10794 = shadow.dom.parse_tag(tag_def);
var tag_name = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10794,(0),null);
var tag_id = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10794,(1),null);
var tag_classes = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10794,(2),null);
if(cljs.core.truth_(tag_id)){
(tag_props["id"] = tag_id);
} else {
}

if(cljs.core.truth_(tag_classes)){
(tag_props["class"] = shadow.dom.merge_class_string(class$,tag_classes));
} else {
}

var G__10797 = goog.dom.createDom(tag_name,tag_props);
shadow.dom.set_attrs(G__10797,cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(props,new cljs.core.Keyword(null,"class","class",-2030961996)));

return G__10797;
});
shadow.dom.append = (function shadow$dom$append(var_args){
var G__10799 = arguments.length;
switch (G__10799) {
case 1:
return shadow.dom.append.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.append.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.append.cljs$core$IFn$_invoke$arity$1 = (function (node){
if(cljs.core.truth_(node)){
var temp__5804__auto__ = shadow.dom.dom_node(node);
if(cljs.core.truth_(temp__5804__auto__)){
var n = temp__5804__auto__;
document.body.appendChild(n);

return n;
} else {
return null;
}
} else {
return null;
}
}));

(shadow.dom.append.cljs$core$IFn$_invoke$arity$2 = (function (el,node){
if(cljs.core.truth_(node)){
var temp__5804__auto__ = shadow.dom.dom_node(node);
if(cljs.core.truth_(temp__5804__auto__)){
var n = temp__5804__auto__;
shadow.dom.dom_node(el).appendChild(n);

return n;
} else {
return null;
}
} else {
return null;
}
}));

(shadow.dom.append.cljs$lang$maxFixedArity = 2);

shadow.dom.destructure_node = (function shadow$dom$destructure_node(create_fn,p__10800){
var vec__10804 = p__10800;
var seq__10805 = cljs.core.seq(vec__10804);
var first__10806 = cljs.core.first(seq__10805);
var seq__10805__$1 = cljs.core.next(seq__10805);
var nn = first__10806;
var first__10806__$1 = cljs.core.first(seq__10805__$1);
var seq__10805__$2 = cljs.core.next(seq__10805__$1);
var np = first__10806__$1;
var nc = seq__10805__$2;
var node = vec__10804;
if((nn instanceof cljs.core.Keyword)){
} else {
throw cljs.core.ex_info.cljs$core$IFn$_invoke$arity$2("invalid dom node",new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"node","node",581201198),node], null));
}

if((((np == null)) && ((nc == null)))){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){var G__10807 = nn;
var G__10808 = cljs.core.PersistentArrayMap.EMPTY;
return (create_fn.cljs$core$IFn$_invoke$arity$2 ? create_fn.cljs$core$IFn$_invoke$arity$2(G__10807,G__10808) : create_fn.call(null,G__10807,G__10808));
})(),cljs.core.List.EMPTY], null);
} else {
if(cljs.core.map_QMARK_(np)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(create_fn.cljs$core$IFn$_invoke$arity$2 ? create_fn.cljs$core$IFn$_invoke$arity$2(nn,np) : create_fn.call(null,nn,np)),nc], null);
} else {
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(function (){var G__10809 = nn;
var G__10810 = cljs.core.PersistentArrayMap.EMPTY;
return (create_fn.cljs$core$IFn$_invoke$arity$2 ? create_fn.cljs$core$IFn$_invoke$arity$2(G__10809,G__10810) : create_fn.call(null,G__10809,G__10810));
})(),cljs.core.conj.cljs$core$IFn$_invoke$arity$2(nc,np)], null);

}
}
});
shadow.dom.make_dom_node = (function shadow$dom$make_dom_node(structure){
var vec__10811 = shadow.dom.destructure_node(shadow.dom.create_dom_node,structure);
var node = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10811,(0),null);
var node_children = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10811,(1),null);
var seq__10814_11117 = cljs.core.seq(node_children);
var chunk__10815_11118 = null;
var count__10816_11119 = (0);
var i__10817_11120 = (0);
while(true){
if((i__10817_11120 < count__10816_11119)){
var child_struct_11121 = chunk__10815_11118.cljs$core$IIndexed$_nth$arity$2(null,i__10817_11120);
var children_11122 = shadow.dom.dom_node(child_struct_11121);
if(cljs.core.seq_QMARK_(children_11122)){
var seq__10851_11123 = cljs.core.seq(cljs.core.map.cljs$core$IFn$_invoke$arity$2(shadow.dom.dom_node,children_11122));
var chunk__10853_11124 = null;
var count__10854_11125 = (0);
var i__10855_11126 = (0);
while(true){
if((i__10855_11126 < count__10854_11125)){
var child_11127 = chunk__10853_11124.cljs$core$IIndexed$_nth$arity$2(null,i__10855_11126);
if(cljs.core.truth_(child_11127)){
shadow.dom.append.cljs$core$IFn$_invoke$arity$2(node,child_11127);


var G__11128 = seq__10851_11123;
var G__11129 = chunk__10853_11124;
var G__11130 = count__10854_11125;
var G__11131 = (i__10855_11126 + (1));
seq__10851_11123 = G__11128;
chunk__10853_11124 = G__11129;
count__10854_11125 = G__11130;
i__10855_11126 = G__11131;
continue;
} else {
var G__11132 = seq__10851_11123;
var G__11133 = chunk__10853_11124;
var G__11134 = count__10854_11125;
var G__11135 = (i__10855_11126 + (1));
seq__10851_11123 = G__11132;
chunk__10853_11124 = G__11133;
count__10854_11125 = G__11134;
i__10855_11126 = G__11135;
continue;
}
} else {
var temp__5804__auto___11136 = cljs.core.seq(seq__10851_11123);
if(temp__5804__auto___11136){
var seq__10851_11137__$1 = temp__5804__auto___11136;
if(cljs.core.chunked_seq_QMARK_(seq__10851_11137__$1)){
var c__5525__auto___11138 = cljs.core.chunk_first(seq__10851_11137__$1);
var G__11139 = cljs.core.chunk_rest(seq__10851_11137__$1);
var G__11140 = c__5525__auto___11138;
var G__11141 = cljs.core.count(c__5525__auto___11138);
var G__11142 = (0);
seq__10851_11123 = G__11139;
chunk__10853_11124 = G__11140;
count__10854_11125 = G__11141;
i__10855_11126 = G__11142;
continue;
} else {
var child_11143 = cljs.core.first(seq__10851_11137__$1);
if(cljs.core.truth_(child_11143)){
shadow.dom.append.cljs$core$IFn$_invoke$arity$2(node,child_11143);


var G__11144 = cljs.core.next(seq__10851_11137__$1);
var G__11145 = null;
var G__11146 = (0);
var G__11147 = (0);
seq__10851_11123 = G__11144;
chunk__10853_11124 = G__11145;
count__10854_11125 = G__11146;
i__10855_11126 = G__11147;
continue;
} else {
var G__11148 = cljs.core.next(seq__10851_11137__$1);
var G__11149 = null;
var G__11150 = (0);
var G__11151 = (0);
seq__10851_11123 = G__11148;
chunk__10853_11124 = G__11149;
count__10854_11125 = G__11150;
i__10855_11126 = G__11151;
continue;
}
}
} else {
}
}
break;
}
} else {
shadow.dom.append.cljs$core$IFn$_invoke$arity$2(node,children_11122);
}


var G__11152 = seq__10814_11117;
var G__11153 = chunk__10815_11118;
var G__11154 = count__10816_11119;
var G__11155 = (i__10817_11120 + (1));
seq__10814_11117 = G__11152;
chunk__10815_11118 = G__11153;
count__10816_11119 = G__11154;
i__10817_11120 = G__11155;
continue;
} else {
var temp__5804__auto___11156 = cljs.core.seq(seq__10814_11117);
if(temp__5804__auto___11156){
var seq__10814_11157__$1 = temp__5804__auto___11156;
if(cljs.core.chunked_seq_QMARK_(seq__10814_11157__$1)){
var c__5525__auto___11158 = cljs.core.chunk_first(seq__10814_11157__$1);
var G__11159 = cljs.core.chunk_rest(seq__10814_11157__$1);
var G__11160 = c__5525__auto___11158;
var G__11161 = cljs.core.count(c__5525__auto___11158);
var G__11162 = (0);
seq__10814_11117 = G__11159;
chunk__10815_11118 = G__11160;
count__10816_11119 = G__11161;
i__10817_11120 = G__11162;
continue;
} else {
var child_struct_11163 = cljs.core.first(seq__10814_11157__$1);
var children_11164 = shadow.dom.dom_node(child_struct_11163);
if(cljs.core.seq_QMARK_(children_11164)){
var seq__10872_11165 = cljs.core.seq(cljs.core.map.cljs$core$IFn$_invoke$arity$2(shadow.dom.dom_node,children_11164));
var chunk__10874_11166 = null;
var count__10875_11167 = (0);
var i__10876_11168 = (0);
while(true){
if((i__10876_11168 < count__10875_11167)){
var child_11169 = chunk__10874_11166.cljs$core$IIndexed$_nth$arity$2(null,i__10876_11168);
if(cljs.core.truth_(child_11169)){
shadow.dom.append.cljs$core$IFn$_invoke$arity$2(node,child_11169);


var G__11170 = seq__10872_11165;
var G__11171 = chunk__10874_11166;
var G__11172 = count__10875_11167;
var G__11173 = (i__10876_11168 + (1));
seq__10872_11165 = G__11170;
chunk__10874_11166 = G__11171;
count__10875_11167 = G__11172;
i__10876_11168 = G__11173;
continue;
} else {
var G__11174 = seq__10872_11165;
var G__11175 = chunk__10874_11166;
var G__11176 = count__10875_11167;
var G__11177 = (i__10876_11168 + (1));
seq__10872_11165 = G__11174;
chunk__10874_11166 = G__11175;
count__10875_11167 = G__11176;
i__10876_11168 = G__11177;
continue;
}
} else {
var temp__5804__auto___11178__$1 = cljs.core.seq(seq__10872_11165);
if(temp__5804__auto___11178__$1){
var seq__10872_11179__$1 = temp__5804__auto___11178__$1;
if(cljs.core.chunked_seq_QMARK_(seq__10872_11179__$1)){
var c__5525__auto___11180 = cljs.core.chunk_first(seq__10872_11179__$1);
var G__11181 = cljs.core.chunk_rest(seq__10872_11179__$1);
var G__11182 = c__5525__auto___11180;
var G__11183 = cljs.core.count(c__5525__auto___11180);
var G__11184 = (0);
seq__10872_11165 = G__11181;
chunk__10874_11166 = G__11182;
count__10875_11167 = G__11183;
i__10876_11168 = G__11184;
continue;
} else {
var child_11185 = cljs.core.first(seq__10872_11179__$1);
if(cljs.core.truth_(child_11185)){
shadow.dom.append.cljs$core$IFn$_invoke$arity$2(node,child_11185);


var G__11186 = cljs.core.next(seq__10872_11179__$1);
var G__11187 = null;
var G__11188 = (0);
var G__11189 = (0);
seq__10872_11165 = G__11186;
chunk__10874_11166 = G__11187;
count__10875_11167 = G__11188;
i__10876_11168 = G__11189;
continue;
} else {
var G__11190 = cljs.core.next(seq__10872_11179__$1);
var G__11191 = null;
var G__11192 = (0);
var G__11193 = (0);
seq__10872_11165 = G__11190;
chunk__10874_11166 = G__11191;
count__10875_11167 = G__11192;
i__10876_11168 = G__11193;
continue;
}
}
} else {
}
}
break;
}
} else {
shadow.dom.append.cljs$core$IFn$_invoke$arity$2(node,children_11164);
}


var G__11194 = cljs.core.next(seq__10814_11157__$1);
var G__11195 = null;
var G__11196 = (0);
var G__11197 = (0);
seq__10814_11117 = G__11194;
chunk__10815_11118 = G__11195;
count__10816_11119 = G__11196;
i__10817_11120 = G__11197;
continue;
}
} else {
}
}
break;
}

return node;
});
(cljs.core.Keyword.prototype.shadow$dom$IElement$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.Keyword.prototype.shadow$dom$IElement$_to_dom$arity$1 = (function (this$){
var this$__$1 = this;
return shadow.dom.make_dom_node(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [this$__$1], null));
}));

(cljs.core.PersistentVector.prototype.shadow$dom$IElement$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.PersistentVector.prototype.shadow$dom$IElement$_to_dom$arity$1 = (function (this$){
var this$__$1 = this;
return shadow.dom.make_dom_node(this$__$1);
}));

(cljs.core.LazySeq.prototype.shadow$dom$IElement$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.LazySeq.prototype.shadow$dom$IElement$_to_dom$arity$1 = (function (this$){
var this$__$1 = this;
return cljs.core.map.cljs$core$IFn$_invoke$arity$2(shadow.dom._to_dom,this$__$1);
}));
if(cljs.core.truth_(((typeof HTMLElement) != 'undefined'))){
(HTMLElement.prototype.shadow$dom$IElement$ = cljs.core.PROTOCOL_SENTINEL);

(HTMLElement.prototype.shadow$dom$IElement$_to_dom$arity$1 = (function (this$){
var this$__$1 = this;
return this$__$1;
}));
} else {
}
if(cljs.core.truth_(((typeof DocumentFragment) != 'undefined'))){
(DocumentFragment.prototype.shadow$dom$IElement$ = cljs.core.PROTOCOL_SENTINEL);

(DocumentFragment.prototype.shadow$dom$IElement$_to_dom$arity$1 = (function (this$){
var this$__$1 = this;
return this$__$1;
}));
} else {
}
/**
 * clear node children
 */
shadow.dom.reset = (function shadow$dom$reset(node){
return goog.dom.removeChildren(shadow.dom.dom_node(node));
});
shadow.dom.remove = (function shadow$dom$remove(node){
if((((!((node == null))))?(((((node.cljs$lang$protocol_mask$partition0$ & (8388608))) || ((cljs.core.PROTOCOL_SENTINEL === node.cljs$core$ISeqable$))))?true:false):false)){
var seq__10924 = cljs.core.seq(node);
var chunk__10925 = null;
var count__10926 = (0);
var i__10927 = (0);
while(true){
if((i__10927 < count__10926)){
var n = chunk__10925.cljs$core$IIndexed$_nth$arity$2(null,i__10927);
(shadow.dom.remove.cljs$core$IFn$_invoke$arity$1 ? shadow.dom.remove.cljs$core$IFn$_invoke$arity$1(n) : shadow.dom.remove.call(null,n));


var G__11198 = seq__10924;
var G__11199 = chunk__10925;
var G__11200 = count__10926;
var G__11201 = (i__10927 + (1));
seq__10924 = G__11198;
chunk__10925 = G__11199;
count__10926 = G__11200;
i__10927 = G__11201;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__10924);
if(temp__5804__auto__){
var seq__10924__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__10924__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__10924__$1);
var G__11202 = cljs.core.chunk_rest(seq__10924__$1);
var G__11203 = c__5525__auto__;
var G__11204 = cljs.core.count(c__5525__auto__);
var G__11205 = (0);
seq__10924 = G__11202;
chunk__10925 = G__11203;
count__10926 = G__11204;
i__10927 = G__11205;
continue;
} else {
var n = cljs.core.first(seq__10924__$1);
(shadow.dom.remove.cljs$core$IFn$_invoke$arity$1 ? shadow.dom.remove.cljs$core$IFn$_invoke$arity$1(n) : shadow.dom.remove.call(null,n));


var G__11206 = cljs.core.next(seq__10924__$1);
var G__11207 = null;
var G__11208 = (0);
var G__11209 = (0);
seq__10924 = G__11206;
chunk__10925 = G__11207;
count__10926 = G__11208;
i__10927 = G__11209;
continue;
}
} else {
return null;
}
}
break;
}
} else {
return goog.dom.removeNode(node);
}
});
shadow.dom.replace_node = (function shadow$dom$replace_node(old,new$){
return goog.dom.replaceNode(shadow.dom.dom_node(new$),shadow.dom.dom_node(old));
});
shadow.dom.text = (function shadow$dom$text(var_args){
var G__10934 = arguments.length;
switch (G__10934) {
case 2:
return shadow.dom.text.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 1:
return shadow.dom.text.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.text.cljs$core$IFn$_invoke$arity$2 = (function (el,new_text){
return (shadow.dom.dom_node(el).innerText = new_text);
}));

(shadow.dom.text.cljs$core$IFn$_invoke$arity$1 = (function (el){
return shadow.dom.dom_node(el).innerText;
}));

(shadow.dom.text.cljs$lang$maxFixedArity = 2);

shadow.dom.check = (function shadow$dom$check(var_args){
var G__10936 = arguments.length;
switch (G__10936) {
case 1:
return shadow.dom.check.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.check.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.check.cljs$core$IFn$_invoke$arity$1 = (function (el){
return shadow.dom.check.cljs$core$IFn$_invoke$arity$2(el,true);
}));

(shadow.dom.check.cljs$core$IFn$_invoke$arity$2 = (function (el,checked){
return (shadow.dom.dom_node(el).checked = checked);
}));

(shadow.dom.check.cljs$lang$maxFixedArity = 2);

shadow.dom.checked_QMARK_ = (function shadow$dom$checked_QMARK_(el){
return shadow.dom.dom_node(el).checked;
});
shadow.dom.form_elements = (function shadow$dom$form_elements(el){
return (new shadow.dom.NativeColl(shadow.dom.dom_node(el).elements));
});
shadow.dom.children = (function shadow$dom$children(el){
return (new shadow.dom.NativeColl(shadow.dom.dom_node(el).children));
});
shadow.dom.child_nodes = (function shadow$dom$child_nodes(el){
return (new shadow.dom.NativeColl(shadow.dom.dom_node(el).childNodes));
});
shadow.dom.attr = (function shadow$dom$attr(var_args){
var G__10938 = arguments.length;
switch (G__10938) {
case 2:
return shadow.dom.attr.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return shadow.dom.attr.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.attr.cljs$core$IFn$_invoke$arity$2 = (function (el,key){
return shadow.dom.dom_node(el).getAttribute(cljs.core.name(key));
}));

(shadow.dom.attr.cljs$core$IFn$_invoke$arity$3 = (function (el,key,default$){
var or__5002__auto__ = shadow.dom.dom_node(el).getAttribute(cljs.core.name(key));
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return default$;
}
}));

(shadow.dom.attr.cljs$lang$maxFixedArity = 3);

shadow.dom.del_attr = (function shadow$dom$del_attr(el,key){
return shadow.dom.dom_node(el).removeAttribute(cljs.core.name(key));
});
shadow.dom.data = (function shadow$dom$data(el,key){
return shadow.dom.dom_node(el).getAttribute(["data-",cljs.core.name(key)].join(''));
});
shadow.dom.set_data = (function shadow$dom$set_data(el,key,value){
return shadow.dom.dom_node(el).setAttribute(["data-",cljs.core.name(key)].join(''),cljs.core.str.cljs$core$IFn$_invoke$arity$1(value));
});
shadow.dom.set_html = (function shadow$dom$set_html(node,text){
return (shadow.dom.dom_node(node).innerHTML = text);
});
shadow.dom.get_html = (function shadow$dom$get_html(node){
return shadow.dom.dom_node(node).innerHTML;
});
shadow.dom.fragment = (function shadow$dom$fragment(var_args){
var args__5732__auto__ = [];
var len__5726__auto___11213 = arguments.length;
var i__5727__auto___11214 = (0);
while(true){
if((i__5727__auto___11214 < len__5726__auto___11213)){
args__5732__auto__.push((arguments[i__5727__auto___11214]));

var G__11215 = (i__5727__auto___11214 + (1));
i__5727__auto___11214 = G__11215;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((0) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((0)),(0),null)):null);
return shadow.dom.fragment.cljs$core$IFn$_invoke$arity$variadic(argseq__5733__auto__);
});

(shadow.dom.fragment.cljs$core$IFn$_invoke$arity$variadic = (function (nodes){
var fragment = document.createDocumentFragment();
var seq__10940_11216 = cljs.core.seq(nodes);
var chunk__10941_11217 = null;
var count__10942_11218 = (0);
var i__10943_11219 = (0);
while(true){
if((i__10943_11219 < count__10942_11218)){
var node_11220 = chunk__10941_11217.cljs$core$IIndexed$_nth$arity$2(null,i__10943_11219);
fragment.appendChild(shadow.dom._to_dom(node_11220));


var G__11221 = seq__10940_11216;
var G__11222 = chunk__10941_11217;
var G__11223 = count__10942_11218;
var G__11224 = (i__10943_11219 + (1));
seq__10940_11216 = G__11221;
chunk__10941_11217 = G__11222;
count__10942_11218 = G__11223;
i__10943_11219 = G__11224;
continue;
} else {
var temp__5804__auto___11225 = cljs.core.seq(seq__10940_11216);
if(temp__5804__auto___11225){
var seq__10940_11226__$1 = temp__5804__auto___11225;
if(cljs.core.chunked_seq_QMARK_(seq__10940_11226__$1)){
var c__5525__auto___11227 = cljs.core.chunk_first(seq__10940_11226__$1);
var G__11228 = cljs.core.chunk_rest(seq__10940_11226__$1);
var G__11229 = c__5525__auto___11227;
var G__11230 = cljs.core.count(c__5525__auto___11227);
var G__11231 = (0);
seq__10940_11216 = G__11228;
chunk__10941_11217 = G__11229;
count__10942_11218 = G__11230;
i__10943_11219 = G__11231;
continue;
} else {
var node_11232 = cljs.core.first(seq__10940_11226__$1);
fragment.appendChild(shadow.dom._to_dom(node_11232));


var G__11233 = cljs.core.next(seq__10940_11226__$1);
var G__11234 = null;
var G__11235 = (0);
var G__11236 = (0);
seq__10940_11216 = G__11233;
chunk__10941_11217 = G__11234;
count__10942_11218 = G__11235;
i__10943_11219 = G__11236;
continue;
}
} else {
}
}
break;
}

return (new shadow.dom.NativeColl(fragment));
}));

(shadow.dom.fragment.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(shadow.dom.fragment.cljs$lang$applyTo = (function (seq10939){
var self__5712__auto__ = this;
return self__5712__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq(seq10939));
}));

/**
 * given a html string, eval all <script> tags and return the html without the scripts
 * don't do this for everything, only content you trust.
 */
shadow.dom.eval_scripts = (function shadow$dom$eval_scripts(s){
var scripts = cljs.core.re_seq(/<script[^>]*?>(.+?)<\/script>/,s);
var seq__10944_11237 = cljs.core.seq(scripts);
var chunk__10945_11238 = null;
var count__10946_11239 = (0);
var i__10947_11240 = (0);
while(true){
if((i__10947_11240 < count__10946_11239)){
var vec__10954_11241 = chunk__10945_11238.cljs$core$IIndexed$_nth$arity$2(null,i__10947_11240);
var script_tag_11242 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10954_11241,(0),null);
var script_body_11243 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10954_11241,(1),null);
eval(script_body_11243);


var G__11244 = seq__10944_11237;
var G__11245 = chunk__10945_11238;
var G__11246 = count__10946_11239;
var G__11247 = (i__10947_11240 + (1));
seq__10944_11237 = G__11244;
chunk__10945_11238 = G__11245;
count__10946_11239 = G__11246;
i__10947_11240 = G__11247;
continue;
} else {
var temp__5804__auto___11248 = cljs.core.seq(seq__10944_11237);
if(temp__5804__auto___11248){
var seq__10944_11249__$1 = temp__5804__auto___11248;
if(cljs.core.chunked_seq_QMARK_(seq__10944_11249__$1)){
var c__5525__auto___11250 = cljs.core.chunk_first(seq__10944_11249__$1);
var G__11251 = cljs.core.chunk_rest(seq__10944_11249__$1);
var G__11252 = c__5525__auto___11250;
var G__11253 = cljs.core.count(c__5525__auto___11250);
var G__11254 = (0);
seq__10944_11237 = G__11251;
chunk__10945_11238 = G__11252;
count__10946_11239 = G__11253;
i__10947_11240 = G__11254;
continue;
} else {
var vec__10957_11255 = cljs.core.first(seq__10944_11249__$1);
var script_tag_11256 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10957_11255,(0),null);
var script_body_11257 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10957_11255,(1),null);
eval(script_body_11257);


var G__11258 = cljs.core.next(seq__10944_11249__$1);
var G__11259 = null;
var G__11260 = (0);
var G__11261 = (0);
seq__10944_11237 = G__11258;
chunk__10945_11238 = G__11259;
count__10946_11239 = G__11260;
i__10947_11240 = G__11261;
continue;
}
} else {
}
}
break;
}

return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3((function (s__$1,p__10960){
var vec__10961 = p__10960;
var script_tag = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10961,(0),null);
var script_body = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10961,(1),null);
return clojure.string.replace(s__$1,script_tag,"");
}),s,scripts);
});
shadow.dom.str__GT_fragment = (function shadow$dom$str__GT_fragment(s){
var el = document.createElement("div");
(el.innerHTML = s);

return (new shadow.dom.NativeColl(goog.dom.childrenToNode_(document,el)));
});
shadow.dom.node_name = (function shadow$dom$node_name(el){
return shadow.dom.dom_node(el).nodeName;
});
shadow.dom.ancestor_by_class = (function shadow$dom$ancestor_by_class(el,cls){
return goog.dom.getAncestorByClass(shadow.dom.dom_node(el),cls);
});
shadow.dom.ancestor_by_tag = (function shadow$dom$ancestor_by_tag(var_args){
var G__10965 = arguments.length;
switch (G__10965) {
case 2:
return shadow.dom.ancestor_by_tag.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return shadow.dom.ancestor_by_tag.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.ancestor_by_tag.cljs$core$IFn$_invoke$arity$2 = (function (el,tag){
return goog.dom.getAncestorByTagNameAndClass(shadow.dom.dom_node(el),cljs.core.name(tag));
}));

(shadow.dom.ancestor_by_tag.cljs$core$IFn$_invoke$arity$3 = (function (el,tag,cls){
return goog.dom.getAncestorByTagNameAndClass(shadow.dom.dom_node(el),cljs.core.name(tag),cljs.core.name(cls));
}));

(shadow.dom.ancestor_by_tag.cljs$lang$maxFixedArity = 3);

shadow.dom.get_value = (function shadow$dom$get_value(dom){
return goog.dom.forms.getValue(shadow.dom.dom_node(dom));
});
shadow.dom.set_value = (function shadow$dom$set_value(dom,value){
return goog.dom.forms.setValue(shadow.dom.dom_node(dom),value);
});
shadow.dom.px = (function shadow$dom$px(value){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1((value | (0))),"px"].join('');
});
shadow.dom.pct = (function shadow$dom$pct(value){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(value),"%"].join('');
});
shadow.dom.remove_style_STAR_ = (function shadow$dom$remove_style_STAR_(el,style){
return el.style.removeProperty(cljs.core.name(style));
});
shadow.dom.remove_style = (function shadow$dom$remove_style(el,style){
var el__$1 = shadow.dom.dom_node(el);
return shadow.dom.remove_style_STAR_(el__$1,style);
});
shadow.dom.remove_styles = (function shadow$dom$remove_styles(el,style_keys){
var el__$1 = shadow.dom.dom_node(el);
var seq__10966 = cljs.core.seq(style_keys);
var chunk__10967 = null;
var count__10968 = (0);
var i__10969 = (0);
while(true){
if((i__10969 < count__10968)){
var it = chunk__10967.cljs$core$IIndexed$_nth$arity$2(null,i__10969);
shadow.dom.remove_style_STAR_(el__$1,it);


var G__11263 = seq__10966;
var G__11264 = chunk__10967;
var G__11265 = count__10968;
var G__11266 = (i__10969 + (1));
seq__10966 = G__11263;
chunk__10967 = G__11264;
count__10968 = G__11265;
i__10969 = G__11266;
continue;
} else {
var temp__5804__auto__ = cljs.core.seq(seq__10966);
if(temp__5804__auto__){
var seq__10966__$1 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__10966__$1)){
var c__5525__auto__ = cljs.core.chunk_first(seq__10966__$1);
var G__11267 = cljs.core.chunk_rest(seq__10966__$1);
var G__11268 = c__5525__auto__;
var G__11269 = cljs.core.count(c__5525__auto__);
var G__11270 = (0);
seq__10966 = G__11267;
chunk__10967 = G__11268;
count__10968 = G__11269;
i__10969 = G__11270;
continue;
} else {
var it = cljs.core.first(seq__10966__$1);
shadow.dom.remove_style_STAR_(el__$1,it);


var G__11271 = cljs.core.next(seq__10966__$1);
var G__11272 = null;
var G__11273 = (0);
var G__11274 = (0);
seq__10966 = G__11271;
chunk__10967 = G__11272;
count__10968 = G__11273;
i__10969 = G__11274;
continue;
}
} else {
return null;
}
}
break;
}
});

/**
* @constructor
 * @implements {cljs.core.IRecord}
 * @implements {cljs.core.IKVReduce}
 * @implements {cljs.core.IEquiv}
 * @implements {cljs.core.IHash}
 * @implements {cljs.core.ICollection}
 * @implements {cljs.core.ICounted}
 * @implements {cljs.core.ISeqable}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.ICloneable}
 * @implements {cljs.core.IPrintWithWriter}
 * @implements {cljs.core.IIterable}
 * @implements {cljs.core.IWithMeta}
 * @implements {cljs.core.IAssociative}
 * @implements {cljs.core.IMap}
 * @implements {cljs.core.ILookup}
*/
shadow.dom.Coordinate = (function (x,y,__meta,__extmap,__hash){
this.x = x;
this.y = y;
this.__meta = __meta;
this.__extmap = __extmap;
this.__hash = __hash;
this.cljs$lang$protocol_mask$partition0$ = 2230716170;
this.cljs$lang$protocol_mask$partition1$ = 139264;
});
(shadow.dom.Coordinate.prototype.cljs$core$ILookup$_lookup$arity$2 = (function (this__5300__auto__,k__5301__auto__){
var self__ = this;
var this__5300__auto____$1 = this;
return this__5300__auto____$1.cljs$core$ILookup$_lookup$arity$3(null,k__5301__auto__,null);
}));

(shadow.dom.Coordinate.prototype.cljs$core$ILookup$_lookup$arity$3 = (function (this__5302__auto__,k10971,else__5303__auto__){
var self__ = this;
var this__5302__auto____$1 = this;
var G__10975 = k10971;
var G__10975__$1 = (((G__10975 instanceof cljs.core.Keyword))?G__10975.fqn:null);
switch (G__10975__$1) {
case "x":
return self__.x;

break;
case "y":
return self__.y;

break;
default:
return cljs.core.get.cljs$core$IFn$_invoke$arity$3(self__.__extmap,k10971,else__5303__auto__);

}
}));

(shadow.dom.Coordinate.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = (function (this__5320__auto__,f__5321__auto__,init__5322__auto__){
var self__ = this;
var this__5320__auto____$1 = this;
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3((function (ret__5323__auto__,p__10976){
var vec__10977 = p__10976;
var k__5324__auto__ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10977,(0),null);
var v__5325__auto__ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10977,(1),null);
return (f__5321__auto__.cljs$core$IFn$_invoke$arity$3 ? f__5321__auto__.cljs$core$IFn$_invoke$arity$3(ret__5323__auto__,k__5324__auto__,v__5325__auto__) : f__5321__auto__.call(null,ret__5323__auto__,k__5324__auto__,v__5325__auto__));
}),init__5322__auto__,this__5320__auto____$1);
}));

(shadow.dom.Coordinate.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = (function (this__5315__auto__,writer__5316__auto__,opts__5317__auto__){
var self__ = this;
var this__5315__auto____$1 = this;
var pr_pair__5318__auto__ = (function (keyval__5319__auto__){
return cljs.core.pr_sequential_writer(writer__5316__auto__,cljs.core.pr_writer,""," ","",opts__5317__auto__,keyval__5319__auto__);
});
return cljs.core.pr_sequential_writer(writer__5316__auto__,pr_pair__5318__auto__,"#shadow.dom.Coordinate{",", ","}",opts__5317__auto__,cljs.core.concat.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(new cljs.core.PersistentVector(null,2,(5),cljs.core.PersistentVector.EMPTY_NODE,[new cljs.core.Keyword(null,"x","x",2099068185),self__.x],null)),(new cljs.core.PersistentVector(null,2,(5),cljs.core.PersistentVector.EMPTY_NODE,[new cljs.core.Keyword(null,"y","y",-1757859776),self__.y],null))], null),self__.__extmap));
}));

(shadow.dom.Coordinate.prototype.cljs$core$IIterable$_iterator$arity$1 = (function (G__10970){
var self__ = this;
var G__10970__$1 = this;
return (new cljs.core.RecordIter((0),G__10970__$1,2,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"x","x",2099068185),new cljs.core.Keyword(null,"y","y",-1757859776)], null),(cljs.core.truth_(self__.__extmap)?cljs.core._iterator(self__.__extmap):cljs.core.nil_iter())));
}));

(shadow.dom.Coordinate.prototype.cljs$core$IMeta$_meta$arity$1 = (function (this__5298__auto__){
var self__ = this;
var this__5298__auto____$1 = this;
return self__.__meta;
}));

(shadow.dom.Coordinate.prototype.cljs$core$ICloneable$_clone$arity$1 = (function (this__5295__auto__){
var self__ = this;
var this__5295__auto____$1 = this;
return (new shadow.dom.Coordinate(self__.x,self__.y,self__.__meta,self__.__extmap,self__.__hash));
}));

(shadow.dom.Coordinate.prototype.cljs$core$ICounted$_count$arity$1 = (function (this__5304__auto__){
var self__ = this;
var this__5304__auto____$1 = this;
return (2 + cljs.core.count(self__.__extmap));
}));

(shadow.dom.Coordinate.prototype.cljs$core$IHash$_hash$arity$1 = (function (this__5296__auto__){
var self__ = this;
var this__5296__auto____$1 = this;
var h__5111__auto__ = self__.__hash;
if((!((h__5111__auto__ == null)))){
return h__5111__auto__;
} else {
var h__5111__auto____$1 = (function (coll__5297__auto__){
return (145542109 ^ cljs.core.hash_unordered_coll(coll__5297__auto__));
})(this__5296__auto____$1);
(self__.__hash = h__5111__auto____$1);

return h__5111__auto____$1;
}
}));

(shadow.dom.Coordinate.prototype.cljs$core$IEquiv$_equiv$arity$2 = (function (this10972,other10973){
var self__ = this;
var this10972__$1 = this;
return (((!((other10973 == null)))) && ((((this10972__$1.constructor === other10973.constructor)) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this10972__$1.x,other10973.x)) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this10972__$1.y,other10973.y)) && (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this10972__$1.__extmap,other10973.__extmap)))))))));
}));

(shadow.dom.Coordinate.prototype.cljs$core$IMap$_dissoc$arity$2 = (function (this__5310__auto__,k__5311__auto__){
var self__ = this;
var this__5310__auto____$1 = this;
if(cljs.core.contains_QMARK_(new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"y","y",-1757859776),null,new cljs.core.Keyword(null,"x","x",2099068185),null], null), null),k__5311__auto__)){
return cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(cljs.core._with_meta(cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,this__5310__auto____$1),self__.__meta),k__5311__auto__);
} else {
return (new shadow.dom.Coordinate(self__.x,self__.y,self__.__meta,cljs.core.not_empty(cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(self__.__extmap,k__5311__auto__)),null));
}
}));

(shadow.dom.Coordinate.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = (function (this__5307__auto__,k10971){
var self__ = this;
var this__5307__auto____$1 = this;
var G__10980 = k10971;
var G__10980__$1 = (((G__10980 instanceof cljs.core.Keyword))?G__10980.fqn:null);
switch (G__10980__$1) {
case "x":
case "y":
return true;

break;
default:
return cljs.core.contains_QMARK_(self__.__extmap,k10971);

}
}));

(shadow.dom.Coordinate.prototype.cljs$core$IAssociative$_assoc$arity$3 = (function (this__5308__auto__,k__5309__auto__,G__10970){
var self__ = this;
var this__5308__auto____$1 = this;
var pred__10981 = cljs.core.keyword_identical_QMARK_;
var expr__10982 = k__5309__auto__;
if(cljs.core.truth_((pred__10981.cljs$core$IFn$_invoke$arity$2 ? pred__10981.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"x","x",2099068185),expr__10982) : pred__10981.call(null,new cljs.core.Keyword(null,"x","x",2099068185),expr__10982)))){
return (new shadow.dom.Coordinate(G__10970,self__.y,self__.__meta,self__.__extmap,null));
} else {
if(cljs.core.truth_((pred__10981.cljs$core$IFn$_invoke$arity$2 ? pred__10981.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"y","y",-1757859776),expr__10982) : pred__10981.call(null,new cljs.core.Keyword(null,"y","y",-1757859776),expr__10982)))){
return (new shadow.dom.Coordinate(self__.x,G__10970,self__.__meta,self__.__extmap,null));
} else {
return (new shadow.dom.Coordinate(self__.x,self__.y,self__.__meta,cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(self__.__extmap,k__5309__auto__,G__10970),null));
}
}
}));

(shadow.dom.Coordinate.prototype.cljs$core$ISeqable$_seq$arity$1 = (function (this__5313__auto__){
var self__ = this;
var this__5313__auto____$1 = this;
return cljs.core.seq(cljs.core.concat.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(new cljs.core.MapEntry(new cljs.core.Keyword(null,"x","x",2099068185),self__.x,null)),(new cljs.core.MapEntry(new cljs.core.Keyword(null,"y","y",-1757859776),self__.y,null))], null),self__.__extmap));
}));

(shadow.dom.Coordinate.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (this__5299__auto__,G__10970){
var self__ = this;
var this__5299__auto____$1 = this;
return (new shadow.dom.Coordinate(self__.x,self__.y,G__10970,self__.__extmap,self__.__hash));
}));

(shadow.dom.Coordinate.prototype.cljs$core$ICollection$_conj$arity$2 = (function (this__5305__auto__,entry__5306__auto__){
var self__ = this;
var this__5305__auto____$1 = this;
if(cljs.core.vector_QMARK_(entry__5306__auto__)){
return this__5305__auto____$1.cljs$core$IAssociative$_assoc$arity$3(null,cljs.core._nth(entry__5306__auto__,(0)),cljs.core._nth(entry__5306__auto__,(1)));
} else {
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj,this__5305__auto____$1,entry__5306__auto__);
}
}));

(shadow.dom.Coordinate.getBasis = (function (){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"x","x",-555367584,null),new cljs.core.Symbol(null,"y","y",-117328249,null)], null);
}));

(shadow.dom.Coordinate.cljs$lang$type = true);

(shadow.dom.Coordinate.cljs$lang$ctorPrSeq = (function (this__5346__auto__){
return (new cljs.core.List(null,"shadow.dom/Coordinate",null,(1),null));
}));

(shadow.dom.Coordinate.cljs$lang$ctorPrWriter = (function (this__5346__auto__,writer__5347__auto__){
return cljs.core._write(writer__5347__auto__,"shadow.dom/Coordinate");
}));

/**
 * Positional factory function for shadow.dom/Coordinate.
 */
shadow.dom.__GT_Coordinate = (function shadow$dom$__GT_Coordinate(x,y){
return (new shadow.dom.Coordinate(x,y,null,null,null));
});

/**
 * Factory function for shadow.dom/Coordinate, taking a map of keywords to field values.
 */
shadow.dom.map__GT_Coordinate = (function shadow$dom$map__GT_Coordinate(G__10974){
var extmap__5342__auto__ = (function (){var G__10984 = cljs.core.dissoc.cljs$core$IFn$_invoke$arity$variadic(G__10974,new cljs.core.Keyword(null,"x","x",2099068185),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"y","y",-1757859776)], 0));
if(cljs.core.record_QMARK_(G__10974)){
return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,G__10984);
} else {
return G__10984;
}
})();
return (new shadow.dom.Coordinate(new cljs.core.Keyword(null,"x","x",2099068185).cljs$core$IFn$_invoke$arity$1(G__10974),new cljs.core.Keyword(null,"y","y",-1757859776).cljs$core$IFn$_invoke$arity$1(G__10974),null,cljs.core.not_empty(extmap__5342__auto__),null));
});

shadow.dom.get_position = (function shadow$dom$get_position(el){
var pos = goog.style.getPosition(shadow.dom.dom_node(el));
return shadow.dom.__GT_Coordinate(pos.x,pos.y);
});
shadow.dom.get_client_position = (function shadow$dom$get_client_position(el){
var pos = goog.style.getClientPosition(shadow.dom.dom_node(el));
return shadow.dom.__GT_Coordinate(pos.x,pos.y);
});
shadow.dom.get_page_offset = (function shadow$dom$get_page_offset(el){
var pos = goog.style.getPageOffset(shadow.dom.dom_node(el));
return shadow.dom.__GT_Coordinate(pos.x,pos.y);
});

/**
* @constructor
 * @implements {cljs.core.IRecord}
 * @implements {cljs.core.IKVReduce}
 * @implements {cljs.core.IEquiv}
 * @implements {cljs.core.IHash}
 * @implements {cljs.core.ICollection}
 * @implements {cljs.core.ICounted}
 * @implements {cljs.core.ISeqable}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.ICloneable}
 * @implements {cljs.core.IPrintWithWriter}
 * @implements {cljs.core.IIterable}
 * @implements {cljs.core.IWithMeta}
 * @implements {cljs.core.IAssociative}
 * @implements {cljs.core.IMap}
 * @implements {cljs.core.ILookup}
*/
shadow.dom.Size = (function (w,h,__meta,__extmap,__hash){
this.w = w;
this.h = h;
this.__meta = __meta;
this.__extmap = __extmap;
this.__hash = __hash;
this.cljs$lang$protocol_mask$partition0$ = 2230716170;
this.cljs$lang$protocol_mask$partition1$ = 139264;
});
(shadow.dom.Size.prototype.cljs$core$ILookup$_lookup$arity$2 = (function (this__5300__auto__,k__5301__auto__){
var self__ = this;
var this__5300__auto____$1 = this;
return this__5300__auto____$1.cljs$core$ILookup$_lookup$arity$3(null,k__5301__auto__,null);
}));

(shadow.dom.Size.prototype.cljs$core$ILookup$_lookup$arity$3 = (function (this__5302__auto__,k10986,else__5303__auto__){
var self__ = this;
var this__5302__auto____$1 = this;
var G__10990 = k10986;
var G__10990__$1 = (((G__10990 instanceof cljs.core.Keyword))?G__10990.fqn:null);
switch (G__10990__$1) {
case "w":
return self__.w;

break;
case "h":
return self__.h;

break;
default:
return cljs.core.get.cljs$core$IFn$_invoke$arity$3(self__.__extmap,k10986,else__5303__auto__);

}
}));

(shadow.dom.Size.prototype.cljs$core$IKVReduce$_kv_reduce$arity$3 = (function (this__5320__auto__,f__5321__auto__,init__5322__auto__){
var self__ = this;
var this__5320__auto____$1 = this;
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3((function (ret__5323__auto__,p__10991){
var vec__10992 = p__10991;
var k__5324__auto__ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10992,(0),null);
var v__5325__auto__ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__10992,(1),null);
return (f__5321__auto__.cljs$core$IFn$_invoke$arity$3 ? f__5321__auto__.cljs$core$IFn$_invoke$arity$3(ret__5323__auto__,k__5324__auto__,v__5325__auto__) : f__5321__auto__.call(null,ret__5323__auto__,k__5324__auto__,v__5325__auto__));
}),init__5322__auto__,this__5320__auto____$1);
}));

(shadow.dom.Size.prototype.cljs$core$IPrintWithWriter$_pr_writer$arity$3 = (function (this__5315__auto__,writer__5316__auto__,opts__5317__auto__){
var self__ = this;
var this__5315__auto____$1 = this;
var pr_pair__5318__auto__ = (function (keyval__5319__auto__){
return cljs.core.pr_sequential_writer(writer__5316__auto__,cljs.core.pr_writer,""," ","",opts__5317__auto__,keyval__5319__auto__);
});
return cljs.core.pr_sequential_writer(writer__5316__auto__,pr_pair__5318__auto__,"#shadow.dom.Size{",", ","}",opts__5317__auto__,cljs.core.concat.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(new cljs.core.PersistentVector(null,2,(5),cljs.core.PersistentVector.EMPTY_NODE,[new cljs.core.Keyword(null,"w","w",354169001),self__.w],null)),(new cljs.core.PersistentVector(null,2,(5),cljs.core.PersistentVector.EMPTY_NODE,[new cljs.core.Keyword(null,"h","h",1109658740),self__.h],null))], null),self__.__extmap));
}));

(shadow.dom.Size.prototype.cljs$core$IIterable$_iterator$arity$1 = (function (G__10985){
var self__ = this;
var G__10985__$1 = this;
return (new cljs.core.RecordIter((0),G__10985__$1,2,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"w","w",354169001),new cljs.core.Keyword(null,"h","h",1109658740)], null),(cljs.core.truth_(self__.__extmap)?cljs.core._iterator(self__.__extmap):cljs.core.nil_iter())));
}));

(shadow.dom.Size.prototype.cljs$core$IMeta$_meta$arity$1 = (function (this__5298__auto__){
var self__ = this;
var this__5298__auto____$1 = this;
return self__.__meta;
}));

(shadow.dom.Size.prototype.cljs$core$ICloneable$_clone$arity$1 = (function (this__5295__auto__){
var self__ = this;
var this__5295__auto____$1 = this;
return (new shadow.dom.Size(self__.w,self__.h,self__.__meta,self__.__extmap,self__.__hash));
}));

(shadow.dom.Size.prototype.cljs$core$ICounted$_count$arity$1 = (function (this__5304__auto__){
var self__ = this;
var this__5304__auto____$1 = this;
return (2 + cljs.core.count(self__.__extmap));
}));

(shadow.dom.Size.prototype.cljs$core$IHash$_hash$arity$1 = (function (this__5296__auto__){
var self__ = this;
var this__5296__auto____$1 = this;
var h__5111__auto__ = self__.__hash;
if((!((h__5111__auto__ == null)))){
return h__5111__auto__;
} else {
var h__5111__auto____$1 = (function (coll__5297__auto__){
return (-1228019642 ^ cljs.core.hash_unordered_coll(coll__5297__auto__));
})(this__5296__auto____$1);
(self__.__hash = h__5111__auto____$1);

return h__5111__auto____$1;
}
}));

(shadow.dom.Size.prototype.cljs$core$IEquiv$_equiv$arity$2 = (function (this10987,other10988){
var self__ = this;
var this10987__$1 = this;
return (((!((other10988 == null)))) && ((((this10987__$1.constructor === other10988.constructor)) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this10987__$1.w,other10988.w)) && (((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this10987__$1.h,other10988.h)) && (cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(this10987__$1.__extmap,other10988.__extmap)))))))));
}));

(shadow.dom.Size.prototype.cljs$core$IMap$_dissoc$arity$2 = (function (this__5310__auto__,k__5311__auto__){
var self__ = this;
var this__5310__auto____$1 = this;
if(cljs.core.contains_QMARK_(new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"w","w",354169001),null,new cljs.core.Keyword(null,"h","h",1109658740),null], null), null),k__5311__auto__)){
return cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(cljs.core._with_meta(cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,this__5310__auto____$1),self__.__meta),k__5311__auto__);
} else {
return (new shadow.dom.Size(self__.w,self__.h,self__.__meta,cljs.core.not_empty(cljs.core.dissoc.cljs$core$IFn$_invoke$arity$2(self__.__extmap,k__5311__auto__)),null));
}
}));

(shadow.dom.Size.prototype.cljs$core$IAssociative$_contains_key_QMARK_$arity$2 = (function (this__5307__auto__,k10986){
var self__ = this;
var this__5307__auto____$1 = this;
var G__10995 = k10986;
var G__10995__$1 = (((G__10995 instanceof cljs.core.Keyword))?G__10995.fqn:null);
switch (G__10995__$1) {
case "w":
case "h":
return true;

break;
default:
return cljs.core.contains_QMARK_(self__.__extmap,k10986);

}
}));

(shadow.dom.Size.prototype.cljs$core$IAssociative$_assoc$arity$3 = (function (this__5308__auto__,k__5309__auto__,G__10985){
var self__ = this;
var this__5308__auto____$1 = this;
var pred__10996 = cljs.core.keyword_identical_QMARK_;
var expr__10997 = k__5309__auto__;
if(cljs.core.truth_((pred__10996.cljs$core$IFn$_invoke$arity$2 ? pred__10996.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"w","w",354169001),expr__10997) : pred__10996.call(null,new cljs.core.Keyword(null,"w","w",354169001),expr__10997)))){
return (new shadow.dom.Size(G__10985,self__.h,self__.__meta,self__.__extmap,null));
} else {
if(cljs.core.truth_((pred__10996.cljs$core$IFn$_invoke$arity$2 ? pred__10996.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"h","h",1109658740),expr__10997) : pred__10996.call(null,new cljs.core.Keyword(null,"h","h",1109658740),expr__10997)))){
return (new shadow.dom.Size(self__.w,G__10985,self__.__meta,self__.__extmap,null));
} else {
return (new shadow.dom.Size(self__.w,self__.h,self__.__meta,cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(self__.__extmap,k__5309__auto__,G__10985),null));
}
}
}));

(shadow.dom.Size.prototype.cljs$core$ISeqable$_seq$arity$1 = (function (this__5313__auto__){
var self__ = this;
var this__5313__auto____$1 = this;
return cljs.core.seq(cljs.core.concat.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [(new cljs.core.MapEntry(new cljs.core.Keyword(null,"w","w",354169001),self__.w,null)),(new cljs.core.MapEntry(new cljs.core.Keyword(null,"h","h",1109658740),self__.h,null))], null),self__.__extmap));
}));

(shadow.dom.Size.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (this__5299__auto__,G__10985){
var self__ = this;
var this__5299__auto____$1 = this;
return (new shadow.dom.Size(self__.w,self__.h,G__10985,self__.__extmap,self__.__hash));
}));

(shadow.dom.Size.prototype.cljs$core$ICollection$_conj$arity$2 = (function (this__5305__auto__,entry__5306__auto__){
var self__ = this;
var this__5305__auto____$1 = this;
if(cljs.core.vector_QMARK_(entry__5306__auto__)){
return this__5305__auto____$1.cljs$core$IAssociative$_assoc$arity$3(null,cljs.core._nth(entry__5306__auto__,(0)),cljs.core._nth(entry__5306__auto__,(1)));
} else {
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._conj,this__5305__auto____$1,entry__5306__auto__);
}
}));

(shadow.dom.Size.getBasis = (function (){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"w","w",1994700528,null),new cljs.core.Symbol(null,"h","h",-1544777029,null)], null);
}));

(shadow.dom.Size.cljs$lang$type = true);

(shadow.dom.Size.cljs$lang$ctorPrSeq = (function (this__5346__auto__){
return (new cljs.core.List(null,"shadow.dom/Size",null,(1),null));
}));

(shadow.dom.Size.cljs$lang$ctorPrWriter = (function (this__5346__auto__,writer__5347__auto__){
return cljs.core._write(writer__5347__auto__,"shadow.dom/Size");
}));

/**
 * Positional factory function for shadow.dom/Size.
 */
shadow.dom.__GT_Size = (function shadow$dom$__GT_Size(w,h){
return (new shadow.dom.Size(w,h,null,null,null));
});

/**
 * Factory function for shadow.dom/Size, taking a map of keywords to field values.
 */
shadow.dom.map__GT_Size = (function shadow$dom$map__GT_Size(G__10989){
var extmap__5342__auto__ = (function (){var G__10999 = cljs.core.dissoc.cljs$core$IFn$_invoke$arity$variadic(G__10989,new cljs.core.Keyword(null,"w","w",354169001),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"h","h",1109658740)], 0));
if(cljs.core.record_QMARK_(G__10989)){
return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,G__10999);
} else {
return G__10999;
}
})();
return (new shadow.dom.Size(new cljs.core.Keyword(null,"w","w",354169001).cljs$core$IFn$_invoke$arity$1(G__10989),new cljs.core.Keyword(null,"h","h",1109658740).cljs$core$IFn$_invoke$arity$1(G__10989),null,cljs.core.not_empty(extmap__5342__auto__),null));
});

shadow.dom.size__GT_clj = (function shadow$dom$size__GT_clj(size){
return (new shadow.dom.Size(size.width,size.height,null,null,null));
});
shadow.dom.get_size = (function shadow$dom$get_size(el){
return shadow.dom.size__GT_clj(goog.style.getSize(shadow.dom.dom_node(el)));
});
shadow.dom.get_height = (function shadow$dom$get_height(el){
return shadow.dom.get_size(el).h;
});
shadow.dom.get_viewport_size = (function shadow$dom$get_viewport_size(){
return shadow.dom.size__GT_clj(goog.dom.getViewportSize());
});
shadow.dom.first_child = (function shadow$dom$first_child(el){
return (shadow.dom.dom_node(el).children[(0)]);
});
shadow.dom.select_option_values = (function shadow$dom$select_option_values(el){
var native$ = shadow.dom.dom_node(el);
var opts = (native$["options"]);
var a__5590__auto__ = opts;
var l__5591__auto__ = a__5590__auto__.length;
var i = (0);
var ret = cljs.core.PersistentVector.EMPTY;
while(true){
if((i < l__5591__auto__)){
var G__11279 = (i + (1));
var G__11280 = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(ret,(opts[i]["value"]));
i = G__11279;
ret = G__11280;
continue;
} else {
return ret;
}
break;
}
});
shadow.dom.build_url = (function shadow$dom$build_url(path,query_params){
if(cljs.core.empty_QMARK_(query_params)){
return path;
} else {
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(path),"?",clojure.string.join.cljs$core$IFn$_invoke$arity$2("&",cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p__11004){
var vec__11005 = p__11004;
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11005,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11005,(1),null);
return [cljs.core.name(k),"=",cljs.core.str.cljs$core$IFn$_invoke$arity$1(encodeURIComponent(cljs.core.str.cljs$core$IFn$_invoke$arity$1(v)))].join('');
}),query_params))].join('');
}
});
shadow.dom.redirect = (function shadow$dom$redirect(var_args){
var G__11009 = arguments.length;
switch (G__11009) {
case 1:
return shadow.dom.redirect.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return shadow.dom.redirect.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(shadow.dom.redirect.cljs$core$IFn$_invoke$arity$1 = (function (path){
return shadow.dom.redirect.cljs$core$IFn$_invoke$arity$2(path,cljs.core.PersistentArrayMap.EMPTY);
}));

(shadow.dom.redirect.cljs$core$IFn$_invoke$arity$2 = (function (path,query_params){
return (document["location"]["href"] = shadow.dom.build_url(path,query_params));
}));

(shadow.dom.redirect.cljs$lang$maxFixedArity = 2);

shadow.dom.reload_BANG_ = (function shadow$dom$reload_BANG_(){
return (document.location.href = document.location.href);
});
shadow.dom.tag_name = (function shadow$dom$tag_name(el){
var dom = shadow.dom.dom_node(el);
return dom.tagName;
});
shadow.dom.insert_after = (function shadow$dom$insert_after(ref,new$){
var new_node = shadow.dom.dom_node(new$);
goog.dom.insertSiblingAfter(new_node,shadow.dom.dom_node(ref));

return new_node;
});
shadow.dom.insert_before = (function shadow$dom$insert_before(ref,new$){
var new_node = shadow.dom.dom_node(new$);
goog.dom.insertSiblingBefore(new_node,shadow.dom.dom_node(ref));

return new_node;
});
shadow.dom.insert_first = (function shadow$dom$insert_first(ref,new$){
var temp__5802__auto__ = shadow.dom.dom_node(ref).firstChild;
if(cljs.core.truth_(temp__5802__auto__)){
var child = temp__5802__auto__;
return shadow.dom.insert_before(child,new$);
} else {
return shadow.dom.append.cljs$core$IFn$_invoke$arity$2(ref,new$);
}
});
shadow.dom.index_of = (function shadow$dom$index_of(el){
var el__$1 = shadow.dom.dom_node(el);
var i = (0);
while(true){
var ps = el__$1.previousSibling;
if((ps == null)){
return i;
} else {
var G__11282 = ps;
var G__11283 = (i + (1));
el__$1 = G__11282;
i = G__11283;
continue;
}
break;
}
});
shadow.dom.get_parent = (function shadow$dom$get_parent(el){
return goog.dom.getParentElement(shadow.dom.dom_node(el));
});
shadow.dom.parents = (function shadow$dom$parents(el){
var parent = shadow.dom.get_parent(el);
if(cljs.core.truth_(parent)){
return cljs.core.cons(parent,(new cljs.core.LazySeq(null,(function (){
return (shadow.dom.parents.cljs$core$IFn$_invoke$arity$1 ? shadow.dom.parents.cljs$core$IFn$_invoke$arity$1(parent) : shadow.dom.parents.call(null,parent));
}),null,null)));
} else {
return null;
}
});
shadow.dom.matches = (function shadow$dom$matches(el,sel){
return shadow.dom.dom_node(el).matches(sel);
});
shadow.dom.get_next_sibling = (function shadow$dom$get_next_sibling(el){
return goog.dom.getNextElementSibling(shadow.dom.dom_node(el));
});
shadow.dom.get_previous_sibling = (function shadow$dom$get_previous_sibling(el){
return goog.dom.getPreviousElementSibling(shadow.dom.dom_node(el));
});
shadow.dom.xmlns = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentArrayMap(null, 2, ["svg","http://www.w3.org/2000/svg","xlink","http://www.w3.org/1999/xlink"], null));
shadow.dom.create_svg_node = (function shadow$dom$create_svg_node(tag_def,props){
var vec__11010 = shadow.dom.parse_tag(tag_def);
var tag_name = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11010,(0),null);
var tag_id = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11010,(1),null);
var tag_classes = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11010,(2),null);
var el = document.createElementNS("http://www.w3.org/2000/svg",tag_name);
if(cljs.core.truth_(tag_id)){
el.setAttribute("id",tag_id);
} else {
}

if(cljs.core.truth_(tag_classes)){
el.setAttribute("class",shadow.dom.merge_class_string(new cljs.core.Keyword(null,"class","class",-2030961996).cljs$core$IFn$_invoke$arity$1(props),tag_classes));
} else {
}

var seq__11013_11284 = cljs.core.seq(props);
var chunk__11014_11285 = null;
var count__11015_11286 = (0);
var i__11016_11287 = (0);
while(true){
if((i__11016_11287 < count__11015_11286)){
var vec__11023_11288 = chunk__11014_11285.cljs$core$IIndexed$_nth$arity$2(null,i__11016_11287);
var k_11289 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11023_11288,(0),null);
var v_11290 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11023_11288,(1),null);
el.setAttributeNS((function (){var temp__5804__auto__ = cljs.core.namespace(k_11289);
if(cljs.core.truth_(temp__5804__auto__)){
var ns = temp__5804__auto__;
return cljs.core.get.cljs$core$IFn$_invoke$arity$2(cljs.core.deref(shadow.dom.xmlns),ns);
} else {
return null;
}
})(),cljs.core.name(k_11289),v_11290);


var G__11291 = seq__11013_11284;
var G__11292 = chunk__11014_11285;
var G__11293 = count__11015_11286;
var G__11294 = (i__11016_11287 + (1));
seq__11013_11284 = G__11291;
chunk__11014_11285 = G__11292;
count__11015_11286 = G__11293;
i__11016_11287 = G__11294;
continue;
} else {
var temp__5804__auto___11295 = cljs.core.seq(seq__11013_11284);
if(temp__5804__auto___11295){
var seq__11013_11296__$1 = temp__5804__auto___11295;
if(cljs.core.chunked_seq_QMARK_(seq__11013_11296__$1)){
var c__5525__auto___11297 = cljs.core.chunk_first(seq__11013_11296__$1);
var G__11298 = cljs.core.chunk_rest(seq__11013_11296__$1);
var G__11299 = c__5525__auto___11297;
var G__11300 = cljs.core.count(c__5525__auto___11297);
var G__11301 = (0);
seq__11013_11284 = G__11298;
chunk__11014_11285 = G__11299;
count__11015_11286 = G__11300;
i__11016_11287 = G__11301;
continue;
} else {
var vec__11026_11302 = cljs.core.first(seq__11013_11296__$1);
var k_11303 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11026_11302,(0),null);
var v_11304 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11026_11302,(1),null);
el.setAttributeNS((function (){var temp__5804__auto____$1 = cljs.core.namespace(k_11303);
if(cljs.core.truth_(temp__5804__auto____$1)){
var ns = temp__5804__auto____$1;
return cljs.core.get.cljs$core$IFn$_invoke$arity$2(cljs.core.deref(shadow.dom.xmlns),ns);
} else {
return null;
}
})(),cljs.core.name(k_11303),v_11304);


var G__11305 = cljs.core.next(seq__11013_11296__$1);
var G__11306 = null;
var G__11307 = (0);
var G__11308 = (0);
seq__11013_11284 = G__11305;
chunk__11014_11285 = G__11306;
count__11015_11286 = G__11307;
i__11016_11287 = G__11308;
continue;
}
} else {
}
}
break;
}

return el;
});
shadow.dom.svg_node = (function shadow$dom$svg_node(el){
if((el == null)){
return null;
} else {
if((((!((el == null))))?((((false) || ((cljs.core.PROTOCOL_SENTINEL === el.shadow$dom$SVGElement$))))?true:false):false)){
return el.shadow$dom$SVGElement$_to_svg$arity$1(null);
} else {
return el;

}
}
});
shadow.dom.make_svg_node = (function shadow$dom$make_svg_node(structure){
var vec__11030 = shadow.dom.destructure_node(shadow.dom.create_svg_node,structure);
var node = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11030,(0),null);
var node_children = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__11030,(1),null);
var seq__11033_11309 = cljs.core.seq(node_children);
var chunk__11035_11310 = null;
var count__11036_11311 = (0);
var i__11037_11312 = (0);
while(true){
if((i__11037_11312 < count__11036_11311)){
var child_struct_11313 = chunk__11035_11310.cljs$core$IIndexed$_nth$arity$2(null,i__11037_11312);
if((!((child_struct_11313 == null)))){
if(typeof child_struct_11313 === 'string'){
var text_11314 = (node["textContent"]);
(node["textContent"] = [cljs.core.str.cljs$core$IFn$_invoke$arity$1(text_11314),child_struct_11313].join(''));
} else {
var children_11315 = shadow.dom.svg_node(child_struct_11313);
if(cljs.core.seq_QMARK_(children_11315)){
var seq__11051_11316 = cljs.core.seq(children_11315);
var chunk__11053_11317 = null;
var count__11054_11318 = (0);
var i__11055_11319 = (0);
while(true){
if((i__11055_11319 < count__11054_11318)){
var child_11320 = chunk__11053_11317.cljs$core$IIndexed$_nth$arity$2(null,i__11055_11319);
if(cljs.core.truth_(child_11320)){
node.appendChild(child_11320);


var G__11321 = seq__11051_11316;
var G__11322 = chunk__11053_11317;
var G__11323 = count__11054_11318;
var G__11324 = (i__11055_11319 + (1));
seq__11051_11316 = G__11321;
chunk__11053_11317 = G__11322;
count__11054_11318 = G__11323;
i__11055_11319 = G__11324;
continue;
} else {
var G__11325 = seq__11051_11316;
var G__11326 = chunk__11053_11317;
var G__11327 = count__11054_11318;
var G__11328 = (i__11055_11319 + (1));
seq__11051_11316 = G__11325;
chunk__11053_11317 = G__11326;
count__11054_11318 = G__11327;
i__11055_11319 = G__11328;
continue;
}
} else {
var temp__5804__auto___11329 = cljs.core.seq(seq__11051_11316);
if(temp__5804__auto___11329){
var seq__11051_11330__$1 = temp__5804__auto___11329;
if(cljs.core.chunked_seq_QMARK_(seq__11051_11330__$1)){
var c__5525__auto___11331 = cljs.core.chunk_first(seq__11051_11330__$1);
var G__11332 = cljs.core.chunk_rest(seq__11051_11330__$1);
var G__11333 = c__5525__auto___11331;
var G__11334 = cljs.core.count(c__5525__auto___11331);
var G__11335 = (0);
seq__11051_11316 = G__11332;
chunk__11053_11317 = G__11333;
count__11054_11318 = G__11334;
i__11055_11319 = G__11335;
continue;
} else {
var child_11336 = cljs.core.first(seq__11051_11330__$1);
if(cljs.core.truth_(child_11336)){
node.appendChild(child_11336);


var G__11337 = cljs.core.next(seq__11051_11330__$1);
var G__11338 = null;
var G__11339 = (0);
var G__11340 = (0);
seq__11051_11316 = G__11337;
chunk__11053_11317 = G__11338;
count__11054_11318 = G__11339;
i__11055_11319 = G__11340;
continue;
} else {
var G__11341 = cljs.core.next(seq__11051_11330__$1);
var G__11342 = null;
var G__11343 = (0);
var G__11344 = (0);
seq__11051_11316 = G__11341;
chunk__11053_11317 = G__11342;
count__11054_11318 = G__11343;
i__11055_11319 = G__11344;
continue;
}
}
} else {
}
}
break;
}
} else {
node.appendChild(children_11315);
}
}


var G__11345 = seq__11033_11309;
var G__11346 = chunk__11035_11310;
var G__11347 = count__11036_11311;
var G__11348 = (i__11037_11312 + (1));
seq__11033_11309 = G__11345;
chunk__11035_11310 = G__11346;
count__11036_11311 = G__11347;
i__11037_11312 = G__11348;
continue;
} else {
var G__11349 = seq__11033_11309;
var G__11350 = chunk__11035_11310;
var G__11351 = count__11036_11311;
var G__11352 = (i__11037_11312 + (1));
seq__11033_11309 = G__11349;
chunk__11035_11310 = G__11350;
count__11036_11311 = G__11351;
i__11037_11312 = G__11352;
continue;
}
} else {
var temp__5804__auto___11353 = cljs.core.seq(seq__11033_11309);
if(temp__5804__auto___11353){
var seq__11033_11354__$1 = temp__5804__auto___11353;
if(cljs.core.chunked_seq_QMARK_(seq__11033_11354__$1)){
var c__5525__auto___11355 = cljs.core.chunk_first(seq__11033_11354__$1);
var G__11356 = cljs.core.chunk_rest(seq__11033_11354__$1);
var G__11357 = c__5525__auto___11355;
var G__11358 = cljs.core.count(c__5525__auto___11355);
var G__11359 = (0);
seq__11033_11309 = G__11356;
chunk__11035_11310 = G__11357;
count__11036_11311 = G__11358;
i__11037_11312 = G__11359;
continue;
} else {
var child_struct_11360 = cljs.core.first(seq__11033_11354__$1);
if((!((child_struct_11360 == null)))){
if(typeof child_struct_11360 === 'string'){
var text_11361 = (node["textContent"]);
(node["textContent"] = [cljs.core.str.cljs$core$IFn$_invoke$arity$1(text_11361),child_struct_11360].join(''));
} else {
var children_11362 = shadow.dom.svg_node(child_struct_11360);
if(cljs.core.seq_QMARK_(children_11362)){
var seq__11057_11363 = cljs.core.seq(children_11362);
var chunk__11059_11364 = null;
var count__11060_11365 = (0);
var i__11061_11366 = (0);
while(true){
if((i__11061_11366 < count__11060_11365)){
var child_11367 = chunk__11059_11364.cljs$core$IIndexed$_nth$arity$2(null,i__11061_11366);
if(cljs.core.truth_(child_11367)){
node.appendChild(child_11367);


var G__11368 = seq__11057_11363;
var G__11369 = chunk__11059_11364;
var G__11370 = count__11060_11365;
var G__11371 = (i__11061_11366 + (1));
seq__11057_11363 = G__11368;
chunk__11059_11364 = G__11369;
count__11060_11365 = G__11370;
i__11061_11366 = G__11371;
continue;
} else {
var G__11372 = seq__11057_11363;
var G__11373 = chunk__11059_11364;
var G__11374 = count__11060_11365;
var G__11375 = (i__11061_11366 + (1));
seq__11057_11363 = G__11372;
chunk__11059_11364 = G__11373;
count__11060_11365 = G__11374;
i__11061_11366 = G__11375;
continue;
}
} else {
var temp__5804__auto___11376__$1 = cljs.core.seq(seq__11057_11363);
if(temp__5804__auto___11376__$1){
var seq__11057_11377__$1 = temp__5804__auto___11376__$1;
if(cljs.core.chunked_seq_QMARK_(seq__11057_11377__$1)){
var c__5525__auto___11378 = cljs.core.chunk_first(seq__11057_11377__$1);
var G__11379 = cljs.core.chunk_rest(seq__11057_11377__$1);
var G__11380 = c__5525__auto___11378;
var G__11381 = cljs.core.count(c__5525__auto___11378);
var G__11382 = (0);
seq__11057_11363 = G__11379;
chunk__11059_11364 = G__11380;
count__11060_11365 = G__11381;
i__11061_11366 = G__11382;
continue;
} else {
var child_11383 = cljs.core.first(seq__11057_11377__$1);
if(cljs.core.truth_(child_11383)){
node.appendChild(child_11383);


var G__11384 = cljs.core.next(seq__11057_11377__$1);
var G__11385 = null;
var G__11386 = (0);
var G__11387 = (0);
seq__11057_11363 = G__11384;
chunk__11059_11364 = G__11385;
count__11060_11365 = G__11386;
i__11061_11366 = G__11387;
continue;
} else {
var G__11388 = cljs.core.next(seq__11057_11377__$1);
var G__11389 = null;
var G__11390 = (0);
var G__11391 = (0);
seq__11057_11363 = G__11388;
chunk__11059_11364 = G__11389;
count__11060_11365 = G__11390;
i__11061_11366 = G__11391;
continue;
}
}
} else {
}
}
break;
}
} else {
node.appendChild(children_11362);
}
}


var G__11392 = cljs.core.next(seq__11033_11354__$1);
var G__11393 = null;
var G__11394 = (0);
var G__11395 = (0);
seq__11033_11309 = G__11392;
chunk__11035_11310 = G__11393;
count__11036_11311 = G__11394;
i__11037_11312 = G__11395;
continue;
} else {
var G__11396 = cljs.core.next(seq__11033_11354__$1);
var G__11397 = null;
var G__11398 = (0);
var G__11399 = (0);
seq__11033_11309 = G__11396;
chunk__11035_11310 = G__11397;
count__11036_11311 = G__11398;
i__11037_11312 = G__11399;
continue;
}
}
} else {
}
}
break;
}

return node;
});
(shadow.dom.SVGElement["string"] = true);

(shadow.dom._to_svg["string"] = (function (this$){
if((this$ instanceof cljs.core.Keyword)){
return shadow.dom.make_svg_node(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [this$], null));
} else {
throw cljs.core.ex_info.cljs$core$IFn$_invoke$arity$2("strings cannot be in svgs",new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"this","this",-611633625),this$], null));
}
}));

(cljs.core.PersistentVector.prototype.shadow$dom$SVGElement$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.PersistentVector.prototype.shadow$dom$SVGElement$_to_svg$arity$1 = (function (this$){
var this$__$1 = this;
return shadow.dom.make_svg_node(this$__$1);
}));

(cljs.core.LazySeq.prototype.shadow$dom$SVGElement$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.LazySeq.prototype.shadow$dom$SVGElement$_to_svg$arity$1 = (function (this$){
var this$__$1 = this;
return cljs.core.map.cljs$core$IFn$_invoke$arity$2(shadow.dom._to_svg,this$__$1);
}));

(shadow.dom.SVGElement["null"] = true);

(shadow.dom._to_svg["null"] = (function (_){
return null;
}));
shadow.dom.svg = (function shadow$dom$svg(var_args){
var args__5732__auto__ = [];
var len__5726__auto___11400 = arguments.length;
var i__5727__auto___11401 = (0);
while(true){
if((i__5727__auto___11401 < len__5726__auto___11400)){
args__5732__auto__.push((arguments[i__5727__auto___11401]));

var G__11402 = (i__5727__auto___11401 + (1));
i__5727__auto___11401 = G__11402;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return shadow.dom.svg.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(shadow.dom.svg.cljs$core$IFn$_invoke$arity$variadic = (function (attrs,children){
return shadow.dom._to_svg(cljs.core.vec(cljs.core.concat.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"svg","svg",856789142),attrs], null),children)));
}));

(shadow.dom.svg.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(shadow.dom.svg.cljs$lang$applyTo = (function (seq11063){
var G__11064 = cljs.core.first(seq11063);
var seq11063__$1 = cljs.core.next(seq11063);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__11064,seq11063__$1);
}));


//# sourceMappingURL=shadow.dom.js.map
