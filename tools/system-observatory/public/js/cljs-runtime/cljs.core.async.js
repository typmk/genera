goog.provide('cljs.core.async');
goog.scope(function(){
  cljs.core.async.goog$module$goog$array = goog.module.get('goog.array');
});

/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Handler}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async12985 = (function (f,blockable,meta12986){
this.f = f;
this.blockable = blockable;
this.meta12986 = meta12986;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async12985.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_12987,meta12986__$1){
var self__ = this;
var _12987__$1 = this;
return (new cljs.core.async.t_cljs$core$async12985(self__.f,self__.blockable,meta12986__$1));
}));

(cljs.core.async.t_cljs$core$async12985.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_12987){
var self__ = this;
var _12987__$1 = this;
return self__.meta12986;
}));

(cljs.core.async.t_cljs$core$async12985.prototype.cljs$core$async$impl$protocols$Handler$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async12985.prototype.cljs$core$async$impl$protocols$Handler$active_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return true;
}));

(cljs.core.async.t_cljs$core$async12985.prototype.cljs$core$async$impl$protocols$Handler$blockable_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.blockable;
}));

(cljs.core.async.t_cljs$core$async12985.prototype.cljs$core$async$impl$protocols$Handler$commit$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.f;
}));

(cljs.core.async.t_cljs$core$async12985.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"f","f",43394975,null),new cljs.core.Symbol(null,"blockable","blockable",-28395259,null),new cljs.core.Symbol(null,"meta12986","meta12986",-13519050,null)], null);
}));

(cljs.core.async.t_cljs$core$async12985.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async12985.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async12985");

(cljs.core.async.t_cljs$core$async12985.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async12985");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async12985.
 */
cljs.core.async.__GT_t_cljs$core$async12985 = (function cljs$core$async$__GT_t_cljs$core$async12985(f,blockable,meta12986){
return (new cljs.core.async.t_cljs$core$async12985(f,blockable,meta12986));
});


cljs.core.async.fn_handler = (function cljs$core$async$fn_handler(var_args){
var G__12984 = arguments.length;
switch (G__12984) {
case 1:
return cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$1 = (function (f){
return cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$2(f,true);
}));

(cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$2 = (function (f,blockable){
return (new cljs.core.async.t_cljs$core$async12985(f,blockable,cljs.core.PersistentArrayMap.EMPTY));
}));

(cljs.core.async.fn_handler.cljs$lang$maxFixedArity = 2);

/**
 * Returns a fixed buffer of size n. When full, puts will block/park.
 */
cljs.core.async.buffer = (function cljs$core$async$buffer(n){
return cljs.core.async.impl.buffers.fixed_buffer(n);
});
/**
 * Returns a buffer of size n. When full, puts will complete but
 *   val will be dropped (no transfer).
 */
cljs.core.async.dropping_buffer = (function cljs$core$async$dropping_buffer(n){
return cljs.core.async.impl.buffers.dropping_buffer(n);
});
/**
 * Returns a buffer of size n. When full, puts will complete, and be
 *   buffered, but oldest elements in buffer will be dropped (not
 *   transferred).
 */
cljs.core.async.sliding_buffer = (function cljs$core$async$sliding_buffer(n){
return cljs.core.async.impl.buffers.sliding_buffer(n);
});
/**
 * Returns true if a channel created with buff will never block. That is to say,
 * puts into this buffer will never cause the buffer to be full. 
 */
cljs.core.async.unblocking_buffer_QMARK_ = (function cljs$core$async$unblocking_buffer_QMARK_(buff){
if((!((buff == null)))){
if(((false) || ((cljs.core.PROTOCOL_SENTINEL === buff.cljs$core$async$impl$protocols$UnblockingBuffer$)))){
return true;
} else {
if((!buff.cljs$lang$protocol_mask$partition$)){
return cljs.core.native_satisfies_QMARK_(cljs.core.async.impl.protocols.UnblockingBuffer,buff);
} else {
return false;
}
}
} else {
return cljs.core.native_satisfies_QMARK_(cljs.core.async.impl.protocols.UnblockingBuffer,buff);
}
});
/**
 * Creates a channel with an optional buffer, an optional transducer (like (map f),
 *   (filter p) etc or a composition thereof), and an optional exception handler.
 *   If buf-or-n is a number, will create and use a fixed buffer of that size. If a
 *   transducer is supplied a buffer must be specified. ex-handler must be a
 *   fn of one argument - if an exception occurs during transformation it will be called
 *   with the thrown value as an argument, and any non-nil return value will be placed
 *   in the channel.
 */
cljs.core.async.chan = (function cljs$core$async$chan(var_args){
var G__12990 = arguments.length;
switch (G__12990) {
case 0:
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$0();

break;
case 1:
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.chan.cljs$core$IFn$_invoke$arity$0 = (function (){
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(null);
}));

(cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1 = (function (buf_or_n){
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$3(buf_or_n,null,null);
}));

(cljs.core.async.chan.cljs$core$IFn$_invoke$arity$2 = (function (buf_or_n,xform){
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$3(buf_or_n,xform,null);
}));

(cljs.core.async.chan.cljs$core$IFn$_invoke$arity$3 = (function (buf_or_n,xform,ex_handler){
var buf_or_n__$1 = ((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(buf_or_n,(0)))?null:buf_or_n);
if(cljs.core.truth_(xform)){
if(cljs.core.truth_(buf_or_n__$1)){
} else {
throw (new Error(["Assert failed: ","buffer must be supplied when transducer is","\n","buf-or-n"].join('')));
}
} else {
}

return cljs.core.async.impl.channels.chan.cljs$core$IFn$_invoke$arity$3(((typeof buf_or_n__$1 === 'number')?cljs.core.async.buffer(buf_or_n__$1):buf_or_n__$1),xform,ex_handler);
}));

(cljs.core.async.chan.cljs$lang$maxFixedArity = 3);

/**
 * Creates a promise channel with an optional transducer, and an optional
 *   exception-handler. A promise channel can take exactly one value that consumers
 *   will receive. Once full, puts complete but val is dropped (no transfer).
 *   Consumers will block until either a value is placed in the channel or the
 *   channel is closed. See chan for the semantics of xform and ex-handler.
 */
cljs.core.async.promise_chan = (function cljs$core$async$promise_chan(var_args){
var G__12992 = arguments.length;
switch (G__12992) {
case 0:
return cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$0();

break;
case 1:
return cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$0 = (function (){
return cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$1(null);
}));

(cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$1 = (function (xform){
return cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$2(xform,null);
}));

(cljs.core.async.promise_chan.cljs$core$IFn$_invoke$arity$2 = (function (xform,ex_handler){
return cljs.core.async.chan.cljs$core$IFn$_invoke$arity$3(cljs.core.async.impl.buffers.promise_buffer(),xform,ex_handler);
}));

(cljs.core.async.promise_chan.cljs$lang$maxFixedArity = 2);

/**
 * Returns a channel that will close after msecs
 */
cljs.core.async.timeout = (function cljs$core$async$timeout(msecs){
return cljs.core.async.impl.timers.timeout(msecs);
});
/**
 * takes a val from port. Must be called inside a (go ...) block. Will
 *   return nil if closed. Will park if nothing is available.
 *   Returns true unless port is already closed
 */
cljs.core.async._LT__BANG_ = (function cljs$core$async$_LT__BANG_(port){
throw (new Error("<! used not in (go ...) block"));
});
/**
 * Asynchronously takes a val from port, passing to fn1. Will pass nil
 * if closed. If on-caller? (default true) is true, and value is
 * immediately available, will call fn1 on calling thread.
 * Returns nil.
 */
cljs.core.async.take_BANG_ = (function cljs$core$async$take_BANG_(var_args){
var G__12994 = arguments.length;
switch (G__12994) {
case 2:
return cljs.core.async.take_BANG_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.take_BANG_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.take_BANG_.cljs$core$IFn$_invoke$arity$2 = (function (port,fn1){
return cljs.core.async.take_BANG_.cljs$core$IFn$_invoke$arity$3(port,fn1,true);
}));

(cljs.core.async.take_BANG_.cljs$core$IFn$_invoke$arity$3 = (function (port,fn1,on_caller_QMARK_){
var ret = cljs.core.async.impl.protocols.take_BANG_(port,cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$1(fn1));
if(cljs.core.truth_(ret)){
var val_14564 = cljs.core.deref(ret);
if(cljs.core.truth_(on_caller_QMARK_)){
(fn1.cljs$core$IFn$_invoke$arity$1 ? fn1.cljs$core$IFn$_invoke$arity$1(val_14564) : fn1.call(null,val_14564));
} else {
cljs.core.async.impl.dispatch.run((function (){
return (fn1.cljs$core$IFn$_invoke$arity$1 ? fn1.cljs$core$IFn$_invoke$arity$1(val_14564) : fn1.call(null,val_14564));
}));
}
} else {
}

return null;
}));

(cljs.core.async.take_BANG_.cljs$lang$maxFixedArity = 3);

cljs.core.async.nop = (function cljs$core$async$nop(_){
return null;
});
cljs.core.async.fhnop = cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$1(cljs.core.async.nop);
/**
 * puts a val into port. nil values are not allowed. Must be called
 *   inside a (go ...) block. Will park if no buffer space is available.
 *   Returns true unless port is already closed.
 */
cljs.core.async._GT__BANG_ = (function cljs$core$async$_GT__BANG_(port,val){
throw (new Error(">! used not in (go ...) block"));
});
/**
 * Asynchronously puts a val into port, calling fn1 (if supplied) when
 * complete. nil values are not allowed. Will throw if closed. If
 * on-caller? (default true) is true, and the put is immediately
 * accepted, will call fn1 on calling thread.  Returns nil.
 */
cljs.core.async.put_BANG_ = (function cljs$core$async$put_BANG_(var_args){
var G__12996 = arguments.length;
switch (G__12996) {
case 2:
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
case 4:
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2 = (function (port,val){
var temp__5802__auto__ = cljs.core.async.impl.protocols.put_BANG_(port,val,cljs.core.async.fhnop);
if(cljs.core.truth_(temp__5802__auto__)){
var ret = temp__5802__auto__;
return cljs.core.deref(ret);
} else {
return true;
}
}));

(cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$3 = (function (port,val,fn1){
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$4(port,val,fn1,true);
}));

(cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$4 = (function (port,val,fn1,on_caller_QMARK_){
var temp__5802__auto__ = cljs.core.async.impl.protocols.put_BANG_(port,val,cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$1(fn1));
if(cljs.core.truth_(temp__5802__auto__)){
var retb = temp__5802__auto__;
var ret = cljs.core.deref(retb);
if(cljs.core.truth_(on_caller_QMARK_)){
(fn1.cljs$core$IFn$_invoke$arity$1 ? fn1.cljs$core$IFn$_invoke$arity$1(ret) : fn1.call(null,ret));
} else {
cljs.core.async.impl.dispatch.run((function (){
return (fn1.cljs$core$IFn$_invoke$arity$1 ? fn1.cljs$core$IFn$_invoke$arity$1(ret) : fn1.call(null,ret));
}));
}

return ret;
} else {
return true;
}
}));

(cljs.core.async.put_BANG_.cljs$lang$maxFixedArity = 4);

cljs.core.async.close_BANG_ = (function cljs$core$async$close_BANG_(port){
return cljs.core.async.impl.protocols.close_BANG_(port);
});
cljs.core.async.random_array = (function cljs$core$async$random_array(n){
var a = (new Array(n));
var n__5593__auto___14566 = n;
var x_14567 = (0);
while(true){
if((x_14567 < n__5593__auto___14566)){
(a[x_14567] = x_14567);

var G__14568 = (x_14567 + (1));
x_14567 = G__14568;
continue;
} else {
}
break;
}

cljs.core.async.goog$module$goog$array.shuffle(a);

return a;
});

/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Handler}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async12997 = (function (flag,meta12998){
this.flag = flag;
this.meta12998 = meta12998;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async12997.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_12999,meta12998__$1){
var self__ = this;
var _12999__$1 = this;
return (new cljs.core.async.t_cljs$core$async12997(self__.flag,meta12998__$1));
}));

(cljs.core.async.t_cljs$core$async12997.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_12999){
var self__ = this;
var _12999__$1 = this;
return self__.meta12998;
}));

(cljs.core.async.t_cljs$core$async12997.prototype.cljs$core$async$impl$protocols$Handler$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async12997.prototype.cljs$core$async$impl$protocols$Handler$active_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.deref(self__.flag);
}));

(cljs.core.async.t_cljs$core$async12997.prototype.cljs$core$async$impl$protocols$Handler$blockable_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return true;
}));

(cljs.core.async.t_cljs$core$async12997.prototype.cljs$core$async$impl$protocols$Handler$commit$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
cljs.core.reset_BANG_(self__.flag,null);

return true;
}));

(cljs.core.async.t_cljs$core$async12997.getBasis = (function (){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"flag","flag",-1565787888,null),new cljs.core.Symbol(null,"meta12998","meta12998",-1151579592,null)], null);
}));

(cljs.core.async.t_cljs$core$async12997.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async12997.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async12997");

(cljs.core.async.t_cljs$core$async12997.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async12997");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async12997.
 */
cljs.core.async.__GT_t_cljs$core$async12997 = (function cljs$core$async$__GT_t_cljs$core$async12997(flag,meta12998){
return (new cljs.core.async.t_cljs$core$async12997(flag,meta12998));
});


cljs.core.async.alt_flag = (function cljs$core$async$alt_flag(){
var flag = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(true);
return (new cljs.core.async.t_cljs$core$async12997(flag,cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Handler}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async13000 = (function (flag,cb,meta13001){
this.flag = flag;
this.cb = cb;
this.meta13001 = meta13001;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async13000.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13002,meta13001__$1){
var self__ = this;
var _13002__$1 = this;
return (new cljs.core.async.t_cljs$core$async13000(self__.flag,self__.cb,meta13001__$1));
}));

(cljs.core.async.t_cljs$core$async13000.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13002){
var self__ = this;
var _13002__$1 = this;
return self__.meta13001;
}));

(cljs.core.async.t_cljs$core$async13000.prototype.cljs$core$async$impl$protocols$Handler$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13000.prototype.cljs$core$async$impl$protocols$Handler$active_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.active_QMARK_(self__.flag);
}));

(cljs.core.async.t_cljs$core$async13000.prototype.cljs$core$async$impl$protocols$Handler$blockable_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return true;
}));

(cljs.core.async.t_cljs$core$async13000.prototype.cljs$core$async$impl$protocols$Handler$commit$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
cljs.core.async.impl.protocols.commit(self__.flag);

return self__.cb;
}));

(cljs.core.async.t_cljs$core$async13000.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"flag","flag",-1565787888,null),new cljs.core.Symbol(null,"cb","cb",-2064487928,null),new cljs.core.Symbol(null,"meta13001","meta13001",-1878891923,null)], null);
}));

(cljs.core.async.t_cljs$core$async13000.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async13000.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async13000");

(cljs.core.async.t_cljs$core$async13000.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async13000");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async13000.
 */
cljs.core.async.__GT_t_cljs$core$async13000 = (function cljs$core$async$__GT_t_cljs$core$async13000(flag,cb,meta13001){
return (new cljs.core.async.t_cljs$core$async13000(flag,cb,meta13001));
});


cljs.core.async.alt_handler = (function cljs$core$async$alt_handler(flag,cb){
return (new cljs.core.async.t_cljs$core$async13000(flag,cb,cljs.core.PersistentArrayMap.EMPTY));
});
/**
 * returns derefable [val port] if immediate, nil if enqueued
 */
cljs.core.async.do_alts = (function cljs$core$async$do_alts(fret,ports,opts){
if((cljs.core.count(ports) > (0))){
} else {
throw (new Error(["Assert failed: ","alts must have at least one channel operation","\n","(pos? (count ports))"].join('')));
}

var flag = cljs.core.async.alt_flag();
var n = cljs.core.count(ports);
var idxs = cljs.core.async.random_array(n);
var priority = new cljs.core.Keyword(null,"priority","priority",1431093715).cljs$core$IFn$_invoke$arity$1(opts);
var ret = (function (){var i = (0);
while(true){
if((i < n)){
var idx = (cljs.core.truth_(priority)?i:(idxs[i]));
var port = cljs.core.nth.cljs$core$IFn$_invoke$arity$2(ports,idx);
var wport = ((cljs.core.vector_QMARK_(port))?(port.cljs$core$IFn$_invoke$arity$1 ? port.cljs$core$IFn$_invoke$arity$1((0)) : port.call(null,(0))):null);
var vbox = (cljs.core.truth_(wport)?(function (){var val = (port.cljs$core$IFn$_invoke$arity$1 ? port.cljs$core$IFn$_invoke$arity$1((1)) : port.call(null,(1)));
return cljs.core.async.impl.protocols.put_BANG_(wport,val,cljs.core.async.alt_handler(flag,((function (i,val,idx,port,wport,flag,n,idxs,priority){
return (function (p1__13003_SHARP_){
var G__13005 = new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [p1__13003_SHARP_,wport], null);
return (fret.cljs$core$IFn$_invoke$arity$1 ? fret.cljs$core$IFn$_invoke$arity$1(G__13005) : fret.call(null,G__13005));
});})(i,val,idx,port,wport,flag,n,idxs,priority))
));
})():cljs.core.async.impl.protocols.take_BANG_(port,cljs.core.async.alt_handler(flag,((function (i,idx,port,wport,flag,n,idxs,priority){
return (function (p1__13004_SHARP_){
var G__13006 = new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [p1__13004_SHARP_,port], null);
return (fret.cljs$core$IFn$_invoke$arity$1 ? fret.cljs$core$IFn$_invoke$arity$1(G__13006) : fret.call(null,G__13006));
});})(i,idx,port,wport,flag,n,idxs,priority))
)));
if(cljs.core.truth_(vbox)){
return cljs.core.async.impl.channels.box(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.deref(vbox),(function (){var or__5002__auto__ = wport;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return port;
}
})()], null));
} else {
var G__14569 = (i + (1));
i = G__14569;
continue;
}
} else {
return null;
}
break;
}
})();
var or__5002__auto__ = ret;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
if(cljs.core.contains_QMARK_(opts,new cljs.core.Keyword(null,"default","default",-1987822328))){
var temp__5804__auto__ = (function (){var and__5000__auto__ = flag.cljs$core$async$impl$protocols$Handler$active_QMARK_$arity$1(null);
if(cljs.core.truth_(and__5000__auto__)){
return flag.cljs$core$async$impl$protocols$Handler$commit$arity$1(null);
} else {
return and__5000__auto__;
}
})();
if(cljs.core.truth_(temp__5804__auto__)){
var got = temp__5804__auto__;
return cljs.core.async.impl.channels.box(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"default","default",-1987822328).cljs$core$IFn$_invoke$arity$1(opts),new cljs.core.Keyword(null,"default","default",-1987822328)], null));
} else {
return null;
}
} else {
return null;
}
}
});
/**
 * Completes at most one of several channel operations. Must be called
 * inside a (go ...) block. ports is a vector of channel endpoints,
 * which can be either a channel to take from or a vector of
 *   [channel-to-put-to val-to-put], in any combination. Takes will be
 *   made as if by <!, and puts will be made as if by >!. Unless
 *   the :priority option is true, if more than one port operation is
 *   ready a non-deterministic choice will be made. If no operation is
 *   ready and a :default value is supplied, [default-val :default] will
 *   be returned, otherwise alts! will park until the first operation to
 *   become ready completes. Returns [val port] of the completed
 *   operation, where val is the value taken for takes, and a
 *   boolean (true unless already closed, as per put!) for puts.
 * 
 *   opts are passed as :key val ... Supported options:
 * 
 *   :default val - the value to use if none of the operations are immediately ready
 *   :priority true - (default nil) when true, the operations will be tried in order.
 * 
 *   Note: there is no guarantee that the port exps or val exprs will be
 *   used, nor in what order should they be, so they should not be
 *   depended upon for side effects.
 */
cljs.core.async.alts_BANG_ = (function cljs$core$async$alts_BANG_(var_args){
var args__5732__auto__ = [];
var len__5726__auto___14570 = arguments.length;
var i__5727__auto___14571 = (0);
while(true){
if((i__5727__auto___14571 < len__5726__auto___14570)){
args__5732__auto__.push((arguments[i__5727__auto___14571]));

var G__14572 = (i__5727__auto___14571 + (1));
i__5727__auto___14571 = G__14572;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((1) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((1)),(0),null)):null);
return cljs.core.async.alts_BANG_.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__5733__auto__);
});

(cljs.core.async.alts_BANG_.cljs$core$IFn$_invoke$arity$variadic = (function (ports,p__13009){
var map__13010 = p__13009;
var map__13010__$1 = cljs.core.__destructure_map(map__13010);
var opts = map__13010__$1;
throw (new Error("alts! used not in (go ...) block"));
}));

(cljs.core.async.alts_BANG_.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(cljs.core.async.alts_BANG_.cljs$lang$applyTo = (function (seq13007){
var G__13008 = cljs.core.first(seq13007);
var seq13007__$1 = cljs.core.next(seq13007);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__13008,seq13007__$1);
}));

/**
 * Puts a val into port if it's possible to do so immediately.
 *   nil values are not allowed. Never blocks. Returns true if offer succeeds.
 */
cljs.core.async.offer_BANG_ = (function cljs$core$async$offer_BANG_(port,val){
var ret = cljs.core.async.impl.protocols.put_BANG_(port,val,cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$2(cljs.core.async.nop,false));
if(cljs.core.truth_(ret)){
return cljs.core.deref(ret);
} else {
return null;
}
});
/**
 * Takes a val from port if it's possible to do so immediately.
 *   Never blocks. Returns value if successful, nil otherwise.
 */
cljs.core.async.poll_BANG_ = (function cljs$core$async$poll_BANG_(port){
var ret = cljs.core.async.impl.protocols.take_BANG_(port,cljs.core.async.fn_handler.cljs$core$IFn$_invoke$arity$2(cljs.core.async.nop,false));
if(cljs.core.truth_(ret)){
return cljs.core.deref(ret);
} else {
return null;
}
});
/**
 * Takes elements from the from channel and supplies them to the to
 * channel. By default, the to channel will be closed when the from
 * channel closes, but can be determined by the close?  parameter. Will
 * stop consuming the from channel if the to channel closes
 */
cljs.core.async.pipe = (function cljs$core$async$pipe(var_args){
var G__13012 = arguments.length;
switch (G__13012) {
case 2:
return cljs.core.async.pipe.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.pipe.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.pipe.cljs$core$IFn$_invoke$arity$2 = (function (from,to){
return cljs.core.async.pipe.cljs$core$IFn$_invoke$arity$3(from,to,true);
}));

(cljs.core.async.pipe.cljs$core$IFn$_invoke$arity$3 = (function (from,to,close_QMARK_){
var c__12922__auto___14574 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13036){
var state_val_13037 = (state_13036[(1)]);
if((state_val_13037 === (7))){
var inst_13032 = (state_13036[(2)]);
var state_13036__$1 = state_13036;
var statearr_13038_14575 = state_13036__$1;
(statearr_13038_14575[(2)] = inst_13032);

(statearr_13038_14575[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (1))){
var state_13036__$1 = state_13036;
var statearr_13039_14576 = state_13036__$1;
(statearr_13039_14576[(2)] = null);

(statearr_13039_14576[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (4))){
var inst_13015 = (state_13036[(7)]);
var inst_13015__$1 = (state_13036[(2)]);
var inst_13016 = (inst_13015__$1 == null);
var state_13036__$1 = (function (){var statearr_13040 = state_13036;
(statearr_13040[(7)] = inst_13015__$1);

return statearr_13040;
})();
if(cljs.core.truth_(inst_13016)){
var statearr_13041_14577 = state_13036__$1;
(statearr_13041_14577[(1)] = (5));

} else {
var statearr_13042_14578 = state_13036__$1;
(statearr_13042_14578[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (13))){
var state_13036__$1 = state_13036;
var statearr_13043_14579 = state_13036__$1;
(statearr_13043_14579[(2)] = null);

(statearr_13043_14579[(1)] = (14));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (6))){
var inst_13015 = (state_13036[(7)]);
var state_13036__$1 = state_13036;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13036__$1,(11),to,inst_13015);
} else {
if((state_val_13037 === (3))){
var inst_13034 = (state_13036[(2)]);
var state_13036__$1 = state_13036;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13036__$1,inst_13034);
} else {
if((state_val_13037 === (12))){
var state_13036__$1 = state_13036;
var statearr_13044_14580 = state_13036__$1;
(statearr_13044_14580[(2)] = null);

(statearr_13044_14580[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (2))){
var state_13036__$1 = state_13036;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13036__$1,(4),from);
} else {
if((state_val_13037 === (11))){
var inst_13025 = (state_13036[(2)]);
var state_13036__$1 = state_13036;
if(cljs.core.truth_(inst_13025)){
var statearr_13045_14581 = state_13036__$1;
(statearr_13045_14581[(1)] = (12));

} else {
var statearr_13046_14582 = state_13036__$1;
(statearr_13046_14582[(1)] = (13));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (9))){
var state_13036__$1 = state_13036;
var statearr_13047_14583 = state_13036__$1;
(statearr_13047_14583[(2)] = null);

(statearr_13047_14583[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (5))){
var state_13036__$1 = state_13036;
if(cljs.core.truth_(close_QMARK_)){
var statearr_13048_14585 = state_13036__$1;
(statearr_13048_14585[(1)] = (8));

} else {
var statearr_13049_14586 = state_13036__$1;
(statearr_13049_14586[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (14))){
var inst_13030 = (state_13036[(2)]);
var state_13036__$1 = state_13036;
var statearr_13050_14587 = state_13036__$1;
(statearr_13050_14587[(2)] = inst_13030);

(statearr_13050_14587[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (10))){
var inst_13022 = (state_13036[(2)]);
var state_13036__$1 = state_13036;
var statearr_13051_14588 = state_13036__$1;
(statearr_13051_14588[(2)] = inst_13022);

(statearr_13051_14588[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13037 === (8))){
var inst_13019 = cljs.core.async.close_BANG_(to);
var state_13036__$1 = state_13036;
var statearr_13052_14589 = state_13036__$1;
(statearr_13052_14589[(2)] = inst_13019);

(statearr_13052_14589[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_13053 = [null,null,null,null,null,null,null,null];
(statearr_13053[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_13053[(1)] = (1));

return statearr_13053;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_13036){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13036);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13054){var ex__12826__auto__ = e13054;
var statearr_13055_14591 = state_13036;
(statearr_13055_14591[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13036[(4)]))){
var statearr_13056_14592 = state_13036;
(statearr_13056_14592[(1)] = cljs.core.first((state_13036[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14593 = state_13036;
state_13036 = G__14593;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_13036){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_13036);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13057 = f__12923__auto__();
(statearr_13057[(6)] = c__12922__auto___14574);

return statearr_13057;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return to;
}));

(cljs.core.async.pipe.cljs$lang$maxFixedArity = 3);

cljs.core.async.pipeline_STAR_ = (function cljs$core$async$pipeline_STAR_(n,to,xf,from,close_QMARK_,ex_handler,type){
if((n > (0))){
} else {
throw (new Error("Assert failed: (pos? n)"));
}

var jobs = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(n);
var results = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(n);
var process__$1 = (function (p__13058){
var vec__13059 = p__13058;
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13059,(0),null);
var p = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13059,(1),null);
var job = vec__13059;
if((job == null)){
cljs.core.async.close_BANG_(results);

return null;
} else {
var res = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$3((1),xf,ex_handler);
var c__12922__auto___14594 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13066){
var state_val_13067 = (state_13066[(1)]);
if((state_val_13067 === (1))){
var state_13066__$1 = state_13066;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13066__$1,(2),res,v);
} else {
if((state_val_13067 === (2))){
var inst_13063 = (state_13066[(2)]);
var inst_13064 = cljs.core.async.close_BANG_(res);
var state_13066__$1 = (function (){var statearr_13068 = state_13066;
(statearr_13068[(7)] = inst_13063);

return statearr_13068;
})();
return cljs.core.async.impl.ioc_helpers.return_chan(state_13066__$1,inst_13064);
} else {
return null;
}
}
});
return (function() {
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = null;
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0 = (function (){
var statearr_13069 = [null,null,null,null,null,null,null,null];
(statearr_13069[(0)] = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__);

(statearr_13069[(1)] = (1));

return statearr_13069;
});
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1 = (function (state_13066){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13066);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13070){var ex__12826__auto__ = e13070;
var statearr_13071_14595 = state_13066;
(statearr_13071_14595[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13066[(4)]))){
var statearr_13072_14596 = state_13066;
(statearr_13072_14596[(1)] = cljs.core.first((state_13066[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14598 = state_13066;
state_13066 = G__14598;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = function(state_13066){
switch(arguments.length){
case 0:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1.call(this,state_13066);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0;
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1;
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13073 = f__12923__auto__();
(statearr_13073[(6)] = c__12922__auto___14594);

return statearr_13073;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2(p,res);

return true;
}
});
var async = (function (p__13074){
var vec__13075 = p__13074;
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13075,(0),null);
var p = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__13075,(1),null);
var job = vec__13075;
if((job == null)){
cljs.core.async.close_BANG_(results);

return null;
} else {
var res = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
(xf.cljs$core$IFn$_invoke$arity$2 ? xf.cljs$core$IFn$_invoke$arity$2(v,res) : xf.call(null,v,res));

cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2(p,res);

return true;
}
});
var n__5593__auto___14600 = n;
var __14601 = (0);
while(true){
if((__14601 < n__5593__auto___14600)){
var G__13078_14602 = type;
var G__13078_14603__$1 = (((G__13078_14602 instanceof cljs.core.Keyword))?G__13078_14602.fqn:null);
switch (G__13078_14603__$1) {
case "compute":
var c__12922__auto___14605 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run(((function (__14601,c__12922__auto___14605,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async){
return (function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = ((function (__14601,c__12922__auto___14605,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async){
return (function (state_13091){
var state_val_13092 = (state_13091[(1)]);
if((state_val_13092 === (1))){
var state_13091__$1 = state_13091;
var statearr_13093_14606 = state_13091__$1;
(statearr_13093_14606[(2)] = null);

(statearr_13093_14606[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13092 === (2))){
var state_13091__$1 = state_13091;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13091__$1,(4),jobs);
} else {
if((state_val_13092 === (3))){
var inst_13089 = (state_13091[(2)]);
var state_13091__$1 = state_13091;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13091__$1,inst_13089);
} else {
if((state_val_13092 === (4))){
var inst_13081 = (state_13091[(2)]);
var inst_13082 = process__$1(inst_13081);
var state_13091__$1 = state_13091;
if(cljs.core.truth_(inst_13082)){
var statearr_13094_14607 = state_13091__$1;
(statearr_13094_14607[(1)] = (5));

} else {
var statearr_13095_14608 = state_13091__$1;
(statearr_13095_14608[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13092 === (5))){
var state_13091__$1 = state_13091;
var statearr_13096_14610 = state_13091__$1;
(statearr_13096_14610[(2)] = null);

(statearr_13096_14610[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13092 === (6))){
var state_13091__$1 = state_13091;
var statearr_13097_14611 = state_13091__$1;
(statearr_13097_14611[(2)] = null);

(statearr_13097_14611[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13092 === (7))){
var inst_13087 = (state_13091[(2)]);
var state_13091__$1 = state_13091;
var statearr_13098_14612 = state_13091__$1;
(statearr_13098_14612[(2)] = inst_13087);

(statearr_13098_14612[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
});})(__14601,c__12922__auto___14605,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async))
;
return ((function (__14601,switch__12822__auto__,c__12922__auto___14605,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async){
return (function() {
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = null;
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0 = (function (){
var statearr_13099 = [null,null,null,null,null,null,null];
(statearr_13099[(0)] = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__);

(statearr_13099[(1)] = (1));

return statearr_13099;
});
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1 = (function (state_13091){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13091);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13100){var ex__12826__auto__ = e13100;
var statearr_13101_14613 = state_13091;
(statearr_13101_14613[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13091[(4)]))){
var statearr_13102_14614 = state_13091;
(statearr_13102_14614[(1)] = cljs.core.first((state_13091[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14615 = state_13091;
state_13091 = G__14615;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = function(state_13091){
switch(arguments.length){
case 0:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1.call(this,state_13091);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0;
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1;
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__;
})()
;})(__14601,switch__12822__auto__,c__12922__auto___14605,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async))
})();
var state__12924__auto__ = (function (){var statearr_13103 = f__12923__auto__();
(statearr_13103[(6)] = c__12922__auto___14605);

return statearr_13103;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
});})(__14601,c__12922__auto___14605,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async))
);


break;
case "async":
var c__12922__auto___14616 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run(((function (__14601,c__12922__auto___14616,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async){
return (function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = ((function (__14601,c__12922__auto___14616,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async){
return (function (state_13116){
var state_val_13117 = (state_13116[(1)]);
if((state_val_13117 === (1))){
var state_13116__$1 = state_13116;
var statearr_13118_14617 = state_13116__$1;
(statearr_13118_14617[(2)] = null);

(statearr_13118_14617[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13117 === (2))){
var state_13116__$1 = state_13116;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13116__$1,(4),jobs);
} else {
if((state_val_13117 === (3))){
var inst_13114 = (state_13116[(2)]);
var state_13116__$1 = state_13116;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13116__$1,inst_13114);
} else {
if((state_val_13117 === (4))){
var inst_13106 = (state_13116[(2)]);
var inst_13107 = async(inst_13106);
var state_13116__$1 = state_13116;
if(cljs.core.truth_(inst_13107)){
var statearr_13119_14618 = state_13116__$1;
(statearr_13119_14618[(1)] = (5));

} else {
var statearr_13120_14619 = state_13116__$1;
(statearr_13120_14619[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13117 === (5))){
var state_13116__$1 = state_13116;
var statearr_13121_14620 = state_13116__$1;
(statearr_13121_14620[(2)] = null);

(statearr_13121_14620[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13117 === (6))){
var state_13116__$1 = state_13116;
var statearr_13122_14621 = state_13116__$1;
(statearr_13122_14621[(2)] = null);

(statearr_13122_14621[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13117 === (7))){
var inst_13112 = (state_13116[(2)]);
var state_13116__$1 = state_13116;
var statearr_13123_14622 = state_13116__$1;
(statearr_13123_14622[(2)] = inst_13112);

(statearr_13123_14622[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
});})(__14601,c__12922__auto___14616,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async))
;
return ((function (__14601,switch__12822__auto__,c__12922__auto___14616,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async){
return (function() {
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = null;
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0 = (function (){
var statearr_13124 = [null,null,null,null,null,null,null];
(statearr_13124[(0)] = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__);

(statearr_13124[(1)] = (1));

return statearr_13124;
});
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1 = (function (state_13116){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13116);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13125){var ex__12826__auto__ = e13125;
var statearr_13126_14623 = state_13116;
(statearr_13126_14623[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13116[(4)]))){
var statearr_13127_14624 = state_13116;
(statearr_13127_14624[(1)] = cljs.core.first((state_13116[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14625 = state_13116;
state_13116 = G__14625;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = function(state_13116){
switch(arguments.length){
case 0:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1.call(this,state_13116);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0;
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1;
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__;
})()
;})(__14601,switch__12822__auto__,c__12922__auto___14616,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async))
})();
var state__12924__auto__ = (function (){var statearr_13128 = f__12923__auto__();
(statearr_13128[(6)] = c__12922__auto___14616);

return statearr_13128;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
});})(__14601,c__12922__auto___14616,G__13078_14602,G__13078_14603__$1,n__5593__auto___14600,jobs,results,process__$1,async))
);


break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__13078_14603__$1)].join('')));

}

var G__14626 = (__14601 + (1));
__14601 = G__14626;
continue;
} else {
}
break;
}

var c__12922__auto___14627 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13150){
var state_val_13151 = (state_13150[(1)]);
if((state_val_13151 === (7))){
var inst_13146 = (state_13150[(2)]);
var state_13150__$1 = state_13150;
var statearr_13152_14628 = state_13150__$1;
(statearr_13152_14628[(2)] = inst_13146);

(statearr_13152_14628[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13151 === (1))){
var state_13150__$1 = state_13150;
var statearr_13153_14629 = state_13150__$1;
(statearr_13153_14629[(2)] = null);

(statearr_13153_14629[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13151 === (4))){
var inst_13131 = (state_13150[(7)]);
var inst_13131__$1 = (state_13150[(2)]);
var inst_13132 = (inst_13131__$1 == null);
var state_13150__$1 = (function (){var statearr_13154 = state_13150;
(statearr_13154[(7)] = inst_13131__$1);

return statearr_13154;
})();
if(cljs.core.truth_(inst_13132)){
var statearr_13155_14630 = state_13150__$1;
(statearr_13155_14630[(1)] = (5));

} else {
var statearr_13156_14631 = state_13150__$1;
(statearr_13156_14631[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13151 === (6))){
var inst_13131 = (state_13150[(7)]);
var inst_13136 = (state_13150[(8)]);
var inst_13136__$1 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
var inst_13137 = cljs.core.PersistentVector.EMPTY_NODE;
var inst_13138 = [inst_13131,inst_13136__$1];
var inst_13139 = (new cljs.core.PersistentVector(null,2,(5),inst_13137,inst_13138,null));
var state_13150__$1 = (function (){var statearr_13157 = state_13150;
(statearr_13157[(8)] = inst_13136__$1);

return statearr_13157;
})();
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13150__$1,(8),jobs,inst_13139);
} else {
if((state_val_13151 === (3))){
var inst_13148 = (state_13150[(2)]);
var state_13150__$1 = state_13150;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13150__$1,inst_13148);
} else {
if((state_val_13151 === (2))){
var state_13150__$1 = state_13150;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13150__$1,(4),from);
} else {
if((state_val_13151 === (9))){
var inst_13143 = (state_13150[(2)]);
var state_13150__$1 = (function (){var statearr_13158 = state_13150;
(statearr_13158[(9)] = inst_13143);

return statearr_13158;
})();
var statearr_13159_14632 = state_13150__$1;
(statearr_13159_14632[(2)] = null);

(statearr_13159_14632[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13151 === (5))){
var inst_13134 = cljs.core.async.close_BANG_(jobs);
var state_13150__$1 = state_13150;
var statearr_13160_14633 = state_13150__$1;
(statearr_13160_14633[(2)] = inst_13134);

(statearr_13160_14633[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13151 === (8))){
var inst_13136 = (state_13150[(8)]);
var inst_13141 = (state_13150[(2)]);
var state_13150__$1 = (function (){var statearr_13161 = state_13150;
(statearr_13161[(10)] = inst_13141);

return statearr_13161;
})();
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13150__$1,(9),results,inst_13136);
} else {
return null;
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = null;
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0 = (function (){
var statearr_13162 = [null,null,null,null,null,null,null,null,null,null,null];
(statearr_13162[(0)] = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__);

(statearr_13162[(1)] = (1));

return statearr_13162;
});
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1 = (function (state_13150){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13150);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13163){var ex__12826__auto__ = e13163;
var statearr_13164_14634 = state_13150;
(statearr_13164_14634[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13150[(4)]))){
var statearr_13165_14635 = state_13150;
(statearr_13165_14635[(1)] = cljs.core.first((state_13150[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14636 = state_13150;
state_13150 = G__14636;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = function(state_13150){
switch(arguments.length){
case 0:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1.call(this,state_13150);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0;
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1;
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13166 = f__12923__auto__();
(statearr_13166[(6)] = c__12922__auto___14627);

return statearr_13166;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


var c__12922__auto__ = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13204){
var state_val_13205 = (state_13204[(1)]);
if((state_val_13205 === (7))){
var inst_13200 = (state_13204[(2)]);
var state_13204__$1 = state_13204;
var statearr_13206_14637 = state_13204__$1;
(statearr_13206_14637[(2)] = inst_13200);

(statearr_13206_14637[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (20))){
var state_13204__$1 = state_13204;
var statearr_13207_14638 = state_13204__$1;
(statearr_13207_14638[(2)] = null);

(statearr_13207_14638[(1)] = (21));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (1))){
var state_13204__$1 = state_13204;
var statearr_13208_14639 = state_13204__$1;
(statearr_13208_14639[(2)] = null);

(statearr_13208_14639[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (4))){
var inst_13169 = (state_13204[(7)]);
var inst_13169__$1 = (state_13204[(2)]);
var inst_13170 = (inst_13169__$1 == null);
var state_13204__$1 = (function (){var statearr_13209 = state_13204;
(statearr_13209[(7)] = inst_13169__$1);

return statearr_13209;
})();
if(cljs.core.truth_(inst_13170)){
var statearr_13210_14640 = state_13204__$1;
(statearr_13210_14640[(1)] = (5));

} else {
var statearr_13211_14641 = state_13204__$1;
(statearr_13211_14641[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (15))){
var inst_13182 = (state_13204[(8)]);
var state_13204__$1 = state_13204;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13204__$1,(18),to,inst_13182);
} else {
if((state_val_13205 === (21))){
var inst_13195 = (state_13204[(2)]);
var state_13204__$1 = state_13204;
var statearr_13212_14642 = state_13204__$1;
(statearr_13212_14642[(2)] = inst_13195);

(statearr_13212_14642[(1)] = (13));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (13))){
var inst_13197 = (state_13204[(2)]);
var state_13204__$1 = (function (){var statearr_13213 = state_13204;
(statearr_13213[(9)] = inst_13197);

return statearr_13213;
})();
var statearr_13214_14643 = state_13204__$1;
(statearr_13214_14643[(2)] = null);

(statearr_13214_14643[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (6))){
var inst_13169 = (state_13204[(7)]);
var state_13204__$1 = state_13204;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13204__$1,(11),inst_13169);
} else {
if((state_val_13205 === (17))){
var inst_13190 = (state_13204[(2)]);
var state_13204__$1 = state_13204;
if(cljs.core.truth_(inst_13190)){
var statearr_13215_14644 = state_13204__$1;
(statearr_13215_14644[(1)] = (19));

} else {
var statearr_13216_14645 = state_13204__$1;
(statearr_13216_14645[(1)] = (20));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (3))){
var inst_13202 = (state_13204[(2)]);
var state_13204__$1 = state_13204;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13204__$1,inst_13202);
} else {
if((state_val_13205 === (12))){
var inst_13179 = (state_13204[(10)]);
var state_13204__$1 = state_13204;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13204__$1,(14),inst_13179);
} else {
if((state_val_13205 === (2))){
var state_13204__$1 = state_13204;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13204__$1,(4),results);
} else {
if((state_val_13205 === (19))){
var state_13204__$1 = state_13204;
var statearr_13217_14646 = state_13204__$1;
(statearr_13217_14646[(2)] = null);

(statearr_13217_14646[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (11))){
var inst_13179 = (state_13204[(2)]);
var state_13204__$1 = (function (){var statearr_13218 = state_13204;
(statearr_13218[(10)] = inst_13179);

return statearr_13218;
})();
var statearr_13219_14647 = state_13204__$1;
(statearr_13219_14647[(2)] = null);

(statearr_13219_14647[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (9))){
var state_13204__$1 = state_13204;
var statearr_13220_14648 = state_13204__$1;
(statearr_13220_14648[(2)] = null);

(statearr_13220_14648[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (5))){
var state_13204__$1 = state_13204;
if(cljs.core.truth_(close_QMARK_)){
var statearr_13221_14649 = state_13204__$1;
(statearr_13221_14649[(1)] = (8));

} else {
var statearr_13222_14650 = state_13204__$1;
(statearr_13222_14650[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (14))){
var inst_13184 = (state_13204[(11)]);
var inst_13182 = (state_13204[(8)]);
var inst_13182__$1 = (state_13204[(2)]);
var inst_13183 = (inst_13182__$1 == null);
var inst_13184__$1 = cljs.core.not(inst_13183);
var state_13204__$1 = (function (){var statearr_13223 = state_13204;
(statearr_13223[(11)] = inst_13184__$1);

(statearr_13223[(8)] = inst_13182__$1);

return statearr_13223;
})();
if(inst_13184__$1){
var statearr_13224_14652 = state_13204__$1;
(statearr_13224_14652[(1)] = (15));

} else {
var statearr_13225_14653 = state_13204__$1;
(statearr_13225_14653[(1)] = (16));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (16))){
var inst_13184 = (state_13204[(11)]);
var state_13204__$1 = state_13204;
var statearr_13226_14654 = state_13204__$1;
(statearr_13226_14654[(2)] = inst_13184);

(statearr_13226_14654[(1)] = (17));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (10))){
var inst_13176 = (state_13204[(2)]);
var state_13204__$1 = state_13204;
var statearr_13227_14655 = state_13204__$1;
(statearr_13227_14655[(2)] = inst_13176);

(statearr_13227_14655[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (18))){
var inst_13187 = (state_13204[(2)]);
var state_13204__$1 = state_13204;
var statearr_13228_14656 = state_13204__$1;
(statearr_13228_14656[(2)] = inst_13187);

(statearr_13228_14656[(1)] = (17));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13205 === (8))){
var inst_13173 = cljs.core.async.close_BANG_(to);
var state_13204__$1 = state_13204;
var statearr_13229_14657 = state_13204__$1;
(statearr_13229_14657[(2)] = inst_13173);

(statearr_13229_14657[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = null;
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0 = (function (){
var statearr_13230 = [null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_13230[(0)] = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__);

(statearr_13230[(1)] = (1));

return statearr_13230;
});
var cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1 = (function (state_13204){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13204);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13231){var ex__12826__auto__ = e13231;
var statearr_13232_14658 = state_13204;
(statearr_13232_14658[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13204[(4)]))){
var statearr_13233_14659 = state_13204;
(statearr_13233_14659[(1)] = cljs.core.first((state_13204[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14660 = state_13204;
state_13204 = G__14660;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__ = function(state_13204){
switch(arguments.length){
case 0:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1.call(this,state_13204);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____0;
cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$pipeline_STAR__$_state_machine__12823__auto____1;
return cljs$core$async$pipeline_STAR__$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13234 = f__12923__auto__();
(statearr_13234[(6)] = c__12922__auto__);

return statearr_13234;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));

return c__12922__auto__;
});
/**
 * Takes elements from the from channel and supplies them to the to
 *   channel, subject to the async function af, with parallelism n. af
 *   must be a function of two arguments, the first an input value and
 *   the second a channel on which to place the result(s). The
 *   presumption is that af will return immediately, having launched some
 *   asynchronous operation whose completion/callback will put results on
 *   the channel, then close! it. Outputs will be returned in order
 *   relative to the inputs. By default, the to channel will be closed
 *   when the from channel closes, but can be determined by the close?
 *   parameter. Will stop consuming the from channel if the to channel
 *   closes. See also pipeline, pipeline-blocking.
 */
cljs.core.async.pipeline_async = (function cljs$core$async$pipeline_async(var_args){
var G__13236 = arguments.length;
switch (G__13236) {
case 4:
return cljs.core.async.pipeline_async.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
case 5:
return cljs.core.async.pipeline_async.cljs$core$IFn$_invoke$arity$5((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]),(arguments[(4)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.pipeline_async.cljs$core$IFn$_invoke$arity$4 = (function (n,to,af,from){
return cljs.core.async.pipeline_async.cljs$core$IFn$_invoke$arity$5(n,to,af,from,true);
}));

(cljs.core.async.pipeline_async.cljs$core$IFn$_invoke$arity$5 = (function (n,to,af,from,close_QMARK_){
return cljs.core.async.pipeline_STAR_(n,to,af,from,close_QMARK_,null,new cljs.core.Keyword(null,"async","async",1050769601));
}));

(cljs.core.async.pipeline_async.cljs$lang$maxFixedArity = 5);

/**
 * Takes elements from the from channel and supplies them to the to
 *   channel, subject to the transducer xf, with parallelism n. Because
 *   it is parallel, the transducer will be applied independently to each
 *   element, not across elements, and may produce zero or more outputs
 *   per input.  Outputs will be returned in order relative to the
 *   inputs. By default, the to channel will be closed when the from
 *   channel closes, but can be determined by the close?  parameter. Will
 *   stop consuming the from channel if the to channel closes.
 * 
 *   Note this is supplied for API compatibility with the Clojure version.
 *   Values of N > 1 will not result in actual concurrency in a
 *   single-threaded runtime.
 */
cljs.core.async.pipeline = (function cljs$core$async$pipeline(var_args){
var G__13238 = arguments.length;
switch (G__13238) {
case 4:
return cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
case 5:
return cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$5((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]),(arguments[(4)]));

break;
case 6:
return cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$6((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]),(arguments[(4)]),(arguments[(5)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$4 = (function (n,to,xf,from){
return cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$5(n,to,xf,from,true);
}));

(cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$5 = (function (n,to,xf,from,close_QMARK_){
return cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$6(n,to,xf,from,close_QMARK_,null);
}));

(cljs.core.async.pipeline.cljs$core$IFn$_invoke$arity$6 = (function (n,to,xf,from,close_QMARK_,ex_handler){
return cljs.core.async.pipeline_STAR_(n,to,xf,from,close_QMARK_,ex_handler,new cljs.core.Keyword(null,"compute","compute",1555393130));
}));

(cljs.core.async.pipeline.cljs$lang$maxFixedArity = 6);

/**
 * Takes a predicate and a source channel and returns a vector of two
 *   channels, the first of which will contain the values for which the
 *   predicate returned true, the second those for which it returned
 *   false.
 * 
 *   The out channels will be unbuffered by default, or two buf-or-ns can
 *   be supplied. The channels will close after the source channel has
 *   closed.
 */
cljs.core.async.split = (function cljs$core$async$split(var_args){
var G__13240 = arguments.length;
switch (G__13240) {
case 2:
return cljs.core.async.split.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 4:
return cljs.core.async.split.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.split.cljs$core$IFn$_invoke$arity$2 = (function (p,ch){
return cljs.core.async.split.cljs$core$IFn$_invoke$arity$4(p,ch,null,null);
}));

(cljs.core.async.split.cljs$core$IFn$_invoke$arity$4 = (function (p,ch,t_buf_or_n,f_buf_or_n){
var tc = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(t_buf_or_n);
var fc = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(f_buf_or_n);
var c__12922__auto___14666 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13266){
var state_val_13267 = (state_13266[(1)]);
if((state_val_13267 === (7))){
var inst_13262 = (state_13266[(2)]);
var state_13266__$1 = state_13266;
var statearr_13268_14667 = state_13266__$1;
(statearr_13268_14667[(2)] = inst_13262);

(statearr_13268_14667[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (1))){
var state_13266__$1 = state_13266;
var statearr_13269_14669 = state_13266__$1;
(statearr_13269_14669[(2)] = null);

(statearr_13269_14669[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (4))){
var inst_13243 = (state_13266[(7)]);
var inst_13243__$1 = (state_13266[(2)]);
var inst_13244 = (inst_13243__$1 == null);
var state_13266__$1 = (function (){var statearr_13270 = state_13266;
(statearr_13270[(7)] = inst_13243__$1);

return statearr_13270;
})();
if(cljs.core.truth_(inst_13244)){
var statearr_13271_14670 = state_13266__$1;
(statearr_13271_14670[(1)] = (5));

} else {
var statearr_13272_14671 = state_13266__$1;
(statearr_13272_14671[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (13))){
var state_13266__$1 = state_13266;
var statearr_13273_14672 = state_13266__$1;
(statearr_13273_14672[(2)] = null);

(statearr_13273_14672[(1)] = (14));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (6))){
var inst_13243 = (state_13266[(7)]);
var inst_13249 = (p.cljs$core$IFn$_invoke$arity$1 ? p.cljs$core$IFn$_invoke$arity$1(inst_13243) : p.call(null,inst_13243));
var state_13266__$1 = state_13266;
if(cljs.core.truth_(inst_13249)){
var statearr_13274_14673 = state_13266__$1;
(statearr_13274_14673[(1)] = (9));

} else {
var statearr_13275_14674 = state_13266__$1;
(statearr_13275_14674[(1)] = (10));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (3))){
var inst_13264 = (state_13266[(2)]);
var state_13266__$1 = state_13266;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13266__$1,inst_13264);
} else {
if((state_val_13267 === (12))){
var state_13266__$1 = state_13266;
var statearr_13276_14675 = state_13266__$1;
(statearr_13276_14675[(2)] = null);

(statearr_13276_14675[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (2))){
var state_13266__$1 = state_13266;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13266__$1,(4),ch);
} else {
if((state_val_13267 === (11))){
var inst_13243 = (state_13266[(7)]);
var inst_13253 = (state_13266[(2)]);
var state_13266__$1 = state_13266;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13266__$1,(8),inst_13253,inst_13243);
} else {
if((state_val_13267 === (9))){
var state_13266__$1 = state_13266;
var statearr_13277_14676 = state_13266__$1;
(statearr_13277_14676[(2)] = tc);

(statearr_13277_14676[(1)] = (11));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (5))){
var inst_13246 = cljs.core.async.close_BANG_(tc);
var inst_13247 = cljs.core.async.close_BANG_(fc);
var state_13266__$1 = (function (){var statearr_13278 = state_13266;
(statearr_13278[(8)] = inst_13246);

return statearr_13278;
})();
var statearr_13279_14679 = state_13266__$1;
(statearr_13279_14679[(2)] = inst_13247);

(statearr_13279_14679[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (14))){
var inst_13260 = (state_13266[(2)]);
var state_13266__$1 = state_13266;
var statearr_13280_14680 = state_13266__$1;
(statearr_13280_14680[(2)] = inst_13260);

(statearr_13280_14680[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (10))){
var state_13266__$1 = state_13266;
var statearr_13281_14681 = state_13266__$1;
(statearr_13281_14681[(2)] = fc);

(statearr_13281_14681[(1)] = (11));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13267 === (8))){
var inst_13255 = (state_13266[(2)]);
var state_13266__$1 = state_13266;
if(cljs.core.truth_(inst_13255)){
var statearr_13282_14682 = state_13266__$1;
(statearr_13282_14682[(1)] = (12));

} else {
var statearr_13283_14683 = state_13266__$1;
(statearr_13283_14683[(1)] = (13));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_13284 = [null,null,null,null,null,null,null,null,null];
(statearr_13284[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_13284[(1)] = (1));

return statearr_13284;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_13266){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13266);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13285){var ex__12826__auto__ = e13285;
var statearr_13286_14684 = state_13266;
(statearr_13286_14684[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13266[(4)]))){
var statearr_13287_14685 = state_13266;
(statearr_13287_14685[(1)] = cljs.core.first((state_13266[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14689 = state_13266;
state_13266 = G__14689;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_13266){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_13266);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13288 = f__12923__auto__();
(statearr_13288[(6)] = c__12922__auto___14666);

return statearr_13288;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [tc,fc], null);
}));

(cljs.core.async.split.cljs$lang$maxFixedArity = 4);

/**
 * f should be a function of 2 arguments. Returns a channel containing
 *   the single result of applying f to init and the first item from the
 *   channel, then applying f to that result and the 2nd item, etc. If
 *   the channel closes without yielding items, returns init and f is not
 *   called. ch must close before reduce produces a result.
 */
cljs.core.async.reduce = (function cljs$core$async$reduce(f,init,ch){
var c__12922__auto__ = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13310){
var state_val_13311 = (state_13310[(1)]);
if((state_val_13311 === (7))){
var inst_13306 = (state_13310[(2)]);
var state_13310__$1 = state_13310;
var statearr_13312_14690 = state_13310__$1;
(statearr_13312_14690[(2)] = inst_13306);

(statearr_13312_14690[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (1))){
var inst_13289 = init;
var inst_13290 = inst_13289;
var state_13310__$1 = (function (){var statearr_13313 = state_13310;
(statearr_13313[(7)] = inst_13290);

return statearr_13313;
})();
var statearr_13314_14691 = state_13310__$1;
(statearr_13314_14691[(2)] = null);

(statearr_13314_14691[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (4))){
var inst_13293 = (state_13310[(8)]);
var inst_13293__$1 = (state_13310[(2)]);
var inst_13294 = (inst_13293__$1 == null);
var state_13310__$1 = (function (){var statearr_13315 = state_13310;
(statearr_13315[(8)] = inst_13293__$1);

return statearr_13315;
})();
if(cljs.core.truth_(inst_13294)){
var statearr_13316_14692 = state_13310__$1;
(statearr_13316_14692[(1)] = (5));

} else {
var statearr_13317_14693 = state_13310__$1;
(statearr_13317_14693[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (6))){
var inst_13293 = (state_13310[(8)]);
var inst_13290 = (state_13310[(7)]);
var inst_13297 = (state_13310[(9)]);
var inst_13297__$1 = (f.cljs$core$IFn$_invoke$arity$2 ? f.cljs$core$IFn$_invoke$arity$2(inst_13290,inst_13293) : f.call(null,inst_13290,inst_13293));
var inst_13298 = cljs.core.reduced_QMARK_(inst_13297__$1);
var state_13310__$1 = (function (){var statearr_13318 = state_13310;
(statearr_13318[(9)] = inst_13297__$1);

return statearr_13318;
})();
if(inst_13298){
var statearr_13319_14694 = state_13310__$1;
(statearr_13319_14694[(1)] = (8));

} else {
var statearr_13320_14695 = state_13310__$1;
(statearr_13320_14695[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (3))){
var inst_13308 = (state_13310[(2)]);
var state_13310__$1 = state_13310;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13310__$1,inst_13308);
} else {
if((state_val_13311 === (2))){
var state_13310__$1 = state_13310;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13310__$1,(4),ch);
} else {
if((state_val_13311 === (9))){
var inst_13297 = (state_13310[(9)]);
var inst_13290 = inst_13297;
var state_13310__$1 = (function (){var statearr_13321 = state_13310;
(statearr_13321[(7)] = inst_13290);

return statearr_13321;
})();
var statearr_13322_14696 = state_13310__$1;
(statearr_13322_14696[(2)] = null);

(statearr_13322_14696[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (5))){
var inst_13290 = (state_13310[(7)]);
var state_13310__$1 = state_13310;
var statearr_13323_14697 = state_13310__$1;
(statearr_13323_14697[(2)] = inst_13290);

(statearr_13323_14697[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (10))){
var inst_13304 = (state_13310[(2)]);
var state_13310__$1 = state_13310;
var statearr_13324_14698 = state_13310__$1;
(statearr_13324_14698[(2)] = inst_13304);

(statearr_13324_14698[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13311 === (8))){
var inst_13297 = (state_13310[(9)]);
var inst_13300 = cljs.core.deref(inst_13297);
var state_13310__$1 = state_13310;
var statearr_13325_14699 = state_13310__$1;
(statearr_13325_14699[(2)] = inst_13300);

(statearr_13325_14699[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$reduce_$_state_machine__12823__auto__ = null;
var cljs$core$async$reduce_$_state_machine__12823__auto____0 = (function (){
var statearr_13326 = [null,null,null,null,null,null,null,null,null,null];
(statearr_13326[(0)] = cljs$core$async$reduce_$_state_machine__12823__auto__);

(statearr_13326[(1)] = (1));

return statearr_13326;
});
var cljs$core$async$reduce_$_state_machine__12823__auto____1 = (function (state_13310){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13310);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13327){var ex__12826__auto__ = e13327;
var statearr_13328_14700 = state_13310;
(statearr_13328_14700[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13310[(4)]))){
var statearr_13329_14701 = state_13310;
(statearr_13329_14701[(1)] = cljs.core.first((state_13310[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14702 = state_13310;
state_13310 = G__14702;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$reduce_$_state_machine__12823__auto__ = function(state_13310){
switch(arguments.length){
case 0:
return cljs$core$async$reduce_$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$reduce_$_state_machine__12823__auto____1.call(this,state_13310);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$reduce_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$reduce_$_state_machine__12823__auto____0;
cljs$core$async$reduce_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$reduce_$_state_machine__12823__auto____1;
return cljs$core$async$reduce_$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13330 = f__12923__auto__();
(statearr_13330[(6)] = c__12922__auto__);

return statearr_13330;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));

return c__12922__auto__;
});
/**
 * async/reduces a channel with a transformation (xform f).
 *   Returns a channel containing the result.  ch must close before
 *   transduce produces a result.
 */
cljs.core.async.transduce = (function cljs$core$async$transduce(xform,f,init,ch){
var f__$1 = (xform.cljs$core$IFn$_invoke$arity$1 ? xform.cljs$core$IFn$_invoke$arity$1(f) : xform.call(null,f));
var c__12922__auto__ = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13336){
var state_val_13337 = (state_13336[(1)]);
if((state_val_13337 === (1))){
var inst_13331 = cljs.core.async.reduce(f__$1,init,ch);
var state_13336__$1 = state_13336;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13336__$1,(2),inst_13331);
} else {
if((state_val_13337 === (2))){
var inst_13333 = (state_13336[(2)]);
var inst_13334 = (f__$1.cljs$core$IFn$_invoke$arity$1 ? f__$1.cljs$core$IFn$_invoke$arity$1(inst_13333) : f__$1.call(null,inst_13333));
var state_13336__$1 = state_13336;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13336__$1,inst_13334);
} else {
return null;
}
}
});
return (function() {
var cljs$core$async$transduce_$_state_machine__12823__auto__ = null;
var cljs$core$async$transduce_$_state_machine__12823__auto____0 = (function (){
var statearr_13338 = [null,null,null,null,null,null,null];
(statearr_13338[(0)] = cljs$core$async$transduce_$_state_machine__12823__auto__);

(statearr_13338[(1)] = (1));

return statearr_13338;
});
var cljs$core$async$transduce_$_state_machine__12823__auto____1 = (function (state_13336){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13336);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13339){var ex__12826__auto__ = e13339;
var statearr_13340_14703 = state_13336;
(statearr_13340_14703[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13336[(4)]))){
var statearr_13341_14704 = state_13336;
(statearr_13341_14704[(1)] = cljs.core.first((state_13336[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14705 = state_13336;
state_13336 = G__14705;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$transduce_$_state_machine__12823__auto__ = function(state_13336){
switch(arguments.length){
case 0:
return cljs$core$async$transduce_$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$transduce_$_state_machine__12823__auto____1.call(this,state_13336);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$transduce_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$transduce_$_state_machine__12823__auto____0;
cljs$core$async$transduce_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$transduce_$_state_machine__12823__auto____1;
return cljs$core$async$transduce_$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13342 = f__12923__auto__();
(statearr_13342[(6)] = c__12922__auto__);

return statearr_13342;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));

return c__12922__auto__;
});
/**
 * Puts the contents of coll into the supplied channel.
 * 
 *   By default the channel will be closed after the items are copied,
 *   but can be determined by the close? parameter.
 * 
 *   Returns a channel which will close after the items are copied.
 */
cljs.core.async.onto_chan_BANG_ = (function cljs$core$async$onto_chan_BANG_(var_args){
var G__13344 = arguments.length;
switch (G__13344) {
case 2:
return cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$2 = (function (ch,coll){
return cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$3(ch,coll,true);
}));

(cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$3 = (function (ch,coll,close_QMARK_){
var c__12922__auto__ = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13369){
var state_val_13370 = (state_13369[(1)]);
if((state_val_13370 === (7))){
var inst_13351 = (state_13369[(2)]);
var state_13369__$1 = state_13369;
var statearr_13371_14707 = state_13369__$1;
(statearr_13371_14707[(2)] = inst_13351);

(statearr_13371_14707[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (1))){
var inst_13345 = cljs.core.seq(coll);
var inst_13346 = inst_13345;
var state_13369__$1 = (function (){var statearr_13372 = state_13369;
(statearr_13372[(7)] = inst_13346);

return statearr_13372;
})();
var statearr_13373_14708 = state_13369__$1;
(statearr_13373_14708[(2)] = null);

(statearr_13373_14708[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (4))){
var inst_13346 = (state_13369[(7)]);
var inst_13349 = cljs.core.first(inst_13346);
var state_13369__$1 = state_13369;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13369__$1,(7),ch,inst_13349);
} else {
if((state_val_13370 === (13))){
var inst_13363 = (state_13369[(2)]);
var state_13369__$1 = state_13369;
var statearr_13374_14709 = state_13369__$1;
(statearr_13374_14709[(2)] = inst_13363);

(statearr_13374_14709[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (6))){
var inst_13354 = (state_13369[(2)]);
var state_13369__$1 = state_13369;
if(cljs.core.truth_(inst_13354)){
var statearr_13375_14710 = state_13369__$1;
(statearr_13375_14710[(1)] = (8));

} else {
var statearr_13376_14711 = state_13369__$1;
(statearr_13376_14711[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (3))){
var inst_13367 = (state_13369[(2)]);
var state_13369__$1 = state_13369;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13369__$1,inst_13367);
} else {
if((state_val_13370 === (12))){
var state_13369__$1 = state_13369;
var statearr_13377_14712 = state_13369__$1;
(statearr_13377_14712[(2)] = null);

(statearr_13377_14712[(1)] = (13));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (2))){
var inst_13346 = (state_13369[(7)]);
var state_13369__$1 = state_13369;
if(cljs.core.truth_(inst_13346)){
var statearr_13378_14713 = state_13369__$1;
(statearr_13378_14713[(1)] = (4));

} else {
var statearr_13379_14714 = state_13369__$1;
(statearr_13379_14714[(1)] = (5));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (11))){
var inst_13360 = cljs.core.async.close_BANG_(ch);
var state_13369__$1 = state_13369;
var statearr_13380_14715 = state_13369__$1;
(statearr_13380_14715[(2)] = inst_13360);

(statearr_13380_14715[(1)] = (13));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (9))){
var state_13369__$1 = state_13369;
if(cljs.core.truth_(close_QMARK_)){
var statearr_13381_14716 = state_13369__$1;
(statearr_13381_14716[(1)] = (11));

} else {
var statearr_13382_14717 = state_13369__$1;
(statearr_13382_14717[(1)] = (12));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (5))){
var inst_13346 = (state_13369[(7)]);
var state_13369__$1 = state_13369;
var statearr_13383_14718 = state_13369__$1;
(statearr_13383_14718[(2)] = inst_13346);

(statearr_13383_14718[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (10))){
var inst_13365 = (state_13369[(2)]);
var state_13369__$1 = state_13369;
var statearr_13384_14719 = state_13369__$1;
(statearr_13384_14719[(2)] = inst_13365);

(statearr_13384_14719[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13370 === (8))){
var inst_13346 = (state_13369[(7)]);
var inst_13356 = cljs.core.next(inst_13346);
var inst_13346__$1 = inst_13356;
var state_13369__$1 = (function (){var statearr_13385 = state_13369;
(statearr_13385[(7)] = inst_13346__$1);

return statearr_13385;
})();
var statearr_13386_14723 = state_13369__$1;
(statearr_13386_14723[(2)] = null);

(statearr_13386_14723[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_13387 = [null,null,null,null,null,null,null,null];
(statearr_13387[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_13387[(1)] = (1));

return statearr_13387;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_13369){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13369);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13388){var ex__12826__auto__ = e13388;
var statearr_13389_14724 = state_13369;
(statearr_13389_14724[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13369[(4)]))){
var statearr_13390_14725 = state_13369;
(statearr_13390_14725[(1)] = cljs.core.first((state_13369[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14726 = state_13369;
state_13369 = G__14726;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_13369){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_13369);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13391 = f__12923__auto__();
(statearr_13391[(6)] = c__12922__auto__);

return statearr_13391;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));

return c__12922__auto__;
}));

(cljs.core.async.onto_chan_BANG_.cljs$lang$maxFixedArity = 3);

/**
 * Creates and returns a channel which contains the contents of coll,
 *   closing when exhausted.
 */
cljs.core.async.to_chan_BANG_ = (function cljs$core$async$to_chan_BANG_(coll){
var ch = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(cljs.core.bounded_count((100),coll));
cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$2(ch,coll);

return ch;
});
/**
 * Deprecated - use onto-chan!
 */
cljs.core.async.onto_chan = (function cljs$core$async$onto_chan(var_args){
var G__13393 = arguments.length;
switch (G__13393) {
case 2:
return cljs.core.async.onto_chan.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.onto_chan.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.onto_chan.cljs$core$IFn$_invoke$arity$2 = (function (ch,coll){
return cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$3(ch,coll,true);
}));

(cljs.core.async.onto_chan.cljs$core$IFn$_invoke$arity$3 = (function (ch,coll,close_QMARK_){
return cljs.core.async.onto_chan_BANG_.cljs$core$IFn$_invoke$arity$3(ch,coll,close_QMARK_);
}));

(cljs.core.async.onto_chan.cljs$lang$maxFixedArity = 3);

/**
 * Deprecated - use to-chan!
 */
cljs.core.async.to_chan = (function cljs$core$async$to_chan(coll){
return cljs.core.async.to_chan_BANG_(coll);
});

/**
 * @interface
 */
cljs.core.async.Mux = function(){};

var cljs$core$async$Mux$muxch_STAR_$dyn_14728 = (function (_){
var x__5350__auto__ = (((_ == null))?null:_);
var m__5351__auto__ = (cljs.core.async.muxch_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(_) : m__5351__auto__.call(null,_));
} else {
var m__5349__auto__ = (cljs.core.async.muxch_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(_) : m__5349__auto__.call(null,_));
} else {
throw cljs.core.missing_protocol("Mux.muxch*",_);
}
}
});
cljs.core.async.muxch_STAR_ = (function cljs$core$async$muxch_STAR_(_){
if((((!((_ == null)))) && ((!((_.cljs$core$async$Mux$muxch_STAR_$arity$1 == null)))))){
return _.cljs$core$async$Mux$muxch_STAR_$arity$1(_);
} else {
return cljs$core$async$Mux$muxch_STAR_$dyn_14728(_);
}
});


/**
 * @interface
 */
cljs.core.async.Mult = function(){};

var cljs$core$async$Mult$tap_STAR_$dyn_14729 = (function (m,ch,close_QMARK_){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.tap_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$3 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$3(m,ch,close_QMARK_) : m__5351__auto__.call(null,m,ch,close_QMARK_));
} else {
var m__5349__auto__ = (cljs.core.async.tap_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$3 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$3(m,ch,close_QMARK_) : m__5349__auto__.call(null,m,ch,close_QMARK_));
} else {
throw cljs.core.missing_protocol("Mult.tap*",m);
}
}
});
cljs.core.async.tap_STAR_ = (function cljs$core$async$tap_STAR_(m,ch,close_QMARK_){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mult$tap_STAR_$arity$3 == null)))))){
return m.cljs$core$async$Mult$tap_STAR_$arity$3(m,ch,close_QMARK_);
} else {
return cljs$core$async$Mult$tap_STAR_$dyn_14729(m,ch,close_QMARK_);
}
});

var cljs$core$async$Mult$untap_STAR_$dyn_14730 = (function (m,ch){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.untap_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(m,ch) : m__5351__auto__.call(null,m,ch));
} else {
var m__5349__auto__ = (cljs.core.async.untap_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(m,ch) : m__5349__auto__.call(null,m,ch));
} else {
throw cljs.core.missing_protocol("Mult.untap*",m);
}
}
});
cljs.core.async.untap_STAR_ = (function cljs$core$async$untap_STAR_(m,ch){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mult$untap_STAR_$arity$2 == null)))))){
return m.cljs$core$async$Mult$untap_STAR_$arity$2(m,ch);
} else {
return cljs$core$async$Mult$untap_STAR_$dyn_14730(m,ch);
}
});

var cljs$core$async$Mult$untap_all_STAR_$dyn_14731 = (function (m){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.untap_all_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(m) : m__5351__auto__.call(null,m));
} else {
var m__5349__auto__ = (cljs.core.async.untap_all_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(m) : m__5349__auto__.call(null,m));
} else {
throw cljs.core.missing_protocol("Mult.untap-all*",m);
}
}
});
cljs.core.async.untap_all_STAR_ = (function cljs$core$async$untap_all_STAR_(m){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mult$untap_all_STAR_$arity$1 == null)))))){
return m.cljs$core$async$Mult$untap_all_STAR_$arity$1(m);
} else {
return cljs$core$async$Mult$untap_all_STAR_$dyn_14731(m);
}
});


/**
* @constructor
 * @implements {cljs.core.async.Mult}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.async.Mux}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async13394 = (function (ch,cs,meta13395){
this.ch = ch;
this.cs = cs;
this.meta13395 = meta13395;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13396,meta13395__$1){
var self__ = this;
var _13396__$1 = this;
return (new cljs.core.async.t_cljs$core$async13394(self__.ch,self__.cs,meta13395__$1));
}));

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13396){
var self__ = this;
var _13396__$1 = this;
return self__.meta13395;
}));

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$async$Mux$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$async$Mux$muxch_STAR_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.ch;
}));

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$async$Mult$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$async$Mult$tap_STAR_$arity$3 = (function (_,ch__$1,close_QMARK_){
var self__ = this;
var ___$1 = this;
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(self__.cs,cljs.core.assoc,ch__$1,close_QMARK_);

return null;
}));

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$async$Mult$untap_STAR_$arity$2 = (function (_,ch__$1){
var self__ = this;
var ___$1 = this;
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(self__.cs,cljs.core.dissoc,ch__$1);

return null;
}));

(cljs.core.async.t_cljs$core$async13394.prototype.cljs$core$async$Mult$untap_all_STAR_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
cljs.core.reset_BANG_(self__.cs,cljs.core.PersistentArrayMap.EMPTY);

return null;
}));

(cljs.core.async.t_cljs$core$async13394.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"ch","ch",1085813622,null),new cljs.core.Symbol(null,"cs","cs",-117024463,null),new cljs.core.Symbol(null,"meta13395","meta13395",-1764020311,null)], null);
}));

(cljs.core.async.t_cljs$core$async13394.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async13394.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async13394");

(cljs.core.async.t_cljs$core$async13394.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async13394");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async13394.
 */
cljs.core.async.__GT_t_cljs$core$async13394 = (function cljs$core$async$__GT_t_cljs$core$async13394(ch,cs,meta13395){
return (new cljs.core.async.t_cljs$core$async13394(ch,cs,meta13395));
});


/**
 * Creates and returns a mult(iple) of the supplied channel. Channels
 *   containing copies of the channel can be created with 'tap', and
 *   detached with 'untap'.
 * 
 *   Each item is distributed to all taps in parallel and synchronously,
 *   i.e. each tap must accept before the next item is distributed. Use
 *   buffering/windowing to prevent slow taps from holding up the mult.
 * 
 *   Items received when there are no taps get dropped.
 * 
 *   If a tap puts to a closed channel, it will be removed from the mult.
 */
cljs.core.async.mult = (function cljs$core$async$mult(ch){
var cs = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var m = (new cljs.core.async.t_cljs$core$async13394(ch,cs,cljs.core.PersistentArrayMap.EMPTY));
var dchan = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
var dctr = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(null);
var done = (function (_){
if((cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(dctr,cljs.core.dec) === (0))){
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2(dchan,true);
} else {
return null;
}
});
var c__12922__auto___14736 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13529){
var state_val_13530 = (state_13529[(1)]);
if((state_val_13530 === (7))){
var inst_13525 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13531_14737 = state_13529__$1;
(statearr_13531_14737[(2)] = inst_13525);

(statearr_13531_14737[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (20))){
var inst_13430 = (state_13529[(7)]);
var inst_13442 = cljs.core.first(inst_13430);
var inst_13443 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13442,(0),null);
var inst_13444 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13442,(1),null);
var state_13529__$1 = (function (){var statearr_13532 = state_13529;
(statearr_13532[(8)] = inst_13443);

return statearr_13532;
})();
if(cljs.core.truth_(inst_13444)){
var statearr_13533_14738 = state_13529__$1;
(statearr_13533_14738[(1)] = (22));

} else {
var statearr_13534_14739 = state_13529__$1;
(statearr_13534_14739[(1)] = (23));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (27))){
var inst_13399 = (state_13529[(9)]);
var inst_13479 = (state_13529[(10)]);
var inst_13474 = (state_13529[(11)]);
var inst_13472 = (state_13529[(12)]);
var inst_13479__$1 = cljs.core._nth(inst_13472,inst_13474);
var inst_13480 = cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$3(inst_13479__$1,inst_13399,done);
var state_13529__$1 = (function (){var statearr_13535 = state_13529;
(statearr_13535[(10)] = inst_13479__$1);

return statearr_13535;
})();
if(cljs.core.truth_(inst_13480)){
var statearr_13536_14740 = state_13529__$1;
(statearr_13536_14740[(1)] = (30));

} else {
var statearr_13537_14741 = state_13529__$1;
(statearr_13537_14741[(1)] = (31));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (1))){
var state_13529__$1 = state_13529;
var statearr_13538_14742 = state_13529__$1;
(statearr_13538_14742[(2)] = null);

(statearr_13538_14742[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (24))){
var inst_13430 = (state_13529[(7)]);
var inst_13449 = (state_13529[(2)]);
var inst_13450 = cljs.core.next(inst_13430);
var inst_13408 = inst_13450;
var inst_13409 = null;
var inst_13410 = (0);
var inst_13411 = (0);
var state_13529__$1 = (function (){var statearr_13539 = state_13529;
(statearr_13539[(13)] = inst_13409);

(statearr_13539[(14)] = inst_13411);

(statearr_13539[(15)] = inst_13408);

(statearr_13539[(16)] = inst_13449);

(statearr_13539[(17)] = inst_13410);

return statearr_13539;
})();
var statearr_13540_14743 = state_13529__$1;
(statearr_13540_14743[(2)] = null);

(statearr_13540_14743[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (39))){
var state_13529__$1 = state_13529;
var statearr_13544_14744 = state_13529__$1;
(statearr_13544_14744[(2)] = null);

(statearr_13544_14744[(1)] = (41));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (4))){
var inst_13399 = (state_13529[(9)]);
var inst_13399__$1 = (state_13529[(2)]);
var inst_13400 = (inst_13399__$1 == null);
var state_13529__$1 = (function (){var statearr_13545 = state_13529;
(statearr_13545[(9)] = inst_13399__$1);

return statearr_13545;
})();
if(cljs.core.truth_(inst_13400)){
var statearr_13546_14745 = state_13529__$1;
(statearr_13546_14745[(1)] = (5));

} else {
var statearr_13547_14746 = state_13529__$1;
(statearr_13547_14746[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (15))){
var inst_13409 = (state_13529[(13)]);
var inst_13411 = (state_13529[(14)]);
var inst_13408 = (state_13529[(15)]);
var inst_13410 = (state_13529[(17)]);
var inst_13426 = (state_13529[(2)]);
var inst_13427 = (inst_13411 + (1));
var tmp13541 = inst_13409;
var tmp13542 = inst_13408;
var tmp13543 = inst_13410;
var inst_13408__$1 = tmp13542;
var inst_13409__$1 = tmp13541;
var inst_13410__$1 = tmp13543;
var inst_13411__$1 = inst_13427;
var state_13529__$1 = (function (){var statearr_13548 = state_13529;
(statearr_13548[(13)] = inst_13409__$1);

(statearr_13548[(14)] = inst_13411__$1);

(statearr_13548[(15)] = inst_13408__$1);

(statearr_13548[(17)] = inst_13410__$1);

(statearr_13548[(18)] = inst_13426);

return statearr_13548;
})();
var statearr_13549_14747 = state_13529__$1;
(statearr_13549_14747[(2)] = null);

(statearr_13549_14747[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (21))){
var inst_13453 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13553_14748 = state_13529__$1;
(statearr_13553_14748[(2)] = inst_13453);

(statearr_13553_14748[(1)] = (18));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (31))){
var inst_13479 = (state_13529[(10)]);
var inst_13483 = m.cljs$core$async$Mult$untap_STAR_$arity$2(null,inst_13479);
var state_13529__$1 = state_13529;
var statearr_13554_14749 = state_13529__$1;
(statearr_13554_14749[(2)] = inst_13483);

(statearr_13554_14749[(1)] = (32));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (32))){
var inst_13471 = (state_13529[(19)]);
var inst_13474 = (state_13529[(11)]);
var inst_13472 = (state_13529[(12)]);
var inst_13473 = (state_13529[(20)]);
var inst_13485 = (state_13529[(2)]);
var inst_13486 = (inst_13474 + (1));
var tmp13550 = inst_13471;
var tmp13551 = inst_13472;
var tmp13552 = inst_13473;
var inst_13471__$1 = tmp13550;
var inst_13472__$1 = tmp13551;
var inst_13473__$1 = tmp13552;
var inst_13474__$1 = inst_13486;
var state_13529__$1 = (function (){var statearr_13555 = state_13529;
(statearr_13555[(19)] = inst_13471__$1);

(statearr_13555[(11)] = inst_13474__$1);

(statearr_13555[(12)] = inst_13472__$1);

(statearr_13555[(20)] = inst_13473__$1);

(statearr_13555[(21)] = inst_13485);

return statearr_13555;
})();
var statearr_13556_14750 = state_13529__$1;
(statearr_13556_14750[(2)] = null);

(statearr_13556_14750[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (40))){
var inst_13498 = (state_13529[(22)]);
var inst_13502 = m.cljs$core$async$Mult$untap_STAR_$arity$2(null,inst_13498);
var state_13529__$1 = state_13529;
var statearr_13557_14751 = state_13529__$1;
(statearr_13557_14751[(2)] = inst_13502);

(statearr_13557_14751[(1)] = (41));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (33))){
var inst_13489 = (state_13529[(23)]);
var inst_13491 = cljs.core.chunked_seq_QMARK_(inst_13489);
var state_13529__$1 = state_13529;
if(inst_13491){
var statearr_13558_14752 = state_13529__$1;
(statearr_13558_14752[(1)] = (36));

} else {
var statearr_13559_14753 = state_13529__$1;
(statearr_13559_14753[(1)] = (37));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (13))){
var inst_13420 = (state_13529[(24)]);
var inst_13423 = cljs.core.async.close_BANG_(inst_13420);
var state_13529__$1 = state_13529;
var statearr_13560_14758 = state_13529__$1;
(statearr_13560_14758[(2)] = inst_13423);

(statearr_13560_14758[(1)] = (15));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (22))){
var inst_13443 = (state_13529[(8)]);
var inst_13446 = cljs.core.async.close_BANG_(inst_13443);
var state_13529__$1 = state_13529;
var statearr_13561_14759 = state_13529__$1;
(statearr_13561_14759[(2)] = inst_13446);

(statearr_13561_14759[(1)] = (24));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (36))){
var inst_13489 = (state_13529[(23)]);
var inst_13493 = cljs.core.chunk_first(inst_13489);
var inst_13494 = cljs.core.chunk_rest(inst_13489);
var inst_13495 = cljs.core.count(inst_13493);
var inst_13471 = inst_13494;
var inst_13472 = inst_13493;
var inst_13473 = inst_13495;
var inst_13474 = (0);
var state_13529__$1 = (function (){var statearr_13562 = state_13529;
(statearr_13562[(19)] = inst_13471);

(statearr_13562[(11)] = inst_13474);

(statearr_13562[(12)] = inst_13472);

(statearr_13562[(20)] = inst_13473);

return statearr_13562;
})();
var statearr_13563_14760 = state_13529__$1;
(statearr_13563_14760[(2)] = null);

(statearr_13563_14760[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (41))){
var inst_13489 = (state_13529[(23)]);
var inst_13504 = (state_13529[(2)]);
var inst_13505 = cljs.core.next(inst_13489);
var inst_13471 = inst_13505;
var inst_13472 = null;
var inst_13473 = (0);
var inst_13474 = (0);
var state_13529__$1 = (function (){var statearr_13564 = state_13529;
(statearr_13564[(19)] = inst_13471);

(statearr_13564[(25)] = inst_13504);

(statearr_13564[(11)] = inst_13474);

(statearr_13564[(12)] = inst_13472);

(statearr_13564[(20)] = inst_13473);

return statearr_13564;
})();
var statearr_13565_14761 = state_13529__$1;
(statearr_13565_14761[(2)] = null);

(statearr_13565_14761[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (43))){
var state_13529__$1 = state_13529;
var statearr_13566_14762 = state_13529__$1;
(statearr_13566_14762[(2)] = null);

(statearr_13566_14762[(1)] = (44));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (29))){
var inst_13513 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13567_14763 = state_13529__$1;
(statearr_13567_14763[(2)] = inst_13513);

(statearr_13567_14763[(1)] = (26));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (44))){
var inst_13522 = (state_13529[(2)]);
var state_13529__$1 = (function (){var statearr_13568 = state_13529;
(statearr_13568[(26)] = inst_13522);

return statearr_13568;
})();
var statearr_13569_14764 = state_13529__$1;
(statearr_13569_14764[(2)] = null);

(statearr_13569_14764[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (6))){
var inst_13463 = (state_13529[(27)]);
var inst_13462 = cljs.core.deref(cs);
var inst_13463__$1 = cljs.core.keys(inst_13462);
var inst_13464 = cljs.core.count(inst_13463__$1);
var inst_13465 = cljs.core.reset_BANG_(dctr,inst_13464);
var inst_13470 = cljs.core.seq(inst_13463__$1);
var inst_13471 = inst_13470;
var inst_13472 = null;
var inst_13473 = (0);
var inst_13474 = (0);
var state_13529__$1 = (function (){var statearr_13570 = state_13529;
(statearr_13570[(28)] = inst_13465);

(statearr_13570[(19)] = inst_13471);

(statearr_13570[(27)] = inst_13463__$1);

(statearr_13570[(11)] = inst_13474);

(statearr_13570[(12)] = inst_13472);

(statearr_13570[(20)] = inst_13473);

return statearr_13570;
})();
var statearr_13571_14765 = state_13529__$1;
(statearr_13571_14765[(2)] = null);

(statearr_13571_14765[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (28))){
var inst_13471 = (state_13529[(19)]);
var inst_13489 = (state_13529[(23)]);
var inst_13489__$1 = cljs.core.seq(inst_13471);
var state_13529__$1 = (function (){var statearr_13572 = state_13529;
(statearr_13572[(23)] = inst_13489__$1);

return statearr_13572;
})();
if(inst_13489__$1){
var statearr_13573_14766 = state_13529__$1;
(statearr_13573_14766[(1)] = (33));

} else {
var statearr_13574_14768 = state_13529__$1;
(statearr_13574_14768[(1)] = (34));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (25))){
var inst_13474 = (state_13529[(11)]);
var inst_13473 = (state_13529[(20)]);
var inst_13476 = (inst_13474 < inst_13473);
var inst_13477 = inst_13476;
var state_13529__$1 = state_13529;
if(cljs.core.truth_(inst_13477)){
var statearr_13575_14770 = state_13529__$1;
(statearr_13575_14770[(1)] = (27));

} else {
var statearr_13576_14771 = state_13529__$1;
(statearr_13576_14771[(1)] = (28));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (34))){
var state_13529__$1 = state_13529;
var statearr_13577_14772 = state_13529__$1;
(statearr_13577_14772[(2)] = null);

(statearr_13577_14772[(1)] = (35));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (17))){
var state_13529__$1 = state_13529;
var statearr_13578_14773 = state_13529__$1;
(statearr_13578_14773[(2)] = null);

(statearr_13578_14773[(1)] = (18));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (3))){
var inst_13527 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13529__$1,inst_13527);
} else {
if((state_val_13530 === (12))){
var inst_13458 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13579_14775 = state_13529__$1;
(statearr_13579_14775[(2)] = inst_13458);

(statearr_13579_14775[(1)] = (9));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (2))){
var state_13529__$1 = state_13529;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13529__$1,(4),ch);
} else {
if((state_val_13530 === (23))){
var state_13529__$1 = state_13529;
var statearr_13580_14776 = state_13529__$1;
(statearr_13580_14776[(2)] = null);

(statearr_13580_14776[(1)] = (24));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (35))){
var inst_13511 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13581_14777 = state_13529__$1;
(statearr_13581_14777[(2)] = inst_13511);

(statearr_13581_14777[(1)] = (29));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (19))){
var inst_13430 = (state_13529[(7)]);
var inst_13434 = cljs.core.chunk_first(inst_13430);
var inst_13435 = cljs.core.chunk_rest(inst_13430);
var inst_13436 = cljs.core.count(inst_13434);
var inst_13408 = inst_13435;
var inst_13409 = inst_13434;
var inst_13410 = inst_13436;
var inst_13411 = (0);
var state_13529__$1 = (function (){var statearr_13582 = state_13529;
(statearr_13582[(13)] = inst_13409);

(statearr_13582[(14)] = inst_13411);

(statearr_13582[(15)] = inst_13408);

(statearr_13582[(17)] = inst_13410);

return statearr_13582;
})();
var statearr_13583_14778 = state_13529__$1;
(statearr_13583_14778[(2)] = null);

(statearr_13583_14778[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (11))){
var inst_13408 = (state_13529[(15)]);
var inst_13430 = (state_13529[(7)]);
var inst_13430__$1 = cljs.core.seq(inst_13408);
var state_13529__$1 = (function (){var statearr_13584 = state_13529;
(statearr_13584[(7)] = inst_13430__$1);

return statearr_13584;
})();
if(inst_13430__$1){
var statearr_13585_14779 = state_13529__$1;
(statearr_13585_14779[(1)] = (16));

} else {
var statearr_13586_14780 = state_13529__$1;
(statearr_13586_14780[(1)] = (17));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (9))){
var inst_13460 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13587_14781 = state_13529__$1;
(statearr_13587_14781[(2)] = inst_13460);

(statearr_13587_14781[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (5))){
var inst_13406 = cljs.core.deref(cs);
var inst_13407 = cljs.core.seq(inst_13406);
var inst_13408 = inst_13407;
var inst_13409 = null;
var inst_13410 = (0);
var inst_13411 = (0);
var state_13529__$1 = (function (){var statearr_13588 = state_13529;
(statearr_13588[(13)] = inst_13409);

(statearr_13588[(14)] = inst_13411);

(statearr_13588[(15)] = inst_13408);

(statearr_13588[(17)] = inst_13410);

return statearr_13588;
})();
var statearr_13589_14782 = state_13529__$1;
(statearr_13589_14782[(2)] = null);

(statearr_13589_14782[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (14))){
var state_13529__$1 = state_13529;
var statearr_13590_14783 = state_13529__$1;
(statearr_13590_14783[(2)] = null);

(statearr_13590_14783[(1)] = (15));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (45))){
var inst_13519 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13591_14784 = state_13529__$1;
(statearr_13591_14784[(2)] = inst_13519);

(statearr_13591_14784[(1)] = (44));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (26))){
var inst_13463 = (state_13529[(27)]);
var inst_13515 = (state_13529[(2)]);
var inst_13516 = cljs.core.seq(inst_13463);
var state_13529__$1 = (function (){var statearr_13592 = state_13529;
(statearr_13592[(29)] = inst_13515);

return statearr_13592;
})();
if(inst_13516){
var statearr_13593_14788 = state_13529__$1;
(statearr_13593_14788[(1)] = (42));

} else {
var statearr_13594_14789 = state_13529__$1;
(statearr_13594_14789[(1)] = (43));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (16))){
var inst_13430 = (state_13529[(7)]);
var inst_13432 = cljs.core.chunked_seq_QMARK_(inst_13430);
var state_13529__$1 = state_13529;
if(inst_13432){
var statearr_13595_14790 = state_13529__$1;
(statearr_13595_14790[(1)] = (19));

} else {
var statearr_13596_14791 = state_13529__$1;
(statearr_13596_14791[(1)] = (20));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (38))){
var inst_13508 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13597_14792 = state_13529__$1;
(statearr_13597_14792[(2)] = inst_13508);

(statearr_13597_14792[(1)] = (35));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (30))){
var state_13529__$1 = state_13529;
var statearr_13598_14793 = state_13529__$1;
(statearr_13598_14793[(2)] = null);

(statearr_13598_14793[(1)] = (32));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (10))){
var inst_13409 = (state_13529[(13)]);
var inst_13411 = (state_13529[(14)]);
var inst_13419 = cljs.core._nth(inst_13409,inst_13411);
var inst_13420 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13419,(0),null);
var inst_13421 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13419,(1),null);
var state_13529__$1 = (function (){var statearr_13599 = state_13529;
(statearr_13599[(24)] = inst_13420);

return statearr_13599;
})();
if(cljs.core.truth_(inst_13421)){
var statearr_13600_14794 = state_13529__$1;
(statearr_13600_14794[(1)] = (13));

} else {
var statearr_13601_14795 = state_13529__$1;
(statearr_13601_14795[(1)] = (14));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (18))){
var inst_13456 = (state_13529[(2)]);
var state_13529__$1 = state_13529;
var statearr_13602_14796 = state_13529__$1;
(statearr_13602_14796[(2)] = inst_13456);

(statearr_13602_14796[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (42))){
var state_13529__$1 = state_13529;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13529__$1,(45),dchan);
} else {
if((state_val_13530 === (37))){
var inst_13399 = (state_13529[(9)]);
var inst_13498 = (state_13529[(22)]);
var inst_13489 = (state_13529[(23)]);
var inst_13498__$1 = cljs.core.first(inst_13489);
var inst_13499 = cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$3(inst_13498__$1,inst_13399,done);
var state_13529__$1 = (function (){var statearr_13603 = state_13529;
(statearr_13603[(22)] = inst_13498__$1);

return statearr_13603;
})();
if(cljs.core.truth_(inst_13499)){
var statearr_13604_14797 = state_13529__$1;
(statearr_13604_14797[(1)] = (39));

} else {
var statearr_13605_14798 = state_13529__$1;
(statearr_13605_14798[(1)] = (40));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13530 === (8))){
var inst_13411 = (state_13529[(14)]);
var inst_13410 = (state_13529[(17)]);
var inst_13413 = (inst_13411 < inst_13410);
var inst_13414 = inst_13413;
var state_13529__$1 = state_13529;
if(cljs.core.truth_(inst_13414)){
var statearr_13606_14801 = state_13529__$1;
(statearr_13606_14801[(1)] = (10));

} else {
var statearr_13607_14802 = state_13529__$1;
(statearr_13607_14802[(1)] = (11));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$mult_$_state_machine__12823__auto__ = null;
var cljs$core$async$mult_$_state_machine__12823__auto____0 = (function (){
var statearr_13608 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_13608[(0)] = cljs$core$async$mult_$_state_machine__12823__auto__);

(statearr_13608[(1)] = (1));

return statearr_13608;
});
var cljs$core$async$mult_$_state_machine__12823__auto____1 = (function (state_13529){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13529);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13609){var ex__12826__auto__ = e13609;
var statearr_13610_14804 = state_13529;
(statearr_13610_14804[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13529[(4)]))){
var statearr_13611_14805 = state_13529;
(statearr_13611_14805[(1)] = cljs.core.first((state_13529[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14806 = state_13529;
state_13529 = G__14806;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$mult_$_state_machine__12823__auto__ = function(state_13529){
switch(arguments.length){
case 0:
return cljs$core$async$mult_$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$mult_$_state_machine__12823__auto____1.call(this,state_13529);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$mult_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$mult_$_state_machine__12823__auto____0;
cljs$core$async$mult_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$mult_$_state_machine__12823__auto____1;
return cljs$core$async$mult_$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13612 = f__12923__auto__();
(statearr_13612[(6)] = c__12922__auto___14736);

return statearr_13612;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return m;
});
/**
 * Copies the mult source onto the supplied channel.
 * 
 *   By default the channel will be closed when the source closes,
 *   but can be determined by the close? parameter.
 */
cljs.core.async.tap = (function cljs$core$async$tap(var_args){
var G__13614 = arguments.length;
switch (G__13614) {
case 2:
return cljs.core.async.tap.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.tap.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.tap.cljs$core$IFn$_invoke$arity$2 = (function (mult,ch){
return cljs.core.async.tap.cljs$core$IFn$_invoke$arity$3(mult,ch,true);
}));

(cljs.core.async.tap.cljs$core$IFn$_invoke$arity$3 = (function (mult,ch,close_QMARK_){
cljs.core.async.tap_STAR_(mult,ch,close_QMARK_);

return ch;
}));

(cljs.core.async.tap.cljs$lang$maxFixedArity = 3);

/**
 * Disconnects a target channel from a mult
 */
cljs.core.async.untap = (function cljs$core$async$untap(mult,ch){
return cljs.core.async.untap_STAR_(mult,ch);
});
/**
 * Disconnects all target channels from a mult
 */
cljs.core.async.untap_all = (function cljs$core$async$untap_all(mult){
return cljs.core.async.untap_all_STAR_(mult);
});

/**
 * @interface
 */
cljs.core.async.Mix = function(){};

var cljs$core$async$Mix$admix_STAR_$dyn_14808 = (function (m,ch){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.admix_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(m,ch) : m__5351__auto__.call(null,m,ch));
} else {
var m__5349__auto__ = (cljs.core.async.admix_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(m,ch) : m__5349__auto__.call(null,m,ch));
} else {
throw cljs.core.missing_protocol("Mix.admix*",m);
}
}
});
cljs.core.async.admix_STAR_ = (function cljs$core$async$admix_STAR_(m,ch){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mix$admix_STAR_$arity$2 == null)))))){
return m.cljs$core$async$Mix$admix_STAR_$arity$2(m,ch);
} else {
return cljs$core$async$Mix$admix_STAR_$dyn_14808(m,ch);
}
});

var cljs$core$async$Mix$unmix_STAR_$dyn_14809 = (function (m,ch){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.unmix_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(m,ch) : m__5351__auto__.call(null,m,ch));
} else {
var m__5349__auto__ = (cljs.core.async.unmix_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(m,ch) : m__5349__auto__.call(null,m,ch));
} else {
throw cljs.core.missing_protocol("Mix.unmix*",m);
}
}
});
cljs.core.async.unmix_STAR_ = (function cljs$core$async$unmix_STAR_(m,ch){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mix$unmix_STAR_$arity$2 == null)))))){
return m.cljs$core$async$Mix$unmix_STAR_$arity$2(m,ch);
} else {
return cljs$core$async$Mix$unmix_STAR_$dyn_14809(m,ch);
}
});

var cljs$core$async$Mix$unmix_all_STAR_$dyn_14810 = (function (m){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.unmix_all_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(m) : m__5351__auto__.call(null,m));
} else {
var m__5349__auto__ = (cljs.core.async.unmix_all_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(m) : m__5349__auto__.call(null,m));
} else {
throw cljs.core.missing_protocol("Mix.unmix-all*",m);
}
}
});
cljs.core.async.unmix_all_STAR_ = (function cljs$core$async$unmix_all_STAR_(m){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mix$unmix_all_STAR_$arity$1 == null)))))){
return m.cljs$core$async$Mix$unmix_all_STAR_$arity$1(m);
} else {
return cljs$core$async$Mix$unmix_all_STAR_$dyn_14810(m);
}
});

var cljs$core$async$Mix$toggle_STAR_$dyn_14813 = (function (m,state_map){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.toggle_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(m,state_map) : m__5351__auto__.call(null,m,state_map));
} else {
var m__5349__auto__ = (cljs.core.async.toggle_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(m,state_map) : m__5349__auto__.call(null,m,state_map));
} else {
throw cljs.core.missing_protocol("Mix.toggle*",m);
}
}
});
cljs.core.async.toggle_STAR_ = (function cljs$core$async$toggle_STAR_(m,state_map){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mix$toggle_STAR_$arity$2 == null)))))){
return m.cljs$core$async$Mix$toggle_STAR_$arity$2(m,state_map);
} else {
return cljs$core$async$Mix$toggle_STAR_$dyn_14813(m,state_map);
}
});

var cljs$core$async$Mix$solo_mode_STAR_$dyn_14814 = (function (m,mode){
var x__5350__auto__ = (((m == null))?null:m);
var m__5351__auto__ = (cljs.core.async.solo_mode_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(m,mode) : m__5351__auto__.call(null,m,mode));
} else {
var m__5349__auto__ = (cljs.core.async.solo_mode_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(m,mode) : m__5349__auto__.call(null,m,mode));
} else {
throw cljs.core.missing_protocol("Mix.solo-mode*",m);
}
}
});
cljs.core.async.solo_mode_STAR_ = (function cljs$core$async$solo_mode_STAR_(m,mode){
if((((!((m == null)))) && ((!((m.cljs$core$async$Mix$solo_mode_STAR_$arity$2 == null)))))){
return m.cljs$core$async$Mix$solo_mode_STAR_$arity$2(m,mode);
} else {
return cljs$core$async$Mix$solo_mode_STAR_$dyn_14814(m,mode);
}
});

cljs.core.async.ioc_alts_BANG_ = (function cljs$core$async$ioc_alts_BANG_(var_args){
var args__5732__auto__ = [];
var len__5726__auto___14815 = arguments.length;
var i__5727__auto___14816 = (0);
while(true){
if((i__5727__auto___14816 < len__5726__auto___14815)){
args__5732__auto__.push((arguments[i__5727__auto___14816]));

var G__14819 = (i__5727__auto___14816 + (1));
i__5727__auto___14816 = G__14819;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((3) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((3)),(0),null)):null);
return cljs.core.async.ioc_alts_BANG_.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),argseq__5733__auto__);
});

(cljs.core.async.ioc_alts_BANG_.cljs$core$IFn$_invoke$arity$variadic = (function (state,cont_block,ports,p__13619){
var map__13620 = p__13619;
var map__13620__$1 = cljs.core.__destructure_map(map__13620);
var opts = map__13620__$1;
var statearr_13621_14820 = state;
(statearr_13621_14820[(1)] = cont_block);


var temp__5804__auto__ = cljs.core.async.do_alts((function (val){
var statearr_13622_14821 = state;
(statearr_13622_14821[(2)] = val);


return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state);
}),ports,opts);
if(cljs.core.truth_(temp__5804__auto__)){
var cb = temp__5804__auto__;
var statearr_13623_14822 = state;
(statearr_13623_14822[(2)] = cljs.core.deref(cb));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}));

(cljs.core.async.ioc_alts_BANG_.cljs$lang$maxFixedArity = (3));

/** @this {Function} */
(cljs.core.async.ioc_alts_BANG_.cljs$lang$applyTo = (function (seq13615){
var G__13616 = cljs.core.first(seq13615);
var seq13615__$1 = cljs.core.next(seq13615);
var G__13617 = cljs.core.first(seq13615__$1);
var seq13615__$2 = cljs.core.next(seq13615__$1);
var G__13618 = cljs.core.first(seq13615__$2);
var seq13615__$3 = cljs.core.next(seq13615__$2);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__13616,G__13617,G__13618,seq13615__$3);
}));


/**
* @constructor
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.async.Mix}
 * @implements {cljs.core.async.Mux}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async13624 = (function (change,solo_mode,pick,cs,calc_state,out,changed,solo_modes,attrs,meta13625){
this.change = change;
this.solo_mode = solo_mode;
this.pick = pick;
this.cs = cs;
this.calc_state = calc_state;
this.out = out;
this.changed = changed;
this.solo_modes = solo_modes;
this.attrs = attrs;
this.meta13625 = meta13625;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13626,meta13625__$1){
var self__ = this;
var _13626__$1 = this;
return (new cljs.core.async.t_cljs$core$async13624(self__.change,self__.solo_mode,self__.pick,self__.cs,self__.calc_state,self__.out,self__.changed,self__.solo_modes,self__.attrs,meta13625__$1));
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13626){
var self__ = this;
var _13626__$1 = this;
return self__.meta13625;
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mux$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mux$muxch_STAR_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.out;
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mix$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mix$admix_STAR_$arity$2 = (function (_,ch){
var self__ = this;
var ___$1 = this;
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(self__.cs,cljs.core.assoc,ch,cljs.core.PersistentArrayMap.EMPTY);

return (self__.changed.cljs$core$IFn$_invoke$arity$0 ? self__.changed.cljs$core$IFn$_invoke$arity$0() : self__.changed.call(null));
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mix$unmix_STAR_$arity$2 = (function (_,ch){
var self__ = this;
var ___$1 = this;
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(self__.cs,cljs.core.dissoc,ch);

return (self__.changed.cljs$core$IFn$_invoke$arity$0 ? self__.changed.cljs$core$IFn$_invoke$arity$0() : self__.changed.call(null));
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mix$unmix_all_STAR_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
cljs.core.reset_BANG_(self__.cs,cljs.core.PersistentArrayMap.EMPTY);

return (self__.changed.cljs$core$IFn$_invoke$arity$0 ? self__.changed.cljs$core$IFn$_invoke$arity$0() : self__.changed.call(null));
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mix$toggle_STAR_$arity$2 = (function (_,state_map){
var self__ = this;
var ___$1 = this;
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(self__.cs,cljs.core.partial.cljs$core$IFn$_invoke$arity$2(cljs.core.merge_with,cljs.core.merge),state_map);

return (self__.changed.cljs$core$IFn$_invoke$arity$0 ? self__.changed.cljs$core$IFn$_invoke$arity$0() : self__.changed.call(null));
}));

(cljs.core.async.t_cljs$core$async13624.prototype.cljs$core$async$Mix$solo_mode_STAR_$arity$2 = (function (_,mode){
var self__ = this;
var ___$1 = this;
if(cljs.core.truth_((self__.solo_modes.cljs$core$IFn$_invoke$arity$1 ? self__.solo_modes.cljs$core$IFn$_invoke$arity$1(mode) : self__.solo_modes.call(null,mode)))){
} else {
throw (new Error(["Assert failed: ",["mode must be one of: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(self__.solo_modes)].join(''),"\n","(solo-modes mode)"].join('')));
}

cljs.core.reset_BANG_(self__.solo_mode,mode);

return (self__.changed.cljs$core$IFn$_invoke$arity$0 ? self__.changed.cljs$core$IFn$_invoke$arity$0() : self__.changed.call(null));
}));

(cljs.core.async.t_cljs$core$async13624.getBasis = (function (){
return new cljs.core.PersistentVector(null, 10, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"change","change",477485025,null),new cljs.core.Symbol(null,"solo-mode","solo-mode",2031788074,null),new cljs.core.Symbol(null,"pick","pick",1300068175,null),new cljs.core.Symbol(null,"cs","cs",-117024463,null),new cljs.core.Symbol(null,"calc-state","calc-state",-349968968,null),new cljs.core.Symbol(null,"out","out",729986010,null),new cljs.core.Symbol(null,"changed","changed",-2083710852,null),new cljs.core.Symbol(null,"solo-modes","solo-modes",882180540,null),new cljs.core.Symbol(null,"attrs","attrs",-450137186,null),new cljs.core.Symbol(null,"meta13625","meta13625",-1600442897,null)], null);
}));

(cljs.core.async.t_cljs$core$async13624.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async13624.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async13624");

(cljs.core.async.t_cljs$core$async13624.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async13624");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async13624.
 */
cljs.core.async.__GT_t_cljs$core$async13624 = (function cljs$core$async$__GT_t_cljs$core$async13624(change,solo_mode,pick,cs,calc_state,out,changed,solo_modes,attrs,meta13625){
return (new cljs.core.async.t_cljs$core$async13624(change,solo_mode,pick,cs,calc_state,out,changed,solo_modes,attrs,meta13625));
});


/**
 * Creates and returns a mix of one or more input channels which will
 *   be put on the supplied out channel. Input sources can be added to
 *   the mix with 'admix', and removed with 'unmix'. A mix supports
 *   soloing, muting and pausing multiple inputs atomically using
 *   'toggle', and can solo using either muting or pausing as determined
 *   by 'solo-mode'.
 * 
 *   Each channel can have zero or more boolean modes set via 'toggle':
 * 
 *   :solo - when true, only this (ond other soloed) channel(s) will appear
 *        in the mix output channel. :mute and :pause states of soloed
 *        channels are ignored. If solo-mode is :mute, non-soloed
 *        channels are muted, if :pause, non-soloed channels are
 *        paused.
 * 
 *   :mute - muted channels will have their contents consumed but not included in the mix
 *   :pause - paused channels will not have their contents consumed (and thus also not included in the mix)
 */
cljs.core.async.mix = (function cljs$core$async$mix(out){
var cs = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var solo_modes = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"pause","pause",-2095325672),null,new cljs.core.Keyword(null,"mute","mute",1151223646),null], null), null);
var attrs = cljs.core.conj.cljs$core$IFn$_invoke$arity$2(solo_modes,new cljs.core.Keyword(null,"solo","solo",-316350075));
var solo_mode = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"mute","mute",1151223646));
var change = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(cljs.core.async.sliding_buffer((1)));
var changed = (function (){
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2(change,true);
});
var pick = (function (attr,chs){
return cljs.core.reduce_kv((function (ret,c,v){
if(cljs.core.truth_((attr.cljs$core$IFn$_invoke$arity$1 ? attr.cljs$core$IFn$_invoke$arity$1(v) : attr.call(null,v)))){
return cljs.core.conj.cljs$core$IFn$_invoke$arity$2(ret,c);
} else {
return ret;
}
}),cljs.core.PersistentHashSet.EMPTY,chs);
});
var calc_state = (function (){
var chs = cljs.core.deref(cs);
var mode = cljs.core.deref(solo_mode);
var solos = pick(new cljs.core.Keyword(null,"solo","solo",-316350075),chs);
var pauses = pick(new cljs.core.Keyword(null,"pause","pause",-2095325672),chs);
return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"solos","solos",1441458643),solos,new cljs.core.Keyword(null,"mutes","mutes",1068806309),pick(new cljs.core.Keyword(null,"mute","mute",1151223646),chs),new cljs.core.Keyword(null,"reads","reads",-1215067361),cljs.core.conj.cljs$core$IFn$_invoke$arity$2(((((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(mode,new cljs.core.Keyword(null,"pause","pause",-2095325672))) && ((!(cljs.core.empty_QMARK_(solos))))))?cljs.core.vec(solos):cljs.core.vec(cljs.core.remove.cljs$core$IFn$_invoke$arity$2(pauses,cljs.core.keys(chs)))),change)], null);
});
var m = (new cljs.core.async.t_cljs$core$async13624(change,solo_mode,pick,cs,calc_state,out,changed,solo_modes,attrs,cljs.core.PersistentArrayMap.EMPTY));
var c__12922__auto___14847 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13694){
var state_val_13695 = (state_13694[(1)]);
if((state_val_13695 === (7))){
var inst_13654 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
if(cljs.core.truth_(inst_13654)){
var statearr_13696_14848 = state_13694__$1;
(statearr_13696_14848[(1)] = (8));

} else {
var statearr_13697_14849 = state_13694__$1;
(statearr_13697_14849[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (20))){
var inst_13647 = (state_13694[(7)]);
var state_13694__$1 = state_13694;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13694__$1,(23),out,inst_13647);
} else {
if((state_val_13695 === (1))){
var inst_13630 = calc_state();
var inst_13631 = cljs.core.__destructure_map(inst_13630);
var inst_13632 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13631,new cljs.core.Keyword(null,"solos","solos",1441458643));
var inst_13633 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13631,new cljs.core.Keyword(null,"mutes","mutes",1068806309));
var inst_13634 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13631,new cljs.core.Keyword(null,"reads","reads",-1215067361));
var inst_13635 = inst_13630;
var state_13694__$1 = (function (){var statearr_13698 = state_13694;
(statearr_13698[(8)] = inst_13634);

(statearr_13698[(9)] = inst_13635);

(statearr_13698[(10)] = inst_13632);

(statearr_13698[(11)] = inst_13633);

return statearr_13698;
})();
var statearr_13699_14850 = state_13694__$1;
(statearr_13699_14850[(2)] = null);

(statearr_13699_14850[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (24))){
var inst_13638 = (state_13694[(12)]);
var inst_13635 = inst_13638;
var state_13694__$1 = (function (){var statearr_13700 = state_13694;
(statearr_13700[(9)] = inst_13635);

return statearr_13700;
})();
var statearr_13701_14851 = state_13694__$1;
(statearr_13701_14851[(2)] = null);

(statearr_13701_14851[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (4))){
var inst_13647 = (state_13694[(7)]);
var inst_13649 = (state_13694[(13)]);
var inst_13646 = (state_13694[(2)]);
var inst_13647__$1 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13646,(0),null);
var inst_13648 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13646,(1),null);
var inst_13649__$1 = (inst_13647__$1 == null);
var state_13694__$1 = (function (){var statearr_13702 = state_13694;
(statearr_13702[(7)] = inst_13647__$1);

(statearr_13702[(14)] = inst_13648);

(statearr_13702[(13)] = inst_13649__$1);

return statearr_13702;
})();
if(cljs.core.truth_(inst_13649__$1)){
var statearr_13703_14852 = state_13694__$1;
(statearr_13703_14852[(1)] = (5));

} else {
var statearr_13704_14853 = state_13694__$1;
(statearr_13704_14853[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (15))){
var inst_13668 = (state_13694[(15)]);
var inst_13639 = (state_13694[(16)]);
var inst_13668__$1 = cljs.core.empty_QMARK_(inst_13639);
var state_13694__$1 = (function (){var statearr_13705 = state_13694;
(statearr_13705[(15)] = inst_13668__$1);

return statearr_13705;
})();
if(inst_13668__$1){
var statearr_13706_14854 = state_13694__$1;
(statearr_13706_14854[(1)] = (17));

} else {
var statearr_13707_14855 = state_13694__$1;
(statearr_13707_14855[(1)] = (18));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (21))){
var inst_13638 = (state_13694[(12)]);
var inst_13635 = inst_13638;
var state_13694__$1 = (function (){var statearr_13708 = state_13694;
(statearr_13708[(9)] = inst_13635);

return statearr_13708;
})();
var statearr_13709_14868 = state_13694__$1;
(statearr_13709_14868[(2)] = null);

(statearr_13709_14868[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (13))){
var inst_13661 = (state_13694[(2)]);
var inst_13662 = calc_state();
var inst_13635 = inst_13662;
var state_13694__$1 = (function (){var statearr_13710 = state_13694;
(statearr_13710[(9)] = inst_13635);

(statearr_13710[(17)] = inst_13661);

return statearr_13710;
})();
var statearr_13711_14869 = state_13694__$1;
(statearr_13711_14869[(2)] = null);

(statearr_13711_14869[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (22))){
var inst_13688 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
var statearr_13712_14870 = state_13694__$1;
(statearr_13712_14870[(2)] = inst_13688);

(statearr_13712_14870[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (6))){
var inst_13648 = (state_13694[(14)]);
var inst_13652 = cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(inst_13648,change);
var state_13694__$1 = state_13694;
var statearr_13713_14874 = state_13694__$1;
(statearr_13713_14874[(2)] = inst_13652);

(statearr_13713_14874[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (25))){
var state_13694__$1 = state_13694;
var statearr_13714_14875 = state_13694__$1;
(statearr_13714_14875[(2)] = null);

(statearr_13714_14875[(1)] = (26));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (17))){
var inst_13648 = (state_13694[(14)]);
var inst_13640 = (state_13694[(18)]);
var inst_13670 = (inst_13640.cljs$core$IFn$_invoke$arity$1 ? inst_13640.cljs$core$IFn$_invoke$arity$1(inst_13648) : inst_13640.call(null,inst_13648));
var inst_13671 = cljs.core.not(inst_13670);
var state_13694__$1 = state_13694;
var statearr_13715_14876 = state_13694__$1;
(statearr_13715_14876[(2)] = inst_13671);

(statearr_13715_14876[(1)] = (19));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (3))){
var inst_13692 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13694__$1,inst_13692);
} else {
if((state_val_13695 === (12))){
var state_13694__$1 = state_13694;
var statearr_13716_14879 = state_13694__$1;
(statearr_13716_14879[(2)] = null);

(statearr_13716_14879[(1)] = (13));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (2))){
var inst_13638 = (state_13694[(12)]);
var inst_13635 = (state_13694[(9)]);
var inst_13638__$1 = cljs.core.__destructure_map(inst_13635);
var inst_13639 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13638__$1,new cljs.core.Keyword(null,"solos","solos",1441458643));
var inst_13640 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13638__$1,new cljs.core.Keyword(null,"mutes","mutes",1068806309));
var inst_13641 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13638__$1,new cljs.core.Keyword(null,"reads","reads",-1215067361));
var state_13694__$1 = (function (){var statearr_13717 = state_13694;
(statearr_13717[(12)] = inst_13638__$1);

(statearr_13717[(18)] = inst_13640);

(statearr_13717[(16)] = inst_13639);

return statearr_13717;
})();
return cljs.core.async.ioc_alts_BANG_(state_13694__$1,(4),inst_13641);
} else {
if((state_val_13695 === (23))){
var inst_13679 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
if(cljs.core.truth_(inst_13679)){
var statearr_13718_14880 = state_13694__$1;
(statearr_13718_14880[(1)] = (24));

} else {
var statearr_13719_14881 = state_13694__$1;
(statearr_13719_14881[(1)] = (25));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (19))){
var inst_13674 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
var statearr_13720_14882 = state_13694__$1;
(statearr_13720_14882[(2)] = inst_13674);

(statearr_13720_14882[(1)] = (16));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (11))){
var inst_13648 = (state_13694[(14)]);
var inst_13658 = cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(cs,cljs.core.dissoc,inst_13648);
var state_13694__$1 = state_13694;
var statearr_13721_14883 = state_13694__$1;
(statearr_13721_14883[(2)] = inst_13658);

(statearr_13721_14883[(1)] = (13));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (9))){
var inst_13648 = (state_13694[(14)]);
var inst_13665 = (state_13694[(19)]);
var inst_13639 = (state_13694[(16)]);
var inst_13665__$1 = (inst_13639.cljs$core$IFn$_invoke$arity$1 ? inst_13639.cljs$core$IFn$_invoke$arity$1(inst_13648) : inst_13639.call(null,inst_13648));
var state_13694__$1 = (function (){var statearr_13722 = state_13694;
(statearr_13722[(19)] = inst_13665__$1);

return statearr_13722;
})();
if(cljs.core.truth_(inst_13665__$1)){
var statearr_13723_14887 = state_13694__$1;
(statearr_13723_14887[(1)] = (14));

} else {
var statearr_13724_14888 = state_13694__$1;
(statearr_13724_14888[(1)] = (15));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (5))){
var inst_13649 = (state_13694[(13)]);
var state_13694__$1 = state_13694;
var statearr_13725_14889 = state_13694__$1;
(statearr_13725_14889[(2)] = inst_13649);

(statearr_13725_14889[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (14))){
var inst_13665 = (state_13694[(19)]);
var state_13694__$1 = state_13694;
var statearr_13726_14890 = state_13694__$1;
(statearr_13726_14890[(2)] = inst_13665);

(statearr_13726_14890[(1)] = (16));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (26))){
var inst_13684 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
var statearr_13727_14893 = state_13694__$1;
(statearr_13727_14893[(2)] = inst_13684);

(statearr_13727_14893[(1)] = (22));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (16))){
var inst_13676 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
if(cljs.core.truth_(inst_13676)){
var statearr_13728_14896 = state_13694__$1;
(statearr_13728_14896[(1)] = (20));

} else {
var statearr_13729_14897 = state_13694__$1;
(statearr_13729_14897[(1)] = (21));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (10))){
var inst_13690 = (state_13694[(2)]);
var state_13694__$1 = state_13694;
var statearr_13730_14898 = state_13694__$1;
(statearr_13730_14898[(2)] = inst_13690);

(statearr_13730_14898[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (18))){
var inst_13668 = (state_13694[(15)]);
var state_13694__$1 = state_13694;
var statearr_13731_14899 = state_13694__$1;
(statearr_13731_14899[(2)] = inst_13668);

(statearr_13731_14899[(1)] = (19));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13695 === (8))){
var inst_13647 = (state_13694[(7)]);
var inst_13656 = (inst_13647 == null);
var state_13694__$1 = state_13694;
if(cljs.core.truth_(inst_13656)){
var statearr_13732_14900 = state_13694__$1;
(statearr_13732_14900[(1)] = (11));

} else {
var statearr_13733_14901 = state_13694__$1;
(statearr_13733_14901[(1)] = (12));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$mix_$_state_machine__12823__auto__ = null;
var cljs$core$async$mix_$_state_machine__12823__auto____0 = (function (){
var statearr_13734 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_13734[(0)] = cljs$core$async$mix_$_state_machine__12823__auto__);

(statearr_13734[(1)] = (1));

return statearr_13734;
});
var cljs$core$async$mix_$_state_machine__12823__auto____1 = (function (state_13694){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13694);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13735){var ex__12826__auto__ = e13735;
var statearr_13736_14903 = state_13694;
(statearr_13736_14903[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13694[(4)]))){
var statearr_13737_14905 = state_13694;
(statearr_13737_14905[(1)] = cljs.core.first((state_13694[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14906 = state_13694;
state_13694 = G__14906;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$mix_$_state_machine__12823__auto__ = function(state_13694){
switch(arguments.length){
case 0:
return cljs$core$async$mix_$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$mix_$_state_machine__12823__auto____1.call(this,state_13694);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$mix_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$mix_$_state_machine__12823__auto____0;
cljs$core$async$mix_$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$mix_$_state_machine__12823__auto____1;
return cljs$core$async$mix_$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13738 = f__12923__auto__();
(statearr_13738[(6)] = c__12922__auto___14847);

return statearr_13738;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return m;
});
/**
 * Adds ch as an input to the mix
 */
cljs.core.async.admix = (function cljs$core$async$admix(mix,ch){
return cljs.core.async.admix_STAR_(mix,ch);
});
/**
 * Removes ch as an input to the mix
 */
cljs.core.async.unmix = (function cljs$core$async$unmix(mix,ch){
return cljs.core.async.unmix_STAR_(mix,ch);
});
/**
 * removes all inputs from the mix
 */
cljs.core.async.unmix_all = (function cljs$core$async$unmix_all(mix){
return cljs.core.async.unmix_all_STAR_(mix);
});
/**
 * Atomically sets the state(s) of one or more channels in a mix. The
 *   state map is a map of channels -> channel-state-map. A
 *   channel-state-map is a map of attrs -> boolean, where attr is one or
 *   more of :mute, :pause or :solo. Any states supplied are merged with
 *   the current state.
 * 
 *   Note that channels can be added to a mix via toggle, which can be
 *   used to add channels in a particular (e.g. paused) state.
 */
cljs.core.async.toggle = (function cljs$core$async$toggle(mix,state_map){
return cljs.core.async.toggle_STAR_(mix,state_map);
});
/**
 * Sets the solo mode of the mix. mode must be one of :mute or :pause
 */
cljs.core.async.solo_mode = (function cljs$core$async$solo_mode(mix,mode){
return cljs.core.async.solo_mode_STAR_(mix,mode);
});

/**
 * @interface
 */
cljs.core.async.Pub = function(){};

var cljs$core$async$Pub$sub_STAR_$dyn_14912 = (function (p,v,ch,close_QMARK_){
var x__5350__auto__ = (((p == null))?null:p);
var m__5351__auto__ = (cljs.core.async.sub_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$4 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$4(p,v,ch,close_QMARK_) : m__5351__auto__.call(null,p,v,ch,close_QMARK_));
} else {
var m__5349__auto__ = (cljs.core.async.sub_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$4 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$4(p,v,ch,close_QMARK_) : m__5349__auto__.call(null,p,v,ch,close_QMARK_));
} else {
throw cljs.core.missing_protocol("Pub.sub*",p);
}
}
});
cljs.core.async.sub_STAR_ = (function cljs$core$async$sub_STAR_(p,v,ch,close_QMARK_){
if((((!((p == null)))) && ((!((p.cljs$core$async$Pub$sub_STAR_$arity$4 == null)))))){
return p.cljs$core$async$Pub$sub_STAR_$arity$4(p,v,ch,close_QMARK_);
} else {
return cljs$core$async$Pub$sub_STAR_$dyn_14912(p,v,ch,close_QMARK_);
}
});

var cljs$core$async$Pub$unsub_STAR_$dyn_14916 = (function (p,v,ch){
var x__5350__auto__ = (((p == null))?null:p);
var m__5351__auto__ = (cljs.core.async.unsub_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$3 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$3(p,v,ch) : m__5351__auto__.call(null,p,v,ch));
} else {
var m__5349__auto__ = (cljs.core.async.unsub_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$3 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$3(p,v,ch) : m__5349__auto__.call(null,p,v,ch));
} else {
throw cljs.core.missing_protocol("Pub.unsub*",p);
}
}
});
cljs.core.async.unsub_STAR_ = (function cljs$core$async$unsub_STAR_(p,v,ch){
if((((!((p == null)))) && ((!((p.cljs$core$async$Pub$unsub_STAR_$arity$3 == null)))))){
return p.cljs$core$async$Pub$unsub_STAR_$arity$3(p,v,ch);
} else {
return cljs$core$async$Pub$unsub_STAR_$dyn_14916(p,v,ch);
}
});

var cljs$core$async$Pub$unsub_all_STAR_$dyn_14917 = (function() {
var G__14918 = null;
var G__14918__1 = (function (p){
var x__5350__auto__ = (((p == null))?null:p);
var m__5351__auto__ = (cljs.core.async.unsub_all_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(p) : m__5351__auto__.call(null,p));
} else {
var m__5349__auto__ = (cljs.core.async.unsub_all_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(p) : m__5349__auto__.call(null,p));
} else {
throw cljs.core.missing_protocol("Pub.unsub-all*",p);
}
}
});
var G__14918__2 = (function (p,v){
var x__5350__auto__ = (((p == null))?null:p);
var m__5351__auto__ = (cljs.core.async.unsub_all_STAR_[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(p,v) : m__5351__auto__.call(null,p,v));
} else {
var m__5349__auto__ = (cljs.core.async.unsub_all_STAR_["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(p,v) : m__5349__auto__.call(null,p,v));
} else {
throw cljs.core.missing_protocol("Pub.unsub-all*",p);
}
}
});
G__14918 = function(p,v){
switch(arguments.length){
case 1:
return G__14918__1.call(this,p);
case 2:
return G__14918__2.call(this,p,v);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
G__14918.cljs$core$IFn$_invoke$arity$1 = G__14918__1;
G__14918.cljs$core$IFn$_invoke$arity$2 = G__14918__2;
return G__14918;
})()
;
cljs.core.async.unsub_all_STAR_ = (function cljs$core$async$unsub_all_STAR_(var_args){
var G__13740 = arguments.length;
switch (G__13740) {
case 1:
return cljs.core.async.unsub_all_STAR_.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.unsub_all_STAR_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.unsub_all_STAR_.cljs$core$IFn$_invoke$arity$1 = (function (p){
if((((!((p == null)))) && ((!((p.cljs$core$async$Pub$unsub_all_STAR_$arity$1 == null)))))){
return p.cljs$core$async$Pub$unsub_all_STAR_$arity$1(p);
} else {
return cljs$core$async$Pub$unsub_all_STAR_$dyn_14917(p);
}
}));

(cljs.core.async.unsub_all_STAR_.cljs$core$IFn$_invoke$arity$2 = (function (p,v){
if((((!((p == null)))) && ((!((p.cljs$core$async$Pub$unsub_all_STAR_$arity$2 == null)))))){
return p.cljs$core$async$Pub$unsub_all_STAR_$arity$2(p,v);
} else {
return cljs$core$async$Pub$unsub_all_STAR_$dyn_14917(p,v);
}
}));

(cljs.core.async.unsub_all_STAR_.cljs$lang$maxFixedArity = 2);



/**
* @constructor
 * @implements {cljs.core.async.Pub}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.async.Mux}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async13744 = (function (ch,topic_fn,buf_fn,mults,ensure_mult,meta13745){
this.ch = ch;
this.topic_fn = topic_fn;
this.buf_fn = buf_fn;
this.mults = mults;
this.ensure_mult = ensure_mult;
this.meta13745 = meta13745;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13746,meta13745__$1){
var self__ = this;
var _13746__$1 = this;
return (new cljs.core.async.t_cljs$core$async13744(self__.ch,self__.topic_fn,self__.buf_fn,self__.mults,self__.ensure_mult,meta13745__$1));
}));

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13746){
var self__ = this;
var _13746__$1 = this;
return self__.meta13745;
}));

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Mux$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Mux$muxch_STAR_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.ch;
}));

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Pub$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Pub$sub_STAR_$arity$4 = (function (p,topic,ch__$1,close_QMARK_){
var self__ = this;
var p__$1 = this;
var m = (self__.ensure_mult.cljs$core$IFn$_invoke$arity$1 ? self__.ensure_mult.cljs$core$IFn$_invoke$arity$1(topic) : self__.ensure_mult.call(null,topic));
return cljs.core.async.tap.cljs$core$IFn$_invoke$arity$3(m,ch__$1,close_QMARK_);
}));

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Pub$unsub_STAR_$arity$3 = (function (p,topic,ch__$1){
var self__ = this;
var p__$1 = this;
var temp__5804__auto__ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(cljs.core.deref(self__.mults),topic);
if(cljs.core.truth_(temp__5804__auto__)){
var m = temp__5804__auto__;
return cljs.core.async.untap(m,ch__$1);
} else {
return null;
}
}));

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Pub$unsub_all_STAR_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.reset_BANG_(self__.mults,cljs.core.PersistentArrayMap.EMPTY);
}));

(cljs.core.async.t_cljs$core$async13744.prototype.cljs$core$async$Pub$unsub_all_STAR_$arity$2 = (function (_,topic){
var self__ = this;
var ___$1 = this;
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(self__.mults,cljs.core.dissoc,topic);
}));

(cljs.core.async.t_cljs$core$async13744.getBasis = (function (){
return new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"ch","ch",1085813622,null),new cljs.core.Symbol(null,"topic-fn","topic-fn",-862449736,null),new cljs.core.Symbol(null,"buf-fn","buf-fn",-1200281591,null),new cljs.core.Symbol(null,"mults","mults",-461114485,null),new cljs.core.Symbol(null,"ensure-mult","ensure-mult",1796584816,null),new cljs.core.Symbol(null,"meta13745","meta13745",-495813522,null)], null);
}));

(cljs.core.async.t_cljs$core$async13744.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async13744.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async13744");

(cljs.core.async.t_cljs$core$async13744.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async13744");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async13744.
 */
cljs.core.async.__GT_t_cljs$core$async13744 = (function cljs$core$async$__GT_t_cljs$core$async13744(ch,topic_fn,buf_fn,mults,ensure_mult,meta13745){
return (new cljs.core.async.t_cljs$core$async13744(ch,topic_fn,buf_fn,mults,ensure_mult,meta13745));
});


/**
 * Creates and returns a pub(lication) of the supplied channel,
 *   partitioned into topics by the topic-fn. topic-fn will be applied to
 *   each value on the channel and the result will determine the 'topic'
 *   on which that value will be put. Channels can be subscribed to
 *   receive copies of topics using 'sub', and unsubscribed using
 *   'unsub'. Each topic will be handled by an internal mult on a
 *   dedicated channel. By default these internal channels are
 *   unbuffered, but a buf-fn can be supplied which, given a topic,
 *   creates a buffer with desired properties.
 * 
 *   Each item is distributed to all subs in parallel and synchronously,
 *   i.e. each sub must accept before the next item is distributed. Use
 *   buffering/windowing to prevent slow subs from holding up the pub.
 * 
 *   Items received when there are no matching subs get dropped.
 * 
 *   Note that if buf-fns are used then each topic is handled
 *   asynchronously, i.e. if a channel is subscribed to more than one
 *   topic it should not expect them to be interleaved identically with
 *   the source.
 */
cljs.core.async.pub = (function cljs$core$async$pub(var_args){
var G__13743 = arguments.length;
switch (G__13743) {
case 2:
return cljs.core.async.pub.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.pub.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.pub.cljs$core$IFn$_invoke$arity$2 = (function (ch,topic_fn){
return cljs.core.async.pub.cljs$core$IFn$_invoke$arity$3(ch,topic_fn,cljs.core.constantly(null));
}));

(cljs.core.async.pub.cljs$core$IFn$_invoke$arity$3 = (function (ch,topic_fn,buf_fn){
var mults = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var ensure_mult = (function (topic){
var or__5002__auto__ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(cljs.core.deref(mults),topic);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return cljs.core.get.cljs$core$IFn$_invoke$arity$2(cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(mults,(function (p1__13741_SHARP_){
if(cljs.core.truth_((p1__13741_SHARP_.cljs$core$IFn$_invoke$arity$1 ? p1__13741_SHARP_.cljs$core$IFn$_invoke$arity$1(topic) : p1__13741_SHARP_.call(null,topic)))){
return p1__13741_SHARP_;
} else {
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(p1__13741_SHARP_,topic,cljs.core.async.mult(cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((buf_fn.cljs$core$IFn$_invoke$arity$1 ? buf_fn.cljs$core$IFn$_invoke$arity$1(topic) : buf_fn.call(null,topic)))));
}
})),topic);
}
});
var p = (new cljs.core.async.t_cljs$core$async13744(ch,topic_fn,buf_fn,mults,ensure_mult,cljs.core.PersistentArrayMap.EMPTY));
var c__12922__auto___14929 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13818){
var state_val_13819 = (state_13818[(1)]);
if((state_val_13819 === (7))){
var inst_13814 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
var statearr_13820_14930 = state_13818__$1;
(statearr_13820_14930[(2)] = inst_13814);

(statearr_13820_14930[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (20))){
var state_13818__$1 = state_13818;
var statearr_13821_14931 = state_13818__$1;
(statearr_13821_14931[(2)] = null);

(statearr_13821_14931[(1)] = (21));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (1))){
var state_13818__$1 = state_13818;
var statearr_13822_14932 = state_13818__$1;
(statearr_13822_14932[(2)] = null);

(statearr_13822_14932[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (24))){
var inst_13797 = (state_13818[(7)]);
var inst_13806 = cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(mults,cljs.core.dissoc,inst_13797);
var state_13818__$1 = state_13818;
var statearr_13823_14933 = state_13818__$1;
(statearr_13823_14933[(2)] = inst_13806);

(statearr_13823_14933[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (4))){
var inst_13749 = (state_13818[(8)]);
var inst_13749__$1 = (state_13818[(2)]);
var inst_13750 = (inst_13749__$1 == null);
var state_13818__$1 = (function (){var statearr_13824 = state_13818;
(statearr_13824[(8)] = inst_13749__$1);

return statearr_13824;
})();
if(cljs.core.truth_(inst_13750)){
var statearr_13825_14934 = state_13818__$1;
(statearr_13825_14934[(1)] = (5));

} else {
var statearr_13826_14935 = state_13818__$1;
(statearr_13826_14935[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (15))){
var inst_13791 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
var statearr_13827_14936 = state_13818__$1;
(statearr_13827_14936[(2)] = inst_13791);

(statearr_13827_14936[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (21))){
var inst_13811 = (state_13818[(2)]);
var state_13818__$1 = (function (){var statearr_13828 = state_13818;
(statearr_13828[(9)] = inst_13811);

return statearr_13828;
})();
var statearr_13829_14937 = state_13818__$1;
(statearr_13829_14937[(2)] = null);

(statearr_13829_14937[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (13))){
var inst_13773 = (state_13818[(10)]);
var inst_13775 = cljs.core.chunked_seq_QMARK_(inst_13773);
var state_13818__$1 = state_13818;
if(inst_13775){
var statearr_13830_14938 = state_13818__$1;
(statearr_13830_14938[(1)] = (16));

} else {
var statearr_13831_14939 = state_13818__$1;
(statearr_13831_14939[(1)] = (17));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (22))){
var inst_13803 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
if(cljs.core.truth_(inst_13803)){
var statearr_13832_14942 = state_13818__$1;
(statearr_13832_14942[(1)] = (23));

} else {
var statearr_13833_14943 = state_13818__$1;
(statearr_13833_14943[(1)] = (24));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (6))){
var inst_13797 = (state_13818[(7)]);
var inst_13799 = (state_13818[(11)]);
var inst_13749 = (state_13818[(8)]);
var inst_13797__$1 = (topic_fn.cljs$core$IFn$_invoke$arity$1 ? topic_fn.cljs$core$IFn$_invoke$arity$1(inst_13749) : topic_fn.call(null,inst_13749));
var inst_13798 = cljs.core.deref(mults);
var inst_13799__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(inst_13798,inst_13797__$1);
var state_13818__$1 = (function (){var statearr_13834 = state_13818;
(statearr_13834[(7)] = inst_13797__$1);

(statearr_13834[(11)] = inst_13799__$1);

return statearr_13834;
})();
if(cljs.core.truth_(inst_13799__$1)){
var statearr_13835_14946 = state_13818__$1;
(statearr_13835_14946[(1)] = (19));

} else {
var statearr_13836_14947 = state_13818__$1;
(statearr_13836_14947[(1)] = (20));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (25))){
var inst_13808 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
var statearr_13837_14948 = state_13818__$1;
(statearr_13837_14948[(2)] = inst_13808);

(statearr_13837_14948[(1)] = (21));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (17))){
var inst_13773 = (state_13818[(10)]);
var inst_13782 = cljs.core.first(inst_13773);
var inst_13783 = cljs.core.async.muxch_STAR_(inst_13782);
var inst_13784 = cljs.core.async.close_BANG_(inst_13783);
var inst_13785 = cljs.core.next(inst_13773);
var inst_13759 = inst_13785;
var inst_13760 = null;
var inst_13761 = (0);
var inst_13762 = (0);
var state_13818__$1 = (function (){var statearr_13838 = state_13818;
(statearr_13838[(12)] = inst_13761);

(statearr_13838[(13)] = inst_13760);

(statearr_13838[(14)] = inst_13762);

(statearr_13838[(15)] = inst_13784);

(statearr_13838[(16)] = inst_13759);

return statearr_13838;
})();
var statearr_13839_14949 = state_13818__$1;
(statearr_13839_14949[(2)] = null);

(statearr_13839_14949[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (3))){
var inst_13816 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13818__$1,inst_13816);
} else {
if((state_val_13819 === (12))){
var inst_13793 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
var statearr_13840_14950 = state_13818__$1;
(statearr_13840_14950[(2)] = inst_13793);

(statearr_13840_14950[(1)] = (9));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (2))){
var state_13818__$1 = state_13818;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13818__$1,(4),ch);
} else {
if((state_val_13819 === (23))){
var state_13818__$1 = state_13818;
var statearr_13841_14951 = state_13818__$1;
(statearr_13841_14951[(2)] = null);

(statearr_13841_14951[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (19))){
var inst_13799 = (state_13818[(11)]);
var inst_13749 = (state_13818[(8)]);
var inst_13801 = cljs.core.async.muxch_STAR_(inst_13799);
var state_13818__$1 = state_13818;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13818__$1,(22),inst_13801,inst_13749);
} else {
if((state_val_13819 === (11))){
var inst_13773 = (state_13818[(10)]);
var inst_13759 = (state_13818[(16)]);
var inst_13773__$1 = cljs.core.seq(inst_13759);
var state_13818__$1 = (function (){var statearr_13842 = state_13818;
(statearr_13842[(10)] = inst_13773__$1);

return statearr_13842;
})();
if(inst_13773__$1){
var statearr_13843_14952 = state_13818__$1;
(statearr_13843_14952[(1)] = (13));

} else {
var statearr_13844_14953 = state_13818__$1;
(statearr_13844_14953[(1)] = (14));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (9))){
var inst_13795 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
var statearr_13845_14954 = state_13818__$1;
(statearr_13845_14954[(2)] = inst_13795);

(statearr_13845_14954[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (5))){
var inst_13756 = cljs.core.deref(mults);
var inst_13757 = cljs.core.vals(inst_13756);
var inst_13758 = cljs.core.seq(inst_13757);
var inst_13759 = inst_13758;
var inst_13760 = null;
var inst_13761 = (0);
var inst_13762 = (0);
var state_13818__$1 = (function (){var statearr_13846 = state_13818;
(statearr_13846[(12)] = inst_13761);

(statearr_13846[(13)] = inst_13760);

(statearr_13846[(14)] = inst_13762);

(statearr_13846[(16)] = inst_13759);

return statearr_13846;
})();
var statearr_13847_14964 = state_13818__$1;
(statearr_13847_14964[(2)] = null);

(statearr_13847_14964[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (14))){
var state_13818__$1 = state_13818;
var statearr_13851_14965 = state_13818__$1;
(statearr_13851_14965[(2)] = null);

(statearr_13851_14965[(1)] = (15));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (16))){
var inst_13773 = (state_13818[(10)]);
var inst_13777 = cljs.core.chunk_first(inst_13773);
var inst_13778 = cljs.core.chunk_rest(inst_13773);
var inst_13779 = cljs.core.count(inst_13777);
var inst_13759 = inst_13778;
var inst_13760 = inst_13777;
var inst_13761 = inst_13779;
var inst_13762 = (0);
var state_13818__$1 = (function (){var statearr_13852 = state_13818;
(statearr_13852[(12)] = inst_13761);

(statearr_13852[(13)] = inst_13760);

(statearr_13852[(14)] = inst_13762);

(statearr_13852[(16)] = inst_13759);

return statearr_13852;
})();
var statearr_13853_14966 = state_13818__$1;
(statearr_13853_14966[(2)] = null);

(statearr_13853_14966[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (10))){
var inst_13761 = (state_13818[(12)]);
var inst_13760 = (state_13818[(13)]);
var inst_13762 = (state_13818[(14)]);
var inst_13759 = (state_13818[(16)]);
var inst_13767 = cljs.core._nth(inst_13760,inst_13762);
var inst_13768 = cljs.core.async.muxch_STAR_(inst_13767);
var inst_13769 = cljs.core.async.close_BANG_(inst_13768);
var inst_13770 = (inst_13762 + (1));
var tmp13848 = inst_13761;
var tmp13849 = inst_13760;
var tmp13850 = inst_13759;
var inst_13759__$1 = tmp13850;
var inst_13760__$1 = tmp13849;
var inst_13761__$1 = tmp13848;
var inst_13762__$1 = inst_13770;
var state_13818__$1 = (function (){var statearr_13854 = state_13818;
(statearr_13854[(12)] = inst_13761__$1);

(statearr_13854[(13)] = inst_13760__$1);

(statearr_13854[(17)] = inst_13769);

(statearr_13854[(14)] = inst_13762__$1);

(statearr_13854[(16)] = inst_13759__$1);

return statearr_13854;
})();
var statearr_13855_14967 = state_13818__$1;
(statearr_13855_14967[(2)] = null);

(statearr_13855_14967[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (18))){
var inst_13788 = (state_13818[(2)]);
var state_13818__$1 = state_13818;
var statearr_13856_14968 = state_13818__$1;
(statearr_13856_14968[(2)] = inst_13788);

(statearr_13856_14968[(1)] = (15));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13819 === (8))){
var inst_13761 = (state_13818[(12)]);
var inst_13762 = (state_13818[(14)]);
var inst_13764 = (inst_13762 < inst_13761);
var inst_13765 = inst_13764;
var state_13818__$1 = state_13818;
if(cljs.core.truth_(inst_13765)){
var statearr_13857_14969 = state_13818__$1;
(statearr_13857_14969[(1)] = (10));

} else {
var statearr_13858_14970 = state_13818__$1;
(statearr_13858_14970[(1)] = (11));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_13859 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_13859[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_13859[(1)] = (1));

return statearr_13859;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_13818){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13818);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13860){var ex__12826__auto__ = e13860;
var statearr_13861_14972 = state_13818;
(statearr_13861_14972[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13818[(4)]))){
var statearr_13862_14973 = state_13818;
(statearr_13862_14973[(1)] = cljs.core.first((state_13818[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__14975 = state_13818;
state_13818 = G__14975;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_13818){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_13818);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13863 = f__12923__auto__();
(statearr_13863[(6)] = c__12922__auto___14929);

return statearr_13863;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return p;
}));

(cljs.core.async.pub.cljs$lang$maxFixedArity = 3);

/**
 * Subscribes a channel to a topic of a pub.
 * 
 *   By default the channel will be closed when the source closes,
 *   but can be determined by the close? parameter.
 */
cljs.core.async.sub = (function cljs$core$async$sub(var_args){
var G__13865 = arguments.length;
switch (G__13865) {
case 3:
return cljs.core.async.sub.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
case 4:
return cljs.core.async.sub.cljs$core$IFn$_invoke$arity$4((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.sub.cljs$core$IFn$_invoke$arity$3 = (function (p,topic,ch){
return cljs.core.async.sub.cljs$core$IFn$_invoke$arity$4(p,topic,ch,true);
}));

(cljs.core.async.sub.cljs$core$IFn$_invoke$arity$4 = (function (p,topic,ch,close_QMARK_){
return cljs.core.async.sub_STAR_(p,topic,ch,close_QMARK_);
}));

(cljs.core.async.sub.cljs$lang$maxFixedArity = 4);

/**
 * Unsubscribes a channel from a topic of a pub
 */
cljs.core.async.unsub = (function cljs$core$async$unsub(p,topic,ch){
return cljs.core.async.unsub_STAR_(p,topic,ch);
});
/**
 * Unsubscribes all channels from a pub, or a topic of a pub
 */
cljs.core.async.unsub_all = (function cljs$core$async$unsub_all(var_args){
var G__13867 = arguments.length;
switch (G__13867) {
case 1:
return cljs.core.async.unsub_all.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.unsub_all.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.unsub_all.cljs$core$IFn$_invoke$arity$1 = (function (p){
return cljs.core.async.unsub_all_STAR_(p);
}));

(cljs.core.async.unsub_all.cljs$core$IFn$_invoke$arity$2 = (function (p,topic){
return cljs.core.async.unsub_all_STAR_(p,topic);
}));

(cljs.core.async.unsub_all.cljs$lang$maxFixedArity = 2);

/**
 * Takes a function and a collection of source channels, and returns a
 *   channel which contains the values produced by applying f to the set
 *   of first items taken from each source channel, followed by applying
 *   f to the set of second items from each channel, until any one of the
 *   channels is closed, at which point the output channel will be
 *   closed. The returned channel will be unbuffered by default, or a
 *   buf-or-n can be supplied
 */
cljs.core.async.map = (function cljs$core$async$map(var_args){
var G__13869 = arguments.length;
switch (G__13869) {
case 2:
return cljs.core.async.map.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.map.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.map.cljs$core$IFn$_invoke$arity$2 = (function (f,chs){
return cljs.core.async.map.cljs$core$IFn$_invoke$arity$3(f,chs,null);
}));

(cljs.core.async.map.cljs$core$IFn$_invoke$arity$3 = (function (f,chs,buf_or_n){
var chs__$1 = cljs.core.vec(chs);
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var cnt = cljs.core.count(chs__$1);
var rets = cljs.core.object_array.cljs$core$IFn$_invoke$arity$1(cnt);
var dchan = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
var dctr = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(null);
var done = cljs.core.mapv.cljs$core$IFn$_invoke$arity$2((function (i){
return (function (ret){
(rets[i] = ret);

if((cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(dctr,cljs.core.dec) === (0))){
return cljs.core.async.put_BANG_.cljs$core$IFn$_invoke$arity$2(dchan,rets.slice((0)));
} else {
return null;
}
});
}),cljs.core.range.cljs$core$IFn$_invoke$arity$1(cnt));
if((cnt === (0))){
cljs.core.async.close_BANG_(out);
} else {
var c__12922__auto___14980 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13912){
var state_val_13913 = (state_13912[(1)]);
if((state_val_13913 === (7))){
var state_13912__$1 = state_13912;
var statearr_13914_14981 = state_13912__$1;
(statearr_13914_14981[(2)] = null);

(statearr_13914_14981[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (1))){
var state_13912__$1 = state_13912;
var statearr_13915_14982 = state_13912__$1;
(statearr_13915_14982[(2)] = null);

(statearr_13915_14982[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (4))){
var inst_13872 = (state_13912[(7)]);
var inst_13873 = (state_13912[(8)]);
var inst_13875 = (inst_13873 < inst_13872);
var state_13912__$1 = state_13912;
if(cljs.core.truth_(inst_13875)){
var statearr_13916_14984 = state_13912__$1;
(statearr_13916_14984[(1)] = (6));

} else {
var statearr_13917_14985 = state_13912__$1;
(statearr_13917_14985[(1)] = (7));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (15))){
var inst_13898 = (state_13912[(9)]);
var inst_13903 = cljs.core.apply.cljs$core$IFn$_invoke$arity$2(f,inst_13898);
var state_13912__$1 = state_13912;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13912__$1,(17),out,inst_13903);
} else {
if((state_val_13913 === (13))){
var inst_13898 = (state_13912[(9)]);
var inst_13898__$1 = (state_13912[(2)]);
var inst_13899 = cljs.core.some(cljs.core.nil_QMARK_,inst_13898__$1);
var state_13912__$1 = (function (){var statearr_13918 = state_13912;
(statearr_13918[(9)] = inst_13898__$1);

return statearr_13918;
})();
if(cljs.core.truth_(inst_13899)){
var statearr_13919_14986 = state_13912__$1;
(statearr_13919_14986[(1)] = (14));

} else {
var statearr_13920_14987 = state_13912__$1;
(statearr_13920_14987[(1)] = (15));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (6))){
var state_13912__$1 = state_13912;
var statearr_13921_14988 = state_13912__$1;
(statearr_13921_14988[(2)] = null);

(statearr_13921_14988[(1)] = (9));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (17))){
var inst_13905 = (state_13912[(2)]);
var state_13912__$1 = (function (){var statearr_13923 = state_13912;
(statearr_13923[(10)] = inst_13905);

return statearr_13923;
})();
var statearr_13924_14989 = state_13912__$1;
(statearr_13924_14989[(2)] = null);

(statearr_13924_14989[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (3))){
var inst_13910 = (state_13912[(2)]);
var state_13912__$1 = state_13912;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13912__$1,inst_13910);
} else {
if((state_val_13913 === (12))){
var _ = (function (){var statearr_13925 = state_13912;
(statearr_13925[(4)] = cljs.core.rest((state_13912[(4)])));

return statearr_13925;
})();
var state_13912__$1 = state_13912;
var ex13922 = (state_13912__$1[(2)]);
var statearr_13926_14991 = state_13912__$1;
(statearr_13926_14991[(5)] = ex13922);


if((ex13922 instanceof Object)){
var statearr_13927_14992 = state_13912__$1;
(statearr_13927_14992[(1)] = (11));

(statearr_13927_14992[(5)] = null);

} else {
throw ex13922;

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (2))){
var inst_13871 = cljs.core.reset_BANG_(dctr,cnt);
var inst_13872 = cnt;
var inst_13873 = (0);
var state_13912__$1 = (function (){var statearr_13928 = state_13912;
(statearr_13928[(7)] = inst_13872);

(statearr_13928[(8)] = inst_13873);

(statearr_13928[(11)] = inst_13871);

return statearr_13928;
})();
var statearr_13929_14993 = state_13912__$1;
(statearr_13929_14993[(2)] = null);

(statearr_13929_14993[(1)] = (4));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (11))){
var inst_13877 = (state_13912[(2)]);
var inst_13878 = cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(dctr,cljs.core.dec);
var state_13912__$1 = (function (){var statearr_13930 = state_13912;
(statearr_13930[(12)] = inst_13877);

return statearr_13930;
})();
var statearr_13931_14994 = state_13912__$1;
(statearr_13931_14994[(2)] = inst_13878);

(statearr_13931_14994[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (9))){
var inst_13873 = (state_13912[(8)]);
var _ = (function (){var statearr_13932 = state_13912;
(statearr_13932[(4)] = cljs.core.cons((12),(state_13912[(4)])));

return statearr_13932;
})();
var inst_13884 = (chs__$1.cljs$core$IFn$_invoke$arity$1 ? chs__$1.cljs$core$IFn$_invoke$arity$1(inst_13873) : chs__$1.call(null,inst_13873));
var inst_13885 = (done.cljs$core$IFn$_invoke$arity$1 ? done.cljs$core$IFn$_invoke$arity$1(inst_13873) : done.call(null,inst_13873));
var inst_13886 = cljs.core.async.take_BANG_.cljs$core$IFn$_invoke$arity$2(inst_13884,inst_13885);
var ___$1 = (function (){var statearr_13933 = state_13912;
(statearr_13933[(4)] = cljs.core.rest((state_13912[(4)])));

return statearr_13933;
})();
var state_13912__$1 = state_13912;
var statearr_13934_14995 = state_13912__$1;
(statearr_13934_14995[(2)] = inst_13886);

(statearr_13934_14995[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (5))){
var inst_13896 = (state_13912[(2)]);
var state_13912__$1 = (function (){var statearr_13935 = state_13912;
(statearr_13935[(13)] = inst_13896);

return statearr_13935;
})();
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_13912__$1,(13),dchan);
} else {
if((state_val_13913 === (14))){
var inst_13901 = cljs.core.async.close_BANG_(out);
var state_13912__$1 = state_13912;
var statearr_13936_14997 = state_13912__$1;
(statearr_13936_14997[(2)] = inst_13901);

(statearr_13936_14997[(1)] = (16));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (16))){
var inst_13908 = (state_13912[(2)]);
var state_13912__$1 = state_13912;
var statearr_13937_14998 = state_13912__$1;
(statearr_13937_14998[(2)] = inst_13908);

(statearr_13937_14998[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (10))){
var inst_13873 = (state_13912[(8)]);
var inst_13889 = (state_13912[(2)]);
var inst_13890 = (inst_13873 + (1));
var inst_13873__$1 = inst_13890;
var state_13912__$1 = (function (){var statearr_13938 = state_13912;
(statearr_13938[(14)] = inst_13889);

(statearr_13938[(8)] = inst_13873__$1);

return statearr_13938;
})();
var statearr_13939_14999 = state_13912__$1;
(statearr_13939_14999[(2)] = null);

(statearr_13939_14999[(1)] = (4));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13913 === (8))){
var inst_13894 = (state_13912[(2)]);
var state_13912__$1 = state_13912;
var statearr_13940_15000 = state_13912__$1;
(statearr_13940_15000[(2)] = inst_13894);

(statearr_13940_15000[(1)] = (5));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_13941 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_13941[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_13941[(1)] = (1));

return statearr_13941;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_13912){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13912);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13942){var ex__12826__auto__ = e13942;
var statearr_13943_15001 = state_13912;
(statearr_13943_15001[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13912[(4)]))){
var statearr_13944_15002 = state_13912;
(statearr_13944_15002[(1)] = cljs.core.first((state_13912[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15003 = state_13912;
state_13912 = G__15003;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_13912){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_13912);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_13945 = f__12923__auto__();
(statearr_13945[(6)] = c__12922__auto___14980);

return statearr_13945;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));

}

return out;
}));

(cljs.core.async.map.cljs$lang$maxFixedArity = 3);

/**
 * Takes a collection of source channels and returns a channel which
 *   contains all values taken from them. The returned channel will be
 *   unbuffered by default, or a buf-or-n can be supplied. The channel
 *   will close after all the source channels have closed.
 */
cljs.core.async.merge = (function cljs$core$async$merge(var_args){
var G__13948 = arguments.length;
switch (G__13948) {
case 1:
return cljs.core.async.merge.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.merge.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.merge.cljs$core$IFn$_invoke$arity$1 = (function (chs){
return cljs.core.async.merge.cljs$core$IFn$_invoke$arity$2(chs,null);
}));

(cljs.core.async.merge.cljs$core$IFn$_invoke$arity$2 = (function (chs,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var c__12922__auto___15010 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_13980){
var state_val_13981 = (state_13980[(1)]);
if((state_val_13981 === (7))){
var inst_13960 = (state_13980[(7)]);
var inst_13959 = (state_13980[(8)]);
var inst_13959__$1 = (state_13980[(2)]);
var inst_13960__$1 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13959__$1,(0),null);
var inst_13961 = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(inst_13959__$1,(1),null);
var inst_13962 = (inst_13960__$1 == null);
var state_13980__$1 = (function (){var statearr_13982 = state_13980;
(statearr_13982[(7)] = inst_13960__$1);

(statearr_13982[(8)] = inst_13959__$1);

(statearr_13982[(9)] = inst_13961);

return statearr_13982;
})();
if(cljs.core.truth_(inst_13962)){
var statearr_13983_15011 = state_13980__$1;
(statearr_13983_15011[(1)] = (8));

} else {
var statearr_13984_15012 = state_13980__$1;
(statearr_13984_15012[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (1))){
var inst_13949 = cljs.core.vec(chs);
var inst_13950 = inst_13949;
var state_13980__$1 = (function (){var statearr_13985 = state_13980;
(statearr_13985[(10)] = inst_13950);

return statearr_13985;
})();
var statearr_13986_15013 = state_13980__$1;
(statearr_13986_15013[(2)] = null);

(statearr_13986_15013[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (4))){
var inst_13950 = (state_13980[(10)]);
var state_13980__$1 = state_13980;
return cljs.core.async.ioc_alts_BANG_(state_13980__$1,(7),inst_13950);
} else {
if((state_val_13981 === (6))){
var inst_13976 = (state_13980[(2)]);
var state_13980__$1 = state_13980;
var statearr_13987_15015 = state_13980__$1;
(statearr_13987_15015[(2)] = inst_13976);

(statearr_13987_15015[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (3))){
var inst_13978 = (state_13980[(2)]);
var state_13980__$1 = state_13980;
return cljs.core.async.impl.ioc_helpers.return_chan(state_13980__$1,inst_13978);
} else {
if((state_val_13981 === (2))){
var inst_13950 = (state_13980[(10)]);
var inst_13952 = cljs.core.count(inst_13950);
var inst_13953 = (inst_13952 > (0));
var state_13980__$1 = state_13980;
if(cljs.core.truth_(inst_13953)){
var statearr_13989_15016 = state_13980__$1;
(statearr_13989_15016[(1)] = (4));

} else {
var statearr_13990_15017 = state_13980__$1;
(statearr_13990_15017[(1)] = (5));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (11))){
var inst_13950 = (state_13980[(10)]);
var inst_13969 = (state_13980[(2)]);
var tmp13988 = inst_13950;
var inst_13950__$1 = tmp13988;
var state_13980__$1 = (function (){var statearr_13991 = state_13980;
(statearr_13991[(11)] = inst_13969);

(statearr_13991[(10)] = inst_13950__$1);

return statearr_13991;
})();
var statearr_13992_15019 = state_13980__$1;
(statearr_13992_15019[(2)] = null);

(statearr_13992_15019[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (9))){
var inst_13960 = (state_13980[(7)]);
var state_13980__$1 = state_13980;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_13980__$1,(11),out,inst_13960);
} else {
if((state_val_13981 === (5))){
var inst_13974 = cljs.core.async.close_BANG_(out);
var state_13980__$1 = state_13980;
var statearr_13993_15020 = state_13980__$1;
(statearr_13993_15020[(2)] = inst_13974);

(statearr_13993_15020[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (10))){
var inst_13972 = (state_13980[(2)]);
var state_13980__$1 = state_13980;
var statearr_13994_15021 = state_13980__$1;
(statearr_13994_15021[(2)] = inst_13972);

(statearr_13994_15021[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_13981 === (8))){
var inst_13960 = (state_13980[(7)]);
var inst_13950 = (state_13980[(10)]);
var inst_13959 = (state_13980[(8)]);
var inst_13961 = (state_13980[(9)]);
var inst_13964 = (function (){var cs = inst_13950;
var vec__13955 = inst_13959;
var v = inst_13960;
var c = inst_13961;
return (function (p1__13946_SHARP_){
return cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(c,p1__13946_SHARP_);
});
})();
var inst_13965 = cljs.core.filterv(inst_13964,inst_13950);
var inst_13950__$1 = inst_13965;
var state_13980__$1 = (function (){var statearr_13995 = state_13980;
(statearr_13995[(10)] = inst_13950__$1);

return statearr_13995;
})();
var statearr_13996_15023 = state_13980__$1;
(statearr_13996_15023[(2)] = null);

(statearr_13996_15023[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_13997 = [null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_13997[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_13997[(1)] = (1));

return statearr_13997;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_13980){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_13980);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e13998){var ex__12826__auto__ = e13998;
var statearr_13999_15024 = state_13980;
(statearr_13999_15024[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_13980[(4)]))){
var statearr_14000_15025 = state_13980;
(statearr_14000_15025[(1)] = cljs.core.first((state_13980[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15026 = state_13980;
state_13980 = G__15026;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_13980){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_13980);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14005 = f__12923__auto__();
(statearr_14005[(6)] = c__12922__auto___15010);

return statearr_14005;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return out;
}));

(cljs.core.async.merge.cljs$lang$maxFixedArity = 2);

/**
 * Returns a channel containing the single (collection) result of the
 *   items taken from the channel conjoined to the supplied
 *   collection. ch must close before into produces a result.
 */
cljs.core.async.into = (function cljs$core$async$into(coll,ch){
return cljs.core.async.reduce(cljs.core.conj,coll,ch);
});
/**
 * Returns a channel that will return, at most, n items from ch. After n items
 * have been returned, or ch has been closed, the return chanel will close.
 * 
 *   The output channel is unbuffered by default, unless buf-or-n is given.
 */
cljs.core.async.take = (function cljs$core$async$take(var_args){
var G__14007 = arguments.length;
switch (G__14007) {
case 2:
return cljs.core.async.take.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.take.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.take.cljs$core$IFn$_invoke$arity$2 = (function (n,ch){
return cljs.core.async.take.cljs$core$IFn$_invoke$arity$3(n,ch,null);
}));

(cljs.core.async.take.cljs$core$IFn$_invoke$arity$3 = (function (n,ch,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var c__12922__auto___15031 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_14031){
var state_val_14032 = (state_14031[(1)]);
if((state_val_14032 === (7))){
var inst_14013 = (state_14031[(7)]);
var inst_14013__$1 = (state_14031[(2)]);
var inst_14014 = (inst_14013__$1 == null);
var inst_14015 = cljs.core.not(inst_14014);
var state_14031__$1 = (function (){var statearr_14033 = state_14031;
(statearr_14033[(7)] = inst_14013__$1);

return statearr_14033;
})();
if(inst_14015){
var statearr_14034_15032 = state_14031__$1;
(statearr_14034_15032[(1)] = (8));

} else {
var statearr_14035_15034 = state_14031__$1;
(statearr_14035_15034[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (1))){
var inst_14008 = (0);
var state_14031__$1 = (function (){var statearr_14036 = state_14031;
(statearr_14036[(8)] = inst_14008);

return statearr_14036;
})();
var statearr_14037_15035 = state_14031__$1;
(statearr_14037_15035[(2)] = null);

(statearr_14037_15035[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (4))){
var state_14031__$1 = state_14031;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_14031__$1,(7),ch);
} else {
if((state_val_14032 === (6))){
var inst_14026 = (state_14031[(2)]);
var state_14031__$1 = state_14031;
var statearr_14040_15044 = state_14031__$1;
(statearr_14040_15044[(2)] = inst_14026);

(statearr_14040_15044[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (3))){
var inst_14028 = (state_14031[(2)]);
var inst_14029 = cljs.core.async.close_BANG_(out);
var state_14031__$1 = (function (){var statearr_14041 = state_14031;
(statearr_14041[(9)] = inst_14028);

return statearr_14041;
})();
return cljs.core.async.impl.ioc_helpers.return_chan(state_14031__$1,inst_14029);
} else {
if((state_val_14032 === (2))){
var inst_14008 = (state_14031[(8)]);
var inst_14010 = (inst_14008 < n);
var state_14031__$1 = state_14031;
if(cljs.core.truth_(inst_14010)){
var statearr_14042_15057 = state_14031__$1;
(statearr_14042_15057[(1)] = (4));

} else {
var statearr_14043_15058 = state_14031__$1;
(statearr_14043_15058[(1)] = (5));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (11))){
var inst_14008 = (state_14031[(8)]);
var inst_14018 = (state_14031[(2)]);
var inst_14019 = (inst_14008 + (1));
var inst_14008__$1 = inst_14019;
var state_14031__$1 = (function (){var statearr_14044 = state_14031;
(statearr_14044[(8)] = inst_14008__$1);

(statearr_14044[(10)] = inst_14018);

return statearr_14044;
})();
var statearr_14045_15059 = state_14031__$1;
(statearr_14045_15059[(2)] = null);

(statearr_14045_15059[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (9))){
var state_14031__$1 = state_14031;
var statearr_14046_15060 = state_14031__$1;
(statearr_14046_15060[(2)] = null);

(statearr_14046_15060[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (5))){
var state_14031__$1 = state_14031;
var statearr_14047_15062 = state_14031__$1;
(statearr_14047_15062[(2)] = null);

(statearr_14047_15062[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (10))){
var inst_14023 = (state_14031[(2)]);
var state_14031__$1 = state_14031;
var statearr_14048_15066 = state_14031__$1;
(statearr_14048_15066[(2)] = inst_14023);

(statearr_14048_15066[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14032 === (8))){
var inst_14013 = (state_14031[(7)]);
var state_14031__$1 = state_14031;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14031__$1,(11),out,inst_14013);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_14049 = [null,null,null,null,null,null,null,null,null,null,null];
(statearr_14049[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_14049[(1)] = (1));

return statearr_14049;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_14031){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_14031);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e14050){var ex__12826__auto__ = e14050;
var statearr_14051_15067 = state_14031;
(statearr_14051_15067[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_14031[(4)]))){
var statearr_14052_15068 = state_14031;
(statearr_14052_15068[(1)] = cljs.core.first((state_14031[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15069 = state_14031;
state_14031 = G__15069;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_14031){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_14031);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14055 = f__12923__auto__();
(statearr_14055[(6)] = c__12922__auto___15031);

return statearr_14055;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return out;
}));

(cljs.core.async.take.cljs$lang$maxFixedArity = 3);


/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Handler}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async14065 = (function (f,ch,meta14062,_,fn1,meta14066){
this.f = f;
this.ch = ch;
this.meta14062 = meta14062;
this._ = _;
this.fn1 = fn1;
this.meta14066 = meta14066;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async14065.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_14067,meta14066__$1){
var self__ = this;
var _14067__$1 = this;
return (new cljs.core.async.t_cljs$core$async14065(self__.f,self__.ch,self__.meta14062,self__._,self__.fn1,meta14066__$1));
}));

(cljs.core.async.t_cljs$core$async14065.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_14067){
var self__ = this;
var _14067__$1 = this;
return self__.meta14066;
}));

(cljs.core.async.t_cljs$core$async14065.prototype.cljs$core$async$impl$protocols$Handler$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14065.prototype.cljs$core$async$impl$protocols$Handler$active_QMARK_$arity$1 = (function (___$1){
var self__ = this;
var ___$2 = this;
return cljs.core.async.impl.protocols.active_QMARK_(self__.fn1);
}));

(cljs.core.async.t_cljs$core$async14065.prototype.cljs$core$async$impl$protocols$Handler$blockable_QMARK_$arity$1 = (function (___$1){
var self__ = this;
var ___$2 = this;
return true;
}));

(cljs.core.async.t_cljs$core$async14065.prototype.cljs$core$async$impl$protocols$Handler$commit$arity$1 = (function (___$1){
var self__ = this;
var ___$2 = this;
var f1 = cljs.core.async.impl.protocols.commit(self__.fn1);
return (function (p1__14056_SHARP_){
var G__14074 = (((p1__14056_SHARP_ == null))?null:(self__.f.cljs$core$IFn$_invoke$arity$1 ? self__.f.cljs$core$IFn$_invoke$arity$1(p1__14056_SHARP_) : self__.f.call(null,p1__14056_SHARP_)));
return (f1.cljs$core$IFn$_invoke$arity$1 ? f1.cljs$core$IFn$_invoke$arity$1(G__14074) : f1.call(null,G__14074));
});
}));

(cljs.core.async.t_cljs$core$async14065.getBasis = (function (){
return new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"f","f",43394975,null),new cljs.core.Symbol(null,"ch","ch",1085813622,null),new cljs.core.Symbol(null,"meta14062","meta14062",-247353854,null),cljs.core.with_meta(new cljs.core.Symbol(null,"_","_",-1201019570,null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"tag","tag",-1290361223),new cljs.core.Symbol("cljs.core.async","t_cljs$core$async14061","cljs.core.async/t_cljs$core$async14061",-292216976,null)], null)),new cljs.core.Symbol(null,"fn1","fn1",895834444,null),new cljs.core.Symbol(null,"meta14066","meta14066",1669642240,null)], null);
}));

(cljs.core.async.t_cljs$core$async14065.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async14065.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async14065");

(cljs.core.async.t_cljs$core$async14065.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async14065");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async14065.
 */
cljs.core.async.__GT_t_cljs$core$async14065 = (function cljs$core$async$__GT_t_cljs$core$async14065(f,ch,meta14062,_,fn1,meta14066){
return (new cljs.core.async.t_cljs$core$async14065(f,ch,meta14062,_,fn1,meta14066));
});



/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Channel}
 * @implements {cljs.core.async.impl.protocols.WritePort}
 * @implements {cljs.core.async.impl.protocols.ReadPort}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async14061 = (function (f,ch,meta14062){
this.f = f;
this.ch = ch;
this.meta14062 = meta14062;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_14063,meta14062__$1){
var self__ = this;
var _14063__$1 = this;
return (new cljs.core.async.t_cljs$core$async14061(self__.f,self__.ch,meta14062__$1));
}));

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_14063){
var self__ = this;
var _14063__$1 = this;
return self__.meta14062;
}));

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$Channel$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$Channel$close_BANG_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.close_BANG_(self__.ch);
}));

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$Channel$closed_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.closed_QMARK_(self__.ch);
}));

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$ReadPort$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$ReadPort$take_BANG_$arity$2 = (function (_,fn1){
var self__ = this;
var ___$1 = this;
var ret = cljs.core.async.impl.protocols.take_BANG_(self__.ch,(new cljs.core.async.t_cljs$core$async14065(self__.f,self__.ch,self__.meta14062,___$1,fn1,cljs.core.PersistentArrayMap.EMPTY)));
if(cljs.core.truth_((function (){var and__5000__auto__ = ret;
if(cljs.core.truth_(and__5000__auto__)){
return (!((cljs.core.deref(ret) == null)));
} else {
return and__5000__auto__;
}
})())){
return cljs.core.async.impl.channels.box((function (){var G__14078 = cljs.core.deref(ret);
return (self__.f.cljs$core$IFn$_invoke$arity$1 ? self__.f.cljs$core$IFn$_invoke$arity$1(G__14078) : self__.f.call(null,G__14078));
})());
} else {
return ret;
}
}));

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$WritePort$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14061.prototype.cljs$core$async$impl$protocols$WritePort$put_BANG_$arity$3 = (function (_,val,fn1){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.put_BANG_(self__.ch,val,fn1);
}));

(cljs.core.async.t_cljs$core$async14061.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"f","f",43394975,null),new cljs.core.Symbol(null,"ch","ch",1085813622,null),new cljs.core.Symbol(null,"meta14062","meta14062",-247353854,null)], null);
}));

(cljs.core.async.t_cljs$core$async14061.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async14061.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async14061");

(cljs.core.async.t_cljs$core$async14061.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async14061");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async14061.
 */
cljs.core.async.__GT_t_cljs$core$async14061 = (function cljs$core$async$__GT_t_cljs$core$async14061(f,ch,meta14062){
return (new cljs.core.async.t_cljs$core$async14061(f,ch,meta14062));
});


/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.map_LT_ = (function cljs$core$async$map_LT_(f,ch){
return (new cljs.core.async.t_cljs$core$async14061(f,ch,cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Channel}
 * @implements {cljs.core.async.impl.protocols.WritePort}
 * @implements {cljs.core.async.impl.protocols.ReadPort}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async14083 = (function (f,ch,meta14084){
this.f = f;
this.ch = ch;
this.meta14084 = meta14084;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_14085,meta14084__$1){
var self__ = this;
var _14085__$1 = this;
return (new cljs.core.async.t_cljs$core$async14083(self__.f,self__.ch,meta14084__$1));
}));

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_14085){
var self__ = this;
var _14085__$1 = this;
return self__.meta14084;
}));

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$async$impl$protocols$Channel$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$async$impl$protocols$Channel$close_BANG_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.close_BANG_(self__.ch);
}));

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$async$impl$protocols$ReadPort$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$async$impl$protocols$ReadPort$take_BANG_$arity$2 = (function (_,fn1){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.take_BANG_(self__.ch,fn1);
}));

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$async$impl$protocols$WritePort$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14083.prototype.cljs$core$async$impl$protocols$WritePort$put_BANG_$arity$3 = (function (_,val,fn1){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.put_BANG_(self__.ch,(self__.f.cljs$core$IFn$_invoke$arity$1 ? self__.f.cljs$core$IFn$_invoke$arity$1(val) : self__.f.call(null,val)),fn1);
}));

(cljs.core.async.t_cljs$core$async14083.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"f","f",43394975,null),new cljs.core.Symbol(null,"ch","ch",1085813622,null),new cljs.core.Symbol(null,"meta14084","meta14084",-1259678254,null)], null);
}));

(cljs.core.async.t_cljs$core$async14083.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async14083.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async14083");

(cljs.core.async.t_cljs$core$async14083.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async14083");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async14083.
 */
cljs.core.async.__GT_t_cljs$core$async14083 = (function cljs$core$async$__GT_t_cljs$core$async14083(f,ch,meta14084){
return (new cljs.core.async.t_cljs$core$async14083(f,ch,meta14084));
});


/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.map_GT_ = (function cljs$core$async$map_GT_(f,ch){
return (new cljs.core.async.t_cljs$core$async14083(f,ch,cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {cljs.core.async.impl.protocols.Channel}
 * @implements {cljs.core.async.impl.protocols.WritePort}
 * @implements {cljs.core.async.impl.protocols.ReadPort}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
cljs.core.async.t_cljs$core$async14086 = (function (p,ch,meta14087){
this.p = p;
this.ch = ch;
this.meta14087 = meta14087;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_14088,meta14087__$1){
var self__ = this;
var _14088__$1 = this;
return (new cljs.core.async.t_cljs$core$async14086(self__.p,self__.ch,meta14087__$1));
}));

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_14088){
var self__ = this;
var _14088__$1 = this;
return self__.meta14087;
}));

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$Channel$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$Channel$close_BANG_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.close_BANG_(self__.ch);
}));

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$Channel$closed_QMARK_$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.closed_QMARK_(self__.ch);
}));

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$ReadPort$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$ReadPort$take_BANG_$arity$2 = (function (_,fn1){
var self__ = this;
var ___$1 = this;
return cljs.core.async.impl.protocols.take_BANG_(self__.ch,fn1);
}));

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$WritePort$ = cljs.core.PROTOCOL_SENTINEL);

(cljs.core.async.t_cljs$core$async14086.prototype.cljs$core$async$impl$protocols$WritePort$put_BANG_$arity$3 = (function (_,val,fn1){
var self__ = this;
var ___$1 = this;
if(cljs.core.truth_((self__.p.cljs$core$IFn$_invoke$arity$1 ? self__.p.cljs$core$IFn$_invoke$arity$1(val) : self__.p.call(null,val)))){
return cljs.core.async.impl.protocols.put_BANG_(self__.ch,val,fn1);
} else {
return cljs.core.async.impl.channels.box(cljs.core.not(cljs.core.async.impl.protocols.closed_QMARK_(self__.ch)));
}
}));

(cljs.core.async.t_cljs$core$async14086.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"p","p",1791580836,null),new cljs.core.Symbol(null,"ch","ch",1085813622,null),new cljs.core.Symbol(null,"meta14087","meta14087",35681712,null)], null);
}));

(cljs.core.async.t_cljs$core$async14086.cljs$lang$type = true);

(cljs.core.async.t_cljs$core$async14086.cljs$lang$ctorStr = "cljs.core.async/t_cljs$core$async14086");

(cljs.core.async.t_cljs$core$async14086.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"cljs.core.async/t_cljs$core$async14086");
}));

/**
 * Positional factory function for cljs.core.async/t_cljs$core$async14086.
 */
cljs.core.async.__GT_t_cljs$core$async14086 = (function cljs$core$async$__GT_t_cljs$core$async14086(p,ch,meta14087){
return (new cljs.core.async.t_cljs$core$async14086(p,ch,meta14087));
});


/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.filter_GT_ = (function cljs$core$async$filter_GT_(p,ch){
return (new cljs.core.async.t_cljs$core$async14086(p,ch,cljs.core.PersistentArrayMap.EMPTY));
});
/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.remove_GT_ = (function cljs$core$async$remove_GT_(p,ch){
return cljs.core.async.filter_GT_(cljs.core.complement(p),ch);
});
/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.filter_LT_ = (function cljs$core$async$filter_LT_(var_args){
var G__14098 = arguments.length;
switch (G__14098) {
case 2:
return cljs.core.async.filter_LT_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.filter_LT_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.filter_LT_.cljs$core$IFn$_invoke$arity$2 = (function (p,ch){
return cljs.core.async.filter_LT_.cljs$core$IFn$_invoke$arity$3(p,ch,null);
}));

(cljs.core.async.filter_LT_.cljs$core$IFn$_invoke$arity$3 = (function (p,ch,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var c__12922__auto___15073 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_14122){
var state_val_14123 = (state_14122[(1)]);
if((state_val_14123 === (7))){
var inst_14118 = (state_14122[(2)]);
var state_14122__$1 = state_14122;
var statearr_14130_15082 = state_14122__$1;
(statearr_14130_15082[(2)] = inst_14118);

(statearr_14130_15082[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (1))){
var state_14122__$1 = state_14122;
var statearr_14131_15083 = state_14122__$1;
(statearr_14131_15083[(2)] = null);

(statearr_14131_15083[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (4))){
var inst_14104 = (state_14122[(7)]);
var inst_14104__$1 = (state_14122[(2)]);
var inst_14105 = (inst_14104__$1 == null);
var state_14122__$1 = (function (){var statearr_14132 = state_14122;
(statearr_14132[(7)] = inst_14104__$1);

return statearr_14132;
})();
if(cljs.core.truth_(inst_14105)){
var statearr_14133_15088 = state_14122__$1;
(statearr_14133_15088[(1)] = (5));

} else {
var statearr_14134_15097 = state_14122__$1;
(statearr_14134_15097[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (6))){
var inst_14104 = (state_14122[(7)]);
var inst_14109 = (p.cljs$core$IFn$_invoke$arity$1 ? p.cljs$core$IFn$_invoke$arity$1(inst_14104) : p.call(null,inst_14104));
var state_14122__$1 = state_14122;
if(cljs.core.truth_(inst_14109)){
var statearr_14138_15098 = state_14122__$1;
(statearr_14138_15098[(1)] = (8));

} else {
var statearr_14139_15099 = state_14122__$1;
(statearr_14139_15099[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (3))){
var inst_14120 = (state_14122[(2)]);
var state_14122__$1 = state_14122;
return cljs.core.async.impl.ioc_helpers.return_chan(state_14122__$1,inst_14120);
} else {
if((state_val_14123 === (2))){
var state_14122__$1 = state_14122;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_14122__$1,(4),ch);
} else {
if((state_val_14123 === (11))){
var inst_14112 = (state_14122[(2)]);
var state_14122__$1 = state_14122;
var statearr_14140_15100 = state_14122__$1;
(statearr_14140_15100[(2)] = inst_14112);

(statearr_14140_15100[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (9))){
var state_14122__$1 = state_14122;
var statearr_14141_15101 = state_14122__$1;
(statearr_14141_15101[(2)] = null);

(statearr_14141_15101[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (5))){
var inst_14107 = cljs.core.async.close_BANG_(out);
var state_14122__$1 = state_14122;
var statearr_14144_15102 = state_14122__$1;
(statearr_14144_15102[(2)] = inst_14107);

(statearr_14144_15102[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (10))){
var inst_14115 = (state_14122[(2)]);
var state_14122__$1 = (function (){var statearr_14145 = state_14122;
(statearr_14145[(8)] = inst_14115);

return statearr_14145;
})();
var statearr_14146_15103 = state_14122__$1;
(statearr_14146_15103[(2)] = null);

(statearr_14146_15103[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14123 === (8))){
var inst_14104 = (state_14122[(7)]);
var state_14122__$1 = state_14122;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14122__$1,(11),out,inst_14104);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_14150 = [null,null,null,null,null,null,null,null,null];
(statearr_14150[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_14150[(1)] = (1));

return statearr_14150;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_14122){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_14122);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e14151){var ex__12826__auto__ = e14151;
var statearr_14152_15104 = state_14122;
(statearr_14152_15104[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_14122[(4)]))){
var statearr_14153_15105 = state_14122;
(statearr_14153_15105[(1)] = cljs.core.first((state_14122[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15106 = state_14122;
state_14122 = G__15106;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_14122){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_14122);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14154 = f__12923__auto__();
(statearr_14154[(6)] = c__12922__auto___15073);

return statearr_14154;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return out;
}));

(cljs.core.async.filter_LT_.cljs$lang$maxFixedArity = 3);

/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.remove_LT_ = (function cljs$core$async$remove_LT_(var_args){
var G__14156 = arguments.length;
switch (G__14156) {
case 2:
return cljs.core.async.remove_LT_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.remove_LT_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.remove_LT_.cljs$core$IFn$_invoke$arity$2 = (function (p,ch){
return cljs.core.async.remove_LT_.cljs$core$IFn$_invoke$arity$3(p,ch,null);
}));

(cljs.core.async.remove_LT_.cljs$core$IFn$_invoke$arity$3 = (function (p,ch,buf_or_n){
return cljs.core.async.filter_LT_.cljs$core$IFn$_invoke$arity$3(cljs.core.complement(p),ch,buf_or_n);
}));

(cljs.core.async.remove_LT_.cljs$lang$maxFixedArity = 3);

cljs.core.async.mapcat_STAR_ = (function cljs$core$async$mapcat_STAR_(f,in$,out){
var c__12922__auto__ = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_14218){
var state_val_14219 = (state_14218[(1)]);
if((state_val_14219 === (7))){
var inst_14214 = (state_14218[(2)]);
var state_14218__$1 = state_14218;
var statearr_14220_15109 = state_14218__$1;
(statearr_14220_15109[(2)] = inst_14214);

(statearr_14220_15109[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (20))){
var inst_14184 = (state_14218[(7)]);
var inst_14195 = (state_14218[(2)]);
var inst_14196 = cljs.core.next(inst_14184);
var inst_14170 = inst_14196;
var inst_14171 = null;
var inst_14172 = (0);
var inst_14173 = (0);
var state_14218__$1 = (function (){var statearr_14221 = state_14218;
(statearr_14221[(8)] = inst_14170);

(statearr_14221[(9)] = inst_14173);

(statearr_14221[(10)] = inst_14171);

(statearr_14221[(11)] = inst_14172);

(statearr_14221[(12)] = inst_14195);

return statearr_14221;
})();
var statearr_14222_15113 = state_14218__$1;
(statearr_14222_15113[(2)] = null);

(statearr_14222_15113[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (1))){
var state_14218__$1 = state_14218;
var statearr_14223_15114 = state_14218__$1;
(statearr_14223_15114[(2)] = null);

(statearr_14223_15114[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (4))){
var inst_14159 = (state_14218[(13)]);
var inst_14159__$1 = (state_14218[(2)]);
var inst_14160 = (inst_14159__$1 == null);
var state_14218__$1 = (function (){var statearr_14224 = state_14218;
(statearr_14224[(13)] = inst_14159__$1);

return statearr_14224;
})();
if(cljs.core.truth_(inst_14160)){
var statearr_14225_15115 = state_14218__$1;
(statearr_14225_15115[(1)] = (5));

} else {
var statearr_14226_15116 = state_14218__$1;
(statearr_14226_15116[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (15))){
var state_14218__$1 = state_14218;
var statearr_14230_15117 = state_14218__$1;
(statearr_14230_15117[(2)] = null);

(statearr_14230_15117[(1)] = (16));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (21))){
var state_14218__$1 = state_14218;
var statearr_14232_15118 = state_14218__$1;
(statearr_14232_15118[(2)] = null);

(statearr_14232_15118[(1)] = (23));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (13))){
var inst_14170 = (state_14218[(8)]);
var inst_14173 = (state_14218[(9)]);
var inst_14171 = (state_14218[(10)]);
var inst_14172 = (state_14218[(11)]);
var inst_14180 = (state_14218[(2)]);
var inst_14181 = (inst_14173 + (1));
var tmp14227 = inst_14170;
var tmp14228 = inst_14171;
var tmp14229 = inst_14172;
var inst_14170__$1 = tmp14227;
var inst_14171__$1 = tmp14228;
var inst_14172__$1 = tmp14229;
var inst_14173__$1 = inst_14181;
var state_14218__$1 = (function (){var statearr_14233 = state_14218;
(statearr_14233[(8)] = inst_14170__$1);

(statearr_14233[(9)] = inst_14173__$1);

(statearr_14233[(10)] = inst_14171__$1);

(statearr_14233[(14)] = inst_14180);

(statearr_14233[(11)] = inst_14172__$1);

return statearr_14233;
})();
var statearr_14234_15119 = state_14218__$1;
(statearr_14234_15119[(2)] = null);

(statearr_14234_15119[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (22))){
var state_14218__$1 = state_14218;
var statearr_14235_15120 = state_14218__$1;
(statearr_14235_15120[(2)] = null);

(statearr_14235_15120[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (6))){
var inst_14159 = (state_14218[(13)]);
var inst_14168 = (f.cljs$core$IFn$_invoke$arity$1 ? f.cljs$core$IFn$_invoke$arity$1(inst_14159) : f.call(null,inst_14159));
var inst_14169 = cljs.core.seq(inst_14168);
var inst_14170 = inst_14169;
var inst_14171 = null;
var inst_14172 = (0);
var inst_14173 = (0);
var state_14218__$1 = (function (){var statearr_14236 = state_14218;
(statearr_14236[(8)] = inst_14170);

(statearr_14236[(9)] = inst_14173);

(statearr_14236[(10)] = inst_14171);

(statearr_14236[(11)] = inst_14172);

return statearr_14236;
})();
var statearr_14237_15121 = state_14218__$1;
(statearr_14237_15121[(2)] = null);

(statearr_14237_15121[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (17))){
var inst_14184 = (state_14218[(7)]);
var inst_14188 = cljs.core.chunk_first(inst_14184);
var inst_14189 = cljs.core.chunk_rest(inst_14184);
var inst_14190 = cljs.core.count(inst_14188);
var inst_14170 = inst_14189;
var inst_14171 = inst_14188;
var inst_14172 = inst_14190;
var inst_14173 = (0);
var state_14218__$1 = (function (){var statearr_14238 = state_14218;
(statearr_14238[(8)] = inst_14170);

(statearr_14238[(9)] = inst_14173);

(statearr_14238[(10)] = inst_14171);

(statearr_14238[(11)] = inst_14172);

return statearr_14238;
})();
var statearr_14239_15122 = state_14218__$1;
(statearr_14239_15122[(2)] = null);

(statearr_14239_15122[(1)] = (8));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (3))){
var inst_14216 = (state_14218[(2)]);
var state_14218__$1 = state_14218;
return cljs.core.async.impl.ioc_helpers.return_chan(state_14218__$1,inst_14216);
} else {
if((state_val_14219 === (12))){
var inst_14204 = (state_14218[(2)]);
var state_14218__$1 = state_14218;
var statearr_14240_15123 = state_14218__$1;
(statearr_14240_15123[(2)] = inst_14204);

(statearr_14240_15123[(1)] = (9));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (2))){
var state_14218__$1 = state_14218;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_14218__$1,(4),in$);
} else {
if((state_val_14219 === (23))){
var inst_14212 = (state_14218[(2)]);
var state_14218__$1 = state_14218;
var statearr_14241_15124 = state_14218__$1;
(statearr_14241_15124[(2)] = inst_14212);

(statearr_14241_15124[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (19))){
var inst_14199 = (state_14218[(2)]);
var state_14218__$1 = state_14218;
var statearr_14242_15125 = state_14218__$1;
(statearr_14242_15125[(2)] = inst_14199);

(statearr_14242_15125[(1)] = (16));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (11))){
var inst_14170 = (state_14218[(8)]);
var inst_14184 = (state_14218[(7)]);
var inst_14184__$1 = cljs.core.seq(inst_14170);
var state_14218__$1 = (function (){var statearr_14247 = state_14218;
(statearr_14247[(7)] = inst_14184__$1);

return statearr_14247;
})();
if(inst_14184__$1){
var statearr_14249_15127 = state_14218__$1;
(statearr_14249_15127[(1)] = (14));

} else {
var statearr_14255_15128 = state_14218__$1;
(statearr_14255_15128[(1)] = (15));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (9))){
var inst_14206 = (state_14218[(2)]);
var inst_14207 = cljs.core.async.impl.protocols.closed_QMARK_(out);
var state_14218__$1 = (function (){var statearr_14269 = state_14218;
(statearr_14269[(15)] = inst_14206);

return statearr_14269;
})();
if(cljs.core.truth_(inst_14207)){
var statearr_14270_15129 = state_14218__$1;
(statearr_14270_15129[(1)] = (21));

} else {
var statearr_14271_15130 = state_14218__$1;
(statearr_14271_15130[(1)] = (22));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (5))){
var inst_14162 = cljs.core.async.close_BANG_(out);
var state_14218__$1 = state_14218;
var statearr_14272_15131 = state_14218__$1;
(statearr_14272_15131[(2)] = inst_14162);

(statearr_14272_15131[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (14))){
var inst_14184 = (state_14218[(7)]);
var inst_14186 = cljs.core.chunked_seq_QMARK_(inst_14184);
var state_14218__$1 = state_14218;
if(inst_14186){
var statearr_14273_15132 = state_14218__$1;
(statearr_14273_15132[(1)] = (17));

} else {
var statearr_14274_15133 = state_14218__$1;
(statearr_14274_15133[(1)] = (18));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (16))){
var inst_14202 = (state_14218[(2)]);
var state_14218__$1 = state_14218;
var statearr_14275_15134 = state_14218__$1;
(statearr_14275_15134[(2)] = inst_14202);

(statearr_14275_15134[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14219 === (10))){
var inst_14173 = (state_14218[(9)]);
var inst_14171 = (state_14218[(10)]);
var inst_14178 = cljs.core._nth(inst_14171,inst_14173);
var state_14218__$1 = state_14218;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14218__$1,(13),out,inst_14178);
} else {
if((state_val_14219 === (18))){
var inst_14184 = (state_14218[(7)]);
var inst_14193 = cljs.core.first(inst_14184);
var state_14218__$1 = state_14218;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14218__$1,(20),out,inst_14193);
} else {
if((state_val_14219 === (8))){
var inst_14173 = (state_14218[(9)]);
var inst_14172 = (state_14218[(11)]);
var inst_14175 = (inst_14173 < inst_14172);
var inst_14176 = inst_14175;
var state_14218__$1 = state_14218;
if(cljs.core.truth_(inst_14176)){
var statearr_14279_15135 = state_14218__$1;
(statearr_14279_15135[(1)] = (10));

} else {
var statearr_14280_15136 = state_14218__$1;
(statearr_14280_15136[(1)] = (11));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$mapcat_STAR__$_state_machine__12823__auto__ = null;
var cljs$core$async$mapcat_STAR__$_state_machine__12823__auto____0 = (function (){
var statearr_14281 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_14281[(0)] = cljs$core$async$mapcat_STAR__$_state_machine__12823__auto__);

(statearr_14281[(1)] = (1));

return statearr_14281;
});
var cljs$core$async$mapcat_STAR__$_state_machine__12823__auto____1 = (function (state_14218){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_14218);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e14282){var ex__12826__auto__ = e14282;
var statearr_14287_15138 = state_14218;
(statearr_14287_15138[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_14218[(4)]))){
var statearr_14289_15139 = state_14218;
(statearr_14289_15139[(1)] = cljs.core.first((state_14218[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15140 = state_14218;
state_14218 = G__15140;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$mapcat_STAR__$_state_machine__12823__auto__ = function(state_14218){
switch(arguments.length){
case 0:
return cljs$core$async$mapcat_STAR__$_state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$mapcat_STAR__$_state_machine__12823__auto____1.call(this,state_14218);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$mapcat_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$mapcat_STAR__$_state_machine__12823__auto____0;
cljs$core$async$mapcat_STAR__$_state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$mapcat_STAR__$_state_machine__12823__auto____1;
return cljs$core$async$mapcat_STAR__$_state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14294 = f__12923__auto__();
(statearr_14294[(6)] = c__12922__auto__);

return statearr_14294;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));

return c__12922__auto__;
});
/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.mapcat_LT_ = (function cljs$core$async$mapcat_LT_(var_args){
var G__14299 = arguments.length;
switch (G__14299) {
case 2:
return cljs.core.async.mapcat_LT_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.mapcat_LT_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.mapcat_LT_.cljs$core$IFn$_invoke$arity$2 = (function (f,in$){
return cljs.core.async.mapcat_LT_.cljs$core$IFn$_invoke$arity$3(f,in$,null);
}));

(cljs.core.async.mapcat_LT_.cljs$core$IFn$_invoke$arity$3 = (function (f,in$,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
cljs.core.async.mapcat_STAR_(f,in$,out);

return out;
}));

(cljs.core.async.mapcat_LT_.cljs$lang$maxFixedArity = 3);

/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.mapcat_GT_ = (function cljs$core$async$mapcat_GT_(var_args){
var G__14331 = arguments.length;
switch (G__14331) {
case 2:
return cljs.core.async.mapcat_GT_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.mapcat_GT_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.mapcat_GT_.cljs$core$IFn$_invoke$arity$2 = (function (f,out){
return cljs.core.async.mapcat_GT_.cljs$core$IFn$_invoke$arity$3(f,out,null);
}));

(cljs.core.async.mapcat_GT_.cljs$core$IFn$_invoke$arity$3 = (function (f,out,buf_or_n){
var in$ = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
cljs.core.async.mapcat_STAR_(f,in$,out);

return in$;
}));

(cljs.core.async.mapcat_GT_.cljs$lang$maxFixedArity = 3);

/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.unique = (function cljs$core$async$unique(var_args){
var G__14354 = arguments.length;
switch (G__14354) {
case 1:
return cljs.core.async.unique.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return cljs.core.async.unique.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.unique.cljs$core$IFn$_invoke$arity$1 = (function (ch){
return cljs.core.async.unique.cljs$core$IFn$_invoke$arity$2(ch,null);
}));

(cljs.core.async.unique.cljs$core$IFn$_invoke$arity$2 = (function (ch,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var c__12922__auto___15144 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_14379){
var state_val_14380 = (state_14379[(1)]);
if((state_val_14380 === (7))){
var inst_14374 = (state_14379[(2)]);
var state_14379__$1 = state_14379;
var statearr_14381_15145 = state_14379__$1;
(statearr_14381_15145[(2)] = inst_14374);

(statearr_14381_15145[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (1))){
var inst_14356 = null;
var state_14379__$1 = (function (){var statearr_14394 = state_14379;
(statearr_14394[(7)] = inst_14356);

return statearr_14394;
})();
var statearr_14395_15146 = state_14379__$1;
(statearr_14395_15146[(2)] = null);

(statearr_14395_15146[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (4))){
var inst_14359 = (state_14379[(8)]);
var inst_14359__$1 = (state_14379[(2)]);
var inst_14360 = (inst_14359__$1 == null);
var inst_14361 = cljs.core.not(inst_14360);
var state_14379__$1 = (function (){var statearr_14396 = state_14379;
(statearr_14396[(8)] = inst_14359__$1);

return statearr_14396;
})();
if(inst_14361){
var statearr_14397_15147 = state_14379__$1;
(statearr_14397_15147[(1)] = (5));

} else {
var statearr_14398_15148 = state_14379__$1;
(statearr_14398_15148[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (6))){
var state_14379__$1 = state_14379;
var statearr_14399_15149 = state_14379__$1;
(statearr_14399_15149[(2)] = null);

(statearr_14399_15149[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (3))){
var inst_14376 = (state_14379[(2)]);
var inst_14377 = cljs.core.async.close_BANG_(out);
var state_14379__$1 = (function (){var statearr_14400 = state_14379;
(statearr_14400[(9)] = inst_14376);

return statearr_14400;
})();
return cljs.core.async.impl.ioc_helpers.return_chan(state_14379__$1,inst_14377);
} else {
if((state_val_14380 === (2))){
var state_14379__$1 = state_14379;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_14379__$1,(4),ch);
} else {
if((state_val_14380 === (11))){
var inst_14359 = (state_14379[(8)]);
var inst_14368 = (state_14379[(2)]);
var inst_14356 = inst_14359;
var state_14379__$1 = (function (){var statearr_14402 = state_14379;
(statearr_14402[(10)] = inst_14368);

(statearr_14402[(7)] = inst_14356);

return statearr_14402;
})();
var statearr_14403_15150 = state_14379__$1;
(statearr_14403_15150[(2)] = null);

(statearr_14403_15150[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (9))){
var inst_14359 = (state_14379[(8)]);
var state_14379__$1 = state_14379;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14379__$1,(11),out,inst_14359);
} else {
if((state_val_14380 === (5))){
var inst_14359 = (state_14379[(8)]);
var inst_14356 = (state_14379[(7)]);
var inst_14363 = cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(inst_14359,inst_14356);
var state_14379__$1 = state_14379;
if(inst_14363){
var statearr_14405_15151 = state_14379__$1;
(statearr_14405_15151[(1)] = (8));

} else {
var statearr_14406_15152 = state_14379__$1;
(statearr_14406_15152[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (10))){
var inst_14371 = (state_14379[(2)]);
var state_14379__$1 = state_14379;
var statearr_14407_15153 = state_14379__$1;
(statearr_14407_15153[(2)] = inst_14371);

(statearr_14407_15153[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14380 === (8))){
var inst_14356 = (state_14379[(7)]);
var tmp14404 = inst_14356;
var inst_14356__$1 = tmp14404;
var state_14379__$1 = (function (){var statearr_14408 = state_14379;
(statearr_14408[(7)] = inst_14356__$1);

return statearr_14408;
})();
var statearr_14409_15154 = state_14379__$1;
(statearr_14409_15154[(2)] = null);

(statearr_14409_15154[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_14410 = [null,null,null,null,null,null,null,null,null,null,null];
(statearr_14410[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_14410[(1)] = (1));

return statearr_14410;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_14379){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_14379);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e14411){var ex__12826__auto__ = e14411;
var statearr_14412_15155 = state_14379;
(statearr_14412_15155[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_14379[(4)]))){
var statearr_14413_15156 = state_14379;
(statearr_14413_15156[(1)] = cljs.core.first((state_14379[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15157 = state_14379;
state_14379 = G__15157;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_14379){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_14379);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14414 = f__12923__auto__();
(statearr_14414[(6)] = c__12922__auto___15144);

return statearr_14414;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return out;
}));

(cljs.core.async.unique.cljs$lang$maxFixedArity = 2);

/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.partition = (function cljs$core$async$partition(var_args){
var G__14416 = arguments.length;
switch (G__14416) {
case 2:
return cljs.core.async.partition.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.partition.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.partition.cljs$core$IFn$_invoke$arity$2 = (function (n,ch){
return cljs.core.async.partition.cljs$core$IFn$_invoke$arity$3(n,ch,null);
}));

(cljs.core.async.partition.cljs$core$IFn$_invoke$arity$3 = (function (n,ch,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var c__12922__auto___15159 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_14454){
var state_val_14455 = (state_14454[(1)]);
if((state_val_14455 === (7))){
var inst_14450 = (state_14454[(2)]);
var state_14454__$1 = state_14454;
var statearr_14456_15160 = state_14454__$1;
(statearr_14456_15160[(2)] = inst_14450);

(statearr_14456_15160[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (1))){
var inst_14417 = (new Array(n));
var inst_14418 = inst_14417;
var inst_14419 = (0);
var state_14454__$1 = (function (){var statearr_14457 = state_14454;
(statearr_14457[(7)] = inst_14419);

(statearr_14457[(8)] = inst_14418);

return statearr_14457;
})();
var statearr_14458_15161 = state_14454__$1;
(statearr_14458_15161[(2)] = null);

(statearr_14458_15161[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (4))){
var inst_14422 = (state_14454[(9)]);
var inst_14422__$1 = (state_14454[(2)]);
var inst_14423 = (inst_14422__$1 == null);
var inst_14424 = cljs.core.not(inst_14423);
var state_14454__$1 = (function (){var statearr_14459 = state_14454;
(statearr_14459[(9)] = inst_14422__$1);

return statearr_14459;
})();
if(inst_14424){
var statearr_14460_15162 = state_14454__$1;
(statearr_14460_15162[(1)] = (5));

} else {
var statearr_14461_15163 = state_14454__$1;
(statearr_14461_15163[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (15))){
var inst_14444 = (state_14454[(2)]);
var state_14454__$1 = state_14454;
var statearr_14462_15164 = state_14454__$1;
(statearr_14462_15164[(2)] = inst_14444);

(statearr_14462_15164[(1)] = (14));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (13))){
var state_14454__$1 = state_14454;
var statearr_14463_15165 = state_14454__$1;
(statearr_14463_15165[(2)] = null);

(statearr_14463_15165[(1)] = (14));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (6))){
var inst_14419 = (state_14454[(7)]);
var inst_14440 = (inst_14419 > (0));
var state_14454__$1 = state_14454;
if(cljs.core.truth_(inst_14440)){
var statearr_14464_15166 = state_14454__$1;
(statearr_14464_15166[(1)] = (12));

} else {
var statearr_14465_15167 = state_14454__$1;
(statearr_14465_15167[(1)] = (13));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (3))){
var inst_14452 = (state_14454[(2)]);
var state_14454__$1 = state_14454;
return cljs.core.async.impl.ioc_helpers.return_chan(state_14454__$1,inst_14452);
} else {
if((state_val_14455 === (12))){
var inst_14418 = (state_14454[(8)]);
var inst_14442 = cljs.core.vec(inst_14418);
var state_14454__$1 = state_14454;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14454__$1,(15),out,inst_14442);
} else {
if((state_val_14455 === (2))){
var state_14454__$1 = state_14454;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_14454__$1,(4),ch);
} else {
if((state_val_14455 === (11))){
var inst_14434 = (state_14454[(2)]);
var inst_14435 = (new Array(n));
var inst_14418 = inst_14435;
var inst_14419 = (0);
var state_14454__$1 = (function (){var statearr_14466 = state_14454;
(statearr_14466[(10)] = inst_14434);

(statearr_14466[(7)] = inst_14419);

(statearr_14466[(8)] = inst_14418);

return statearr_14466;
})();
var statearr_14467_15168 = state_14454__$1;
(statearr_14467_15168[(2)] = null);

(statearr_14467_15168[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (9))){
var inst_14418 = (state_14454[(8)]);
var inst_14432 = cljs.core.vec(inst_14418);
var state_14454__$1 = state_14454;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14454__$1,(11),out,inst_14432);
} else {
if((state_val_14455 === (5))){
var inst_14427 = (state_14454[(11)]);
var inst_14422 = (state_14454[(9)]);
var inst_14419 = (state_14454[(7)]);
var inst_14418 = (state_14454[(8)]);
var inst_14426 = (inst_14418[inst_14419] = inst_14422);
var inst_14427__$1 = (inst_14419 + (1));
var inst_14428 = (inst_14427__$1 < n);
var state_14454__$1 = (function (){var statearr_14468 = state_14454;
(statearr_14468[(12)] = inst_14426);

(statearr_14468[(11)] = inst_14427__$1);

return statearr_14468;
})();
if(cljs.core.truth_(inst_14428)){
var statearr_14469_15169 = state_14454__$1;
(statearr_14469_15169[(1)] = (8));

} else {
var statearr_14470_15170 = state_14454__$1;
(statearr_14470_15170[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (14))){
var inst_14447 = (state_14454[(2)]);
var inst_14448 = cljs.core.async.close_BANG_(out);
var state_14454__$1 = (function (){var statearr_14472 = state_14454;
(statearr_14472[(13)] = inst_14447);

return statearr_14472;
})();
var statearr_14473_15171 = state_14454__$1;
(statearr_14473_15171[(2)] = inst_14448);

(statearr_14473_15171[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (10))){
var inst_14438 = (state_14454[(2)]);
var state_14454__$1 = state_14454;
var statearr_14474_15172 = state_14454__$1;
(statearr_14474_15172[(2)] = inst_14438);

(statearr_14474_15172[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14455 === (8))){
var inst_14427 = (state_14454[(11)]);
var inst_14418 = (state_14454[(8)]);
var tmp14471 = inst_14418;
var inst_14418__$1 = tmp14471;
var inst_14419 = inst_14427;
var state_14454__$1 = (function (){var statearr_14475 = state_14454;
(statearr_14475[(7)] = inst_14419);

(statearr_14475[(8)] = inst_14418__$1);

return statearr_14475;
})();
var statearr_14476_15173 = state_14454__$1;
(statearr_14476_15173[(2)] = null);

(statearr_14476_15173[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_14477 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_14477[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_14477[(1)] = (1));

return statearr_14477;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_14454){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_14454);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e14478){var ex__12826__auto__ = e14478;
var statearr_14479_15174 = state_14454;
(statearr_14479_15174[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_14454[(4)]))){
var statearr_14480_15175 = state_14454;
(statearr_14480_15175[(1)] = cljs.core.first((state_14454[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15176 = state_14454;
state_14454 = G__15176;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_14454){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_14454);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14481 = f__12923__auto__();
(statearr_14481[(6)] = c__12922__auto___15159);

return statearr_14481;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return out;
}));

(cljs.core.async.partition.cljs$lang$maxFixedArity = 3);

/**
 * Deprecated - this function will be removed. Use transducer instead
 */
cljs.core.async.partition_by = (function cljs$core$async$partition_by(var_args){
var G__14483 = arguments.length;
switch (G__14483) {
case 2:
return cljs.core.async.partition_by.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return cljs.core.async.partition_by.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(cljs.core.async.partition_by.cljs$core$IFn$_invoke$arity$2 = (function (f,ch){
return cljs.core.async.partition_by.cljs$core$IFn$_invoke$arity$3(f,ch,null);
}));

(cljs.core.async.partition_by.cljs$core$IFn$_invoke$arity$3 = (function (f,ch,buf_or_n){
var out = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1(buf_or_n);
var c__12922__auto___15178 = cljs.core.async.chan.cljs$core$IFn$_invoke$arity$1((1));
cljs.core.async.impl.dispatch.run((function (){
var f__12923__auto__ = (function (){var switch__12822__auto__ = (function (state_14528){
var state_val_14529 = (state_14528[(1)]);
if((state_val_14529 === (7))){
var inst_14524 = (state_14528[(2)]);
var state_14528__$1 = state_14528;
var statearr_14530_15179 = state_14528__$1;
(statearr_14530_15179[(2)] = inst_14524);

(statearr_14530_15179[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (1))){
var inst_14484 = [];
var inst_14485 = inst_14484;
var inst_14486 = new cljs.core.Keyword("cljs.core.async","nothing","cljs.core.async/nothing",-69252123);
var state_14528__$1 = (function (){var statearr_14531 = state_14528;
(statearr_14531[(7)] = inst_14486);

(statearr_14531[(8)] = inst_14485);

return statearr_14531;
})();
var statearr_14532_15185 = state_14528__$1;
(statearr_14532_15185[(2)] = null);

(statearr_14532_15185[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (4))){
var inst_14489 = (state_14528[(9)]);
var inst_14489__$1 = (state_14528[(2)]);
var inst_14490 = (inst_14489__$1 == null);
var inst_14491 = cljs.core.not(inst_14490);
var state_14528__$1 = (function (){var statearr_14533 = state_14528;
(statearr_14533[(9)] = inst_14489__$1);

return statearr_14533;
})();
if(inst_14491){
var statearr_14534_15186 = state_14528__$1;
(statearr_14534_15186[(1)] = (5));

} else {
var statearr_14535_15187 = state_14528__$1;
(statearr_14535_15187[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (15))){
var inst_14485 = (state_14528[(8)]);
var inst_14516 = cljs.core.vec(inst_14485);
var state_14528__$1 = state_14528;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14528__$1,(18),out,inst_14516);
} else {
if((state_val_14529 === (13))){
var inst_14511 = (state_14528[(2)]);
var state_14528__$1 = state_14528;
var statearr_14536_15188 = state_14528__$1;
(statearr_14536_15188[(2)] = inst_14511);

(statearr_14536_15188[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (6))){
var inst_14485 = (state_14528[(8)]);
var inst_14513 = inst_14485.length;
var inst_14514 = (inst_14513 > (0));
var state_14528__$1 = state_14528;
if(cljs.core.truth_(inst_14514)){
var statearr_14537_15189 = state_14528__$1;
(statearr_14537_15189[(1)] = (15));

} else {
var statearr_14538_15190 = state_14528__$1;
(statearr_14538_15190[(1)] = (16));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (17))){
var inst_14521 = (state_14528[(2)]);
var inst_14522 = cljs.core.async.close_BANG_(out);
var state_14528__$1 = (function (){var statearr_14539 = state_14528;
(statearr_14539[(10)] = inst_14521);

return statearr_14539;
})();
var statearr_14540_15191 = state_14528__$1;
(statearr_14540_15191[(2)] = inst_14522);

(statearr_14540_15191[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (3))){
var inst_14526 = (state_14528[(2)]);
var state_14528__$1 = state_14528;
return cljs.core.async.impl.ioc_helpers.return_chan(state_14528__$1,inst_14526);
} else {
if((state_val_14529 === (12))){
var inst_14485 = (state_14528[(8)]);
var inst_14504 = cljs.core.vec(inst_14485);
var state_14528__$1 = state_14528;
return cljs.core.async.impl.ioc_helpers.put_BANG_(state_14528__$1,(14),out,inst_14504);
} else {
if((state_val_14529 === (2))){
var state_14528__$1 = state_14528;
return cljs.core.async.impl.ioc_helpers.take_BANG_(state_14528__$1,(4),ch);
} else {
if((state_val_14529 === (11))){
var inst_14489 = (state_14528[(9)]);
var inst_14485 = (state_14528[(8)]);
var inst_14493 = (state_14528[(11)]);
var inst_14501 = inst_14485.push(inst_14489);
var tmp14541 = inst_14485;
var inst_14485__$1 = tmp14541;
var inst_14486 = inst_14493;
var state_14528__$1 = (function (){var statearr_14542 = state_14528;
(statearr_14542[(7)] = inst_14486);

(statearr_14542[(8)] = inst_14485__$1);

(statearr_14542[(12)] = inst_14501);

return statearr_14542;
})();
var statearr_14543_15193 = state_14528__$1;
(statearr_14543_15193[(2)] = null);

(statearr_14543_15193[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (9))){
var inst_14486 = (state_14528[(7)]);
var inst_14497 = cljs.core.keyword_identical_QMARK_(inst_14486,new cljs.core.Keyword("cljs.core.async","nothing","cljs.core.async/nothing",-69252123));
var state_14528__$1 = state_14528;
var statearr_14544_15195 = state_14528__$1;
(statearr_14544_15195[(2)] = inst_14497);

(statearr_14544_15195[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (5))){
var inst_14494 = (state_14528[(13)]);
var inst_14486 = (state_14528[(7)]);
var inst_14489 = (state_14528[(9)]);
var inst_14493 = (state_14528[(11)]);
var inst_14493__$1 = (f.cljs$core$IFn$_invoke$arity$1 ? f.cljs$core$IFn$_invoke$arity$1(inst_14489) : f.call(null,inst_14489));
var inst_14494__$1 = cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(inst_14493__$1,inst_14486);
var state_14528__$1 = (function (){var statearr_14545 = state_14528;
(statearr_14545[(13)] = inst_14494__$1);

(statearr_14545[(11)] = inst_14493__$1);

return statearr_14545;
})();
if(inst_14494__$1){
var statearr_14546_15197 = state_14528__$1;
(statearr_14546_15197[(1)] = (8));

} else {
var statearr_14547_15199 = state_14528__$1;
(statearr_14547_15199[(1)] = (9));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (14))){
var inst_14489 = (state_14528[(9)]);
var inst_14493 = (state_14528[(11)]);
var inst_14506 = (state_14528[(2)]);
var inst_14507 = [];
var inst_14508 = inst_14507.push(inst_14489);
var inst_14485 = inst_14507;
var inst_14486 = inst_14493;
var state_14528__$1 = (function (){var statearr_14548 = state_14528;
(statearr_14548[(14)] = inst_14506);

(statearr_14548[(15)] = inst_14508);

(statearr_14548[(7)] = inst_14486);

(statearr_14548[(8)] = inst_14485);

return statearr_14548;
})();
var statearr_14549_15200 = state_14528__$1;
(statearr_14549_15200[(2)] = null);

(statearr_14549_15200[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (16))){
var state_14528__$1 = state_14528;
var statearr_14550_15201 = state_14528__$1;
(statearr_14550_15201[(2)] = null);

(statearr_14550_15201[(1)] = (17));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (10))){
var inst_14499 = (state_14528[(2)]);
var state_14528__$1 = state_14528;
if(cljs.core.truth_(inst_14499)){
var statearr_14551_15203 = state_14528__$1;
(statearr_14551_15203[(1)] = (11));

} else {
var statearr_14552_15204 = state_14528__$1;
(statearr_14552_15204[(1)] = (12));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (18))){
var inst_14518 = (state_14528[(2)]);
var state_14528__$1 = state_14528;
var statearr_14553_15205 = state_14528__$1;
(statearr_14553_15205[(2)] = inst_14518);

(statearr_14553_15205[(1)] = (17));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_14529 === (8))){
var inst_14494 = (state_14528[(13)]);
var state_14528__$1 = state_14528;
var statearr_14554_15206 = state_14528__$1;
(statearr_14554_15206[(2)] = inst_14494);

(statearr_14554_15206[(1)] = (10));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});
return (function() {
var cljs$core$async$state_machine__12823__auto__ = null;
var cljs$core$async$state_machine__12823__auto____0 = (function (){
var statearr_14555 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_14555[(0)] = cljs$core$async$state_machine__12823__auto__);

(statearr_14555[(1)] = (1));

return statearr_14555;
});
var cljs$core$async$state_machine__12823__auto____1 = (function (state_14528){
while(true){
var ret_value__12824__auto__ = (function (){try{while(true){
var result__12825__auto__ = switch__12822__auto__(state_14528);
if(cljs.core.keyword_identical_QMARK_(result__12825__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__12825__auto__;
}
break;
}
}catch (e14556){var ex__12826__auto__ = e14556;
var statearr_14557_15207 = state_14528;
(statearr_14557_15207[(2)] = ex__12826__auto__);


if(cljs.core.seq((state_14528[(4)]))){
var statearr_14558_15208 = state_14528;
(statearr_14558_15208[(1)] = cljs.core.first((state_14528[(4)])));

} else {
throw ex__12826__auto__;
}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
}})();
if(cljs.core.keyword_identical_QMARK_(ret_value__12824__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__15209 = state_14528;
state_14528 = G__15209;
continue;
} else {
return ret_value__12824__auto__;
}
break;
}
});
cljs$core$async$state_machine__12823__auto__ = function(state_14528){
switch(arguments.length){
case 0:
return cljs$core$async$state_machine__12823__auto____0.call(this);
case 1:
return cljs$core$async$state_machine__12823__auto____1.call(this,state_14528);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$0 = cljs$core$async$state_machine__12823__auto____0;
cljs$core$async$state_machine__12823__auto__.cljs$core$IFn$_invoke$arity$1 = cljs$core$async$state_machine__12823__auto____1;
return cljs$core$async$state_machine__12823__auto__;
})()
})();
var state__12924__auto__ = (function (){var statearr_14559 = f__12923__auto__();
(statearr_14559[(6)] = c__12922__auto___15178);

return statearr_14559;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped(state__12924__auto__);
}));


return out;
}));

(cljs.core.async.partition_by.cljs$lang$maxFixedArity = 3);


//# sourceMappingURL=cljs.core.async.js.map
