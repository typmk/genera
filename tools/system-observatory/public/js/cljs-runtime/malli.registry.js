goog.provide('malli.registry');
/**
 * @define {string}
 */
malli.registry.mode = goog.define("malli.registry.mode","default");
/**
 * @define {string}
 */
malli.registry.type = goog.define("malli.registry.type","default");

/**
 * @interface
 */
malli.registry.Registry = function(){};

var malli$registry$Registry$_schema$dyn_13973 = (function (this$,type){
var x__5350__auto__ = (((this$ == null))?null:this$);
var m__5351__auto__ = (malli.registry._schema[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$2(this$,type) : m__5351__auto__.call(null,this$,type));
} else {
var m__5349__auto__ = (malli.registry._schema["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$2 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$2(this$,type) : m__5349__auto__.call(null,this$,type));
} else {
throw cljs.core.missing_protocol("Registry.-schema",this$);
}
}
});
/**
 * returns the schema from a registry
 */
malli.registry._schema = (function malli$registry$_schema(this$,type){
if((((!((this$ == null)))) && ((!((this$.malli$registry$Registry$_schema$arity$2 == null)))))){
return this$.malli$registry$Registry$_schema$arity$2(this$,type);
} else {
return malli$registry$Registry$_schema$dyn_13973(this$,type);
}
});

var malli$registry$Registry$_schemas$dyn_13974 = (function (this$){
var x__5350__auto__ = (((this$ == null))?null:this$);
var m__5351__auto__ = (malli.registry._schemas[goog.typeOf(x__5350__auto__)]);
if((!((m__5351__auto__ == null)))){
return (m__5351__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5351__auto__.cljs$core$IFn$_invoke$arity$1(this$) : m__5351__auto__.call(null,this$));
} else {
var m__5349__auto__ = (malli.registry._schemas["_"]);
if((!((m__5349__auto__ == null)))){
return (m__5349__auto__.cljs$core$IFn$_invoke$arity$1 ? m__5349__auto__.cljs$core$IFn$_invoke$arity$1(this$) : m__5349__auto__.call(null,this$));
} else {
throw cljs.core.missing_protocol("Registry.-schemas",this$);
}
}
});
/**
 * returns all schemas from a registry
 */
malli.registry._schemas = (function malli$registry$_schemas(this$){
if((((!((this$ == null)))) && ((!((this$.malli$registry$Registry$_schemas$arity$1 == null)))))){
return this$.malli$registry$Registry$_schemas$arity$1(this$);
} else {
return malli$registry$Registry$_schemas$dyn_13974(this$);
}
});

malli.registry.registry_QMARK_ = (function malli$registry$registry_QMARK_(x){
if((!((x == null)))){
if(((false) || ((cljs.core.PROTOCOL_SENTINEL === x.malli$registry$Registry$)))){
return true;
} else {
return false;
}
} else {
return false;
}
});

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13912 = (function (m,fm,meta13913){
this.m = m;
this.fm = fm;
this.meta13913 = meta13913;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13912.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13914,meta13913__$1){
var self__ = this;
var _13914__$1 = this;
return (new malli.registry.t_malli$registry13912(self__.m,self__.fm,meta13913__$1));
}));

(malli.registry.t_malli$registry13912.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13914){
var self__ = this;
var _13914__$1 = this;
return self__.meta13913;
}));

(malli.registry.t_malli$registry13912.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13912.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
return self__.fm.get(type);
}));

(malli.registry.t_malli$registry13912.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.m;
}));

(malli.registry.t_malli$registry13912.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"m","m",-1021758608,null),new cljs.core.Symbol(null,"fm","fm",-1190690268,null),new cljs.core.Symbol(null,"meta13913","meta13913",1568464258,null)], null);
}));

(malli.registry.t_malli$registry13912.cljs$lang$type = true);

(malli.registry.t_malli$registry13912.cljs$lang$ctorStr = "malli.registry/t_malli$registry13912");

(malli.registry.t_malli$registry13912.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13912");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13912.
 */
malli.registry.__GT_t_malli$registry13912 = (function malli$registry$__GT_t_malli$registry13912(m,fm,meta13913){
return (new malli.registry.t_malli$registry13912(m,fm,meta13913));
});


malli.registry.fast_registry = (function malli$registry$fast_registry(m){
var fm = m;
return (new malli.registry.t_malli$registry13912(m,fm,cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13930 = (function (m,meta13931){
this.m = m;
this.meta13931 = meta13931;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13930.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13932,meta13931__$1){
var self__ = this;
var _13932__$1 = this;
return (new malli.registry.t_malli$registry13930(self__.m,meta13931__$1));
}));

(malli.registry.t_malli$registry13930.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13932){
var self__ = this;
var _13932__$1 = this;
return self__.meta13931;
}));

(malli.registry.t_malli$registry13930.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13930.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
return (self__.m.cljs$core$IFn$_invoke$arity$1 ? self__.m.cljs$core$IFn$_invoke$arity$1(type) : self__.m.call(null,type));
}));

(malli.registry.t_malli$registry13930.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return self__.m;
}));

(malli.registry.t_malli$registry13930.getBasis = (function (){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"m","m",-1021758608,null),new cljs.core.Symbol(null,"meta13931","meta13931",959739206,null)], null);
}));

(malli.registry.t_malli$registry13930.cljs$lang$type = true);

(malli.registry.t_malli$registry13930.cljs$lang$ctorStr = "malli.registry/t_malli$registry13930");

(malli.registry.t_malli$registry13930.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13930");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13930.
 */
malli.registry.__GT_t_malli$registry13930 = (function malli$registry$__GT_t_malli$registry13930(m,meta13931){
return (new malli.registry.t_malli$registry13930(m,meta13931));
});


malli.registry.simple_registry = (function malli$registry$simple_registry(m){
return (new malli.registry.t_malli$registry13930(m,cljs.core.PersistentArrayMap.EMPTY));
});
malli.registry.registry = (function malli$registry$registry(_QMARK_registry){
if((_QMARK_registry == null)){
return null;
} else {
if(malli.registry.registry_QMARK_(_QMARK_registry)){
return _QMARK_registry;
} else {
if(cljs.core.map_QMARK_(_QMARK_registry)){
return malli.registry.simple_registry(_QMARK_registry);
} else {
if((((!((_QMARK_registry == null))))?((((false) || ((cljs.core.PROTOCOL_SENTINEL === _QMARK_registry.malli$registry$Registry$))))?true:(((!_QMARK_registry.cljs$lang$protocol_mask$partition$))?cljs.core.native_satisfies_QMARK_(malli.registry.Registry,_QMARK_registry):false)):cljs.core.native_satisfies_QMARK_(malli.registry.Registry,_QMARK_registry))){
return _QMARK_registry;
} else {
return null;
}
}
}
}
});
malli.registry.registry_STAR_ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(malli.registry.simple_registry(cljs.core.PersistentArrayMap.EMPTY));
malli.registry.set_default_registry_BANG_ = (function malli$registry$set_default_registry_BANG_(_QMARK_registry){
if((!((malli.registry.mode === "strict")))){
return cljs.core.reset_BANG_(malli.registry.registry_STAR_,malli.registry.registry(_QMARK_registry));
} else {
throw cljs.core.ex_info.cljs$core$IFn$_invoke$arity$2("can't set default registry, invalid mode",new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"mode","mode",654403691),malli.registry.mode,new cljs.core.Keyword(null,"type","type",1174270348),malli.registry.type], null));
}
});

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13950 = (function (meta13951){
this.meta13951 = meta13951;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13950.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13952,meta13951__$1){
var self__ = this;
var _13952__$1 = this;
return (new malli.registry.t_malli$registry13950(meta13951__$1));
}));

(malli.registry.t_malli$registry13950.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13952){
var self__ = this;
var _13952__$1 = this;
return self__.meta13951;
}));

(malli.registry.t_malli$registry13950.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13950.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
return malli.registry._schema(cljs.core.deref(malli.registry.registry_STAR_),type);
}));

(malli.registry.t_malli$registry13950.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return malli.registry._schemas(cljs.core.deref(malli.registry.registry_STAR_));
}));

(malli.registry.t_malli$registry13950.getBasis = (function (){
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"meta13951","meta13951",-1616219209,null)], null);
}));

(malli.registry.t_malli$registry13950.cljs$lang$type = true);

(malli.registry.t_malli$registry13950.cljs$lang$ctorStr = "malli.registry/t_malli$registry13950");

(malli.registry.t_malli$registry13950.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13950");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13950.
 */
malli.registry.__GT_t_malli$registry13950 = (function malli$registry$__GT_t_malli$registry13950(meta13951){
return (new malli.registry.t_malli$registry13950(meta13951));
});


malli.registry.custom_default_registry = (function malli$registry$custom_default_registry(){
return (new malli.registry.t_malli$registry13950(cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13955 = (function (_QMARK_registries,registries,meta13956){
this._QMARK_registries = _QMARK_registries;
this.registries = registries;
this.meta13956 = meta13956;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13955.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13957,meta13956__$1){
var self__ = this;
var _13957__$1 = this;
return (new malli.registry.t_malli$registry13955(self__._QMARK_registries,self__.registries,meta13956__$1));
}));

(malli.registry.t_malli$registry13955.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13957){
var self__ = this;
var _13957__$1 = this;
return self__.meta13956;
}));

(malli.registry.t_malli$registry13955.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13955.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
return cljs.core.some((function (p1__13953_SHARP_){
return malli.registry._schema(p1__13953_SHARP_,type);
}),self__.registries);
}));

(malli.registry.t_malli$registry13955.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$2(cljs.core.merge,cljs.core.map.cljs$core$IFn$_invoke$arity$2(malli.registry._schemas,cljs.core.reverse(self__.registries)));
}));

(malli.registry.t_malli$registry13955.getBasis = (function (){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"?registries","?registries",2135368100,null),new cljs.core.Symbol(null,"registries","registries",-1366064418,null),new cljs.core.Symbol(null,"meta13956","meta13956",2035442405,null)], null);
}));

(malli.registry.t_malli$registry13955.cljs$lang$type = true);

(malli.registry.t_malli$registry13955.cljs$lang$ctorStr = "malli.registry/t_malli$registry13955");

(malli.registry.t_malli$registry13955.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13955");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13955.
 */
malli.registry.__GT_t_malli$registry13955 = (function malli$registry$__GT_t_malli$registry13955(_QMARK_registries,registries,meta13956){
return (new malli.registry.t_malli$registry13955(_QMARK_registries,registries,meta13956));
});


malli.registry.composite_registry = (function malli$registry$composite_registry(var_args){
var args__5732__auto__ = [];
var len__5726__auto___13975 = arguments.length;
var i__5727__auto___13976 = (0);
while(true){
if((i__5727__auto___13976 < len__5726__auto___13975)){
args__5732__auto__.push((arguments[i__5727__auto___13976]));

var G__13977 = (i__5727__auto___13976 + (1));
i__5727__auto___13976 = G__13977;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((0) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((0)),(0),null)):null);
return malli.registry.composite_registry.cljs$core$IFn$_invoke$arity$variadic(argseq__5733__auto__);
});

(malli.registry.composite_registry.cljs$core$IFn$_invoke$arity$variadic = (function (_QMARK_registries){
var registries = cljs.core.mapv.cljs$core$IFn$_invoke$arity$2(malli.registry.registry,_QMARK_registries);
return (new malli.registry.t_malli$registry13955(_QMARK_registries,registries,cljs.core.PersistentArrayMap.EMPTY));
}));

(malli.registry.composite_registry.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(malli.registry.composite_registry.cljs$lang$applyTo = (function (seq13954){
var self__5712__auto__ = this;
return self__5712__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq(seq13954));
}));


/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13958 = (function (db,meta13959){
this.db = db;
this.meta13959 = meta13959;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13958.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13960,meta13959__$1){
var self__ = this;
var _13960__$1 = this;
return (new malli.registry.t_malli$registry13958(self__.db,meta13959__$1));
}));

(malli.registry.t_malli$registry13958.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13960){
var self__ = this;
var _13960__$1 = this;
return self__.meta13959;
}));

(malli.registry.t_malli$registry13958.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13958.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
return malli.registry._schema(malli.registry.registry(cljs.core.deref(self__.db)),type);
}));

(malli.registry.t_malli$registry13958.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return malli.registry._schemas(malli.registry.registry(cljs.core.deref(self__.db)));
}));

(malli.registry.t_malli$registry13958.getBasis = (function (){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"db","db",-1661185010,null),new cljs.core.Symbol(null,"meta13959","meta13959",-1073157596,null)], null);
}));

(malli.registry.t_malli$registry13958.cljs$lang$type = true);

(malli.registry.t_malli$registry13958.cljs$lang$ctorStr = "malli.registry/t_malli$registry13958");

(malli.registry.t_malli$registry13958.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13958");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13958.
 */
malli.registry.__GT_t_malli$registry13958 = (function malli$registry$__GT_t_malli$registry13958(db,meta13959){
return (new malli.registry.t_malli$registry13958(db,meta13959));
});


malli.registry.mutable_registry = (function malli$registry$mutable_registry(db){
return (new malli.registry.t_malli$registry13958(db,cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13961 = (function (meta13962){
this.meta13962 = meta13962;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13961.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13963,meta13962__$1){
var self__ = this;
var _13963__$1 = this;
return (new malli.registry.t_malli$registry13961(meta13962__$1));
}));

(malli.registry.t_malli$registry13961.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13963){
var self__ = this;
var _13963__$1 = this;
return self__.meta13962;
}));

(malli.registry.t_malli$registry13961.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13961.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
if(cljs.core.var_QMARK_(type)){
return cljs.core.deref(type);
} else {
return null;
}
}));

(malli.registry.t_malli$registry13961.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return null;
}));

(malli.registry.t_malli$registry13961.getBasis = (function (){
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"meta13962","meta13962",-357495125,null)], null);
}));

(malli.registry.t_malli$registry13961.cljs$lang$type = true);

(malli.registry.t_malli$registry13961.cljs$lang$ctorStr = "malli.registry/t_malli$registry13961");

(malli.registry.t_malli$registry13961.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13961");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13961.
 */
malli.registry.__GT_t_malli$registry13961 = (function malli$registry$__GT_t_malli$registry13961(meta13962){
return (new malli.registry.t_malli$registry13961(meta13962));
});


malli.registry.var_registry = (function malli$registry$var_registry(){
return (new malli.registry.t_malli$registry13961(cljs.core.PersistentArrayMap.EMPTY));
});
malli.registry._STAR_registry_STAR_ = cljs.core.PersistentArrayMap.EMPTY;

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13964 = (function (meta13965){
this.meta13965 = meta13965;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13964.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13966,meta13965__$1){
var self__ = this;
var _13966__$1 = this;
return (new malli.registry.t_malli$registry13964(meta13965__$1));
}));

(malli.registry.t_malli$registry13964.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13966){
var self__ = this;
var _13966__$1 = this;
return self__.meta13965;
}));

(malli.registry.t_malli$registry13964.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13964.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,type){
var self__ = this;
var ___$1 = this;
return malli.registry._schema(malli.registry.registry(malli.registry._STAR_registry_STAR_),type);
}));

(malli.registry.t_malli$registry13964.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return malli.registry._schemas(malli.registry.registry(malli.registry._STAR_registry_STAR_));
}));

(malli.registry.t_malli$registry13964.getBasis = (function (){
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"meta13965","meta13965",-1399629969,null)], null);
}));

(malli.registry.t_malli$registry13964.cljs$lang$type = true);

(malli.registry.t_malli$registry13964.cljs$lang$ctorStr = "malli.registry/t_malli$registry13964");

(malli.registry.t_malli$registry13964.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13964");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13964.
 */
malli.registry.__GT_t_malli$registry13964 = (function malli$registry$__GT_t_malli$registry13964(meta13965){
return (new malli.registry.t_malli$registry13964(meta13965));
});


malli.registry.dynamic_registry = (function malli$registry$dynamic_registry(){
return (new malli.registry.t_malli$registry13964(cljs.core.PersistentArrayMap.EMPTY));
});

/**
* @constructor
 * @implements {malli.registry.Registry}
 * @implements {cljs.core.IMeta}
 * @implements {cljs.core.IWithMeta}
*/
malli.registry.t_malli$registry13967 = (function (default_registry,provider,cache_STAR_,registry_STAR_,meta13968){
this.default_registry = default_registry;
this.provider = provider;
this.cache_STAR_ = cache_STAR_;
this.registry_STAR_ = registry_STAR_;
this.meta13968 = meta13968;
this.cljs$lang$protocol_mask$partition0$ = 393216;
this.cljs$lang$protocol_mask$partition1$ = 0;
});
(malli.registry.t_malli$registry13967.prototype.cljs$core$IWithMeta$_with_meta$arity$2 = (function (_13969,meta13968__$1){
var self__ = this;
var _13969__$1 = this;
return (new malli.registry.t_malli$registry13967(self__.default_registry,self__.provider,self__.cache_STAR_,self__.registry_STAR_,meta13968__$1));
}));

(malli.registry.t_malli$registry13967.prototype.cljs$core$IMeta$_meta$arity$1 = (function (_13969){
var self__ = this;
var _13969__$1 = this;
return self__.meta13968;
}));

(malli.registry.t_malli$registry13967.prototype.malli$registry$Registry$ = cljs.core.PROTOCOL_SENTINEL);

(malli.registry.t_malli$registry13967.prototype.malli$registry$Registry$_schema$arity$2 = (function (_,name){
var self__ = this;
var ___$1 = this;
var or__5002__auto__ = (function (){var fexpr__13970 = cljs.core.deref(self__.cache_STAR_);
return (fexpr__13970.cljs$core$IFn$_invoke$arity$1 ? fexpr__13970.cljs$core$IFn$_invoke$arity$1(name) : fexpr__13970.call(null,name));
})();
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
var temp__5804__auto__ = (function (){var G__13971 = name;
var G__13972 = cljs.core.deref(self__.registry_STAR_);
return (self__.provider.cljs$core$IFn$_invoke$arity$2 ? self__.provider.cljs$core$IFn$_invoke$arity$2(G__13971,G__13972) : self__.provider.call(null,G__13971,G__13972));
})();
if(cljs.core.truth_(temp__5804__auto__)){
var schema = temp__5804__auto__;
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$4(self__.cache_STAR_,cljs.core.assoc,name,schema);

return schema;
} else {
return null;
}
}
}));

(malli.registry.t_malli$registry13967.prototype.malli$registry$Registry$_schemas$arity$1 = (function (_){
var self__ = this;
var ___$1 = this;
return cljs.core.deref(self__.cache_STAR_);
}));

(malli.registry.t_malli$registry13967.getBasis = (function (){
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"default-registry","default-registry",732204441,null),new cljs.core.Symbol(null,"provider","provider",1338474627,null),new cljs.core.Symbol(null,"cache*","cache*",-548597526,null),new cljs.core.Symbol(null,"registry*","registry*",-268031273,null),new cljs.core.Symbol(null,"meta13968","meta13968",1585028430,null)], null);
}));

(malli.registry.t_malli$registry13967.cljs$lang$type = true);

(malli.registry.t_malli$registry13967.cljs$lang$ctorStr = "malli.registry/t_malli$registry13967");

(malli.registry.t_malli$registry13967.cljs$lang$ctorPrWriter = (function (this__5287__auto__,writer__5288__auto__,opt__5289__auto__){
return cljs.core._write(writer__5288__auto__,"malli.registry/t_malli$registry13967");
}));

/**
 * Positional factory function for malli.registry/t_malli$registry13967.
 */
malli.registry.__GT_t_malli$registry13967 = (function malli$registry$__GT_t_malli$registry13967(default_registry,provider,cache_STAR_,registry_STAR_,meta13968){
return (new malli.registry.t_malli$registry13967(default_registry,provider,cache_STAR_,registry_STAR_,meta13968));
});


malli.registry.lazy_registry = (function malli$registry$lazy_registry(default_registry,provider){
var cache_STAR_ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var registry_STAR_ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(default_registry);
return cljs.core.reset_BANG_(registry_STAR_,malli.registry.composite_registry.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([default_registry,(new malli.registry.t_malli$registry13967(default_registry,provider,cache_STAR_,registry_STAR_,cljs.core.PersistentArrayMap.EMPTY))], 0)));
});
/**
 * finds a schema from a registry
 */
malli.registry.schema = (function malli$registry$schema(registry,type){
return malli.registry._schema(registry,type);
});
/**
 * finds all schemas from a registry
 */
malli.registry.schemas = (function malli$registry$schemas(registry){
return malli.registry._schemas(registry);
});

//# sourceMappingURL=malli.registry.js.map
