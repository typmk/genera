goog.provide('observatory.views.inspector');
/**
 * Format a value for display based on its type.
 */
observatory.views.inspector.format_value = (function observatory$views$inspector$format_value(v){
if((v == null)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.nil","span.nil",-2082878112),"nil"], null);
} else {
if(cljs.core.boolean_QMARK_(v)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.boolean","span.boolean",1812414298),cljs.core.str.cljs$core$IFn$_invoke$arity$1(v)], null);
} else {
if(typeof v === 'number'){
if((v > (65535))){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.hex","span.hex",-1410411797),["0x",cljs.core.str.cljs$core$IFn$_invoke$arity$1(v.toString((16)))].join('')], null);
} else {
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.number","span.number",1244984954),cljs.core.str.cljs$core$IFn$_invoke$arity$1(v)], null);
}
} else {
if(typeof v === 'string'){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.string","span.string",-721349845),cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([v], 0))], null);
} else {
if((v instanceof cljs.core.Keyword)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.keyword","span.keyword",-1670745058),cljs.core.str.cljs$core$IFn$_invoke$arity$1(v)], null);
} else {
if(cljs.core.vector_QMARK_(v)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.collection","span.collection",-515875089),["[",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.count(v))," items]"].join('')], null);
} else {
if(cljs.core.map_QMARK_(v)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.collection","span.collection",-515875089),["{",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.count(v))," keys}"].join('')], null);
} else {
if(cljs.core.seq_QMARK_(v)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.collection","span.collection",-515875089),["(",cljs.core.str.cljs$core$IFn$_invoke$arity$1(cljs.core.count(v))," items)"].join('')], null);
} else {
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.unknown","span.unknown",-106894179),cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([v], 0))], null);

}
}
}
}
}
}
}
}
});
/**
 * Truncate string to max length.
 */
observatory.views.inspector.truncate = (function observatory$views$inspector$truncate(s,max_len){
if((cljs.core.count(s) > max_len)){
return [cljs.core.subs.cljs$core$IFn$_invoke$arity$3(s,(0),(max_len - (3))),"..."].join('');
} else {
return s;
}
});
/**
 * Check if a value can be drilled into.
 */
observatory.views.inspector.inspectable_QMARK_ = (function observatory$views$inspector$inspectable_QMARK_(v){
return ((cljs.core.map_QMARK_(v)) || (((cljs.core.vector_QMARK_(v)) || (cljs.core.seq_QMARK_(v)))));
});
/**
 * A single field in the inspector.
 */
observatory.views.inspector.field_row = (function observatory$views$inspector$field_row(p__12887){
var map__12888 = p__12887;
var map__12888__$1 = cljs.core.__destructure_map(map__12888);
var key = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12888__$1,new cljs.core.Keyword(null,"key","key",-1516042587));
var value = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12888__$1,new cljs.core.Keyword(null,"value","value",305978217));
var depth = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12888__$1,new cljs.core.Keyword(null,"depth","depth",1768663640));
var on_expand = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12888__$1,new cljs.core.Keyword(null,"on-expand","on-expand",-525903108));
var can_expand_QMARK_ = observatory.views.inspector.inspectable_QMARK_(value);
var indent = (depth * (16));
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.field-row","div.field-row",-358188393),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"padding-left","padding-left",-1180879053),[cljs.core.str.cljs$core$IFn$_invoke$arity$1(indent),"px"].join('')], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.field-key","span.field-key",-749414623),cljs.core.name(key)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.field-sep","span.field-sep",-1286618759),": "], null),((can_expand_QMARK_)?new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.field-value.expandable","button.field-value.expandable",-1354199798),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
if(cljs.core.truth_(on_expand)){
return (on_expand.cljs$core$IFn$_invoke$arity$2 ? on_expand.cljs$core$IFn$_invoke$arity$2(key,value) : on_expand.call(null,key,value));
} else {
return null;
}
})], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.format_value,value], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.expand-hint","span.expand-hint",-88908691)," \u25B6"], null)], null):new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.field-value","span.field-value",1475511354),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.format_value,value], null)], null))], null);
});
/**
 * Render all fields of a map.
 */
observatory.views.inspector.map_fields = (function observatory$views$inspector$map_fields(p__12889){
var map__12890 = p__12889;
var map__12890__$1 = cljs.core.__destructure_map(map__12890);
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12890__$1,new cljs.core.Keyword(null,"data","data",-232669377));
var depth = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12890__$1,new cljs.core.Keyword(null,"depth","depth",1768663640),(0));
var on_select = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12890__$1,new cljs.core.Keyword(null,"on-select","on-select",-192407950));
var max_fields = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12890__$1,new cljs.core.Keyword(null,"max-fields","max-fields",1566177472),(50));
var entries = cljs.core.take.cljs$core$IFn$_invoke$arity$2(max_fields,cljs.core.seq(data));
var remaining = (cljs.core.count(data) - max_fields);
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.map-fields","div.map-fields",-980060600),(function (){var iter__5480__auto__ = (function observatory$views$inspector$map_fields_$_iter__12891(s__12892){
return (new cljs.core.LazySeq(null,(function (){
var s__12892__$1 = s__12892;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12892__$1);
if(temp__5804__auto__){
var s__12892__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12892__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12892__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12894 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12893 = (0);
while(true){
if((i__12893 < size__5479__auto__)){
var vec__12895 = cljs.core._nth(c__5478__auto__,i__12893);
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12895,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12895,(1),null);
cljs.core.chunk_append(b__12894,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.field_row,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"key","key",-1516042587),k,new cljs.core.Keyword(null,"value","value",305978217),v,new cljs.core.Keyword(null,"depth","depth",1768663640),depth,new cljs.core.Keyword(null,"on-expand","on-expand",-525903108),((function (i__12893,vec__12895,k,v,c__5478__auto__,size__5479__auto__,b__12894,s__12892__$2,temp__5804__auto__,entries,remaining,map__12890,map__12890__$1,data,depth,on_select,max_fields){
return (function (k__$1,v__$1){
if(cljs.core.truth_(on_select)){
var G__12898 = new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),((cljs.core.map_QMARK_(v__$1))?new cljs.core.Keyword(null,"map","map",1371690461):((cljs.core.vector_QMARK_(v__$1))?new cljs.core.Keyword(null,"vector","vector",1902966158):new cljs.core.Keyword(null,"value","value",305978217)
)),new cljs.core.Keyword(null,"key","key",-1516042587),k__$1,new cljs.core.Keyword(null,"data","data",-232669377),v__$1], null);
return (on_select.cljs$core$IFn$_invoke$arity$1 ? on_select.cljs$core$IFn$_invoke$arity$1(G__12898) : on_select.call(null,G__12898));
} else {
return null;
}
});})(i__12893,vec__12895,k,v,c__5478__auto__,size__5479__auto__,b__12894,s__12892__$2,temp__5804__auto__,entries,remaining,map__12890,map__12890__$1,data,depth,on_select,max_fields))
], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),cljs.core.str.cljs$core$IFn$_invoke$arity$1(k)], null)));

var G__12924 = (i__12893 + (1));
i__12893 = G__12924;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12894),observatory$views$inspector$map_fields_$_iter__12891(cljs.core.chunk_rest(s__12892__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12894),null);
}
} else {
var vec__12899 = cljs.core.first(s__12892__$2);
var k = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12899,(0),null);
var v = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12899,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.field_row,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"key","key",-1516042587),k,new cljs.core.Keyword(null,"value","value",305978217),v,new cljs.core.Keyword(null,"depth","depth",1768663640),depth,new cljs.core.Keyword(null,"on-expand","on-expand",-525903108),((function (vec__12899,k,v,s__12892__$2,temp__5804__auto__,entries,remaining,map__12890,map__12890__$1,data,depth,on_select,max_fields){
return (function (k__$1,v__$1){
if(cljs.core.truth_(on_select)){
var G__12902 = new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),((cljs.core.map_QMARK_(v__$1))?new cljs.core.Keyword(null,"map","map",1371690461):((cljs.core.vector_QMARK_(v__$1))?new cljs.core.Keyword(null,"vector","vector",1902966158):new cljs.core.Keyword(null,"value","value",305978217)
)),new cljs.core.Keyword(null,"key","key",-1516042587),k__$1,new cljs.core.Keyword(null,"data","data",-232669377),v__$1], null);
return (on_select.cljs$core$IFn$_invoke$arity$1 ? on_select.cljs$core$IFn$_invoke$arity$1(G__12902) : on_select.call(null,G__12902));
} else {
return null;
}
});})(vec__12899,k,v,s__12892__$2,temp__5804__auto__,entries,remaining,map__12890,map__12890__$1,data,depth,on_select,max_fields))
], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),cljs.core.str.cljs$core$IFn$_invoke$arity$1(k)], null)),observatory$views$inspector$map_fields_$_iter__12891(cljs.core.rest(s__12892__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(entries);
})(),(((remaining > (0)))?new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.truncated","div.truncated",-1676423166),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"padding-left","padding-left",-1180879053),[cljs.core.str.cljs$core$IFn$_invoke$arity$1((depth * (16))),"px"].join('')], null)], null),["... and ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(remaining)," more fields"].join('')], null):null)], null);
});
if((typeof observatory !== 'undefined') && (typeof observatory.views !== 'undefined') && (typeof observatory.views.inspector !== 'undefined') && (typeof observatory.views.inspector.inspect_object !== 'undefined')){
} else {
/**
 * Render inspector content based on selection type.
 */
observatory.views.inspector.inspect_object = (function (){var method_table__5599__auto__ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var prefer_table__5600__auto__ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var method_cache__5601__auto__ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var cached_hierarchy__5602__auto__ = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentArrayMap.EMPTY);
var hierarchy__5603__auto__ = cljs.core.get.cljs$core$IFn$_invoke$arity$3(cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"hierarchy","hierarchy",-1053470341),(function (){var fexpr__12903 = cljs.core.get_global_hierarchy;
return (fexpr__12903.cljs$core$IFn$_invoke$arity$0 ? fexpr__12903.cljs$core$IFn$_invoke$arity$0() : fexpr__12903.call(null));
})());
return (new cljs.core.MultiFn(cljs.core.symbol.cljs$core$IFn$_invoke$arity$2("observatory.views.inspector","inspect-object"),(function (selection){
return new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(selection);
}),new cljs.core.Keyword(null,"default","default",-1987822328),hierarchy__5603__auto__,method_table__5599__auto__,prefer_table__5600__auto__,method_cache__5601__auto__,cached_hierarchy__5602__auto__));
})();
}
observatory.views.inspector.inspect_object.cljs$core$IMultiFn$_add_method$arity$3(null,new cljs.core.Keyword(null,"process","process",1643192938),(function (p__12904){
var map__12905 = p__12904;
var map__12905__$1 = cljs.core.__destructure_map(map__12905);
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12905__$1,new cljs.core.Keyword(null,"data","data",-232669377));
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-content","div.inspector-content",-1839007505),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-header","div.inspector-header",1681336344),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3.object-type","h3.object-type",1404719722),"Process"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2.object-title","h2.object-title",-1158508126),new cljs.core.Keyword(null,"name","name",1843675177).cljs$core$IFn$_invoke$arity$1(data)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.object-id","div.object-id",792202789),"PID: ",new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(data)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Overview"], null),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.overview-grid","div.overview-grid",-559973633),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.overview-item","div.overview-item",1871063104),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"CPU"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682).cljs$core$IFn$_invoke$arity$1(data).toFixed((1)),"%"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.overview-item","div.overview-item",1871063104),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Memory"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),(((new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262).cljs$core$IFn$_invoke$arity$1(data) / (1024)) / (1024)) | (0))," MB"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.overview-item","div.overview-item",1871063104),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Threads"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.Keyword(null,"threads","threads",-1717798734).cljs$core$IFn$_invoke$arity$1(data)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.overview-item","div.overview-item",1871063104),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"State"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"state","state",-1988618099).cljs$core$IFn$_invoke$arity$1(data))], null)], null)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"All Fields"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.map_fields,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"data","data",-232669377),data], null)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Actions"], null),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.action-buttons","div.action-buttons",209691206),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"View Memory Regions"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"View Cache Behavior"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"View I/O Activity"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"Attach Trace"], null)], null)], null)], null);
}));
observatory.views.inspector.inspect_object.cljs$core$IMultiFn$_add_method$arity$3(null,new cljs.core.Keyword(null,"cache-line","cache-line",596120169),(function (p__12906){
var map__12907 = p__12906;
var map__12907__$1 = cljs.core.__destructure_map(map__12907);
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12907__$1,new cljs.core.Keyword(null,"data","data",-232669377));
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-content","div.inspector-content",-1839007505),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-header","div.inspector-header",1681336344),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3.object-type","h3.object-type",1404719722),"Cache Line"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2.object-title","h2.object-title",-1158508126),["0x",cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Keyword(null,"tag","tag",-1290361223).cljs$core$IFn$_invoke$arity$1(data).toString((16)))].join('')], null),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.object-id","div.object-id",792202789),"Set ",new cljs.core.Keyword(null,"set-index","set-index",797648137).cljs$core$IFn$_invoke$arity$1(data),", Way ",new cljs.core.Keyword(null,"way-index","way-index",1268630527).cljs$core$IFn$_invoke$arity$1(data)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"State"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.cache-line-state","div.cache-line-state",398489786),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.mesi-badge","span.mesi-badge",-389011479),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),cljs.core.name(new cljs.core.Keyword(null,"state","state",-1988618099).cljs$core$IFn$_invoke$arity$1(data))], null),clojure.string.upper_case(cljs.core.name(new cljs.core.Keyword(null,"state","state",-1988618099).cljs$core$IFn$_invoke$arity$1(data)))], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.lru","span.lru",1273823232),"LRU: ",new cljs.core.Keyword(null,"lru-position","lru-position",-409287516).cljs$core$IFn$_invoke$arity$1(data)], null)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Data (64 bytes)"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.hex-view","div.hex-view",-1859128194),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"pre.hex-dump","pre.hex-dump",1321297839),"00 01 02 03 04 05 06 07  08 09 0A 0B 0C 0D 0E 0F\n10 11 12 13 14 15 16 17  18 19 1A 1B 1C 1D 1E 1F\n..."], null)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Actions"], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.action-buttons","div.action-buttons",209691206),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"View in RAM"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"Track Accesses"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.action-btn","button.action-btn",2047662660),"Decode as Instructions"], null)], null)], null)], null);
}));
observatory.views.inspector.inspect_object.cljs$core$IMultiFn$_add_method$arity$3(null,new cljs.core.Keyword(null,"map","map",1371690461),(function (p__12908){
var map__12909 = p__12908;
var map__12909__$1 = cljs.core.__destructure_map(map__12909);
var key = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12909__$1,new cljs.core.Keyword(null,"key","key",-1516042587));
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12909__$1,new cljs.core.Keyword(null,"data","data",-232669377));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-content","div.inspector-content",-1839007505),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-header","div.inspector-header",1681336344),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3.object-type","h3.object-type",1404719722),"Map"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2.object-title","h2.object-title",-1158508126),cljs.core.str.cljs$core$IFn$_invoke$arity$1(key)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.object-id","div.object-id",792202789),cljs.core.count(data)," keys"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Fields"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.map_fields,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"data","data",-232669377),data], null)], null)], null)], null);
}));
observatory.views.inspector.inspect_object.cljs$core$IMultiFn$_add_method$arity$3(null,new cljs.core.Keyword(null,"vector","vector",1902966158),(function (p__12910){
var map__12911 = p__12910;
var map__12911__$1 = cljs.core.__destructure_map(map__12911);
var key = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12911__$1,new cljs.core.Keyword(null,"key","key",-1516042587));
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12911__$1,new cljs.core.Keyword(null,"data","data",-232669377));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-content","div.inspector-content",-1839007505),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-header","div.inspector-header",1681336344),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3.object-type","h3.object-type",1404719722),"Vector"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2.object-title","h2.object-title",-1158508126),cljs.core.str.cljs$core$IFn$_invoke$arity$1(key)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.object-id","div.object-id",792202789),cljs.core.count(data)," items"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Items"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.vector-items","div.vector-items",-405967827),(function (){var iter__5480__auto__ = (function observatory$views$inspector$iter__12912(s__12913){
return (new cljs.core.LazySeq(null,(function (){
var s__12913__$1 = s__12913;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12913__$1);
if(temp__5804__auto__){
var s__12913__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12913__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12913__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12915 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12914 = (0);
while(true){
if((i__12914 < size__5479__auto__)){
var vec__12916 = cljs.core._nth(c__5478__auto__,i__12914);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12916,(0),null);
var item = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12916,(1),null);
cljs.core.chunk_append(b__12915,cljs.core.with_meta(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.vector-item","div.vector-item",-2061088203),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.index","span.index",-483391535),idx], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.format_value,item], null)], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)));

var G__12925 = (i__12914 + (1));
i__12914 = G__12925;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12915),observatory$views$inspector$iter__12912(cljs.core.chunk_rest(s__12913__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12915),null);
}
} else {
var vec__12919 = cljs.core.first(s__12913__$2);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12919,(0),null);
var item = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12919,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.vector-item","div.vector-item",-2061088203),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.index","span.index",-483391535),idx], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.format_value,item], null)], null)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)),observatory$views$inspector$iter__12912(cljs.core.rest(s__12913__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.take.cljs$core$IFn$_invoke$arity$2((100),cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2(cljs.core.vector,data)));
})()], null)], null)], null);
}));
observatory.views.inspector.inspect_object.cljs$core$IMultiFn$_add_method$arity$3(null,new cljs.core.Keyword(null,"default","default",-1987822328),(function (p__12922){
var map__12923 = p__12922;
var map__12923__$1 = cljs.core.__destructure_map(map__12923);
var type = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12923__$1,new cljs.core.Keyword(null,"type","type",1174270348));
var data = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12923__$1,new cljs.core.Keyword(null,"data","data",-232669377));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-content","div.inspector-content",-1839007505),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-header","div.inspector-header",1681336344),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3.object-type","h3.object-type",1404719722),cljs.core.str.cljs$core$IFn$_invoke$arity$1(type)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2.object-title","h2.object-title",-1158508126),"Object"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-section","div.inspector-section",1890067634),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h4","h4",2004862993),"Data"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"pre.raw-data","pre.raw-data",-72994166),cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([data], 0))], null)], null)], null);
}));
/**
 * The main inspector panel - slides in from the right.
 */
observatory.views.inspector.inspector_panel = (function observatory$views$inspector$inspector_panel(){
var selection = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","selection","observatory.state/selection",-863638346)], null)));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"aside.inspector-panel","aside.inspector-panel",-1426002717),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.inspector-toolbar","div.inspector-toolbar",1923505075),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.close-btn","button.close-btn",-1715272268),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","toggle-inspector","observatory.state/toggle-inspector",1524980449)], null));
})], null),"\u2715"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.back-btn","button.back-btn",-1598918817),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","drill-up","observatory.state/drill-up",686702214)], null));
})], null),"\u2190 Back"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.toolbar-title","span.toolbar-title",1725658043),"Inspector"], null)], null),(cljs.core.truth_(selection)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.inspect_object,selection], null):new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.no-selection","div.no-selection",1390182394),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"p","p",151049309),"Click any object to inspect it."], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"p.hint","p.hint",-144164893),"Processes, cache lines, memory regions - everything is explorable."], null)], null))], null);
});

//# sourceMappingURL=observatory.views.inspector.js.map
