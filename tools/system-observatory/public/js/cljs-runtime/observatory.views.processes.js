goog.provide('observatory.views.processes');
/**
 * Format bytes as human-readable string.
 */
observatory.views.processes.format_bytes = (function observatory$views$processes$format_bytes(bytes){
if((bytes == null)){
return "-";
} else {
if((bytes < (1024))){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(bytes)," B"].join('');
} else {
if((bytes < ((1024) * (1024)))){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(((bytes / (1024)) | (0)))," KB"].join('');
} else {
if((bytes < (((1024) * (1024)) * (1024)))){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1((((bytes / (1024)) / (1024)) | (0)))," MB"].join('');
} else {
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1((((bytes / (1024)) / (1024)) / (1024)).toFixed((1)))," GB"].join('');

}
}
}
}
});
/**
 * Format percentage with one decimal.
 */
observatory.views.processes.format_percent = (function observatory$views$processes$format_percent(pct){
if(cljs.core.truth_(pct)){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(pct.toFixed((1))),"%"].join('');
} else {
return "-";
}
});
/**
 * A single process row. Clickable for inspection.
 */
observatory.views.processes.process_row = (function observatory$views$processes$process_row(p__12814){
var map__12815 = p__12814;
var map__12815__$1 = cljs.core.__destructure_map(map__12815);
var process__$1 = map__12815__$1;
var pid = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12815__$1,new cljs.core.Keyword(null,"pid","pid",1018387698));
var name = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12815__$1,new cljs.core.Keyword(null,"name","name",1843675177));
var cpu_percent = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12815__$1,new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682));
var memory_bytes = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12815__$1,new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262));
var threads = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12815__$1,new cljs.core.Keyword(null,"threads","threads",-1717798734));
var state = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12815__$1,new cljs.core.Keyword(null,"state","state",-1988618099));
return new cljs.core.PersistentVector(null, 8, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"tr.process-row","tr.process-row",542797835),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","select","observatory.state/select",-638815980),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"process","process",1643192938),new cljs.core.Keyword(null,"data","data",-232669377),process__$1], null)], null));
}),new cljs.core.Keyword(null,"class","class",-2030961996),((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(state,new cljs.core.Keyword(null,"stopped","stopped",-1490414640)))?"stopped":null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"td.pid","td.pid",-684609614),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.drill-link","button.drill-link",-669642478),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (e){
e.stopPropagation();

return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","select","observatory.state/select",-638815980),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"process","process",1643192938),new cljs.core.Keyword(null,"data","data",-232669377),process__$1], null)], null));
})], null),pid], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"td.name","td.name",-643029721),name], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"td.cpu","td.cpu",-1324492045),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.metric-cell","div.metric-cell",-258492991),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.bar-bg","div.bar-bg",-1915012355),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.bar-fill.cpu","div.bar-fill.cpu",-1943891696),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"width","width",-384071477),[cljs.core.str.cljs$core$IFn$_invoke$arity$1((function (){var x__5090__auto__ = cpu_percent;
var y__5091__auto__ = (100);
return ((x__5090__auto__ < y__5091__auto__) ? x__5090__auto__ : y__5091__auto__);
})()),"%"].join('')], null)], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),observatory.views.processes.format_percent(cpu_percent)], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"td.memory","td.memory",1551241336),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.metric-cell","div.metric-cell",-258492991),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.bar-bg","div.bar-bg",-1915012355),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.bar-fill.memory","div.bar-fill.memory",-358264094),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"style","style",-496642736),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"width","width",-384071477),[cljs.core.str.cljs$core$IFn$_invoke$arity$1((function (){var x__5090__auto__ = ((memory_bytes / ((((4) * (1024)) * (1024)) * (1024))) * (100));
var y__5091__auto__ = (100);
return ((x__5090__auto__ < y__5091__auto__) ? x__5090__auto__ : y__5091__auto__);
})()),"%"].join('')], null)], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),observatory.views.processes.format_bytes(memory_bytes)], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"td.threads","td.threads",-2056784751),threads], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"td.state","td.state",-121812290),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.state-badge","span.state-badge",494248456),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),cljs.core.name((function (){var or__5002__auto__ = state;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return new cljs.core.Keyword(null,"running","running",1554969103);
}
})())], null),(function (){var or__5002__auto__ = state;
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return new cljs.core.Keyword(null,"running","running",1554969103);
}
})()], null)], null)], null);
});
/**
 * Shows sort direction arrow.
 */
observatory.views.processes.sort_indicator = (function observatory$views$processes$sort_indicator(current_sort,column){
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_sort,column)){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.sort-arrow","span.sort-arrow",-730342613),"\u25BC"], null);
} else {
return null;
}
});
/**
 * Main process table with sortable columns.
 */
observatory.views.processes.process_table = (function observatory$views$processes$process_table(){
var processes = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","processes-by-cpu","observatory.state/processes-by-cpu",-1152846487)], null)));
var current_sort = new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682);
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.process-table-container","div.process-table-container",-1649880240),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"table.process-table","table.process-table",1982535756),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"thead","thead",-291875296),new cljs.core.PersistentVector(null, 7, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"tr","tr",-1424774646),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"th.pid","th.pid",-2015493103),"PID"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"th.name","th.name",-106713050),"Name"], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"th.cpu","th.cpu",-1383581729),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),"sortable"], null),"CPU",new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.sort_indicator,current_sort,new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682)], null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"th.memory","th.memory",-1680877709),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),"sortable"], null),"Memory",new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.sort_indicator,current_sort,new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"th.threads","th.threads",-534707074),"Threads"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"th.state","th.state",652090620),"State"], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"tbody","tbody",-80678300),(function (){var iter__5480__auto__ = (function observatory$views$processes$process_table_$_iter__12816(s__12817){
return (new cljs.core.LazySeq(null,(function (){
var s__12817__$1 = s__12817;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12817__$1);
if(temp__5804__auto__){
var s__12817__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12817__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12817__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12819 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12818 = (0);
while(true){
if((i__12818 < size__5479__auto__)){
var process__$1 = cljs.core._nth(c__5478__auto__,i__12818);
cljs.core.chunk_append(b__12819,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.process_row,process__$1], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(process__$1)], null)));

var G__12820 = (i__12818 + (1));
i__12818 = G__12820;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12819),observatory$views$processes$process_table_$_iter__12816(cljs.core.chunk_rest(s__12817__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12819),null);
}
} else {
var process__$1 = cljs.core.first(s__12817__$2);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.process_row,process__$1], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(process__$1)], null)),observatory$views$processes$process_table_$_iter__12816(cljs.core.rest(s__12817__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(processes);
})()], null)], null)], null);
});
/**
 * Overview stats for all processes.
 */
observatory.views.processes.summary_stats = (function observatory$views$processes$summary_stats(){
var processes = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","processes","observatory.state/processes",-1696660641)], null)));
if(cljs.core.seq(processes)){
var total_cpu = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),processes));
var total_memory = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),processes));
var total_threads = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"threads","threads",-1717798734),processes));
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.summary-stats","div.summary-stats",-1776803187),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-card","div.stat-card",-1315972816),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-value","div.stat-value",1336227416),cljs.core.count(processes)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-label","div.stat-label",-720687688),"Processes"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-card","div.stat-card",-1315972816),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-value","div.stat-value",1336227416),observatory.views.processes.format_percent(total_cpu)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-label","div.stat-label",-720687688),"Total CPU"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-card","div.stat-card",-1315972816),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-value","div.stat-value",1336227416),observatory.views.processes.format_bytes(total_memory)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-label","div.stat-label",-720687688),"Total Memory"], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-card","div.stat-card",-1315972816),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-value","div.stat-value",1336227416),total_threads], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat-label","div.stat-label",-720687688),"Total Threads"], null)], null)], null);
} else {
return null;
}
});
/**
 * Action bar for process operations.
 */
observatory.views.processes.quick_actions = (function observatory$views$processes$quick_actions(){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.quick-actions","div.quick-actions",1933196282),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input.search-input","input.search-input",-470560783),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),"text",new cljs.core.Keyword(null,"placeholder","placeholder",-104873083),"Filter processes..."], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.action-buttons","div.action-buttons",209691206),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.btn","button.btn",-661984613),"Refresh"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.btn","button.btn",-661984613),"Tree View"], null)], null)], null);
});
/**
 * Complete processes view - Task Manager style.
 */
observatory.views.processes.processes_view = (function observatory$views$processes$processes_view(){
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.processes-view","div.processes-view",-927583139),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.view-header","div.view-header",1229518712),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h2","h2",-372662728),"Processes"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"p.hint","p.hint",-144164893),"Click any process to inspect. Drill into memory, cache, I/O."], null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.quick_actions], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.summary_stats], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.process_table], null)], null);
});

//# sourceMappingURL=observatory.views.processes.js.map
