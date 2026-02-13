goog.provide('observatory.state');
observatory.state.initial_db = new cljs.core.PersistentArrayMap(null, 7, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"current-tab","current-tab",-1305174577),new cljs.core.Keyword(null,"processes","processes",-546984164),new cljs.core.Keyword(null,"sidebar-open?","sidebar-open?",-1099774467),true,new cljs.core.Keyword(null,"inspector-open?","inspector-open?",-1261096585),false], null),new cljs.core.Keyword(null,"drill-path","drill-path",-386489135),cljs.core.PersistentVector.EMPTY,new cljs.core.Keyword(null,"selection","selection",975998651),null,new cljs.core.Keyword(null,"system","system",-29381724),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"processes","processes",-546984164),cljs.core.PersistentVector.EMPTY,new cljs.core.Keyword(null,"network-connections","network-connections",-867013151),cljs.core.PersistentVector.EMPTY,new cljs.core.Keyword(null,"devices","devices",1929380599),cljs.core.PersistentVector.EMPTY], null),new cljs.core.Keyword(null,"hardware","hardware",515915884),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"cpu-topology","cpu-topology",340623578),null,new cljs.core.Keyword(null,"cache-hierarchy","cache-hierarchy",-242050477),null,new cljs.core.Keyword(null,"ram-topology","ram-topology",-249728294),null], null),new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"events","events",1792552201),cljs.core.PersistentVector.EMPTY,new cljs.core.Keyword(null,"metadata","metadata",1799301597),null,new cljs.core.Keyword(null,"playback","playback",-2069416706),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"position","position",-2011731912),(0),new cljs.core.Keyword(null,"playing?","playing?",-1884542863),false,new cljs.core.Keyword(null,"speed","speed",1257663751),1.0], null)], null),new cljs.core.Keyword(null,"simulation","simulation",-1001480470),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"enabled?","enabled?",-1376075057),true,new cljs.core.Keyword(null,"cache-state","cache-state",-272624031),null], null)], null);
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","initialize","observatory.state/initialize",-638402604),(function (_,___$1){
return observatory.state.initial_db;
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","set-tab","observatory.state/set-tab",1591970412),(function (db,p__12767){
var vec__12768 = p__12767;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12768,(0),null);
var tab = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12768,(1),null);
return cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"current-tab","current-tab",-1305174577)], null),tab);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","toggle-sidebar","observatory.state/toggle-sidebar",-1463503518),(function (db,_){
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"sidebar-open?","sidebar-open?",-1099774467)], null),cljs.core.not);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","toggle-inspector","observatory.state/toggle-inspector",1524980449),(function (db,_){
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"inspector-open?","inspector-open?",-1261096585)], null),cljs.core.not);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","select","observatory.state/select",-638815980),(function (db,p__12771){
var vec__12772 = p__12771;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12772,(0),null);
var selection = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12772,(1),null);
return cljs.core.assoc_in(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(db,new cljs.core.Keyword(null,"selection","selection",975998651),selection),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"inspector-open?","inspector-open?",-1261096585)], null),true);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","drill-into","observatory.state/drill-into",-437341094),(function (db,p__12775){
var vec__12776 = p__12775;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12776,(0),null);
var path_segment = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12776,(1),null);
return cljs.core.update.cljs$core$IFn$_invoke$arity$4(db,new cljs.core.Keyword(null,"drill-path","drill-path",-386489135),cljs.core.conj,path_segment);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","drill-up","observatory.state/drill-up",686702214),(function (db,_){
return cljs.core.update.cljs$core$IFn$_invoke$arity$3(db,new cljs.core.Keyword(null,"drill-path","drill-path",-386489135),cljs.core.pop);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","drill-reset","observatory.state/drill-reset",-806492159),(function (db,_){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(db,new cljs.core.Keyword(null,"drill-path","drill-path",-386489135),cljs.core.PersistentVector.EMPTY);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","set-processes","observatory.state/set-processes",-1147965999),(function (db,p__12779){
var vec__12780 = p__12779;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12780,(0),null);
var processes = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12780,(1),null);
return cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"system","system",-29381724),new cljs.core.Keyword(null,"processes","processes",-546984164)], null),processes);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","set-cache-hierarchy","observatory.state/set-cache-hierarchy",2111908951),(function (db,p__12783){
var vec__12784 = p__12783;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12784,(0),null);
var hierarchy = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12784,(1),null);
return cljs.core.assoc_in(cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"hardware","hardware",515915884),new cljs.core.Keyword(null,"cache-hierarchy","cache-hierarchy",-242050477)], null),hierarchy),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"simulation","simulation",-1001480470),new cljs.core.Keyword(null,"cache-state","cache-state",-272624031)], null),hierarchy);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","load-sample-data","observatory.state/load-sample-data",-1293454588),(function (db,_){
var processes = (function (){var iter__5480__auto__ = (function observatory$state$iter__12787(s__12788){
return (new cljs.core.LazySeq(null,(function (){
var s__12788__$1 = s__12788;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12788__$1);
if(temp__5804__auto__){
var s__12788__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12788__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12788__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12790 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12789 = (0);
while(true){
if((i__12789 < size__5479__auto__)){
var i = cljs.core._nth(c__5478__auto__,i__12789);
cljs.core.chunk_append(b__12790,observatory.process.make_process.cljs$core$IFn$_invoke$arity$variadic(((1000) + (i * (100))),cljs.core.rand_nth(new cljs.core.PersistentVector(null, 12, 5, cljs.core.PersistentVector.EMPTY_NODE, ["chrome.exe","firefox.exe","code.exe","explorer.exe","System","svchost.exe","dwm.exe","node.exe","rust-analyzer.exe","wsl.exe","docker.exe","spotify.exe"], null)),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"ppid","ppid",215811440),(((i > (0)))?((1000) + (cljs.core.rand_int(i) * (100))):null),new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),cljs.core.rand_int(((((2) * (1024)) * (1024)) * (1024))),new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),(cljs.core.rand.cljs$core$IFn$_invoke$arity$0() * 25.0),new cljs.core.Keyword(null,"threads","threads",-1717798734),(cljs.core.rand_int((100)) + (1))], 0)));

var G__12812 = (i__12789 + (1));
i__12789 = G__12812;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12790),observatory$state$iter__12787(cljs.core.chunk_rest(s__12788__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12790),null);
}
} else {
var i = cljs.core.first(s__12788__$2);
return cljs.core.cons(observatory.process.make_process.cljs$core$IFn$_invoke$arity$variadic(((1000) + (i * (100))),cljs.core.rand_nth(new cljs.core.PersistentVector(null, 12, 5, cljs.core.PersistentVector.EMPTY_NODE, ["chrome.exe","firefox.exe","code.exe","explorer.exe","System","svchost.exe","dwm.exe","node.exe","rust-analyzer.exe","wsl.exe","docker.exe","spotify.exe"], null)),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"ppid","ppid",215811440),(((i > (0)))?((1000) + (cljs.core.rand_int(i) * (100))):null),new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),cljs.core.rand_int(((((2) * (1024)) * (1024)) * (1024))),new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),(cljs.core.rand.cljs$core$IFn$_invoke$arity$0() * 25.0),new cljs.core.Keyword(null,"threads","threads",-1717798734),(cljs.core.rand_int((100)) + (1))], 0)),observatory$state$iter__12787(cljs.core.rest(s__12788__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.range.cljs$core$IFn$_invoke$arity$1((25)));
})();
var cache_hier = observatory.cache.make_cache_hierarchy(new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"l1d","l1d",223086801),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"size-kb","size-kb",-1863311670),(32),new cljs.core.Keyword(null,"associativity","associativity",835945425),(8),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"data","data",-232669377)], null),new cljs.core.Keyword(null,"l2","l2",-462426027),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"size-kb","size-kb",-1863311670),(256),new cljs.core.Keyword(null,"associativity","associativity",835945425),(4),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"unified","unified",-400007218)], null),new cljs.core.Keyword(null,"l3","l3",1415952440),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"size-kb","size-kb",-1863311670),(8192),new cljs.core.Keyword(null,"associativity","associativity",835945425),(16),new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"unified","unified",-400007218)], null)], null));
var events = (function (){var iter__5480__auto__ = (function observatory$state$iter__12791(s__12792){
return (new cljs.core.LazySeq(null,(function (){
var s__12792__$1 = s__12792;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12792__$1);
if(temp__5804__auto__){
var s__12792__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12792__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12792__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12794 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12793 = (0);
while(true){
if((i__12793 < size__5479__auto__)){
var i = cljs.core._nth(c__5478__auto__,i__12793);
cljs.core.chunk_append(b__12794,new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"timestamp","timestamp",579478971),(i * (1000)),new cljs.core.Keyword(null,"type","type",1174270348),cljs.core.rand_nth(new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),new cljs.core.Keyword(null,"page-fault","page-fault",-1895673594),new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651)], null)),new cljs.core.Keyword(null,"pid","pid",1018387698),((1000) + (cljs.core.rand_int((5)) * (100))),new cljs.core.Keyword(null,"tid","tid",-901350880),(1),new cljs.core.Keyword(null,"address","address",559499426),((140694538682368) + cljs.core.rand_int((268435456))),new cljs.core.Keyword(null,"level","level",1290497552),cljs.core.rand_nth(new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"l2","l2",-462426027),new cljs.core.Keyword(null,"l3","l3",1415952440)], null))], null));

var G__12813 = (i__12793 + (1));
i__12793 = G__12813;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12794),observatory$state$iter__12791(cljs.core.chunk_rest(s__12792__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12794),null);
}
} else {
var i = cljs.core.first(s__12792__$2);
return cljs.core.cons(new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"timestamp","timestamp",579478971),(i * (1000)),new cljs.core.Keyword(null,"type","type",1174270348),cljs.core.rand_nth(new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930),new cljs.core.Keyword(null,"page-fault","page-fault",-1895673594),new cljs.core.Keyword(null,"disk-read","disk-read",-1075662651)], null)),new cljs.core.Keyword(null,"pid","pid",1018387698),((1000) + (cljs.core.rand_int((5)) * (100))),new cljs.core.Keyword(null,"tid","tid",-901350880),(1),new cljs.core.Keyword(null,"address","address",559499426),((140694538682368) + cljs.core.rand_int((268435456))),new cljs.core.Keyword(null,"level","level",1290497552),cljs.core.rand_nth(new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"l2","l2",-462426027),new cljs.core.Keyword(null,"l3","l3",1415952440)], null))], null),observatory$state$iter__12791(cljs.core.rest(s__12792__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.range.cljs$core$IFn$_invoke$arity$1((100)));
})();
return cljs.core.assoc_in(cljs.core.assoc_in(cljs.core.assoc_in(cljs.core.assoc_in(cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"system","system",-29381724),new cljs.core.Keyword(null,"processes","processes",-546984164)], null),cljs.core.vec(processes)),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"hardware","hardware",515915884),new cljs.core.Keyword(null,"cache-hierarchy","cache-hierarchy",-242050477)], null),cache_hier),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"simulation","simulation",-1001480470),new cljs.core.Keyword(null,"cache-state","cache-state",-272624031)], null),cache_hier),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"events","events",1792552201)], null),cljs.core.vec(events)),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"metadata","metadata",1799301597)], null),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"start-time","start-time",814801386),cljs.core.str.cljs$core$IFn$_invoke$arity$1((new Date())),new cljs.core.Keyword(null,"event-count","event-count",1113474312),cljs.core.count(events),new cljs.core.Keyword(null,"duration-ns","duration-ns",-544819911),(100000)], null));
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","set-playback-position","observatory.state/set-playback-position",720664023),(function (db,p__12795){
var vec__12796 = p__12795;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12796,(0),null);
var position = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12796,(1),null);
return cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"playback","playback",-2069416706),new cljs.core.Keyword(null,"position","position",-2011731912)], null),position);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","toggle-playback","observatory.state/toggle-playback",1627720313),(function (db,_){
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(db,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"playback","playback",-2069416706),new cljs.core.Keyword(null,"playing?","playing?",-1884542863)], null),cljs.core.not);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","set-playback-speed","observatory.state/set-playback-speed",-1990522384),(function (db,p__12799){
var vec__12800 = p__12799;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12800,(0),null);
var speed = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12800,(1),null);
return cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"playback","playback",-2069416706),new cljs.core.Keyword(null,"speed","speed",1257663751)], null),speed);
}));
re_frame.core.reg_event_db.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword("observatory.state","simulate-access","observatory.state/simulate-access",-1793672144),(function (db,p__12803){
var vec__12804 = p__12803;
var _ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12804,(0),null);
var address = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12804,(1),null);
var cache_state = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"simulation","simulation",-1001480470),new cljs.core.Keyword(null,"cache-state","cache-state",-272624031)], null));
var timestamp = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"playback","playback",-2069416706),new cljs.core.Keyword(null,"position","position",-2011731912)], null));
var map__12807 = observatory.cache.access(cljs.core.get.cljs$core$IFn$_invoke$arity$2(cache_state,new cljs.core.Keyword(null,"l1d","l1d",223086801)),address,timestamp);
var map__12807__$1 = cljs.core.__destructure_map(map__12807);
var cache = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12807__$1,new cljs.core.Keyword(null,"cache","cache",-1237023054));
var result = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12807__$1,new cljs.core.Keyword(null,"result","result",1415092211));
return cljs.core.update_in.cljs$core$IFn$_invoke$arity$4(cljs.core.assoc_in(db,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"simulation","simulation",-1001480470),new cljs.core.Keyword(null,"cache-state","cache-state",-272624031),new cljs.core.Keyword(null,"l1d","l1d",223086801)], null),cache),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"events","events",1792552201)], null),cljs.core.conj,new cljs.core.PersistentArrayMap(null, 6, [new cljs.core.Keyword(null,"timestamp","timestamp",579478971),timestamp,new cljs.core.Keyword(null,"type","type",1174270348),(cljs.core.truth_(new cljs.core.Keyword(null,"hit?","hit?",-364237458).cljs$core$IFn$_invoke$arity$1(result))?new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909):new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930)),new cljs.core.Keyword(null,"address","address",559499426),address,new cljs.core.Keyword(null,"level","level",1290497552),new cljs.core.Keyword(null,"l1","l1",1864175779),new cljs.core.Keyword(null,"set-index","set-index",797648137),new cljs.core.Keyword(null,"set-index","set-index",797648137).cljs$core$IFn$_invoke$arity$1(result),new cljs.core.Keyword(null,"way-index","way-index",1268630527),new cljs.core.Keyword(null,"way-index","way-index",1268630527).cljs$core$IFn$_invoke$arity$1(result)], null));
}));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","view","observatory.state/view",-1059371263),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return new cljs.core.Keyword(null,"view","view",1247994814).cljs$core$IFn$_invoke$arity$1(db);
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","current-tab","observatory.state/current-tab",507392906),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"current-tab","current-tab",-1305174577)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","sidebar-open?","observatory.state/sidebar-open?",887644914),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"sidebar-open?","sidebar-open?",-1099774467)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","inspector-open?","observatory.state/inspector-open?",37541420),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"view","view",1247994814),new cljs.core.Keyword(null,"inspector-open?","inspector-open?",-1261096585)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","drill-path","observatory.state/drill-path",-1774434284),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return new cljs.core.Keyword(null,"drill-path","drill-path",-386489135).cljs$core$IFn$_invoke$arity$1(db);
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","selection","observatory.state/selection",-863638346),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return new cljs.core.Keyword(null,"selection","selection",975998651).cljs$core$IFn$_invoke$arity$1(db);
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","processes","observatory.state/processes",-1696660641),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"system","system",-29381724),new cljs.core.Keyword(null,"processes","processes",-546984164)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","network-connections","observatory.state/network-connections",986385060),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"system","system",-29381724),new cljs.core.Keyword(null,"network-connections","network-connections",-867013151)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","cache-hierarchy","observatory.state/cache-hierarchy",1611142670),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"hardware","hardware",515915884),new cljs.core.Keyword(null,"cache-hierarchy","cache-hierarchy",-242050477)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","cache-state","observatory.state/cache-state",1632104742),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"simulation","simulation",-1001480470),new cljs.core.Keyword(null,"cache-state","cache-state",-272624031)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","trace-events","observatory.state/trace-events",446112557),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"events","events",1792552201)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","trace-metadata","observatory.state/trace-metadata",1553110044),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"metadata","metadata",1799301597)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","playback","observatory.state/playback",120493633),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([(function (db,_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(db,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"trace","trace",-1082747415),new cljs.core.Keyword(null,"playback","playback",-2069416706)], null));
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","processes-by-cpu","observatory.state/processes-by-cpu",-1152846487),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"<-","<-",760412998),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","processes","observatory.state/processes",-1696660641)], null),(function (processes,_){
return cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),cljs.core._GT_,processes);
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","processes-by-memory","observatory.state/processes-by-memory",-939883865),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"<-","<-",760412998),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","processes","observatory.state/processes",-1696660641)], null),(function (processes,_){
return cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),cljs.core._GT_,processes);
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","trace-summary","observatory.state/trace-summary",-1106632419),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"<-","<-",760412998),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-events","observatory.state/trace-events",446112557)], null),(function (events,_){
if(cljs.core.seq(events)){
return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"total","total",1916810418),cljs.core.count(events),new cljs.core.Keyword(null,"by-type","by-type",460217689),cljs.core.frequencies(cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"type","type",1174270348),events)),new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451),observatory.trace.cache_hit_rate(events)], null);
} else {
return null;
}
})], 0));
re_frame.core.reg_sub.cljs$core$IFn$_invoke$arity$variadic(new cljs.core.Keyword("observatory.state","current-data","observatory.state/current-data",414851368),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"<-","<-",760412998),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","processes","observatory.state/processes",-1696660641)], null),new cljs.core.Keyword(null,"<-","<-",760412998),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","drill-path","observatory.state/drill-path",-1774434284)], null),(function (p__12808,_){
var vec__12809 = p__12808;
var processes = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12809,(0),null);
var drill_path = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12809,(1),null);
if(cljs.core.empty_QMARK_(drill_path)){
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"root","root",-448657453),new cljs.core.Keyword(null,"data","data",-232669377),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"processes","processes",-546984164),processes], null)], null);
} else {
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"processes","processes",-546984164),processes], null),drill_path);
}
})], 0));

//# sourceMappingURL=observatory.state.js.map
