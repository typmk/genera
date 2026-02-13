goog.provide('observatory.process');
observatory.process.ProcessState = new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"enum","enum",1679018432),new cljs.core.Keyword(null,"running","running",1554969103),new cljs.core.Keyword(null,"sleeping","sleeping",-1878480086),new cljs.core.Keyword(null,"stopped","stopped",-1490414640),new cljs.core.Keyword(null,"zombie","zombie",2142041507)], null);
/**
 * A region of process virtual address space
 */
observatory.process.MemoryRegion = new cljs.core.PersistentVector(null, 8, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"map","map",1371690461),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"start-address","start-address",992516804),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"end-address","end-address",-2107434331),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"size-bytes","size-bytes",2109643324),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"protection","protection",-246987683),new cljs.core.PersistentVector(null, 7, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"enum","enum",1679018432),new cljs.core.Keyword(null,"read","read",1140058661),new cljs.core.Keyword(null,"write","write",-1857649168),new cljs.core.Keyword(null,"execute","execute",-129499188),new cljs.core.Keyword(null,"read-write","read-write",178022883),new cljs.core.Keyword(null,"read-execute","read-execute",-5111591),new cljs.core.Keyword(null,"all","all",892129742)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.PersistentVector(null, 7, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"enum","enum",1679018432),new cljs.core.Keyword(null,"code","code",1586293142),new cljs.core.Keyword(null,"data","data",-232669377),new cljs.core.Keyword(null,"heap","heap",1039710192),new cljs.core.Keyword(null,"stack","stack",-793405930),new cljs.core.Keyword(null,"mapped-file","mapped-file",852703251),new cljs.core.Keyword(null,"shared","shared",-384145993)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"mapped-file","mapped-file",852703251),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"string","string",-1989541586)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"resident-bytes","resident-bytes",-15598840),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"int","int",-1741416922)], null)], null);
observatory.process.Process = new cljs.core.PersistentVector(null, 14, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"map","map",1371690461),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"pid","pid",1018387698),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ppid","ppid",215811440),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"string","string",-1989541586)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"exe-path","exe-path",51958472),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"string","string",-1989541586)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"cmdline","cmdline",805777748),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"string","string",-1989541586)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"state","state",-1988618099),observatory.process.ProcessState], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"user","user",1532431356),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"string","string",-1989541586)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),new cljs.core.Keyword(null,"double","double",884886883)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"threads","threads",-1717798734),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"handles","handles",-1061347879),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"int","int",-1741416922)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"start-time","start-time",814801386),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.Keyword(null,"string","string",-1989541586)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"memory-regions","memory-regions",-1619486783),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"optional","optional",2053951509),true], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"vector","vector",1902966158),observatory.process.MemoryRegion], null)], null)], null);
/**
 * Create a process record
 */
observatory.process.make_process = (function observatory$process$make_process(var_args){
var args__5732__auto__ = [];
var len__5726__auto___12764 = arguments.length;
var i__5727__auto___12765 = (0);
while(true){
if((i__5727__auto___12765 < len__5726__auto___12764)){
args__5732__auto__.push((arguments[i__5727__auto___12765]));

var G__12766 = (i__5727__auto___12765 + (1));
i__5727__auto___12765 = G__12766;
continue;
} else {
}
break;
}

var argseq__5733__auto__ = ((((2) < args__5732__auto__.length))?(new cljs.core.IndexedSeq(args__5732__auto__.slice((2)),(0),null)):null);
return observatory.process.make_process.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),argseq__5733__auto__);
});

(observatory.process.make_process.cljs$core$IFn$_invoke$arity$variadic = (function (pid,name,p__12738){
var map__12739 = p__12738;
var map__12739__$1 = cljs.core.__destructure_map(map__12739);
var handles = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12739__$1,new cljs.core.Keyword(null,"handles","handles",-1061347879));
var user = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12739__$1,new cljs.core.Keyword(null,"user","user",1532431356));
var memory_bytes = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12739__$1,new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),(0));
var cpu_percent = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12739__$1,new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),0.0);
var exe_path = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12739__$1,new cljs.core.Keyword(null,"exe-path","exe-path",51958472));
var state = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12739__$1,new cljs.core.Keyword(null,"state","state",-1988618099),new cljs.core.Keyword(null,"running","running",1554969103));
var ppid = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12739__$1,new cljs.core.Keyword(null,"ppid","ppid",215811440));
var threads = cljs.core.get.cljs$core$IFn$_invoke$arity$3(map__12739__$1,new cljs.core.Keyword(null,"threads","threads",-1717798734),(1));
var cmdline = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12739__$1,new cljs.core.Keyword(null,"cmdline","cmdline",805777748));
return cljs.core.PersistentHashMap.fromArrays([new cljs.core.Keyword(null,"memory-regions","memory-regions",-1619486783),new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),new cljs.core.Keyword(null,"exe-path","exe-path",51958472),new cljs.core.Keyword(null,"name","name",1843675177),new cljs.core.Keyword(null,"state","state",-1988618099),new cljs.core.Keyword(null,"ppid","ppid",215811440),new cljs.core.Keyword(null,"threads","threads",-1717798734),new cljs.core.Keyword(null,"pid","pid",1018387698),new cljs.core.Keyword(null,"cmdline","cmdline",805777748),new cljs.core.Keyword(null,"handles","handles",-1061347879),new cljs.core.Keyword(null,"user","user",1532431356)],[cljs.core.PersistentVector.EMPTY,memory_bytes,cpu_percent,exe_path,name,state,ppid,threads,pid,cmdline,handles,user]);
}));

(observatory.process.make_process.cljs$lang$maxFixedArity = (2));

/** @this {Function} */
(observatory.process.make_process.cljs$lang$applyTo = (function (seq12735){
var G__12736 = cljs.core.first(seq12735);
var seq12735__$1 = cljs.core.next(seq12735);
var G__12737 = cljs.core.first(seq12735__$1);
var seq12735__$2 = cljs.core.next(seq12735__$1);
var self__5711__auto__ = this;
return self__5711__auto__.cljs$core$IFn$_invoke$arity$variadic(G__12736,G__12737,seq12735__$2);
}));

/**
 * Create a memory region
 */
observatory.process.make_memory_region = (function observatory$process$make_memory_region(start_address,size_bytes,type,protection){
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"start-address","start-address",992516804),start_address,new cljs.core.Keyword(null,"end-address","end-address",-2107434331),(start_address + size_bytes),new cljs.core.Keyword(null,"size-bytes","size-bytes",2109643324),size_bytes,new cljs.core.Keyword(null,"type","type",1174270348),type,new cljs.core.Keyword(null,"protection","protection",-246987683),protection], null);
});
/**
 * Sort processes by CPU usage descending
 */
observatory.process.processes_by_cpu = (function observatory$process$processes_by_cpu(processes){
return cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"cpu-percent","cpu-percent",295455682),cljs.core._GT_,processes);
});
/**
 * Sort processes by memory usage descending
 */
observatory.process.processes_by_memory = (function observatory$process$processes_by_memory(processes){
return cljs.core.sort_by.cljs$core$IFn$_invoke$arity$3(new cljs.core.Keyword(null,"memory-bytes","memory-bytes",-2015947262),cljs.core._GT_,processes);
});
/**
 * Build process tree from flat list.
 * Returns map of {pid -> {:process ... :children [...]}}
 */
observatory.process.process_tree = (function observatory$process$process_tree(processes){
var by_pid = cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,cljs.core.map.cljs$core$IFn$_invoke$arity$2(cljs.core.juxt.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"pid","pid",1018387698),cljs.core.identity),processes));
var children_map = cljs.core.group_by(new cljs.core.Keyword(null,"ppid","ppid",215811440),processes);
var build_node = (function observatory$process$process_tree_$_build_node(proc){
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"process","process",1643192938),proc,new cljs.core.Keyword(null,"children","children",-940561982),cljs.core.mapv.cljs$core$IFn$_invoke$arity$2(observatory$process$process_tree_$_build_node,cljs.core.get.cljs$core$IFn$_invoke$arity$3(children_map,new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(proc),cljs.core.PersistentVector.EMPTY))], null);
});
return cljs.core.mapv.cljs$core$IFn$_invoke$arity$2(build_node,cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12740_SHARP_){
return (cljs.core.get.cljs$core$IFn$_invoke$arity$2(by_pid,new cljs.core.Keyword(null,"ppid","ppid",215811440).cljs$core$IFn$_invoke$arity$1(p1__12740_SHARP_)) == null);
}),processes));
});
/**
 * Flatten process tree back to list with depth info
 */
observatory.process.flatten_tree = (function observatory$process$flatten_tree(tree){
var flatten_node = (function observatory$process$flatten_tree_$_flatten_node(p__12745,depth){
var map__12746 = p__12745;
var map__12746__$1 = cljs.core.__destructure_map(map__12746);
var process__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12746__$1,new cljs.core.Keyword(null,"process","process",1643192938));
var children = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12746__$1,new cljs.core.Keyword(null,"children","children",-940561982));
return cljs.core.cons(cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(process__$1,new cljs.core.Keyword(null,"depth","depth",1768663640),depth),cljs.core.mapcat.cljs$core$IFn$_invoke$arity$variadic((function (p1__12741_SHARP_){
return observatory$process$flatten_tree_$_flatten_node(p1__12741_SHARP_,(depth + (1)));
}),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([children], 0)));
});
return cljs.core.mapcat.cljs$core$IFn$_invoke$arity$variadic((function (p1__12742_SHARP_){
return flatten_node(p1__12742_SHARP_,(0));
}),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([tree], 0));
});
/**
 * Summarize memory regions by type
 */
observatory.process.memory_by_type = (function observatory$process$memory_by_type(process){
return cljs.core.into.cljs$core$IFn$_invoke$arity$2(cljs.core.PersistentArrayMap.EMPTY,cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p__12747){
var vec__12748 = p__12747;
var type = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12748,(0),null);
var regions = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12748,(1),null);
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [type,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"count","count",2139924085),cljs.core.count(regions),new cljs.core.Keyword(null,"total-bytes","total-bytes",1693967112),cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"size-bytes","size-bytes",2109643324),regions))], null)], null);
}),cljs.core.group_by(new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"memory-regions","memory-regions",-1619486783).cljs$core$IFn$_invoke$arity$1(process))));
});
/**
 * Find which memory region contains an address
 */
observatory.process.address_to_region = (function observatory$process$address_to_region(process,address){
return cljs.core.first(cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (p1__12751_SHARP_){
return (((address >= new cljs.core.Keyword(null,"start-address","start-address",992516804).cljs$core$IFn$_invoke$arity$1(p1__12751_SHARP_))) && ((address < new cljs.core.Keyword(null,"end-address","end-address",-2107434331).cljs$core$IFn$_invoke$arity$1(p1__12751_SHARP_))));
}),new cljs.core.Keyword(null,"memory-regions","memory-regions",-1619486783).cljs$core$IFn$_invoke$arity$1(process)));
});
/**
 * Add region info to an address
 */
observatory.process.annotate_address = (function observatory$process$annotate_address(process,address){
var region = observatory.process.address_to_region(process,address);
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"address","address",559499426),address,new cljs.core.Keyword(null,"region-type","region-type",1408831525),new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(region),new cljs.core.Keyword(null,"region-start","region-start",575044006),new cljs.core.Keyword(null,"start-address","start-address",992516804).cljs$core$IFn$_invoke$arity$1(region),new cljs.core.Keyword(null,"offset-in-region","offset-in-region",548465079),(cljs.core.truth_(region)?(address - new cljs.core.Keyword(null,"start-address","start-address",992516804).cljs$core$IFn$_invoke$arity$1(region)):null),new cljs.core.Keyword(null,"mapped-file","mapped-file",852703251),new cljs.core.Keyword(null,"mapped-file","mapped-file",852703251).cljs$core$IFn$_invoke$arity$1(region)], null);
});
/**
 * Get all trace events for a process
 */
observatory.process.process_events = (function observatory$process$process_events(trace,pid){
return observatory.trace.query.cljs$core$IFn$_invoke$arity$variadic(trace,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([observatory.trace.by_pid(pid)], 0));
});
/**
 * Summarize cache behavior for a process
 */
observatory.process.process_cache_summary = (function observatory$process$process_cache_summary(trace,pid){
var events = observatory.process.process_events(trace,pid);
return new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"cache-hit-rate","cache-hit-rate",278974027),observatory.trace.cache_hit_rate(events),new cljs.core.Keyword(null,"cache-by-level","cache-by-level",761151120),observatory.trace.cache_hit_rate_by_level(events),new cljs.core.Keyword(null,"hot-addresses","hot-addresses",-855206770),observatory.trace.hot_addresses(events),new cljs.core.Keyword(null,"page-faults","page-faults",1644874593),observatory.trace.page_fault_summary(events),new cljs.core.Keyword(null,"io","io",-307341917),observatory.trace.io_summary(events)], null);
});
/**
 * Create heatmap of memory access frequency.
 * Returns map of {region-type -> [{:address :count}]}
 */
observatory.process.process_memory_heatmap = (function observatory$process$process_memory_heatmap(trace,process){
var events = observatory.process.process_events(trace,new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(process));
var addresses = observatory.trace.hot_addresses.cljs$core$IFn$_invoke$arity$variadic(events,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"top-n","top-n",1854609254),(100)], 0));
return cljs.core.group_by(new cljs.core.Keyword(null,"region-type","region-type",1408831525),cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p__12752){
var vec__12753 = p__12752;
var addr = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12753,(0),null);
var count = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12753,(1),null);
var annotation = observatory.process.annotate_address(process,addr);
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(annotation,new cljs.core.Keyword(null,"access-count","access-count",169899798),count);
}),addresses));
});
/**
 * Estimate process working set from memory access trace.
 * Working set = unique cache lines accessed in time window.
 */
observatory.process.estimate_working_set = (function observatory$process$estimate_working_set(trace,pid,window_ns,line_size){
var events = cljs.core.sort_by.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"timestamp","timestamp",579478971),observatory.trace.query.cljs$core$IFn$_invoke$arity$variadic(trace,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([observatory.trace.by_pid(pid),observatory.trace.by_type.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([new cljs.core.Keyword(null,"cache-access","cache-access",419857430),new cljs.core.Keyword(null,"cache-hit","cache-hit",-1639869909),new cljs.core.Keyword(null,"cache-miss","cache-miss",-565678930)], 0))], 0)));
var line_mask = (~ (line_size - (1)));
return cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p__12758){
var map__12759 = p__12758;
var map__12759__$1 = cljs.core.__destructure_map(map__12759);
var bucket = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12759__$1,new cljs.core.Keyword(null,"bucket","bucket",1126218366));
var events__$1 = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12759__$1,new cljs.core.Keyword(null,"events","events",1792552201));
return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"window","window",724519534),bucket,new cljs.core.Keyword(null,"unique-lines","unique-lines",1021460275),cljs.core.count(cljs.core.distinct.cljs$core$IFn$_invoke$arity$1(cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__12756_SHARP_){
return (p1__12756_SHARP_ & line_mask);
}),cljs.core.keep.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"address","address",559499426),events__$1)))),new cljs.core.Keyword(null,"working-set-bytes","working-set-bytes",957655249),(line_size * cljs.core.count(cljs.core.distinct.cljs$core$IFn$_invoke$arity$1(cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__12757_SHARP_){
return (p1__12757_SHARP_ & line_mask);
}),cljs.core.keep.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"address","address",559499426),events__$1)))))], null);
}),observatory.trace.timeline_buckets(window_ns,events));
});
/**
 * Analyze cache pressure for a process.
 * 
 * Returns assessment:
 * - :low - working set fits in L1
 * - :medium - working set fits in L2
 * - :high - working set fits in L3
 * - :thrashing - working set exceeds L3
 */
observatory.process.cache_pressure = (function observatory$process$cache_pressure(trace,process,cache_hierarchy,window_ns){
var working_set = observatory.process.estimate_working_set(trace,new cljs.core.Keyword(null,"pid","pid",1018387698).cljs$core$IFn$_invoke$arity$1(process),window_ns,(64));
var avg_bytes = (cljs.core.reduce.cljs$core$IFn$_invoke$arity$3(cljs.core._PLUS_,(0),cljs.core.map.cljs$core$IFn$_invoke$arity$2(new cljs.core.Keyword(null,"working-set-bytes","working-set-bytes",957655249),working_set)) / (function (){var x__5087__auto__ = (1);
var y__5088__auto__ = cljs.core.count(working_set);
return ((x__5087__auto__ > y__5088__auto__) ? x__5087__auto__ : y__5088__auto__);
})());
var l1_size = ((32) * (1024));
var l2_size = ((256) * (1024));
var l3_size = (((8) * (1024)) * (1024));
return new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"working-set-bytes","working-set-bytes",957655249),avg_bytes,new cljs.core.Keyword(null,"pressure","pressure",505343747),(((avg_bytes < l1_size))?new cljs.core.Keyword(null,"low","low",-1601362409):(((avg_bytes < l2_size))?new cljs.core.Keyword(null,"medium","medium",-1864319384):(((avg_bytes < l3_size))?new cljs.core.Keyword(null,"high","high",2027297808):new cljs.core.Keyword(null,"thrashing","thrashing",1768596568)
))),new cljs.core.Keyword(null,"recommendation","recommendation",-776027791),(((avg_bytes < l1_size))?"Working set fits in L1 - optimal":(((avg_bytes < l2_size))?"Working set fits in L2 - good locality":(((avg_bytes < l3_size))?"Working set fits in L3 - consider data layout optimization":"Working set exceeds L3 - memory-bound, consider reducing footprint"
)))], null);
});
/**
 * Compare cache behavior of multiple processes
 */
observatory.process.compare_processes = (function observatory$process$compare_processes(trace,pids){
var summaries = cljs.core.map.cljs$core$IFn$_invoke$arity$2((function (p1__12760_SHARP_){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(observatory.process.process_cache_summary(trace,p1__12760_SHARP_),new cljs.core.Keyword(null,"pid","pid",1018387698),p1__12760_SHARP_);
}),pids);
return new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"processes","processes",-546984164),summaries,new cljs.core.Keyword(null,"best-hit-rate","best-hit-rate",39180984),cljs.core.apply.cljs$core$IFn$_invoke$arity$3(cljs.core.max_key,(function (p1__12761_SHARP_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$3(p1__12761_SHARP_,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"cache-hit-rate","cache-hit-rate",278974027),new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451)], null),(0));
}),summaries),new cljs.core.Keyword(null,"worst-hit-rate","worst-hit-rate",849226743),cljs.core.apply.cljs$core$IFn$_invoke$arity$3(cljs.core.min_key,(function (p1__12762_SHARP_){
return cljs.core.get_in.cljs$core$IFn$_invoke$arity$3(p1__12762_SHARP_,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"cache-hit-rate","cache-hit-rate",278974027),new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451)], null),(1));
}),summaries),new cljs.core.Keyword(null,"most-io","most-io",-14748524),cljs.core.apply.cljs$core$IFn$_invoke$arity$3(cljs.core.max_key,(function (p1__12763_SHARP_){
return (cljs.core.get_in.cljs$core$IFn$_invoke$arity$3(p1__12763_SHARP_,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"io","io",-307341917),new cljs.core.Keyword(null,"read-bytes","read-bytes",-1280034403)], null),(0)) + cljs.core.get_in.cljs$core$IFn$_invoke$arity$3(p1__12763_SHARP_,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"io","io",-307341917),new cljs.core.Keyword(null,"write-bytes","write-bytes",-1890264825)], null),(0)));
}),summaries)], null);
});

//# sourceMappingURL=observatory.process.js.map
