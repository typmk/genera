goog.provide('observatory.views.shell');
/**
 * A single sidebar navigation item.
 */
observatory.views.shell.sidebar_item = (function observatory$views$shell$sidebar_item(p__12972){
var map__12973 = p__12972;
var map__12973__$1 = cljs.core.__destructure_map(map__12973);
var id = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12973__$1,new cljs.core.Keyword(null,"id","id",-1388402092));
var icon = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12973__$1,new cljs.core.Keyword(null,"icon","icon",1679606541));
var label = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12973__$1,new cljs.core.Keyword(null,"label","label",1718410804));
var active_QMARK_ = cljs.core.get.cljs$core$IFn$_invoke$arity$2(map__12973__$1,new cljs.core.Keyword(null,"active?","active?",459499776));
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.sidebar-item","button.sidebar-item",-1332241885),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"class","class",-2030961996),(cljs.core.truth_(active_QMARK_)?"active":null),new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","set-tab","observatory.state/set-tab",1591970412),id], null));
})], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.icon","span.icon",-1181275586),icon], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),label], null)], null);
});
/**
 * Main navigation sidebar.
 */
observatory.views.shell.sidebar = (function observatory$views$shell$sidebar(){
var current_tab = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","current-tab","observatory.state/current-tab",507392906)], null)));
var open_QMARK_ = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","sidebar-open?","observatory.state/sidebar-open?",887644914)], null)));
return new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"nav.sidebar","nav.sidebar",-1914757313),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"class","class",-2030961996),(cljs.core.truth_(open_QMARK_)?null:"collapsed")], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.sidebar-header","div.sidebar-header",-1668016167),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h1.logo","h1.logo",1020109157),"Observatory"], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.collapse-btn","button.collapse-btn",2005828360),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","toggle-sidebar","observatory.state/toggle-sidebar",-1463503518)], null));
})], null),(cljs.core.truth_(open_QMARK_)?"\u25C0":"\u25B6")], null)], null),new cljs.core.PersistentVector(null, 7, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.sidebar-nav","div.sidebar-nav",-37762566),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar_item,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"processes","processes",-546984164),new cljs.core.Keyword(null,"icon","icon",1679606541),"\u2699",new cljs.core.Keyword(null,"label","label",1718410804),"Processes",new cljs.core.Keyword(null,"active?","active?",459499776),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_tab,new cljs.core.Keyword(null,"processes","processes",-546984164))], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar_item,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"network","network",2050004697),new cljs.core.Keyword(null,"icon","icon",1679606541),"\uD83C\uDF10",new cljs.core.Keyword(null,"label","label",1718410804),"Network",new cljs.core.Keyword(null,"active?","active?",459499776),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_tab,new cljs.core.Keyword(null,"network","network",2050004697))], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar_item,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"cache","cache",-1237023054),new cljs.core.Keyword(null,"icon","icon",1679606541),"\uD83D\uDCE6",new cljs.core.Keyword(null,"label","label",1718410804),"Cache",new cljs.core.Keyword(null,"active?","active?",459499776),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_tab,new cljs.core.Keyword(null,"cache","cache",-1237023054))], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar_item,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"memory","memory",-1449401430),new cljs.core.Keyword(null,"icon","icon",1679606541),"\uD83E\uDDE0",new cljs.core.Keyword(null,"label","label",1718410804),"Memory",new cljs.core.Keyword(null,"active?","active?",459499776),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_tab,new cljs.core.Keyword(null,"memory","memory",-1449401430))], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar_item,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"disk","disk",-1750803806),new cljs.core.Keyword(null,"icon","icon",1679606541),"\uD83D\uDCBE",new cljs.core.Keyword(null,"label","label",1718410804),"Disk",new cljs.core.Keyword(null,"active?","active?",459499776),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_tab,new cljs.core.Keyword(null,"disk","disk",-1750803806))], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar_item,new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"id","id",-1388402092),new cljs.core.Keyword(null,"timeline","timeline",192903161),new cljs.core.Keyword(null,"icon","icon",1679606541),"\u23F1",new cljs.core.Keyword(null,"label","label",1718410804),"Timeline",new cljs.core.Keyword(null,"active?","active?",459499776),cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(current_tab,new cljs.core.Keyword(null,"timeline","timeline",192903161))], null)], null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.sidebar-footer","div.sidebar-footer",341610108),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.mode-indicator","div.mode-indicator",-1544799437),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.dot.simulation","span.dot.simulation",986400329)], null),"Simulation Mode"], null)], null)], null);
});
/**
 * Shows current drill path and allows navigation back up.
 */
observatory.views.shell.breadcrumb = (function observatory$views$shell$breadcrumb(){
var drill_path = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","drill-path","observatory.state/drill-path",-1774434284)], null)));
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.breadcrumb","div.breadcrumb",110752603),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.crumb.root","button.crumb.root",-112192733),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","drill-reset","observatory.state/drill-reset",-806492159)], null));
})], null),"\uD83C\uDFE0 Root"], null),(function (){var iter__5480__auto__ = (function observatory$views$shell$breadcrumb_$_iter__12974(s__12975){
return (new cljs.core.LazySeq(null,(function (){
var s__12975__$1 = s__12975;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12975__$1);
if(temp__5804__auto__){
var s__12975__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12975__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12975__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12977 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12976 = (0);
while(true){
if((i__12976 < size__5479__auto__)){
var vec__12978 = cljs.core._nth(c__5478__auto__,i__12976);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12978,(0),null);
var segment = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12978,(1),null);
cljs.core.chunk_append(b__12977,cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.crumb-separator","span.crumb-separator",-1988264010)," \u203A "], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)));

var G__12995 = (i__12976 + (1));
i__12976 = G__12995;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12977),observatory$views$shell$breadcrumb_$_iter__12974(cljs.core.chunk_rest(s__12975__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12977),null);
}
} else {
var vec__12981 = cljs.core.first(s__12975__$2);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12981,(0),null);
var segment = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12981,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.crumb-separator","span.crumb-separator",-1988264010)," \u203A "], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),idx], null)),observatory$views$shell$breadcrumb_$_iter__12974(cljs.core.rest(s__12975__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2(cljs.core.vector,drill_path));
})(),(function (){var iter__5480__auto__ = (function observatory$views$shell$breadcrumb_$_iter__12984(s__12985){
return (new cljs.core.LazySeq(null,(function (){
var s__12985__$1 = s__12985;
while(true){
var temp__5804__auto__ = cljs.core.seq(s__12985__$1);
if(temp__5804__auto__){
var s__12985__$2 = temp__5804__auto__;
if(cljs.core.chunked_seq_QMARK_(s__12985__$2)){
var c__5478__auto__ = cljs.core.chunk_first(s__12985__$2);
var size__5479__auto__ = cljs.core.count(c__5478__auto__);
var b__12987 = cljs.core.chunk_buffer(size__5479__auto__);
if((function (){var i__12986 = (0);
while(true){
if((i__12986 < size__5479__auto__)){
var vec__12988 = cljs.core._nth(c__5478__auto__,i__12986);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12988,(0),null);
var segment = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12988,(1),null);
cljs.core.chunk_append(b__12987,cljs.core.with_meta(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.crumb","button.crumb",-628468572),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),((function (i__12986,vec__12988,idx,segment,c__5478__auto__,size__5479__auto__,b__12987,s__12985__$2,temp__5804__auto__,drill_path){
return (function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","drill-up","observatory.state/drill-up",686702214)], null));
});})(i__12986,vec__12988,idx,segment,c__5478__auto__,size__5479__auto__,b__12987,s__12985__$2,temp__5804__auto__,drill_path))
], null),cljs.core.str.cljs$core$IFn$_invoke$arity$1(segment)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),["seg-",cljs.core.str.cljs$core$IFn$_invoke$arity$1(idx)].join('')], null)));

var G__12996 = (i__12986 + (1));
i__12986 = G__12996;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__12987),observatory$views$shell$breadcrumb_$_iter__12984(cljs.core.chunk_rest(s__12985__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__12987),null);
}
} else {
var vec__12991 = cljs.core.first(s__12985__$2);
var idx = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12991,(0),null);
var segment = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__12991,(1),null);
return cljs.core.cons(cljs.core.with_meta(new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.crumb","button.crumb",-628468572),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),((function (vec__12991,idx,segment,s__12985__$2,temp__5804__auto__,drill_path){
return (function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","drill-up","observatory.state/drill-up",686702214)], null));
});})(vec__12991,idx,segment,s__12985__$2,temp__5804__auto__,drill_path))
], null),cljs.core.str.cljs$core$IFn$_invoke$arity$1(segment)], null),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),["seg-",cljs.core.str.cljs$core$IFn$_invoke$arity$1(idx)].join('')], null)),observatory$views$shell$breadcrumb_$_iter__12984(cljs.core.rest(s__12985__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__5480__auto__(cljs.core.map_indexed.cljs$core$IFn$_invoke$arity$2(cljs.core.vector,drill_path));
})()], null);
});
/**
 * Main content panel - switches based on current tab.
 */
observatory.views.shell.content_panel = (function observatory$views$shell$content_panel(){
var current_tab = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","current-tab","observatory.state/current-tab",507392906)], null)));
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"main.content-panel","main.content-panel",-1746011648),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.breadcrumb], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.content-body","div.content-body",833196221),(function (){var G__12994 = current_tab;
var G__12994__$1 = (((G__12994 instanceof cljs.core.Keyword))?G__12994.fqn:null);
switch (G__12994__$1) {
case "processes":
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.processes.processes_view], null);

break;
case "network":
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.placeholder","div.placeholder",1371803864),"Network connections - coming soon"], null);

break;
case "cache":
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.cache.cache_view], null);

break;
case "memory":
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.placeholder","div.placeholder",1371803864),"Memory view - coming soon"], null);

break;
case "disk":
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.placeholder","div.placeholder",1371803864),"Disk view - coming soon"], null);

break;
case "timeline":
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.timeline.timeline_view], null);

break;
default:
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.placeholder","div.placeholder",1371803864),"Select a view from the sidebar"], null);

}
})()], null)], null);
});
/**
 * Bottom status bar with system stats.
 */
observatory.views.shell.status_bar = (function observatory$views$shell$status_bar(){
var processes = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","processes","observatory.state/processes",-1696660641)], null)));
var trace_summary = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","trace-summary","observatory.state/trace-summary",-1106632419)], null)));
return new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"footer.status-bar","footer.status-bar",-1125975341),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat","div.stat",-459953664),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Processes:"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),cljs.core.count(processes)], null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat","div.stat",-459953664),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Events:"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),(function (){var or__5002__auto__ = new cljs.core.Keyword(null,"total","total",1916810418).cljs$core$IFn$_invoke$arity$1(trace_summary);
if(cljs.core.truth_(or__5002__auto__)){
return or__5002__auto__;
} else {
return (0);
}
})()], null)], null),(function (){var temp__5804__auto__ = new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451).cljs$core$IFn$_invoke$arity$1(trace_summary);
if(cljs.core.truth_(temp__5804__auto__)){
var hit_rate = temp__5804__auto__;
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat","div.stat",-459953664),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.label","span.label",-1921107865),"Cache Hit Rate:"], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span.value","span.value",1986660361),[cljs.core.str.cljs$core$IFn$_invoke$arity$1((((100) * new cljs.core.Keyword(null,"hit-rate","hit-rate",1062313451).cljs$core$IFn$_invoke$arity$1(hit_rate)) | (0))),"%"].join('')], null)], null);
} else {
return null;
}
})(),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.spacer","div.spacer",2037275558)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.stat","div.stat",-459953664),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"button.btn-small","button.btn-small",-1695231080),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"on-click","on-click",1632826543),(function (){
return re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","toggle-inspector","observatory.state/toggle-inspector",1524980449)], null));
})], null),"Toggle Inspector"], null)], null)], null);
});
/**
 * Root component - assembles the full application layout.
 */
observatory.views.shell.main_shell = (function observatory$views$shell$main_shell(){
var inspector_open_QMARK_ = cljs.core.deref(re_frame.core.subscribe.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","inspector-open?","observatory.state/inspector-open?",37541420)], null)));
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.app-shell","div.app-shell",1652175127),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.sidebar], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div.main-area","div.main-area",1569343689),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.content_panel], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.status_bar], null)], null),(cljs.core.truth_(inspector_open_QMARK_)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.inspector.inspector_panel], null):null)], null);
});

//# sourceMappingURL=observatory.views.shell.js.map
