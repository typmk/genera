goog.provide('observatory.app');
/**
 * Mount the root component. Called on hot reload.
 */
observatory.app.mount_root = (function observatory$app$mount_root(){
re_frame.core.clear_subscription_cache_BANG_();

var root_el = document.getElementById("app");
reagent.dom.unmount_component_at_node(root_el);

return reagent.dom.render.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [observatory.views.shell.main_shell], null),root_el);
});
/**
 * Initialize the application.
 */
observatory.app.init = (function observatory$app$init(){
console.log("System Observatory starting...");

re_frame.core.dispatch_sync(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","initialize","observatory.state/initialize",-638402604)], null));

re_frame.core.dispatch(new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword("observatory.state","load-sample-data","observatory.state/load-sample-data",-1293454588)], null));

observatory.app.mount_root();

return console.log("System Observatory ready. Click anything to inspect.");
});

//# sourceMappingURL=observatory.app.js.map
