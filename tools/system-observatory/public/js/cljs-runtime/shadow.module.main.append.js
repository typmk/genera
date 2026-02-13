
shadow.cljs.devtools.client.env.module_loaded('main');

try { observatory.app.init(); } catch (e) { console.error("An error occurred when calling (observatory.app/init)"); console.error(e); }