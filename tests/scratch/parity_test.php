<?php

require __DIR__ . '/vendor/autoload.php';
\Cljp\ErrorHandler::register();

$GLOBALS['inc'] = (function($__L_x) { return ($__L_x + 1);});
$inc = &$GLOBALS['inc'];
$GLOBALS['dec'] = (function($__L_x) { return ($__L_x - 1);});
$dec = &$GLOBALS['dec'];
$GLOBALS['identity'] = (function($__L_x) { return $__L_x;});
$identity = &$GLOBALS['identity'];
$GLOBALS['not'] = (function($__L_x) { if ($__L_x) { return false;} else { return true;}});
$not = &$GLOBALS['not'];
$GLOBALS['nil_QMARK_'] = (function($__L_x) { return is_null($__L_x);});
$nil_QMARK_ = &$GLOBALS['nil_QMARK_'];
$GLOBALS['apply'] = (function($__L_f, ...$__L_args) { $__L_flat_args = (call_user_func($GLOBALS['next'], $__L_args) ? array_merge(array_slice($__L_args, 0, -1), call_user_func($GLOBALS['last'], $__L_args)) : call_user_func($GLOBALS['first'], $__L_args));
return call_user_func_array($__L_f, call_user_func('\Clojure\Php\intoArray', $__L_flat_args));});
$apply = &$GLOBALS['apply'];
$GLOBALS['comp'] = (function(...$__L_fs) { $__L_fs = array_reverse($__L_fs);
return (function(...$__L_args) use (&$__L_fs) { $__L_ret = call_user_func($GLOBALS['apply'], call_user_func($GLOBALS['first'], $__L_fs), $__L_args);
$__L_fs = call_user_func($GLOBALS['next'], $__L_fs);
 while(true) { if ($__L_fs) { $__L_ret = call_user_func(call_user_func($GLOBALS['first'], $__L_fs), $__L_ret); $__L_fs = call_user_func($GLOBALS['next'], $__L_fs); continue;} else { return $__L_ret;} break; }
});});
$comp = &$GLOBALS['comp'];
$GLOBALS['partial'] = (function($__L_f, ...$__L_args) { return (function(...$__L_more) use (&$__L_args, &$__L_f) { return call_user_func($GLOBALS['apply'], $__L_f, array_merge($__L_args, $__L_more));});});
$partial = &$GLOBALS['partial'];
$GLOBALS['count'] = (function($__L_coll) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return 0;} else { return count($__L_coll);}});
$count = &$GLOBALS['count'];
$GLOBALS['seq'] = (function($__L_coll) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return null;} else { if (is_array($__L_coll)) { if ((0 === count($__L_coll))) { return null;} else { return $__L_coll;}} else { if (method_exists($__L_coll, 'seq')) { return $__L_coll->seq();} else { return $__L_coll;}}}});
$seq = &$GLOBALS['seq'];
$GLOBALS['first'] = (function($__L_coll) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return null;} else { if (is_array($__L_coll)) { if ((0 === count($__L_coll))) { return null;} else { return reset($__L_coll);}} else { return $__L_coll->first();}}});
$first = &$GLOBALS['first'];
$GLOBALS['rest'] = (function($__L_coll) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return \Clojure\Php\vec();} else { if (is_array($__L_coll)) { return array_slice($__L_coll, 1);} else { return $__L_coll->rest();}}});
$rest = &$GLOBALS['rest'];
$GLOBALS['next'] = (function($__L_coll) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return null;} else { $__L_r = call_user_func($GLOBALS['rest'], $__L_coll);
return call_user_func($GLOBALS['seq'], $__L_r);}});
$next = &$GLOBALS['next'];
$GLOBALS['cons'] = (function($__L_x, $__L_coll) { return call_user_func('\Clojure\Php\cons', $__L_x, $__L_coll);});
$cons = &$GLOBALS['cons'];
$GLOBALS['vector'] = (function(...$__L_args) { return call_user_func('\Clojure\Php\vec', $__L_args);});
$vector = &$GLOBALS['vector'];
$GLOBALS['conj'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_coll = $__args[0]; $__L_x = $__args[1]; return call_user_func('\Clojure\Php\conj', $__L_coll, $__L_x); } else if ($__n >= 2) { $__L_coll = $__args[0]; $__L_x = $__args[1]; $__L_xs = array_slice($__args, 2); return call_user_func($GLOBALS['reduce'], $GLOBALS['conj'], call_user_func($GLOBALS['conj'], $__L_coll, $__L_x), $__L_xs); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$conj = &$GLOBALS['conj'];
$GLOBALS['reduced'] = (function($__L_x) { return call_user_func('\Clojure\Php\reduced', $__L_x);});
$reduced = &$GLOBALS['reduced'];
$GLOBALS['reduced_QMARK_'] = (function($__L_x) { return call_user_func('\Clojure\Php\isReduced', $__L_x);});
$reduced_QMARK_ = &$GLOBALS['reduced_QMARK_'];
$GLOBALS['unreduced'] = (function($__L_x) { if (call_user_func($GLOBALS['reduced_QMARK_'], $__L_x)) { return call_user_func('\Clojure\Php\unreduced', $__L_x);} else { return $__L_x;}});
$unreduced = &$GLOBALS['unreduced'];
$GLOBALS['reduce'] = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_f = $__args[0]; $__L_val = $__args[1]; $__L_coll = $__args[2]; $__L_val = $__L_val;
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_ret = call_user_func($__L_f, $__L_val, call_user_func($GLOBALS['first'], $__L_coll));
if (call_user_func($GLOBALS['reduced_QMARK_'], $__L_ret)) { return call_user_func($GLOBALS['unreduced'], $__L_ret);} else { $__L_val = $__L_ret; $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;}} else { return $__L_val;} break; }
 } else if ($__n == 2) { $__L_f = $__args[0]; $__L_coll = $__args[1]; if (call_user_func($GLOBALS['seq'], $__L_coll)) { return call_user_func($GLOBALS['reduce'], $__L_f, call_user_func($GLOBALS['first'], $__L_coll), call_user_func($GLOBALS['next'], $__L_coll));} else { return call_user_func($__L_f);} } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$reduce = &$GLOBALS['reduce'];
$GLOBALS['reduce_kv'] = (function($__L_f, $__L_init, $__L_coll) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return $__L_init;} else { $__L_acc = $__L_init;
$__L_ks = call_user_func($GLOBALS['keys'], $__L_coll);
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_ks)) { $__L_k = call_user_func($GLOBALS['first'], $__L_ks);
$__L_ret = call_user_func($__L_f, $__L_acc, $__L_k, call_user_func($GLOBALS['get'], $__L_coll, $__L_k));
if (call_user_func($GLOBALS['reduced_QMARK_'], $__L_ret)) { return call_user_func($GLOBALS['unreduced'], $__L_ret);} else { $__L_acc = $__L_ret; $__L_ks = call_user_func($GLOBALS['next'], $__L_ks); continue;}} else { return $__L_acc;} break; }
}});
$reduce_kv = &$GLOBALS['reduce_kv'];
$GLOBALS['last'] = (function($__L_coll) { $__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['next'], $__L_coll)) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return call_user_func($GLOBALS['first'], $__L_coll);} break; }
});
$last = &$GLOBALS['last'];
$GLOBALS['second'] = (function($__L_coll) { return call_user_func($GLOBALS['first'], call_user_func($GLOBALS['next'], $__L_coll));});
$second = &$GLOBALS['second'];
$GLOBALS['map'] = (function($__L_f, $__L_coll) { return (new \Clojure\Php\LazySeq((function() use (&$__L_coll, &$__L_f) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { return call_user_func($GLOBALS['cons'], call_user_func($__L_f, call_user_func($GLOBALS['first'], $__L_coll)), call_user_func($GLOBALS['map'], $__L_f, call_user_func($GLOBALS['next'], $__L_coll)));}})));});
$map = &$GLOBALS['map'];
$GLOBALS['filter'] = (function($__L_pred, $__L_coll) { return (new \Clojure\Php\LazySeq((function() use (&$__L_pred, &$__L_coll) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
if (call_user_func($__L_pred, $__L_x)) { return call_user_func($GLOBALS['cons'], $__L_x, call_user_func($GLOBALS['filter'], $__L_pred, call_user_func($GLOBALS['next'], $__L_coll)));} else { return call_user_func($GLOBALS['filter'], $__L_pred, call_user_func($GLOBALS['next'], $__L_coll));}}})));});
$filter = &$GLOBALS['filter'];
$GLOBALS['mapv'] = (function($__L_f, $__L_coll) { 'Eager version of map - returns a vector';
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_result = call_user_func($GLOBALS['conj'], $__L_result, call_user_func($__L_f, call_user_func($GLOBALS['first'], $__L_coll))); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_result;} break; }
});
$mapv = &$GLOBALS['mapv'];
$GLOBALS['filterv'] = (function($__L_pred, $__L_coll) { 'Eager version of filter - returns a vector';
$__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
$__L_result = (call_user_func($__L_pred, $__L_x) ? call_user_func($GLOBALS['conj'], $__L_result, $__L_x) : $__L_result); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_result;} break; }
});
$filterv = &$GLOBALS['filterv'];
$GLOBALS['remove'] = (function($__L_pred, $__L_coll) { return call_user_func($GLOBALS['filter'], (function($__L_x) use (&$__L_pred, &$__L_coll) { return call_user_func($GLOBALS['not'], call_user_func($__L_pred, $__L_x));}), $__L_coll);});
$remove = &$GLOBALS['remove'];
$GLOBALS['mapcat'] = (function($__L_f, $__L_coll) { return call_user_func($GLOBALS['apply'], $GLOBALS['concat'], call_user_func($GLOBALS['map'], $__L_f, $__L_coll));});
$mapcat = &$GLOBALS['mapcat'];
$GLOBALS['map_indexed'] = (function($__L_f, $__L_coll) { $__L_result = \Clojure\Php\vec();
$__L_idx = 0;
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_result = call_user_func($GLOBALS['conj'], $__L_result, call_user_func($__L_f, $__L_idx, call_user_func($GLOBALS['first'], $__L_coll))); $__L_idx = call_user_func($GLOBALS['inc'], $__L_idx); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_result;} break; }
});
$map_indexed = &$GLOBALS['map_indexed'];
$GLOBALS['keep'] = (function($__L_f, $__L_coll) { $__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_v = call_user_func($__L_f, call_user_func($GLOBALS['first'], $__L_coll));
$__L_result = (call_user_func($GLOBALS['nil_QMARK_'], $__L_v) ? $__L_result : call_user_func($GLOBALS['conj'], $__L_result, $__L_v)); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_result;} break; }
});
$keep = &$GLOBALS['keep'];
$GLOBALS['keep_indexed'] = (function($__L_f, $__L_coll) { $__L_result = \Clojure\Php\vec();
$__L_idx = 0;
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_v = call_user_func($__L_f, $__L_idx, call_user_func($GLOBALS['first'], $__L_coll));
$__L_result = (call_user_func($GLOBALS['nil_QMARK_'], $__L_v) ? $__L_result : call_user_func($GLOBALS['conj'], $__L_result, $__L_v)); $__L_idx = call_user_func($GLOBALS['inc'], $__L_idx); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_result;} break; }
});
$keep_indexed = &$GLOBALS['keep_indexed'];
$GLOBALS['flatten'] = (function($__L_coll) { $__L_result = \Clojure\Php\vec();
$__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
if ((is_array($__L_x) ? is_array($__L_x) : call_user_func($GLOBALS['coll_QMARK_'], $__L_x))) { $__L_result = call_user_func($GLOBALS['into'], $__L_result, call_user_func($GLOBALS['flatten'], $__L_x)); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_x); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;}} else { return $__L_result;} break; }
});
$flatten = &$GLOBALS['flatten'];
$GLOBALS['interleave'] = (function($__L_c1, $__L_c2) { $__L_result = \Clojure\Php\vec();
$__L_s1 = $__L_c1;
$__L_s2 = $__L_c2;
 while(true) { if ((call_user_func($GLOBALS['seq'], $__L_s1) ? call_user_func($GLOBALS['seq'], $__L_s2) : null)) { $__L_result = call_user_func($GLOBALS['conj'], call_user_func($GLOBALS['conj'], $__L_result, call_user_func($GLOBALS['first'], $__L_s1)), call_user_func($GLOBALS['first'], $__L_s2)); $__L_s1 = call_user_func($GLOBALS['next'], $__L_s1); $__L_s2 = call_user_func($GLOBALS['next'], $__L_s2); continue;} else { return $__L_result;} break; }
});
$interleave = &$GLOBALS['interleave'];
$GLOBALS['interpose'] = (function($__L_sep, $__L_coll) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_result = \Clojure\Php\vec(call_user_func($GLOBALS['first'], $__L_coll));
$__L_coll = call_user_func($GLOBALS['next'], $__L_coll);
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_result = call_user_func($GLOBALS['conj'], call_user_func($GLOBALS['conj'], $__L_result, $__L_sep), call_user_func($GLOBALS['first'], $__L_coll)); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_result;} break; }
} else { return \Clojure\Php\vec();}});
$interpose = &$GLOBALS['interpose'];
$GLOBALS['_EQ_'] = (function(...$__L_args) { if ((count($__L_args) < 2)) { return true;} else { $__L_first_arg = array_shift($__L_args);
$__L_args = $__L_args;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_args)) { if (call_user_func('\Clojure\Php\equals', $__L_first_arg, call_user_func($GLOBALS['first'], $__L_args))) { $__L_args = call_user_func($GLOBALS['next'], $__L_args); continue;}} else { return true;} break; }
}});
$_EQ_ = &$GLOBALS['_EQ_'];
$GLOBALS['not_EQ_'] = (function(...$__L_args) { return call_user_func($GLOBALS['not'], call_user_func($GLOBALS['apply'], $GLOBALS['_EQ_'], $__L_args));});
$not_EQ_ = &$GLOBALS['not_EQ_'];
$GLOBALS['atom'] = (function($__L_x) { return (new \Clojure\Php\Atom($__L_x));});
$atom = &$GLOBALS['atom'];
$GLOBALS['deref'] = (function($__L_x) { return $__L_x->deref();});
$deref = &$GLOBALS['deref'];
$GLOBALS['reset_BANG_'] = (function($__L_x, $__L_newval) { $__L_x->set($__L_newval);
return $__L_newval;});
$reset_BANG_ = &$GLOBALS['reset_BANG_'];
$GLOBALS['swap_BANG_'] = (function($__L_x, $__L_f, ...$__L_args) { $__L_old = $__L_x->deref();
$__L_new = call_user_func($GLOBALS['apply'], $__L_f, $__L_old, $__L_args);
$__L_x->set($__L_new);
return $__L_new;});
$swap_BANG_ = &$GLOBALS['swap_BANG_'];
$GLOBALS['assoc'] = (function($__L_map, $__L_key, $__L_val, ...$__L_kvs) { $__L_m = call_user_func('\Clojure\Php\assoc', $__L_map, $__L_key, $__L_val);
if ($__L_kvs) { return call_user_func($GLOBALS['apply'], $GLOBALS['assoc'], $__L_m, $__L_kvs);} else { return $__L_m;}});
$assoc = &$GLOBALS['assoc'];
$GLOBALS['update'] = (function($__L_m, $__L_k, $__L_f, ...$__L_args) { return call_user_func($GLOBALS['assoc'], $__L_m, $__L_k, call_user_func($GLOBALS['apply'], $__L_f, call_user_func($GLOBALS['get'], $__L_m, $__L_k), $__L_args));});
$update = &$GLOBALS['update'];
$GLOBALS['get'] = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_m = $__args[0]; $__L_k = $__args[1]; $__L_not_found = $__args[2]; $__L_val = call_user_func('\Clojure\Php\get', $__L_m, $__L_k);
if (call_user_func($GLOBALS['nil_QMARK_'], $__L_val)) { return $__L_not_found;} else { return $__L_val;} } else if ($__n == 2) { $__L_m = $__args[0]; $__L_k = $__args[1]; return call_user_func('\Clojure\Php\get', $__L_m, $__L_k); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$get = &$GLOBALS['get'];
$GLOBALS['str'] = (function(...$__L_args) { return implode('', $__L_args);});
$str = &$GLOBALS['str'];
$GLOBALS['print'] = (function(...$__L_more) { return print(call_user_func($GLOBALS['apply'], $GLOBALS['str'], $__L_more));});
$print = &$GLOBALS['print'];
$GLOBALS['println'] = (function(...$__L_more) { print(call_user_func($GLOBALS['apply'], $GLOBALS['str'], $__L_more));
return print(PHP_EOL);});
$println = &$GLOBALS['println'];
$GLOBALS['pr_str'] = (function(...$__L_xs) { 'Returns a string containing the printed representation of xs (EDN format)';
return implode(' ', call_user_func('\Clojure\Php\toArray', call_user_func($GLOBALS['map'], (function($__L_x) use (&$__L_xs) { return call_user_func('\Clojure\Php\prStr', $__L_x);}), $__L_xs)));});
$pr_str = &$GLOBALS['pr_str'];
$GLOBALS['prn_str'] = (function(...$__L_xs) { 'Same as pr-str but with a trailing newline';
return call_user_func($GLOBALS['str'], call_user_func($GLOBALS['apply'], $GLOBALS['pr_str'], $__L_xs), PHP_EOL);});
$prn_str = &$GLOBALS['prn_str'];
$GLOBALS['pr'] = (function(...$__L_xs) { 'Prints the object(s) in a machine-readable form';
return print(call_user_func($GLOBALS['apply'], $GLOBALS['pr_str'], $__L_xs));});
$pr = &$GLOBALS['pr'];
$GLOBALS['prn'] = (function(...$__L_xs) { 'Same as pr followed by newline';
print(call_user_func($GLOBALS['apply'], $GLOBALS['pr_str'], $__L_xs));
return print(PHP_EOL);});
$prn = &$GLOBALS['prn'];
$GLOBALS['print_str'] = (function(...$__L_xs) { 'Returns a string of the values printed by print';
return call_user_func($GLOBALS['apply'], $GLOBALS['str'], $__L_xs);});
$print_str = &$GLOBALS['print_str'];
$GLOBALS['_PLUS_'] = (function(...$__L_xs) { return call_user_func($GLOBALS['apply'], $GLOBALS['reduce'], (function($__L_a, $__L_b) use (&$__L_xs) { return ($__L_a + $__L_b);}), 0, $__L_xs);});
$_PLUS_ = &$GLOBALS['_PLUS_'];
$GLOBALS['_'] = (function($__L_x, ...$__L_xs) { if ($__L_xs) { return call_user_func($GLOBALS['apply'], $GLOBALS['reduce'], (function($__L_a, $__L_b) use (&$__L_x, &$__L_xs) { return ($__L_a - $__L_b);}), $__L_x, $__L_xs);} else { return (0 - $__L_x);}});
$_ = &$GLOBALS['_'];
$GLOBALS['_STAR_'] = (function(...$__L_xs) { return call_user_func($GLOBALS['apply'], $GLOBALS['reduce'], (function($__L_a, $__L_b) use (&$__L_xs) { return ($__L_a * $__L_b);}), 1, $__L_xs);});
$_STAR_ = &$GLOBALS['_STAR_'];
$GLOBALS['_SLASH_'] = (function($__L_x, ...$__L_xs) { if ($__L_xs) { return call_user_func($GLOBALS['apply'], $GLOBALS['reduce'], (function($__L_a, $__L_b) use (&$__L_x, &$__L_xs) { return ($__L_a / $__L_b);}), $__L_x, $__L_xs);} else { return (1 / $__L_x);}});
$_SLASH_ = &$GLOBALS['_SLASH_'];
$GLOBALS['mod'] = (function($__L_num, $__L_div) { $__L_m = ($__L_num % $__L_div);
if (((0 === $__L_m) ? (0 === $__L_m) : (($__L_num > 0) === ($__L_div > 0)))) { return $__L_m;} else { return ($__L_m + $__L_div);}});
$mod = &$GLOBALS['mod'];
$GLOBALS['rem'] = (function($__L_num, $__L_div) { return ($__L_num % $__L_div);});
$rem = &$GLOBALS['rem'];
$GLOBALS['quot'] = (function($__L_num, $__L_div) { return intdiv($__L_num, $__L_div);});
$quot = &$GLOBALS['quot'];
$GLOBALS['max'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x > $__L_y)) { return $__L_x;} else { return $__L_y;} } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return call_user_func($GLOBALS['reduce'], $GLOBALS['max'], call_user_func($GLOBALS['max'], $__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$max = &$GLOBALS['max'];
$GLOBALS['min'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; if (($__L_x < $__L_y)) { return $__L_x;} else { return $__L_y;} } else if ($__n == 1) { $__L_x = $__args[0]; return $__L_x; } else if ($__n >= 2) { $__L_x = $__args[0]; $__L_y = $__args[1]; $__L_more = array_slice($__args, 2); return call_user_func($GLOBALS['reduce'], $GLOBALS['min'], call_user_func($GLOBALS['min'], $__L_x, $__L_y), $__L_more); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$min = &$GLOBALS['min'];
$GLOBALS['abs'] = (function($__L_a) { return abs($__L_a);});
$abs = &$GLOBALS['abs'];
$GLOBALS['zero_QMARK_'] = (function($__L_x) { return (0 === $__L_x);});
$zero_QMARK_ = &$GLOBALS['zero_QMARK_'];
$GLOBALS['pos_QMARK_'] = (function($__L_x) { return ($__L_x > 0);});
$pos_QMARK_ = &$GLOBALS['pos_QMARK_'];
$GLOBALS['neg_QMARK_'] = (function($__L_x) { return ($__L_x < 0);});
$neg_QMARK_ = &$GLOBALS['neg_QMARK_'];
$GLOBALS['even_QMARK_'] = (function($__L_n) { return (0 === ($__L_n % 2));});
$even_QMARK_ = &$GLOBALS['even_QMARK_'];
$GLOBALS['odd_QMARK_'] = (function($__L_n) { return (1 === abs(($__L_n % 2)));});
$odd_QMARK_ = &$GLOBALS['odd_QMARK_'];
$GLOBALS['string_QMARK_'] = (function($__L_x) { return is_string($__L_x);});
$string_QMARK_ = &$GLOBALS['string_QMARK_'];
$GLOBALS['int_QMARK_'] = (function($__L_x) { return is_int($__L_x);});
$int_QMARK_ = &$GLOBALS['int_QMARK_'];
$GLOBALS['integer_QMARK_'] = (function($__L_x) { return is_int($__L_x);});
$integer_QMARK_ = &$GLOBALS['integer_QMARK_'];
$GLOBALS['float_QMARK_'] = (function($__L_x) { return is_float($__L_x);});
$float_QMARK_ = &$GLOBALS['float_QMARK_'];
$GLOBALS['number_QMARK_'] = (function($__L_x) { return is_numeric($__L_x);});
$number_QMARK_ = &$GLOBALS['number_QMARK_'];
$GLOBALS['boolean_QMARK_'] = (function($__L_x) { return is_bool($__L_x);});
$boolean_QMARK_ = &$GLOBALS['boolean_QMARK_'];
$GLOBALS['true_QMARK_'] = (function($__L_x) { return ($__L_x === true);});
$true_QMARK_ = &$GLOBALS['true_QMARK_'];
$GLOBALS['false_QMARK_'] = (function($__L_x) { return ($__L_x === false);});
$false_QMARK_ = &$GLOBALS['false_QMARK_'];
$GLOBALS['array_QMARK_'] = (function($__L_x) { return is_array($__L_x);});
$array_QMARK_ = &$GLOBALS['array_QMARK_'];
$GLOBALS['fn_QMARK_'] = (function($__L_x) { return is_callable($__L_x);});
$fn_QMARK_ = &$GLOBALS['fn_QMARK_'];
$GLOBALS['symbol_QMARK_'] = (function($__L_x) { return is_string($__L_x);});
$symbol_QMARK_ = &$GLOBALS['symbol_QMARK_'];
$GLOBALS['keyword_QMARK_'] = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Kw);});
$keyword_QMARK_ = &$GLOBALS['keyword_QMARK_'];
$GLOBALS['vector_QMARK_'] = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Vec);});
$vector_QMARK_ = &$GLOBALS['vector_QMARK_'];
$GLOBALS['map_QMARK_'] = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Map);});
$map_QMARK_ = &$GLOBALS['map_QMARK_'];
$GLOBALS['set_QMARK_'] = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\Set);});
$set_QMARK_ = &$GLOBALS['set_QMARK_'];
$GLOBALS['list_QMARK_'] = (function($__L_x) { return ($__L_x instanceof \Clojure\Php\PList);});
$list_QMARK_ = &$GLOBALS['list_QMARK_'];
$GLOBALS['coll_QMARK_'] = (function($__L_x) { if (call_user_func($GLOBALS['vector_QMARK_'], $__L_x)) { return call_user_func($GLOBALS['vector_QMARK_'], $__L_x);} else { if (call_user_func($GLOBALS['map_QMARK_'], $__L_x)) { return call_user_func($GLOBALS['map_QMARK_'], $__L_x);} else { if (call_user_func($GLOBALS['set_QMARK_'], $__L_x)) { return call_user_func($GLOBALS['set_QMARK_'], $__L_x);} else { if (call_user_func($GLOBALS['list_QMARK_'], $__L_x)) { return call_user_func($GLOBALS['list_QMARK_'], $__L_x);} else { return is_array($__L_x);}}}}});
$coll_QMARK_ = &$GLOBALS['coll_QMARK_'];
$GLOBALS['seq_QMARK_'] = (function($__L_x) { if (call_user_func($GLOBALS['vector_QMARK_'], $__L_x)) { return call_user_func($GLOBALS['vector_QMARK_'], $__L_x);} else { return call_user_func($GLOBALS['list_QMARK_'], $__L_x);}});
$seq_QMARK_ = &$GLOBALS['seq_QMARK_'];
$GLOBALS['empty_QMARK_'] = (function($__L_x) { return (0 === call_user_func($GLOBALS['count'], $__L_x));});
$empty_QMARK_ = &$GLOBALS['empty_QMARK_'];
$GLOBALS['some_QMARK_'] = (function($__L_x) { return call_user_func($GLOBALS['not'], call_user_func($GLOBALS['nil_QMARK_'], $__L_x));});
$some_QMARK_ = &$GLOBALS['some_QMARK_'];
$GLOBALS['keys'] = (function($__L_m) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_m)) { return null;} else { return call_user_func('\Clojure\Php\keys', $__L_m);}});
$keys = &$GLOBALS['keys'];
$GLOBALS['vals'] = (function($__L_m) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_m)) { return null;} else { return call_user_func('\Clojure\Php\vals', $__L_m);}});
$vals = &$GLOBALS['vals'];
$GLOBALS['contains_QMARK_'] = (function($__L_coll, $__L_key) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_coll)) { return false;} else { return call_user_func('\Clojure\Php\contains', $__L_coll, $__L_key);}});
$contains_QMARK_ = &$GLOBALS['contains_QMARK_'];
$GLOBALS['take'] = (function($__L_n, $__L_coll) { $__L_n = $__L_n;
$__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if ((($__L_n > 0) ? call_user_func($GLOBALS['seq'], $__L_coll) : null)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
$__L_n = call_user_func($GLOBALS['dec'], $__L_n); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_x); continue;} else { return $__L_result;} break; }
});
$take = &$GLOBALS['take'];
$GLOBALS['drop'] = (function($__L_n, $__L_coll) { $__L_n = $__L_n;
$__L_coll = $__L_coll;
 while(true) { if ((($__L_n > 0) ? call_user_func($GLOBALS['seq'], $__L_coll) : null)) { $__L_n = call_user_func($GLOBALS['dec'], $__L_n); $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_coll;} break; }
});
$drop = &$GLOBALS['drop'];
$GLOBALS['take_while'] = (function($__L_pred, $__L_coll) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
if (call_user_func($__L_pred, $__L_x)) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_x); continue;} else { return $__L_result;}} else { return $__L_result;} break; }
});
$take_while = &$GLOBALS['take_while'];
$GLOBALS['drop_while'] = (function($__L_pred, $__L_coll) { $__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { if (call_user_func($__L_pred, call_user_func($GLOBALS['first'], $__L_coll))) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;} else { return $__L_coll;}} else { return $__L_coll;} break; }
});
$drop_while = &$GLOBALS['drop_while'];
$GLOBALS['concat'] = (function(...$__L_colls) { return call_user_func($GLOBALS['reduce'], (function($__L_acc, $__L_c) use (&$__L_colls) { $__L_acc = $__L_acc;
$__L_c = $__L_c;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_c)) { $__L_acc = call_user_func($GLOBALS['conj'], $__L_acc, call_user_func($GLOBALS['first'], $__L_c)); $__L_c = call_user_func($GLOBALS['next'], $__L_c); continue;} else { return $__L_acc;} break; }
}), \Clojure\Php\vec(), $__L_colls);});
$concat = &$GLOBALS['concat'];
$GLOBALS['reverse'] = (function($__L_coll) { $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
return call_user_func('\Clojure\Php\vector', array_reverse($__L_arr));});
$reverse = &$GLOBALS['reverse'];
$GLOBALS['nth'] = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_coll = $__args[0]; $__L_n = $__args[1]; $__L_not_found = $__args[2]; if (call_user_func($GLOBALS['vector_QMARK_'], $__L_coll)) { if ((($__L_n >= 0) ? ($__L_n < call_user_func($GLOBALS['count'], $__L_coll)) : null)) { return call_user_func('\Clojure\Php\get', $__L_coll, $__L_n);} else { return $__L_not_found;}} else { $__L_coll = $__L_coll;
$__L_n = $__L_n;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { if (($__L_n === 0)) { return call_user_func($GLOBALS['first'], $__L_coll);} else { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_n = call_user_func($GLOBALS['dec'], $__L_n); continue;}} else { return $__L_not_found;} break; }
} } else if ($__n == 2) { $__L_coll = $__args[0]; $__L_n = $__args[1]; return call_user_func($GLOBALS['nth'], $__L_coll, $__L_n, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$nth = &$GLOBALS['nth'];
$GLOBALS['range3'] = (function($__L_start, $__L_end, $__L_step) { $__L_n = $__L_start;
$__L_result = \Clojure\Php\vec();
 while(true) { if (($__L_n < $__L_end)) { $__L_current = $__L_n;
$__L_n = ($__L_n + $__L_step); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_current); continue;} else { return $__L_result;} break; }
});
$range3 = &$GLOBALS['range3'];
$GLOBALS['range'] = (function($__L_end_or_start, ...$__L_args) { if (call_user_func($GLOBALS['nil_QMARK_'], call_user_func($GLOBALS['first'], $__L_args))) { return call_user_func($GLOBALS['range3'], 0, $__L_end_or_start, 1);} else { $__L_end = call_user_func($GLOBALS['first'], $__L_args);
$__L_step = (call_user_func($GLOBALS['second'], $__L_args) ? call_user_func($GLOBALS['second'], $__L_args) : 1);
return call_user_func($GLOBALS['range3'], $__L_end_or_start, $__L_end, $__L_step);}});
$range = &$GLOBALS['range'];
$GLOBALS['repeat'] = (function($__L_n, $__L_x) { $__L_i = $__L_n;
$__L_result = \Clojure\Php\vec();
 while(true) { if (($__L_i > 0)) { $__L_i = call_user_func($GLOBALS['dec'], $__L_i); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_x); continue;} else { return $__L_result;} break; }
});
$repeat = &$GLOBALS['repeat'];
$GLOBALS['into'] = (function($__L_to, $__L_from) { return call_user_func($GLOBALS['reduce'], $GLOBALS['conj'], $__L_to, $__L_from);});
$into = &$GLOBALS['into'];
$GLOBALS['zipmap'] = (function($__L_keys, $__L_vals) { $__L_m = \Clojure\Php\hashMap();
$__L_ks = $__L_keys;
$__L_vs = $__L_vals;
 while(true) { if ((call_user_func($GLOBALS['seq'], $__L_ks) ? call_user_func($GLOBALS['seq'], $__L_vs) : null)) { $__L_m = call_user_func($GLOBALS['assoc'], $__L_m, call_user_func($GLOBALS['first'], $__L_ks), call_user_func($GLOBALS['first'], $__L_vs)); $__L_ks = call_user_func($GLOBALS['next'], $__L_ks); $__L_vs = call_user_func($GLOBALS['next'], $__L_vs); continue;} else { return $__L_m;} break; }
});
$zipmap = &$GLOBALS['zipmap'];
$GLOBALS['partition'] = (function(...$__args) { $__n = count($__args); if ($__n == 4) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_pad = $__args[2]; $__L_coll = $__args[3]; $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { $__L_chunk = call_user_func($GLOBALS['take'], $__L_n, $__L_coll);
if (($__L_n === call_user_func($GLOBALS['count'], $__L_chunk))) { $__L_coll = call_user_func($GLOBALS['drop'], $__L_step, $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_chunk); continue;} else { if (call_user_func($GLOBALS['seq'], $__L_chunk)) { return call_user_func($GLOBALS['conj'], $__L_result, call_user_func($GLOBALS['take'], $__L_n, call_user_func($GLOBALS['concat'], $__L_chunk, $__L_pad)));} else { return $__L_result;}} break; }
 } else if ($__n == 3) { $__L_n = $__args[0]; $__L_step = $__args[1]; $__L_coll = $__args[2]; $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { $__L_chunk = call_user_func($GLOBALS['take'], $__L_n, $__L_coll);
if (($__L_n === call_user_func($GLOBALS['count'], $__L_chunk))) { $__L_coll = call_user_func($GLOBALS['drop'], $__L_step, $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_chunk); continue;} else { return $__L_result;} break; }
 } else if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; return call_user_func($GLOBALS['partition'], $__L_n, $__L_n, $__L_coll); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$partition = &$GLOBALS['partition'];
$GLOBALS['distinct'] = (function($__L_coll) { $__L_coll = $__L_coll;
$__L_seen = \Clojure\Php\hashSet();
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
if (call_user_func($GLOBALS['contains_QMARK_'], $__L_seen, $__L_x)) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_seen = $__L_seen; $__L_result = $__L_result; continue;} else { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_seen = call_user_func($GLOBALS['conj'], $__L_seen, $__L_x); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_x); continue;}} else { return $__L_result;} break; }
});
$distinct = &$GLOBALS['distinct'];
$GLOBALS['group_by'] = (function($__L_f, $__L_coll) { return call_user_func($GLOBALS['reduce'], (function($__L_m, $__L_x) use (&$__L_coll, &$__L_f) { $__L_k = call_user_func($__L_f, $__L_x);
return call_user_func($GLOBALS['update'], $__L_m, $__L_k, (function($__L_v) use (&$__L_x, &$__L_m, &$__L_coll, &$__L_k, &$__L_f) { return call_user_func($GLOBALS['conj'], ($__L_v ? $__L_v : \Clojure\Php\vec()), $__L_x);}));}), \Clojure\Php\hashMap(), $__L_coll);});
$group_by = &$GLOBALS['group_by'];
$GLOBALS['frequencies'] = (function($__L_coll) { return call_user_func($GLOBALS['reduce'], (function($__L_m, $__L_x) use (&$__L_coll) { return call_user_func($GLOBALS['update'], $__L_m, $__L_x, (function($__L_v) use (&$__L_x, &$__L_m, &$__L_coll) { return (($__L_v ? $__L_v : 0) + 1);}));}), \Clojure\Php\hashMap(), $__L_coll);});
$frequencies = &$GLOBALS['frequencies'];
$GLOBALS['sort'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_comp = $__args[0]; $__L_coll = $__args[1]; $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
usort($__L_arr, $__L_comp);
return call_user_func('\Clojure\Php\vector', $__L_arr); } else if ($__n == 1) { $__L_coll = $__args[0]; $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
sort($__L_arr);
return call_user_func('\Clojure\Php\vector', $__L_arr); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$sort = &$GLOBALS['sort'];
$GLOBALS['sort_by'] = (function($__L_keyfn, $__L_coll) { $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
usort($__L_arr, (function($__L_a, $__L_b) use (&$__L_arr, &$__L_coll, &$__L_keyfn) { $__L_ka = call_user_func($__L_keyfn, $__L_a);
$__L_kb = call_user_func($__L_keyfn, $__L_b);
if (($__L_ka < $__L_kb)) { return -1;} else { if (($__L_ka > $__L_kb)) { return 1;} else { return 0;}}}));
return call_user_func('\Clojure\Php\vector', $__L_arr);});
$sort_by = &$GLOBALS['sort_by'];
$GLOBALS['merge'] = (function(...$__L_maps) { return call_user_func($GLOBALS['reduce'], (function($__L_m1, $__L_m2) use (&$__L_maps) { if ($__L_m2) { return call_user_func($GLOBALS['reduce'], (function($__L_m, $__L_kv) use (&$__L_m1, &$__L_maps, &$__L_m2) { return call_user_func($GLOBALS['assoc'], $__L_m, call_user_func($GLOBALS['first'], $__L_kv), call_user_func($GLOBALS['second'], $__L_kv));}), $__L_m1, call_user_func($GLOBALS['seq'], $__L_m2));} else { return $__L_m1;}}), \Clojure\Php\hashMap(), $__L_maps);});
$merge = &$GLOBALS['merge'];
$GLOBALS['get_in'] = (function($__L_m, $__L_ks) { return call_user_func($GLOBALS['reduce'], $GLOBALS['get'], $__L_m, $__L_ks);});
$get_in = &$GLOBALS['get_in'];
$GLOBALS['assoc_in'] = (function($__L_m, $__L_ks, $__L_v) { $__L_k = call_user_func($GLOBALS['first'], $__L_ks);
$__L_ks = call_user_func($GLOBALS['next'], $__L_ks);
if ($__L_ks) { return call_user_func($GLOBALS['assoc'], $__L_m, $__L_k, call_user_func($GLOBALS['assoc_in'], call_user_func($GLOBALS['get'], $__L_m, $__L_k), $__L_ks, $__L_v));} else { return call_user_func($GLOBALS['assoc'], $__L_m, $__L_k, $__L_v);}});
$assoc_in = &$GLOBALS['assoc_in'];
$GLOBALS['update_in'] = (function($__L_m, $__L_ks, $__L_f, ...$__L_args) { $__L_k = call_user_func($GLOBALS['first'], $__L_ks);
$__L_ks = call_user_func($GLOBALS['next'], $__L_ks);
if ($__L_ks) { return call_user_func($GLOBALS['assoc'], $__L_m, $__L_k, call_user_func($GLOBALS['apply'], $GLOBALS['update_in'], call_user_func($GLOBALS['get'], $__L_m, $__L_k), $__L_ks, $__L_f, $__L_args));} else { return call_user_func($GLOBALS['assoc'], $__L_m, $__L_k, call_user_func($GLOBALS['apply'], $__L_f, call_user_func($GLOBALS['get'], $__L_m, $__L_k), $__L_args));}});
$update_in = &$GLOBALS['update_in'];
$GLOBALS['dissoc'] = (function($__L_m, ...$__L_ks) { return call_user_func($GLOBALS['reduce'], (function($__L_m, $__L_k) use (&$__L_ks) { return call_user_func('\Clojure\Php\dissoc', $__L_m, $__L_k);}), $__L_m, $__L_ks);});
$dissoc = &$GLOBALS['dissoc'];
$GLOBALS['select_keys'] = (function($__L_m, $__L_ks) { return call_user_func($GLOBALS['reduce'], (function($__L_result, $__L_k) use (&$__L_ks, &$__L_m) { if (call_user_func($GLOBALS['contains_QMARK_'], $__L_m, $__L_k)) { return call_user_func($GLOBALS['assoc'], $__L_result, $__L_k, call_user_func($GLOBALS['get'], $__L_m, $__L_k));} else { return $__L_result;}}), \Clojure\Php\hashMap(), $__L_ks);});
$select_keys = &$GLOBALS['select_keys'];
$GLOBALS['every_QMARK_'] = (function($__L_pred, $__L_coll) { $__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { if (call_user_func($__L_pred, call_user_func($GLOBALS['first'], $__L_coll))) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;}} else { return true;} break; }
});
$every_QMARK_ = &$GLOBALS['every_QMARK_'];
$GLOBALS['some'] = (function($__L_pred, $__L_coll) { $__L_coll = $__L_coll;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_result = call_user_func($__L_pred, call_user_func($GLOBALS['first'], $__L_coll));
if ($__L_result) { return $__L_result;} else { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); continue;}} break; }
});
$some = &$GLOBALS['some'];
$GLOBALS['not_every_QMARK_'] = (function($__L_pred, $__L_coll) { return call_user_func($GLOBALS['not'], call_user_func($GLOBALS['every_QMARK_'], $__L_pred, $__L_coll));});
$not_every_QMARK_ = &$GLOBALS['not_every_QMARK_'];
$GLOBALS['not_any_QMARK_'] = (function($__L_pred, $__L_coll) { return call_user_func($GLOBALS['not'], call_user_func($GLOBALS['some'], $__L_pred, $__L_coll));});
$not_any_QMARK_ = &$GLOBALS['not_any_QMARK_'];
$GLOBALS['constantly'] = (function($__L_x) { return (function(...$__L_args) use (&$__L_x) { return $__L_x;});});
$constantly = &$GLOBALS['constantly'];
$GLOBALS['complement'] = (function($__L_f) { return (function(...$__L_args) use (&$__L_f) { return call_user_func($GLOBALS['not'], call_user_func($GLOBALS['apply'], $__L_f, $__L_args));});});
$complement = &$GLOBALS['complement'];
$GLOBALS['juxt'] = (function(...$__L_fs) { return (function(...$__L_args) use (&$__L_fs) { return call_user_func($GLOBALS['map'], (function($__L_f) use (&$__L_fs, &$__L_args) { return call_user_func($GLOBALS['apply'], $__L_f, $__L_args);}), $__L_fs);});});
$juxt = &$GLOBALS['juxt'];
$GLOBALS['fnil'] = (function($__L_f, $__L_x) { return (function($__L_arg, ...$__L_args) use (&$__L_x, &$__L_f) { return call_user_func($GLOBALS['apply'], $__L_f, (call_user_func($GLOBALS['nil_QMARK_'], $__L_arg) ? $__L_x : $__L_arg), $__L_args);});});
$fnil = &$GLOBALS['fnil'];
$GLOBALS['memoize'] = (function($__L_f) { $__L_cache = call_user_func($GLOBALS['atom'], \Clojure\Php\hashMap());
return (function(...$__L_args) use (&$__L_cache, &$__L_f) { $__L_key = call_user_func($GLOBALS['apply'], $GLOBALS['vector'], $__L_args);
$__L_cached = call_user_func($GLOBALS['get'], call_user_func($GLOBALS['deref'], $__L_cache), $__L_key);
if (call_user_func($GLOBALS['some_QMARK_'], $__L_cached)) { return $__L_cached;} else { $__L_result = call_user_func($GLOBALS['apply'], $__L_f, $__L_args);
call_user_func($GLOBALS['swap_BANG_'], $__L_cache, $GLOBALS['assoc'], $__L_key, $__L_result);
return $__L_result;}});});
$memoize = &$GLOBALS['memoize'];
$GLOBALS['compare'] = (function($__L_x, $__L_y) { if (($__L_x < $__L_y)) { return -1;} else { if (($__L_x > $__L_y)) { return 1;} else { if (\Clojure\Php\Kw::create('else')) { return 0;} else { return null;}}}});
$compare = &$GLOBALS['compare'];
$GLOBALS['identical_QMARK_'] = (function($__L_x, $__L_y) { return ($__L_x === $__L_y);});
$identical_QMARK_ = &$GLOBALS['identical_QMARK_'];
$GLOBALS['name'] = (function($__L_x) { if (call_user_func($GLOBALS['keyword_QMARK_'], $__L_x)) { return $__L_x->getName();} else { if (call_user_func($GLOBALS['symbol_QMARK_'], $__L_x)) { return strval($__L_x);} else { if (call_user_func($GLOBALS['string_QMARK_'], $__L_x)) { return $__L_x;} else { if (\Clojure\Php\Kw::create('else')) { return strval($__L_x);} else { return null;}}}}});
$name = &$GLOBALS['name'];
$GLOBALS['namespace'] = (function($__L_x) { if (call_user_func($GLOBALS['keyword_QMARK_'], $__L_x)) { return $__L_x->getNamespace();}});
$namespace = &$GLOBALS['namespace'];
$GLOBALS['subs'] = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_s = $__args[0]; $__L_start = $__args[1]; $__L_end = $__args[2]; return substr($__L_s, $__L_start, ($__L_end - $__L_start)); } else if ($__n == 2) { $__L_s = $__args[0]; $__L_start = $__args[1]; return substr($__L_s, $__L_start); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$subs = &$GLOBALS['subs'];
$GLOBALS['interleave'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_c1 = $__args[0]; $__L_c2 = $__args[1]; $__L_c1 = $__L_c1;
$__L_c2 = $__L_c2;
$__L_result = \Clojure\Php\vec();
 while(true) { if ((call_user_func($GLOBALS['seq'], $__L_c1) ? call_user_func($GLOBALS['seq'], $__L_c2) : null)) { $__L_c1 = call_user_func($GLOBALS['next'], $__L_c1); $__L_c2 = call_user_func($GLOBALS['next'], $__L_c2); $__L_result = call_user_func($GLOBALS['conj'], call_user_func($GLOBALS['conj'], $__L_result, call_user_func($GLOBALS['first'], $__L_c1)), call_user_func($GLOBALS['first'], $__L_c2)); continue;} else { return $__L_result;} break; }
 } else if ($__n >= 2) { $__L_c1 = $__args[0]; $__L_c2 = $__args[1]; $__L_colls = array_slice($__args, 2); $__L_colls = call_user_func($GLOBALS['cons'], $__L_c1, call_user_func($GLOBALS['cons'], $__L_c2, $__L_colls));
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['every_QMARK_'], $GLOBALS['seq'], $__L_colls)) { $__L_colls = call_user_func($GLOBALS['map'], $GLOBALS['next'], $__L_colls); $__L_result = call_user_func($GLOBALS['into'], $__L_result, call_user_func($GLOBALS['map'], $GLOBALS['first'], $__L_colls)); continue;} else { return $__L_result;} break; }
 } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$interleave = &$GLOBALS['interleave'];
$GLOBALS['interpose'] = (function($__L_sep, $__L_coll) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
$__L_first_QMARK_ = true;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { if ($__L_first_QMARK_) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, call_user_func($GLOBALS['first'], $__L_coll)); $__L_first_QMARK_ = false; continue;} else { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], call_user_func($GLOBALS['conj'], $__L_result, $__L_sep), call_user_func($GLOBALS['first'], $__L_coll)); $__L_first_QMARK_ = false; continue;}} else { return $__L_result;} break; }
});
$interpose = &$GLOBALS['interpose'];
$GLOBALS['flatten'] = (function($__L_coll) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
if (call_user_func($GLOBALS['coll_QMARK_'], $__L_x)) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['into'], $__L_result, call_user_func($GLOBALS['flatten'], $__L_x)); continue;} else { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_x); continue;}} else { return $__L_result;} break; }
});
$flatten = &$GLOBALS['flatten'];
$GLOBALS['mapcat'] = (function($__L_f, $__L_coll) { return call_user_func($GLOBALS['apply'], $GLOBALS['concat'], call_user_func($GLOBALS['map'], $__L_f, $__L_coll));});
$mapcat = &$GLOBALS['mapcat'];
$GLOBALS['keep'] = (function($__L_f, $__L_coll) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_v = call_user_func($__L_f, call_user_func($GLOBALS['first'], $__L_coll));
$__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = (call_user_func($GLOBALS['nil_QMARK_'], $__L_v) ? $__L_result : call_user_func($GLOBALS['conj'], $__L_result, $__L_v)); continue;} else { return $__L_result;} break; }
});
$keep = &$GLOBALS['keep'];
$GLOBALS['keep_indexed'] = (function($__L_f, $__L_coll) { $__L_coll = $__L_coll;
$__L_idx = 0;
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_v = call_user_func($__L_f, $__L_idx, call_user_func($GLOBALS['first'], $__L_coll));
$__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_idx = call_user_func($GLOBALS['inc'], $__L_idx); $__L_result = (call_user_func($GLOBALS['nil_QMARK_'], $__L_v) ? $__L_result : call_user_func($GLOBALS['conj'], $__L_result, $__L_v)); continue;} else { return $__L_result;} break; }
});
$keep_indexed = &$GLOBALS['keep_indexed'];
$GLOBALS['map_indexed'] = (function($__L_f, $__L_coll) { $__L_coll = $__L_coll;
$__L_idx = 0;
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_idx = call_user_func($GLOBALS['inc'], $__L_idx); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, call_user_func($__L_f, $__L_idx, call_user_func($GLOBALS['first'], $__L_coll))); continue;} else { return $__L_result;} break; }
});
$map_indexed = &$GLOBALS['map_indexed'];
$GLOBALS['partition_by'] = (function($__L_f, $__L_coll) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
$__L_current = \Clojure\Php\vec();
$__L_current_val = null;
 while(true) { if (call_user_func($GLOBALS['seq'], $__L_coll)) { $__L_x = call_user_func($GLOBALS['first'], $__L_coll);
$__L_v = call_user_func($__L_f, $__L_x);
if ((call_user_func($GLOBALS['nil_QMARK_'], $__L_current_val) ? call_user_func($GLOBALS['nil_QMARK_'], $__L_current_val) : call_user_func($GLOBALS['_EQ_'], $__L_v, $__L_current_val))) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = $__L_result; $__L_current = call_user_func($GLOBALS['conj'], $__L_current, $__L_x); $__L_current_val = $__L_v; continue;} else { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, $__L_current); $__L_current = \Clojure\Php\vec($__L_x); $__L_current_val = $__L_v; continue;}} else { if (call_user_func($GLOBALS['seq'], $__L_current)) { return call_user_func($GLOBALS['conj'], $__L_result, $__L_current);} else { return $__L_result;}} break; }
});
$partition_by = &$GLOBALS['partition_by'];
$GLOBALS['split_at'] = (function($__L_n, $__L_coll) { return \Clojure\Php\vec(call_user_func($GLOBALS['take'], $__L_n, $__L_coll), call_user_func($GLOBALS['drop'], $__L_n, $__L_coll));});
$split_at = &$GLOBALS['split_at'];
$GLOBALS['split_with'] = (function($__L_pred, $__L_coll) { return \Clojure\Php\vec(call_user_func($GLOBALS['take_while'], $__L_pred, $__L_coll), call_user_func($GLOBALS['drop_while'], $__L_pred, $__L_coll));});
$split_with = &$GLOBALS['split_with'];
$GLOBALS['butlast'] = (function($__L_coll) { $__L_coll = $__L_coll;
$__L_result = \Clojure\Php\vec();
 while(true) { if (call_user_func($GLOBALS['next'], $__L_coll)) { $__L_coll = call_user_func($GLOBALS['next'], $__L_coll); $__L_result = call_user_func($GLOBALS['conj'], $__L_result, call_user_func($GLOBALS['first'], $__L_coll)); continue;} else { return $__L_result;} break; }
});
$butlast = &$GLOBALS['butlast'];
$GLOBALS['shuffle'] = (function($__L_coll) { $__L_arr = call_user_func('\Clojure\Php\toArray', $__L_coll);
shuffle($__L_arr);
return call_user_func('\Clojure\Php\vector', $__L_arr);});
$shuffle = &$GLOBALS['shuffle'];
$GLOBALS['union'] = (function($__L_s1, $__L_s2) { return call_user_func($GLOBALS['reduce'], $GLOBALS['conj'], $__L_s1, $__L_s2);});
$union = &$GLOBALS['union'];
$GLOBALS['intersection'] = (function($__L_s1, $__L_s2) { return call_user_func($GLOBALS['reduce'], (function($__L_result, $__L_x) use (&$__L_s1, &$__L_s2) { if (call_user_func($GLOBALS['contains_QMARK_'], $__L_s2, $__L_x)) { return call_user_func($GLOBALS['conj'], $__L_result, $__L_x);} else { return $__L_result;}}), \Clojure\Php\hashSet(), $__L_s1);});
$intersection = &$GLOBALS['intersection'];
$GLOBALS['difference'] = (function($__L_s1, $__L_s2) { return call_user_func($GLOBALS['reduce'], (function($__L_result, $__L_x) use (&$__L_s1, &$__L_s2) { if (call_user_func($GLOBALS['contains_QMARK_'], $__L_s2, $__L_x)) { return $__L_result;} else { return call_user_func($GLOBALS['conj'], $__L_result, $__L_x);}}), \Clojure\Php\hashSet(), $__L_s1);});
$difference = &$GLOBALS['difference'];
$GLOBALS['subset_QMARK_'] = (function($__L_s1, $__L_s2) { return call_user_func($GLOBALS['every_QMARK_'], (function($__L_x) use (&$__L_s1, &$__L_s2) { return call_user_func($GLOBALS['contains_QMARK_'], $__L_s2, $__L_x);}), $__L_s1);});
$subset_QMARK_ = &$GLOBALS['subset_QMARK_'];
$GLOBALS['superset_QMARK_'] = (function($__L_s1, $__L_s2) { return call_user_func($GLOBALS['subset_QMARK_'], $__L_s2, $__L_s1);});
$superset_QMARK_ = &$GLOBALS['superset_QMARK_'];
$GLOBALS['max'] = (function(...$__L_xs) { return call_user_func($GLOBALS['reduce'], (function($__L_a, $__L_b) use (&$__L_xs) { if (($__L_a > $__L_b)) { return $__L_a;} else { return $__L_b;}}), call_user_func($GLOBALS['first'], $__L_xs), call_user_func($GLOBALS['next'], $__L_xs));});
$max = &$GLOBALS['max'];
$GLOBALS['min'] = (function(...$__L_xs) { return call_user_func($GLOBALS['reduce'], (function($__L_a, $__L_b) use (&$__L_xs) { if (($__L_a < $__L_b)) { return $__L_a;} else { return $__L_b;}}), call_user_func($GLOBALS['first'], $__L_xs), call_user_func($GLOBALS['next'], $__L_xs));});
$min = &$GLOBALS['min'];
$GLOBALS['abs'] = (function($__L_x) { if (($__L_x < 0)) { return (-$__L_x);} else { return $__L_x;}});
$abs = &$GLOBALS['abs'];
$GLOBALS['mod'] = (function($__L_n, $__L_d) { return ($__L_n % $__L_d);});
$mod = &$GLOBALS['mod'];
$GLOBALS['quot'] = (function($__L_n, $__L_d) { return intdiv($__L_n, $__L_d);});
$quot = &$GLOBALS['quot'];
$GLOBALS['rem'] = (function($__L_n, $__L_d) { return ($__L_n % $__L_d);});
$rem = &$GLOBALS['rem'];
$GLOBALS['even_QMARK_'] = (function($__L_n) { return (0 === ($__L_n % 2));});
$even_QMARK_ = &$GLOBALS['even_QMARK_'];
$GLOBALS['odd_QMARK_'] = (function($__L_n) { return call_user_func($GLOBALS['not'], call_user_func($GLOBALS['even_QMARK_'], $__L_n));});
$odd_QMARK_ = &$GLOBALS['odd_QMARK_'];
$GLOBALS['pos_QMARK_'] = (function($__L_n) { return ($__L_n > 0);});
$pos_QMARK_ = &$GLOBALS['pos_QMARK_'];
$GLOBALS['neg_QMARK_'] = (function($__L_n) { return ($__L_n < 0);});
$neg_QMARK_ = &$GLOBALS['neg_QMARK_'];
$GLOBALS['zero_QMARK_'] = (function($__L_n) { return ($__L_n === 0);});
$zero_QMARK_ = &$GLOBALS['zero_QMARK_'];
$GLOBALS['rand'] = (function() { return (rand() / getrandmax());});
$rand = &$GLOBALS['rand'];
$GLOBALS['rand_int'] = (function($__L_n) { return rand(0, call_user_func($GLOBALS['dec'], $__L_n));});
$rand_int = &$GLOBALS['rand_int'];
$GLOBALS['rand_nth'] = (function($__L_coll) { return call_user_func($GLOBALS['nth'], $__L_coll, call_user_func($GLOBALS['rand_int'], call_user_func($GLOBALS['count'], $__L_coll)));});
$rand_nth = &$GLOBALS['rand_nth'];
$GLOBALS['read_string'] = (function($__L_s) { 'Parse an EDN string. Currently uses PHP\'s json_decode for JSON-compatible values.';
return json_decode($__L_s, true);});
$read_string = &$GLOBALS['read_string'];
$GLOBALS['ex_info'] = (function(...$__args) { $__n = count($__args); if ($__n == 3) { $__L_msg = $__args[0]; $__L_data = $__args[1]; $__L_cause = $__args[2]; return call_user_func('\Clojure\Php\exInfo', $__L_msg, $__L_data, $__L_cause); } else if ($__n == 2) { $__L_msg = $__args[0]; $__L_data = $__args[1]; return call_user_func('\Clojure\Php\exInfo', $__L_msg, $__L_data); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$ex_info = &$GLOBALS['ex_info'];
$GLOBALS['ex_data'] = (function($__L_ex) { return call_user_func('\Clojure\Php\exData', $__L_ex);});
$ex_data = &$GLOBALS['ex_data'];
$GLOBALS['ex_message'] = (function($__L_ex) { return call_user_func('\Clojure\Php\exMessage', $__L_ex);});
$ex_message = &$GLOBALS['ex_message'];
$GLOBALS['ex_cause'] = (function($__L_ex) { return call_user_func('\Clojure\Php\exCause', $__L_ex);});
$ex_cause = &$GLOBALS['ex_cause'];
$GLOBALS['Throwable__GT_map'] = (function($__L_ex) { if (call_user_func($GLOBALS['nil_QMARK_'], $__L_ex)) { return null;} else { $__L_data = call_user_func($GLOBALS['ex_data'], $__L_ex);
$__L_msg = call_user_func($GLOBALS['ex_message'], $__L_ex);
$__L_cause = call_user_func($GLOBALS['ex_cause'], $__L_ex);
$__L_class_name = get_class($__L_ex);
return \Clojure\Php\hashMap(\Clojure\Php\Kw::create('type'), $__L_class_name, \Clojure\Php\Kw::create('message'), $__L_msg, \Clojure\Php\Kw::create('data'), $__L_data, \Clojure\Php\Kw::create('cause'), ($__L_cause ? call_user_func($GLOBALS['Throwable__GT_map'], $__L_cause) : null));}});
$Throwable__GT_map = &$GLOBALS['Throwable__GT_map'];
$GLOBALS['ex_triage'] = (function($__L_throwable_map) { $__L___dest_1 = $__L_throwable_map;
$__L_type = call_user_func($GLOBALS['get'], $__L___dest_1, \Clojure\Php\Kw::create('type'));
$__L_message = call_user_func($GLOBALS['get'], $__L___dest_1, \Clojure\Php\Kw::create('message'));
$__L_data = call_user_func($GLOBALS['get'], $__L___dest_1, \Clojure\Php\Kw::create('data'));
$__L_cause = call_user_func($GLOBALS['get'], $__L___dest_1, \Clojure\Php\Kw::create('cause'));
$__L_phase = (call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase')) ? call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase')) : \Clojure\Php\Kw::create('execution'));
$__L_err_type = call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'type'));
$__L_file = call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'file'));
$__L_line = call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'line'));
$__L_col = call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'column'));
$__L_sym = call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'symbol'));
$__L_hint = call_user_func($GLOBALS['get'], $__L_data, \Clojure\Php\Kw::createNs('cljp.error', 'hint'));
return \Clojure\Php\hashMap(\Clojure\Php\Kw::createNs('cljp.error', 'file'), $__L_file, \Clojure\Php\Kw::createNs('cljp.error', 'type'), $__L_err_type, \Clojure\Php\Kw::createNs('cljp.error', 'column'), $__L_col, \Clojure\Php\Kw::createNs('cljp.error', 'message'), $__L_message, \Clojure\Php\Kw::createNs('cljp.error', 'line'), $__L_line, \Clojure\Php\Kw::createNs('cljp.error', 'phase'), $__L_phase, \Clojure\Php\Kw::createNs('cljp.error', 'hint'), $__L_hint, \Clojure\Php\Kw::createNs('cljp.error', 'class'), $__L_type, \Clojure\Php\Kw::createNs('cljp.error', 'cause'), ($__L_cause ? call_user_func($GLOBALS['ex_message'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('message'), call_user_func(\Clojure\Php\Kw::create('message'), $__L_cause))) : null), \Clojure\Php\Kw::createNs('cljp.error', 'symbol'), $__L_sym);});
$ex_triage = &$GLOBALS['ex_triage'];
$GLOBALS['ex_str'] = (function($__L_triage_data) { $__L_phase = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'phase'));
$__L_err_type = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'type'));
$__L_class = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'class'));
$__L_message = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'message'));
$__L_file = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'file'));
$__L_line = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'line'));
$__L_column = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'column'));
$__L_symbol = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'symbol'));
$__L_hint = call_user_func($GLOBALS['get'], $__L_triage_data, \Clojure\Php\Kw::createNs('cljp.error', 'hint'));
$__L_loc = call_user_func($GLOBALS['str'], ($__L_file ? $__L_file : 'REPL'), ':', ($__L_line ? $__L_line : 1), ($__L_column ? call_user_func($GLOBALS['str'], ':', $__L_column) : ''));
$__L_class_str = ($__L_class ? call_user_func($GLOBALS['str'], $__L_class) : '');
$__L_class_simple = basename(str_replace('\\', '/', $__L_class_str));
$__L_type_str = ($__L_err_type ? ltrim(call_user_func($GLOBALS['str'], $__L_err_type), ':') : '');
$__L_type_label = ($__L_err_type ? call_user_func($GLOBALS['str'], ucfirst($__L_type_str), 'Error') : ($__L_class ? $__L_class_simple : 'Error'));
return call_user_func($GLOBALS['str'], $__L_type_label, ': ', $__L_message, ' at (', $__L_loc, ')', ($__L_hint ? call_user_func($GLOBALS['str'], '
Hint: ', $__L_hint) : ''));});
$ex_str = &$GLOBALS['ex_str'];
$GLOBALS['err__GT_msg'] = (function($__L_ex) { return call_user_func($GLOBALS['ex_str'], call_user_func($GLOBALS['ex_triage'], call_user_func($GLOBALS['Throwable__GT_map'], $__L_ex)));});
$err__GT_msg = &$GLOBALS['err__GT_msg'];
$GLOBALS['_STAR_1'] = null;
$_STAR_1 = &$GLOBALS['_STAR_1'];
$GLOBALS['_STAR_2'] = null;
$_STAR_2 = &$GLOBALS['_STAR_2'];
$GLOBALS['_STAR_3'] = null;
$_STAR_3 = &$GLOBALS['_STAR_3'];
$GLOBALS['_STAR_e'] = null;
$_STAR_e = &$GLOBALS['_STAR_e'];
$GLOBALS['_STAR_ns_STAR_'] = 'user';
$_STAR_ns_STAR_ = &$GLOBALS['_STAR_ns_STAR_'];
$GLOBALS['_STAR_print_length_STAR_'] = null;
$_STAR_print_length_STAR_ = &$GLOBALS['_STAR_print_length_STAR_'];
$GLOBALS['_STAR_print_level_STAR_'] = null;
$_STAR_print_level_STAR_ = &$GLOBALS['_STAR_print_level_STAR_'];
$GLOBALS['_STAR_print_meta_STAR_'] = null;
$_STAR_print_meta_STAR_ = &$GLOBALS['_STAR_print_meta_STAR_'];
$GLOBALS['_push_repl_result'] = (function($__L_value) { 'Internal: Update REPL result history.';
return $__L_value;});
$_push_repl_result = &$GLOBALS['_push_repl_result'];
$GLOBALS['symbol'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_ns = $__args[0]; $__L_name = $__args[1]; return call_user_func('\Clojure\Php\sym', $__L_name, $__L_ns); } else if ($__n == 1) { $__L_name = $__args[0]; return call_user_func('\Clojure\Php\sym', $__L_name); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$symbol = &$GLOBALS['symbol'];
$GLOBALS['format'] = (function($__L_fmt, ...$__L_args) { return call_user_func_array('sprintf', call_user_func($GLOBALS['cons'], $__L_fmt, $__L_args));});
$format = &$GLOBALS['format'];
$GLOBALS['realized_QMARK_'] = (function($__L_x) { if (method_exists($__L_x, 'isRealized')) { return $__L_x->isRealized();} else { return true;}});
$realized_QMARK_ = &$GLOBALS['realized_QMARK_'];
$GLOBALS['doall'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; if ($__L_n) { $__L_i = $__L_n;
$__L_s = call_user_func($GLOBALS['seq'], $__L_coll);
 while(true) { if ((($__L_i > 0) ? $__L_s : null)) { $__L_i = call_user_func($GLOBALS['dec'], $__L_i); $__L_s = call_user_func($GLOBALS['next'], $__L_s); continue;}
 break; }
} else { $__L_s = call_user_func($GLOBALS['seq'], $__L_coll);
 while(true) { if ($__L_s) { $__L_s = call_user_func($GLOBALS['next'], $__L_s); continue;}
 break; }
}
return $__L_coll; } else if ($__n == 1) { $__L_coll = $__args[0]; return call_user_func($GLOBALS['doall'], $__L_coll, null); } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$doall = &$GLOBALS['doall'];
$GLOBALS['dorun'] = (function(...$__args) { $__n = count($__args); if ($__n == 2) { $__L_n = $__args[0]; $__L_coll = $__args[1]; $__L_i = $__L_n;
$__L_s = call_user_func($GLOBALS['seq'], $__L_coll);
 while(true) { if ((($__L_i > 0) ? $__L_s : null)) { $__L_i = call_user_func($GLOBALS['dec'], $__L_i); $__L_s = call_user_func($GLOBALS['next'], $__L_s); continue;}
 break; }
return null; } else if ($__n == 1) { $__L_coll = $__args[0]; $__L_s = call_user_func($GLOBALS['seq'], $__L_coll);
 while(true) { if ($__L_s) { $__L_s = call_user_func($GLOBALS['next'], $__L_s); continue;}
 break; }
return null; } else { throw new \InvalidArgumentException('Wrong number of args: ' . $__n); } });
$dorun = &$GLOBALS['dorun'];
$GLOBALS['re_pattern'] = (function($__L_s) { if (is_string($__L_s)) { return $__L_s;} else { throw (new \InvalidArgumentException('re-pattern requires a string'));
}});
$re_pattern = &$GLOBALS['re_pattern'];
$GLOBALS['re_matches'] = (function($__L_re, $__L_s) { $__L_result = array();
$__L_matched = preg_match(call_user_func($GLOBALS['str'], '^', $__L_re, '$'), $__L_s, $__L_result);
if (($__L_matched > 0)) { if ((1 === count($__L_result))) { return call_user_func($GLOBALS['first'], $__L_result);} else { return call_user_func('\Clojure\Php\vec', $__L_result);}}});
$re_matches = &$GLOBALS['re_matches'];
$GLOBALS['re_find'] = (function($__L_re, $__L_s) { $__L_result = array();
$__L_matched = preg_match($__L_re, $__L_s, $__L_result);
if (($__L_matched > 0)) { if ((1 === count($__L_result))) { return call_user_func($GLOBALS['first'], $__L_result);} else { return call_user_func('\Clojure\Php\vec', $__L_result);}}});
$re_find = &$GLOBALS['re_find'];
$GLOBALS['re_seq'] = (function($__L_re, $__L_s) { $__L_result = array();
$__L__ = preg_match_all($__L_re, $__L_s, $__L_result);
if ((count(call_user_func($GLOBALS['first'], $__L_result)) > 0)) { return call_user_func('\Clojure\Php\vec', call_user_func($GLOBALS['first'], $__L_result));}});
$re_seq = &$GLOBALS['re_seq'];
$GLOBALS['re_groups'] = (function($__L_m) { if (call_user_func($GLOBALS['string_QMARK_'], $__L_m)) { return $__L_m;} else { if ((call_user_func($GLOBALS['count'], $__L_m) > 1)) { return $__L_m;} else { return call_user_func($GLOBALS['first'], $__L_m);}}});
$re_groups = &$GLOBALS['re_groups'];
$GLOBALS['line_seq'] = (function($__L_rdr) { return (new \Clojure\Php\LazySeq((function() use (&$__L_rdr) { $__L_line = fgets($__L_rdr);
if (($__L_line !== false)) { return call_user_func($GLOBALS['cons'], rtrim($__L_line, PHP_EOL), call_user_func($GLOBALS['line_seq'], $__L_rdr));}})));});
$line_seq = &$GLOBALS['line_seq'];
$GLOBALS['tree_seq'] = (function($__L_branch_QMARK_, $__L_children, $__L_root) { $__L_walk = (function($__L_node) use (&$__L_branch_QMARK_, &$__L_children, &$__L_root) { return (new \Clojure\Php\LazySeq((function() use (&$__L_branch_QMARK_, &$__L_children, &$__L_node, &$__L_root) { return call_user_func($GLOBALS['cons'], $__L_node, (call_user_func($__L_branch_QMARK_, $__L_node) ? call_user_func($GLOBALS['mapcat'], $GLOBALS['walk'], call_user_func($__L_children, $__L_node)) : null));})));});
return call_user_func($__L_walk, $__L_root);});
$tree_seq = &$GLOBALS['tree_seq'];
$GLOBALS['file_seq'] = (function($__L_dir) { return call_user_func($GLOBALS['tree_seq'], (function($__L_f) use (&$__L_dir) { return is_dir($__L_f);}), (function($__L_d) use (&$__L_dir) { $__L_files = scandir($__L_d);
return call_user_func($GLOBALS['filter'], (function($__L_f) use (&$__L_dir, &$__L_files, &$__L_d) { return call_user_func($GLOBALS['not'], (call_user_func($GLOBALS['_EQ_'], $__L_f, '.') ? call_user_func($GLOBALS['_EQ_'], $__L_f, '.') : call_user_func($GLOBALS['_EQ_'], $__L_f, '..')));}), call_user_func($GLOBALS['map'], (function($__L_f) use (&$__L_dir, &$__L_files, &$__L_d) { return call_user_func($GLOBALS['str'], $__L_d, '/', $__L_f);}), $__L_files));}), $__L_dir);});
$file_seq = &$GLOBALS['file_seq'];
$GLOBALS['xml_seq'] = (function($__L_root) { return call_user_func($GLOBALS['tree_seq'], (function($__L_node) use (&$__L_root) { if (call_user_func($GLOBALS['map_QMARK_'], $__L_node)) { return call_user_func($GLOBALS['contains_QMARK_'], $__L_node, \Clojure\Php\Kw::create('content'));}}), (function($__L_node) use (&$__L_root) { return call_user_func($GLOBALS['get'], $__L_node, \Clojure\Php\Kw::create('content'));}), $__L_root);});
$xml_seq = &$GLOBALS['xml_seq'];
call_user_func($GLOBALS['deftest'], $GLOBALS['test__star'], call_user_func($GLOBALS['testing'], '*', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, (2 * 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 24, (2 * 3 * 4))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, ())), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, 5))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__PLUS_'], call_user_func($GLOBALS['testing'], '+', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, (1 + 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 10, (1 + 2 + 3 + 4))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 0, ())), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, 5))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__'], call_user_func($GLOBALS['testing'], '-', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 7, (10 - 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, (10 - 3 - 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], -5, (-5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__'], call_user_func($GLOBALS['testing'], '/', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, (10 / 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, (20 / 2 / 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__lt'], call_user_func($GLOBALS['testing'], '<', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (1 < 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, (2 < 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (1 < 2 < 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__lt_eq'], call_user_func($GLOBALS['testing'], '<=', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (1 <= 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (2 <= 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, (3 <= 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__eq'], call_user_func($GLOBALS['testing'], '=', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['_EQ_'], 1, 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['_EQ_'], 1, 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2), \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__gt'], call_user_func($GLOBALS['testing'], '>', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (2 > 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, (1 > 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (3 > 2 > 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test__gt_eq'], call_user_func($GLOBALS['testing'], '>=', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (2 >= 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (2 >= 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, (1 >= 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_abs'], call_user_func($GLOBALS['testing'], 'abs', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['abs'], -5))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['abs'], 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_and'], call_user_func($GLOBALS['testing'], 'and', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (true ? true : null))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, (true ? false : null))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], (null ? 1 : null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_apply'], call_user_func($GLOBALS['testing'], 'apply', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, call_user_func($GLOBALS['apply'], $GLOBALS['_PLUS_'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, call_user_func($GLOBALS['apply'], $GLOBALS['_PLUS_'], 1, \Clojure\Php\vec(2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'abc', call_user_func($GLOBALS['apply'], $GLOBALS['str'], \Clojure\Php\vec('a', 'b', 'c'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_assoc'], call_user_func($GLOBALS['testing'], 'assoc', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['assoc'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('b'), 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(\Clojure\Php\Kw::create('x'), 2, 3), call_user_func($GLOBALS['assoc'], \Clojure\Php\vec(1, 2, 3), 0, \Clojure\Php\Kw::create('x'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_assoc_in'], call_user_func($GLOBALS['testing'], 'assoc-in', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 2)), call_user_func($GLOBALS['assoc_in'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1)), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b')), 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1)), call_user_func($GLOBALS['assoc_in'], \Clojure\Php\hashMap(), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b')), 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_associative_p'], call_user_func($GLOBALS['testing'], 'associative?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['associative_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['associative_QMARK_'], \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['associative_QMARK_'], \Clojure\Php\plist(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_atom'], call_user_func($GLOBALS['testing'], 'atom', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['deref'], call_user_func($GLOBALS['atom'], 1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_and'], call_user_func($GLOBALS['testing'], 'bit-and', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, (5 & 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 15, (255 & 15)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_clear'], call_user_func($GLOBALS['testing'], 'bit-clear', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['bit_clear'], 7, 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_flip'], call_user_func($GLOBALS['testing'], 'bit-flip', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 4, call_user_func($GLOBALS['bit_flip'], 0, 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_not'], call_user_func($GLOBALS['testing'], 'bit-not', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], -1, call_user_func($GLOBALS['bit_not'], 0)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_or'], call_user_func($GLOBALS['testing'], 'bit-or', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 7, (5 | 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_set'], call_user_func($GLOBALS['testing'], 'bit-set', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 4, call_user_func($GLOBALS['bit_set'], 0, 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_shift_left'], call_user_func($GLOBALS['testing'], 'bit-shift-left', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 16, (1 << 4)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_shift_right'], call_user_func($GLOBALS['testing'], 'bit-shift-right', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 4, (16 >> 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_test'], call_user_func($GLOBALS['testing'], 'bit-test', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['bit_test'], 5, 0))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['bit_test'], 5, 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_bit_xor'], call_user_func($GLOBALS['testing'], 'bit-xor', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, (5 ^ 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_boolean'], call_user_func($GLOBALS['testing'], 'boolean', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['boolean'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['boolean'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_char'], call_user_func($GLOBALS['testing'], 'char', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['char'], 65))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_set_difference'], call_user_func($GLOBALS['testing'], 'clojure.set/difference', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3), call_user_func($GLOBALS['set'], clojure.set::difference(\Clojure\Php\hashSet(1, 3, 2), \Clojure\Php\hashSet(2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_set_intersection'], call_user_func($GLOBALS['testing'], 'clojure.set/intersection', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(3, 2), call_user_func($GLOBALS['set'], clojure.set::intersection(\Clojure\Php\hashSet(1, 3, 2), \Clojure\Php\hashSet(4, 3, 2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_set_subset_p'], call_user_func($GLOBALS['testing'], 'clojure.set/subset?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.set::subset?(\Clojure\Php\hashSet(1, 2), \Clojure\Php\hashSet(1, 3, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, clojure.set::subset?(\Clojure\Php\hashSet(1, 4), \Clojure\Php\hashSet(1, 3, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_set_superset_p'], call_user_func($GLOBALS['testing'], 'clojure.set/superset?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.set::superset?(\Clojure\Php\hashSet(1, 3, 2), \Clojure\Php\hashSet(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_set_union'], call_user_func($GLOBALS['testing'], 'clojure.set/union', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3, 2), call_user_func($GLOBALS['set'], clojure.set::union(\Clojure\Php\hashSet(1, 2), \Clojure\Php\hashSet(3, 2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_blank_p'], call_user_func($GLOBALS['testing'], 'clojure.string/blank?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.string::blank?('  '))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, clojure.string::blank?('a'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.string::blank?(null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_capitalize'], call_user_func($GLOBALS['testing'], 'clojure.string/capitalize', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'Hello', clojure.string::capitalize('hello')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_ends_with_p'], call_user_func($GLOBALS['testing'], 'clojure.string/ends-with?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.string::ends-with?('hello', 'lo'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, clojure.string::ends-with?('hello', 'he')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_includes_p'], call_user_func($GLOBALS['testing'], 'clojure.string/includes?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.string::includes?('hello', 'ell'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, clojure.string::includes?('hello', 'xyz')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_join'], call_user_func($GLOBALS['testing'], 'clojure.string/join', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], '1, 2, 3', clojure.string::join(', ', \Clojure\Php\vec(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_lower_case'], call_user_func($GLOBALS['testing'], 'clojure.string/lower-case', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'hello', clojure.string::lower-case('HELLO')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_replace'], call_user_func($GLOBALS['testing'], 'clojure.string/replace', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'heLLo', clojure.string::replace('hello', 'l', 'L')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_reverse'], call_user_func($GLOBALS['testing'], 'clojure.string/reverse', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'olleh', clojure.string::reverse('hello')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_split'], call_user_func($GLOBALS['testing'], 'clojure.string/split', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec('a', 'b', 'c'), clojure.string::split('a,b,c', #",")))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_starts_with_p'], call_user_func($GLOBALS['testing'], 'clojure.string/starts-with?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, clojure.string::starts-with?('hello', 'he'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, clojure.string::starts-with?('hello', 'lo')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_trim'], call_user_func($GLOBALS['testing'], 'clojure.string/trim', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'hello', clojure.string::trim('  hello  ')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_string_upper_case'], call_user_func($GLOBALS['testing'], 'clojure.string/upper-case', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'HELLO', clojure.string::upper-case('hello')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_walk_keywordize_keys'], call_user_func($GLOBALS['testing'], 'clojure.walk/keywordize-keys', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), clojure.walk::keywordize-keys(\Clojure\Php\hashMap('a', 1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_walk_postwalk'], call_user_func($GLOBALS['testing'], 'clojure.walk/postwalk', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(2, \Clojure\Php\vec(3, 4)), clojure.walk::postwalk($GLOBALS['inc'], \Clojure\Php\vec(1, \Clojure\Php\vec(2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_walk_prewalk'], call_user_func($GLOBALS['testing'], 'clojure.walk/prewalk', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(2, \Clojure\Php\vec(3)), clojure.walk::prewalk((function($__L_p1__2433_SHARP_) { if (call_user_func($GLOBALS['number_QMARK_'], $__L_p1__2433_SHARP_)) { return call_user_func($GLOBALS['inc'], $__L_p1__2433_SHARP_);} else { return $__L_p1__2433_SHARP_;}}), \Clojure\Php\vec(1, \Clojure\Php\vec(2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_clojure_walk_stringify_keys'], call_user_func($GLOBALS['testing'], 'clojure.walk/stringify-keys', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap('a', 1), clojure.walk::stringify-keys(\Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_coll_p'], call_user_func($GLOBALS['testing'], 'coll?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['coll_QMARK_'], \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['coll_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['coll_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_comp'], call_user_func($GLOBALS['testing'], 'comp', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func(call_user_func($GLOBALS['comp'], $GLOBALS['inc'], $GLOBALS['inc']), 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], '2', call_user_func(call_user_func($GLOBALS['comp'], $GLOBALS['str'], $GLOBALS['inc']), 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_compare'], call_user_func($GLOBALS['testing'], 'compare', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], -1, call_user_func($GLOBALS['compare'], 1, 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['compare'], 2, 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 0, call_user_func($GLOBALS['compare'], 1, 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_compare_and_set_bang'], call_user_func($GLOBALS['testing'], 'compare-and-set!', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); call_user_func($GLOBALS['compare_and_set_BANG_'], $__L_a, 1, 2);
return call_user_func($GLOBALS['deref'], $__L_a); })))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); call_user_func($GLOBALS['compare_and_set_BANG_'], $__L_a, 0, 2);
return call_user_func($GLOBALS['deref'], $__L_a); }))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_complement'], call_user_func($GLOBALS['testing'], 'complement', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func(call_user_func($GLOBALS['complement'], $GLOBALS['even_QMARK_']), 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func(call_user_func($GLOBALS['complement'], $GLOBALS['even_QMARK_']), 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_concat'], call_user_func($GLOBALS['testing'], 'concat', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['concat'], \Clojure\Php\vec(1, 2), \Clojure\Php\vec(3, 4))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['concat'], \Clojure\Php\vec(1), \Clojure\Php\vec(2), \Clojure\Php\vec(3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_conj'], call_user_func($GLOBALS['testing'], 'conj', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2, 3), call_user_func($GLOBALS['conj'], \Clojure\Php\vec(1, 2), 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 1, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['conj'], \Clojure\Php\plist(1, 2), 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3, 2), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['conj'], \Clojure\Php\hashSet(1, 2), 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['conj'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\vec(\Clojure\Php\Kw::create('b'), 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_cons'], call_user_func($GLOBALS['testing'], 'cons', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(0, 1, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['cons'], 0, \Clojure\Php\vec(1, 2))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(0), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['cons'], 0, null))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_constantly'], call_user_func($GLOBALS['testing'], 'constantly', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func(call_user_func($GLOBALS['constantly'], 5), 1, 2, 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_contains_p'], call_user_func($GLOBALS['testing'], 'contains?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['contains_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('a')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['contains_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('b')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['contains_QMARK_'], \Clojure\Php\vec(1, 2, 3), 0))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['contains_QMARK_'], \Clojure\Php\vec(1, 2, 3), 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_count'], call_user_func($GLOBALS['testing'], 'count', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['count'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 0, call_user_func($GLOBALS['count'], null))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['count'], 'hello'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, call_user_func($GLOBALS['count'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_counted_p'], call_user_func($GLOBALS['testing'], 'counted?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['counted_QMARK_'], \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['counted_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_cycle'], call_user_func($GLOBALS['testing'], 'cycle', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3, 1, 2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take'], 6, call_user_func($GLOBALS['cycle'], \Clojure\Php\vec(1, 2, 3))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_dec'], call_user_func($GLOBALS['testing'], 'dec', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 9, call_user_func($GLOBALS['dec'], 10))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], -1, call_user_func($GLOBALS['dec'], 0)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_dedupe'], call_user_func($GLOBALS['testing'], 'dedupe', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3, 1), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['dedupe'], \Clojure\Php\vec(1, 1, 2, 2, 3, 1, 1)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_deref'], call_user_func($GLOBALS['testing'], 'deref', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['deref'], call_user_func($GLOBALS['atom'], 5))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_disj'], call_user_func($GLOBALS['testing'], 'disj', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['disj'], \Clojure\Php\hashSet(1, 3, 2), 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_dissoc'], call_user_func($GLOBALS['testing'], 'dissoc', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['dissoc'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), \Clojure\Php\Kw::create('a')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(), call_user_func($GLOBALS['dissoc'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), \Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_distinct'], call_user_func($GLOBALS['testing'], 'distinct', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['distinct'], \Clojure\Php\vec(1, 2, 1, 3, 2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_double'], call_user_func($GLOBALS['testing'], 'double', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['double'], 3))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_drop'], call_user_func($GLOBALS['testing'], 'drop', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['drop'], 2, \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_drop_last'], call_user_func($GLOBALS['testing'], 'drop-last', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['drop_last'], 2, \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_drop_while'], call_user_func($GLOBALS['testing'], 'drop-while', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['drop_while'], (function($__L_p1__2434_SHARP_) { return ($__L_p1__2434_SHARP_ < 3);}), \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_empty_p'], call_user_func($GLOBALS['testing'], 'empty?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['empty_QMARK_'], \Clojure\Php\vec()))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['empty_QMARK_'], \Clojure\Php\vec(1)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['empty_QMARK_'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_even_p'], call_user_func($GLOBALS['testing'], 'even?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['even_QMARK_'], 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['even_QMARK_'], 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_every_pred'], call_user_func($GLOBALS['testing'], 'every-pred', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func(call_user_func($GLOBALS['every_pred'], $GLOBALS['even_QMARK_'], $GLOBALS['pos_QMARK_']), 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func(call_user_func($GLOBALS['every_pred'], $GLOBALS['even_QMARK_'], $GLOBALS['pos_QMARK_']), -2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_every_p'], call_user_func($GLOBALS['testing'], 'every?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['every_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(2, 4, 6)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['every_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(2, 3, 4)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['every_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec())))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_false_p'], call_user_func($GLOBALS['testing'], 'false?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['false_QMARK_'], false))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['false_QMARK_'], true))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['false_QMARK_'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_ffirst'], call_user_func($GLOBALS['testing'], 'ffirst', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['ffirst'], \Clojure\Php\vec(\Clojure\Php\vec(1, 2), \Clojure\Php\vec(3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_filter'], call_user_func($GLOBALS['testing'], 'filter', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['filter'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_filterv'], call_user_func($GLOBALS['testing'], 'filterv', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(2, 4), call_user_func($GLOBALS['filterv'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 2, 3, 4))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_find'], call_user_func($GLOBALS['testing'], 'find', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), 1), call_user_func($GLOBALS['find'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), \Clojure\Php\Kw::create('a')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['find'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('b'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_first'], call_user_func($GLOBALS['testing'], 'first', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['first'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['first'], null))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['first'], \Clojure\Php\vec())))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_flatten'], call_user_func($GLOBALS['testing'], 'flatten', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3, 4, 5), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['flatten'], \Clojure\Php\vec(\Clojure\Php\vec(1, 2), \Clojure\Php\vec(3, \Clojure\Php\vec(4, 5)))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_float'], call_user_func($GLOBALS['testing'], 'float', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['float'], 3))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_float_p'], call_user_func($GLOBALS['testing'], 'float?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['float_QMARK_'], 1.5))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['float_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_fn_p'], call_user_func($GLOBALS['testing'], 'fn?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['fn_QMARK_'], $GLOBALS['inc']))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['fn_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_fnext'], call_user_func($GLOBALS['testing'], 'fnext', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, call_user_func($GLOBALS['fnext'], \Clojure\Php\vec(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_fnil'], call_user_func($GLOBALS['testing'], 'fnil', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func(call_user_func($GLOBALS['fnil'], $GLOBALS['inc'], 0), null))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, call_user_func(call_user_func($GLOBALS['fnil'], $GLOBALS['inc'], 0), 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_frequencies'], call_user_func($GLOBALS['testing'], 'frequencies', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 2, \Clojure\Php\Kw::create('b'), 1, \Clojure\Php\Kw::create('c'), 3), call_user_func($GLOBALS['frequencies'], \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('c'), \Clojure\Php\Kw::create('c'), \Clojure\Php\Kw::create('c')))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_gensym'], call_user_func($GLOBALS['testing'], 'gensym', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['symbol_QMARK_'], call_user_func($GLOBALS['gensym'])))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['symbol_QMARK_'], call_user_func($GLOBALS['gensym'], 'foo'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_get'], call_user_func($GLOBALS['testing'], 'get', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['get'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('a')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['get'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('b')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\Kw::create('default'), call_user_func($GLOBALS['get'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('default')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['get'], \Clojure\Php\vec(1, 2, 3), 0)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_get_in'], call_user_func($GLOBALS['testing'], 'get-in', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['get_in'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1)), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['get_in'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1)), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('c'))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\Kw::create('default'), call_user_func($GLOBALS['get_in'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1)), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('c')), \Clojure\Php\Kw::create('default'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_group_by'], call_user_func($GLOBALS['testing'], 'group-by', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(false, \Clojure\Php\vec(1, 3), true, \Clojure\Php\vec(2, 4)), call_user_func($GLOBALS['group_by'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 2, 3, 4))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_hash_map'], call_user_func($GLOBALS['testing'], 'hash-map', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['hash_map'], \Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_hash_set'], call_user_func($GLOBALS['testing'], 'hash-set', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3, 2), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['hash_set'], 1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_identical_p'], call_user_func($GLOBALS['testing'], 'identical?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['identical_QMARK_'], null, null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_identity'], call_user_func($GLOBALS['testing'], 'identity', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['identity'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['identity'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_inc'], call_user_func($GLOBALS['testing'], 'inc', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, call_user_func($GLOBALS['inc'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 0, call_user_func($GLOBALS['inc'], -1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_int'], call_user_func($GLOBALS['testing'], 'int', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['int'], 3.7)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_integer_p'], call_user_func($GLOBALS['testing'], 'integer?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['integer_QMARK_'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['integer_QMARK_'], 1.5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_interleave'], call_user_func($GLOBALS['testing'], 'interleave', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, \Clojure\Php\Kw::create('a'), 2, \Clojure\Php\Kw::create('b')), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['interleave'], \Clojure\Php\vec(1, 2), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_interpose'], call_user_func($GLOBALS['testing'], 'interpose', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, \Clojure\Php\Kw::create('sep'), 2, \Clojure\Php\Kw::create('sep'), 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['interpose'], \Clojure\Php\Kw::create('sep'), \Clojure\Php\vec(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_into'], call_user_func($GLOBALS['testing'], 'into', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2, 3), call_user_func($GLOBALS['into'], \Clojure\Php\vec(), \Clojure\Php\plist(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['into'], \Clojure\Php\hashMap(), \Clojure\Php\vec(\Clojure\Php\vec(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\vec(\Clojure\Php\Kw::create('b'), 2))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3, 2), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['into'], \Clojure\Php\hashSet(), \Clojure\Php\vec(1, 2, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_iterate'], call_user_func($GLOBALS['testing'], 'iterate', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(0, 1, 2, 3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take'], 5, call_user_func($GLOBALS['iterate'], $GLOBALS['inc'], 0)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_juxt'], call_user_func($GLOBALS['testing'], 'juxt', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 3), call_user_func(call_user_func($GLOBALS['juxt'], $GLOBALS['first'], $GLOBALS['last']), \Clojure\Php\vec(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_keep'], call_user_func($GLOBALS['testing'], 'keep', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['keep'], (function($__L_p1__2435_SHARP_) { if (call_user_func($GLOBALS['even_QMARK_'], $__L_p1__2435_SHARP_)) { return $__L_p1__2435_SHARP_;}}), \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_keep_indexed'], call_user_func($GLOBALS['testing'], 'keep-indexed', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('c')), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['keep_indexed'], (function($__L_p1__2436_SHARP_, $__L_p2__2437_SHARP_) { if (call_user_func($GLOBALS['even_QMARK_'], $__L_p1__2436_SHARP_)) { return $__L_p2__2437_SHARP_;}}), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('c'), \Clojure\Php\Kw::create('d'))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_keys'], call_user_func($GLOBALS['testing'], 'keys', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(\Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('a')), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['set'], call_user_func($GLOBALS['keys'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_keyword'], call_user_func($GLOBALS['testing'], 'keyword', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['keyword'], 'foo')), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['keyword'], 'foo', 'bar'))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_keyword_p'], call_user_func($GLOBALS['testing'], 'keyword?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['keyword_QMARK_'], \Clojure\Php\Kw::create('foo')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['keyword_QMARK_'], \Clojure\Php\Sym::create('foo'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_last'], call_user_func($GLOBALS['testing'], 'last', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['last'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['last'], \Clojure\Php\vec())))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_list'], call_user_func($GLOBALS['testing'], 'list', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['list'], 1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], (), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['list']))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_list_p'], call_user_func($GLOBALS['testing'], 'list?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['list_QMARK_'], \Clojure\Php\plist(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['list_QMARK_'], \Clojure\Php\vec(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_long'], call_user_func($GLOBALS['testing'], 'long', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['long'], 3.7)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_map'], call_user_func($GLOBALS['testing'], 'map', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2, 3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['map'], $GLOBALS['inc'], \Clojure\Php\vec(1, 2, 3))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(4, 6), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['map'], $GLOBALS['_PLUS_'], \Clojure\Php\vec(1, 2), \Clojure\Php\vec(3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_map_indexed'], call_user_func($GLOBALS['testing'], 'map-indexed', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(\Clojure\Php\vec(0, \Clojure\Php\Kw::create('a')), \Clojure\Php\vec(1, \Clojure\Php\Kw::create('b')), \Clojure\Php\vec(2, \Clojure\Php\Kw::create('c'))), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['map_indexed'], $GLOBALS['vector'], \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('c'))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_map_p'], call_user_func($GLOBALS['testing'], 'map?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['map_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['map_QMARK_'], \Clojure\Php\vec(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_mapcat'], call_user_func($GLOBALS['testing'], 'mapcat', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 1, 2, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['mapcat'], (function($__L_p1__2438_SHARP_) { return call_user_func($GLOBALS['repeat'], 2, $__L_p1__2438_SHARP_);}), \Clojure\Php\vec(1, 2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_mapv'], call_user_func($GLOBALS['testing'], 'mapv', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(2, 3, 4), call_user_func($GLOBALS['mapv'], $GLOBALS['inc'], \Clojure\Php\vec(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_max'], call_user_func($GLOBALS['testing'], 'max', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['max'], 1, 2, 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['max'], 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_memoize'], call_user_func($GLOBALS['testing'], 'memoize', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(3, 3), (call_user_func(function() { $__L_f = call_user_func($GLOBALS['memoize'], $GLOBALS['_PLUS_']); return \Clojure\Php\vec(call_user_func($__L_f, 1, 2), call_user_func($__L_f, 1, 2)); }))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_merge'], call_user_func($GLOBALS['testing'], 'merge', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['merge'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 2), call_user_func($GLOBALS['merge'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_merge_with'], call_user_func($GLOBALS['testing'], 'merge-with', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 3), call_user_func($GLOBALS['merge_with'], $GLOBALS['_PLUS_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_meta'], call_user_func($GLOBALS['testing'], 'meta', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('foo'), 1), call_user_func($GLOBALS['meta'], call_user_func($GLOBALS['with_meta'], \Clojure\Php\vec(), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('foo'), 1))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['meta'], \Clojure\Php\vec())))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_min'], call_user_func($GLOBALS['testing'], 'min', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['min'], 1, 2, 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['min'], 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_mod'], call_user_func($GLOBALS['testing'], 'mod', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, (10 % 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, (-10 % 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_name'], call_user_func($GLOBALS['testing'], 'name', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'foo', call_user_func($GLOBALS['name'], \Clojure\Php\Kw::create('foo')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'bar', call_user_func($GLOBALS['name'], \Clojure\Php\Sym::create('bar'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_namespace'], call_user_func($GLOBALS['testing'], 'namespace', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'foo', call_user_func($GLOBALS['namespace'], \Clojure\Php\Kw::createNs('foo', 'bar')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['namespace'], \Clojure\Php\Kw::create('bar'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_neg_p'], call_user_func($GLOBALS['testing'], 'neg?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['neg_QMARK_'], -1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['neg_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_next'], call_user_func($GLOBALS['testing'], 'next', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['next'], \Clojure\Php\vec(1, 2, 3))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['next'], \Clojure\Php\vec(1)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['next'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_nfirst'], call_user_func($GLOBALS['testing'], 'nfirst', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['nfirst'], \Clojure\Php\vec(\Clojure\Php\vec(1, 2), \Clojure\Php\vec(3, 4))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_nil_p'], call_user_func($GLOBALS['testing'], 'nil?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['nil_QMARK_'], null))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['nil_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_nnext'], call_user_func($GLOBALS['testing'], 'nnext', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['nnext'], \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_not'], call_user_func($GLOBALS['testing'], 'not', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['not'], true))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['not'], false))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['not'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_not_any_p'], call_user_func($GLOBALS['testing'], 'not-any?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['not_any_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 3, 5)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['not_any_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_not_empty'], call_user_func($GLOBALS['testing'], 'not-empty', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2), call_user_func($GLOBALS['not_empty'], \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['not_empty'], \Clojure\Php\vec())))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_not_every_p'], call_user_func($GLOBALS['testing'], 'not-every?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['not_every_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(2, 3, 4)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['not_every_QMARK_'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(2, 4, 6))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_not_eq'], call_user_func($GLOBALS['testing'], 'not=', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['not_EQ_'], 1, 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['not_EQ_'], 1, 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_nth'], call_user_func($GLOBALS['testing'], 'nth', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['nth'], \Clojure\Php\vec(1, 2, 3), 0))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['nth'], \Clojure\Php\vec(1, 2, 3), 2))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\Kw::create('not-found'), call_user_func($GLOBALS['nth'], \Clojure\Php\vec(1, 2, 3), 5, \Clojure\Php\Kw::create('not-found'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_number_p'], call_user_func($GLOBALS['testing'], 'number?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['number_QMARK_'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['number_QMARK_'], 1.5))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['number_QMARK_'], '1')))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_odd_p'], call_user_func($GLOBALS['testing'], 'odd?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['odd_QMARK_'], 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['odd_QMARK_'], 2)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_or'], call_user_func($GLOBALS['testing'], 'or', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, (false ? false : 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, (null ? null : (false ? false : 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], (false ? false : null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_partial'], call_user_func($GLOBALS['testing'], 'partial', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 8, call_user_func(call_user_func($GLOBALS['partial'], $GLOBALS['_PLUS_'], 5), 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 10, call_user_func(call_user_func($GLOBALS['partial'], $GLOBALS['_PLUS_'], 1, 2), 3, 4)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_partition'], call_user_func($GLOBALS['testing'], 'partition', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(call_user_func(1, 2), call_user_func(3, 4)), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['partition'], 2, \Clojure\Php\vec(1, 2, 3, 4))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(call_user_func(1, 2), call_user_func(2, 3), call_user_func(3, 4)), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['partition'], 2, 1, \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_partition_all'], call_user_func($GLOBALS['testing'], 'partition-all', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(call_user_func(1, 2), call_user_func(3, 4), call_user_func(5)), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['partition_all'], 2, \Clojure\Php\vec(1, 2, 3, 4, 5)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_partition_by'], call_user_func($GLOBALS['testing'], 'partition-by', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(call_user_func(1, 1), call_user_func(2, 2), call_user_func(3)), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['partition_by'], $GLOBALS['odd_QMARK_'], \Clojure\Php\vec(1, 1, 2, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_peek'], call_user_func($GLOBALS['testing'], 'peek', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['peek'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func($GLOBALS['peek'], \Clojure\Php\plist(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_pop'], call_user_func($GLOBALS['testing'], 'pop', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2), call_user_func($GLOBALS['pop'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['pop'], \Clojure\Php\plist(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_pos_p'], call_user_func($GLOBALS['testing'], 'pos?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['pos_QMARK_'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['pos_QMARK_'], -1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['pos_QMARK_'], 0)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_quot'], call_user_func($GLOBALS['testing'], 'quot', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 3, call_user_func($GLOBALS['quot'], 10, 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rand'], call_user_func($GLOBALS['testing'], 'rand', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['number_QMARK_'], call_user_func($GLOBALS['rand'])))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (call_user_func($GLOBALS['rand']) < 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rand_int'], call_user_func($GLOBALS['testing'], 'rand-int', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['integer_QMARK_'], call_user_func($GLOBALS['rand_int'], 10)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, (call_user_func($GLOBALS['rand_int'], 10) < 10)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rand_nth'], call_user_func($GLOBALS['testing'], 'rand-nth', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['contains_QMARK_'], \Clojure\Php\hashSet(1, 3, 2), call_user_func($GLOBALS['rand_nth'], \Clojure\Php\vec(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_range'], call_user_func($GLOBALS['testing'], 'range', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(0, 1, 2, 3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 5)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 1, 5)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(0, 2, 4, 6, 8), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 0, 10, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reduce'], call_user_func($GLOBALS['testing'], 'reduce', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, call_user_func($GLOBALS['reduce'], $GLOBALS['_PLUS_'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, call_user_func($GLOBALS['reduce'], $GLOBALS['_PLUS_'], 0, \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 0, call_user_func($GLOBALS['reduce'], $GLOBALS['_PLUS_'], \Clojure\Php\vec())))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reduce_kv'], call_user_func($GLOBALS['testing'], 'reduce-kv', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(1, \Clojure\Php\Kw::create('a'), 2, \Clojure\Php\Kw::create('b')), call_user_func($GLOBALS['reduce_kv'], (function($__L_m, $__L_k, $__L_v) { return call_user_func($GLOBALS['assoc'], $__L_m, $__L_v, $__L_k);}), \Clojure\Php\hashMap(), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reduced'], call_user_func($GLOBALS['testing'], 'reduced', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, call_user_func($GLOBALS['reduce'], (function($__L_acc, $__L_x) { if (($__L_acc > 5)) { return call_user_func($GLOBALS['reduced'], $__L_acc);} else { return ($__L_acc + $__L_x);}}), \Clojure\Php\vec(1, 2, 3, 4, 5))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reduced_p'], call_user_func($GLOBALS['testing'], 'reduced?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['reduced_QMARK_'], call_user_func($GLOBALS['reduced'], 1)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['reduced_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reductions'], call_user_func($GLOBALS['testing'], 'reductions', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 3, 6), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['reductions'], $GLOBALS['_PLUS_'], \Clojure\Php\vec(1, 2, 3))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(0, 1, 3, 6), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['reductions'], $GLOBALS['_PLUS_'], 0, \Clojure\Php\vec(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rem'], call_user_func($GLOBALS['testing'], 'rem', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, (10 % 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], -1, (-10 % 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_remove'], call_user_func($GLOBALS['testing'], 'remove', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['remove'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rename_keys'], call_user_func($GLOBALS['testing'], 'rename-keys', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1), call_user_func($GLOBALS['rename_keys'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b')))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_repeat'], call_user_func($GLOBALS['testing'], 'repeat', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 1, 1), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take'], 3, call_user_func($GLOBALS['repeat'], 1))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(\Clojure\Php\Kw::create('x'), \Clojure\Php\Kw::create('x'), \Clojure\Php\Kw::create('x')), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['repeat'], 3, \Clojure\Php\Kw::create('x')))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_repeatedly'], call_user_func($GLOBALS['testing'], 'repeatedly', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['count'], call_user_func($GLOBALS['take'], 5, call_user_func($GLOBALS['repeatedly'], (function() { return call_user_func($GLOBALS['rand_int'], 10);}))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reset_bang'], call_user_func($GLOBALS['testing'], 'reset!', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); call_user_func($GLOBALS['reset_BANG_'], $__L_a, 5);
return call_user_func($GLOBALS['deref'], $__L_a); }))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rest'], call_user_func($GLOBALS['testing'], 'rest', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['rest'], \Clojure\Php\vec(1, 2, 3))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], (), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['rest'], \Clojure\Php\vec(1))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], (), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['rest'], null))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_reverse'], call_user_func($GLOBALS['testing'], 'reverse', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 2, 1), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['reverse'], \Clojure\Php\vec(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_rseq'], call_user_func($GLOBALS['testing'], 'rseq', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 2, 1), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['rseq'], \Clojure\Php\vec(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_second'], call_user_func($GLOBALS['testing'], 'second', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, call_user_func($GLOBALS['second'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['second'], \Clojure\Php\vec(1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_select_keys'], call_user_func($GLOBALS['testing'], 'select-keys', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('c'), 3), call_user_func($GLOBALS['select_keys'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2, \Clojure\Php\Kw::create('c'), 3), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('c')))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_seq'], call_user_func($GLOBALS['testing'], 'seq', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['seq'], \Clojure\Php\vec(1, 2, 3))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['seq'], \Clojure\Php\vec()))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['seq'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_seq_p'], call_user_func($GLOBALS['testing'], 'seq?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['seq_QMARK_'], call_user_func($GLOBALS['seq'], \Clojure\Php\vec(1, 2))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['seq_QMARK_'], \Clojure\Php\vec(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_sequential_p'], call_user_func($GLOBALS['testing'], 'sequential?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['sequential_QMARK_'], \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['sequential_QMARK_'], \Clojure\Php\plist(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['sequential_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_set'], call_user_func($GLOBALS['testing'], 'set', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 3, 2), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['set'], \Clojure\Php\vec(1, 2, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_set_p'], call_user_func($GLOBALS['testing'], 'set?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['set_QMARK_'], \Clojure\Php\hashSet(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['set_QMARK_'], \Clojure\Php\vec(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_shuffle'], call_user_func($GLOBALS['testing'], 'shuffle', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['count'], call_user_func($GLOBALS['shuffle'], \Clojure\Php\vec(1, 2, 3, 4, 5)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_some'], call_user_func($GLOBALS['testing'], 'some', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['some'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func($GLOBALS['some'], $GLOBALS['even_QMARK_'], \Clojure\Php\vec(1, 3, 5)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, call_user_func($GLOBALS['some'], \Clojure\Php\hashSet(2), \Clojure\Php\vec(1, 2, 3))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_some_fn'], call_user_func($GLOBALS['testing'], 'some-fn', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func(call_user_func($GLOBALS['some_fn'], $GLOBALS['even_QMARK_'], (function($__L_p1__2439_SHARP_) { return call_user_func($GLOBALS['_EQ_'], $__L_p1__2439_SHARP_, 3);})), 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(call_user_func($GLOBALS['some_fn'], $GLOBALS['even_QMARK_'], (function($__L_p1__2440_SHARP_) { return call_user_func($GLOBALS['_EQ_'], $__L_p1__2440_SHARP_, 3);})), 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_some_p'], call_user_func($GLOBALS['testing'], 'some?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['some_QMARK_'], 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['some_QMARK_'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_sort'], call_user_func($GLOBALS['testing'], 'sort', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2, 3), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['sort'], \Clojure\Php\vec(3, 1, 2))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 2, 1), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['sort'], $GLOBALS['_GT_'], \Clojure\Php\vec(1, 2, 3)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_sort_by'], call_user_func($GLOBALS['testing'], 'sort-by', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func('b', 'cc', 'aaa'), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['sort_by'], $GLOBALS['count'], \Clojure\Php\vec('aaa', 'b', 'cc')))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_split_at'], call_user_func($GLOBALS['testing'], 'split-at', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(\Clojure\Php\plist(1, 2), \Clojure\Php\plist(3, 4)), call_user_func($GLOBALS['split_at'], 2, \Clojure\Php\vec(1, 2, 3, 4))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_split_with'], call_user_func($GLOBALS['testing'], 'split-with', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(\Clojure\Php\plist(1, 2), \Clojure\Php\plist(3, 4)), call_user_func($GLOBALS['split_with'], (function($__L_p1__2441_SHARP_) { return ($__L_p1__2441_SHARP_ < 3);}), \Clojure\Php\vec(1, 2, 3, 4))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_str'], call_user_func($GLOBALS['testing'], 'str', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], '', call_user_func($GLOBALS['str']))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'ab', call_user_func($GLOBALS['str'], 'a', 'b'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], '123', call_user_func($GLOBALS['str'], 1, 2, 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], '', call_user_func($GLOBALS['str'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_string_p'], call_user_func($GLOBALS['testing'], 'string?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['string_QMARK_'], 'hello'))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['string_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_subs'], call_user_func($GLOBALS['testing'], 'subs', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'ello', call_user_func($GLOBALS['subs'], 'hello', 1))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 'el', call_user_func($GLOBALS['subs'], 'hello', 1, 3)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_subvec'], call_user_func($GLOBALS['testing'], 'subvec', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(2, 3, 4), call_user_func($GLOBALS['subvec'], \Clojure\Php\vec(1, 2, 3, 4, 5), 1, 4))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(2, 3), call_user_func($GLOBALS['subvec'], \Clojure\Php\vec(1, 2, 3), 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_swap_bang'], call_user_func($GLOBALS['testing'], 'swap!', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 2, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); call_user_func($GLOBALS['swap_BANG_'], $__L_a, $GLOBALS['inc']);
return call_user_func($GLOBALS['deref'], $__L_a); })))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 6, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); call_user_func($GLOBALS['swap_BANG_'], $__L_a, $GLOBALS['_PLUS_'], 5);
return call_user_func($GLOBALS['deref'], $__L_a); }))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_symbol'], call_user_func($GLOBALS['testing'], 'symbol', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['symbol'], 'foo')), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['symbol'], 'ns', 'foo'))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_symbol_p'], call_user_func($GLOBALS['testing'], 'symbol?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['symbol_QMARK_'], \Clojure\Php\Sym::create('foo')))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['symbol_QMARK_'], \Clojure\Php\Kw::create('foo'))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_take'], call_user_func($GLOBALS['testing'], 'take', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take'], 2, \Clojure\Php\vec(1, 2, 3, 4))))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take'], 10, \Clojure\Php\vec(1, 2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_take_last'], call_user_func($GLOBALS['testing'], 'take-last', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(3, 4), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take_last'], 2, \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_take_nth'], call_user_func($GLOBALS['testing'], 'take-nth', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 3, 5), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take_nth'], 2, \Clojure\Php\vec(1, 2, 3, 4, 5)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_take_while'], call_user_func($GLOBALS['testing'], 'take-while', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], call_user_func(1, 2), call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['take_while'], (function($__L_p1__2442_SHARP_) { return ($__L_p1__2442_SHARP_ < 3);}), \Clojure\Php\vec(1, 2, 3, 4)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_trampoline'], call_user_func($GLOBALS['testing'], 'trampoline', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\Kw::create('done'), call_user_func($GLOBALS['trampoline'], (function($__L_n) { if (call_user_func($GLOBALS['zero_QMARK_'], $__L_n)) { return \Clojure\Php\Kw::create('done');} else { return (function() use (&$__L_n) { return call_user_func($GLOBALS['trampoline'], call_user_func($GLOBALS['dec'], $__L_n));});}}), 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_true_p'], call_user_func($GLOBALS['testing'], 'true?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['true_QMARK_'], true))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['true_QMARK_'], false))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['true_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_type'], call_user_func($GLOBALS['testing'], 'type'));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_unreduced'], call_user_func($GLOBALS['testing'], 'unreduced', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['unreduced'], call_user_func($GLOBALS['reduced'], 5)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 5, call_user_func($GLOBALS['unreduced'], 5)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_update'], call_user_func($GLOBALS['testing'], 'update', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 2), call_user_func($GLOBALS['update'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('a'), $GLOBALS['inc']))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 11), call_user_func($GLOBALS['update'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1), \Clojure\Php\Kw::create('a'), $GLOBALS['_PLUS_'], 10)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_update_in'], call_user_func($GLOBALS['testing'], 'update-in', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 2)), call_user_func($GLOBALS['update_in'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 1)), \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b')), $GLOBALS['inc'])))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_vals'], call_user_func($GLOBALS['testing'], 'vals', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashSet(1, 2), call_user_func($GLOBALS['set'], call_user_func($GLOBALS['set'], call_user_func($GLOBALS['vals'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_vary_meta'], call_user_func($GLOBALS['testing'], 'vary-meta', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func(\Clojure\Php\Kw::create('a'), call_user_func($GLOBALS['meta'], call_user_func($GLOBALS['vary_meta'], call_user_func($GLOBALS['with_meta'], \Clojure\Php\vec(), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1)), $GLOBALS['assoc'], \Clojure\Php\Kw::create('b'), 2)))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_vec'], call_user_func($GLOBALS['testing'], 'vec', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2, 3), call_user_func($GLOBALS['vec'], \Clojure\Php\plist(1, 2, 3)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(), call_user_func($GLOBALS['vec'], null)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_vector'], call_user_func($GLOBALS['testing'], 'vector', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(1, 2, 3), call_user_func($GLOBALS['vector'], 1, 2, 3))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\vec(), call_user_func($GLOBALS['vector'])))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_vector_p'], call_user_func($GLOBALS['testing'], 'vector?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['vector_QMARK_'], \Clojure\Php\vec(1, 2)))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['vector_QMARK_'], \Clojure\Php\plist(1, 2))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_with_meta'], call_user_func($GLOBALS['testing'], 'with-meta', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], 1, call_user_func(\Clojure\Php\Kw::create('foo'), call_user_func($GLOBALS['meta'], call_user_func($GLOBALS['with_meta'], \Clojure\Php\vec(), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('foo'), 1))))))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_zero_p'], call_user_func($GLOBALS['testing'], 'zero?', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], true, call_user_func($GLOBALS['zero_QMARK_'], 0))), call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], false, call_user_func($GLOBALS['zero_QMARK_'], 1)))));
call_user_func($GLOBALS['deftest'], $GLOBALS['test_zipmap'], call_user_func($GLOBALS['testing'], 'zipmap', call_user_func($GLOBALS['is'], call_user_func($GLOBALS['_EQ_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2), call_user_func($GLOBALS['zipmap'], \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b')), \Clojure\Php\vec(1, 2))))));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__star'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (2 * 3 * 4 * 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__PLUS_'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (1 + 2 + 3 + 4 + 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (100 - 1 - 2 - 3);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (100 / 2 / 2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__lt'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (1 < 2 < 3);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__lt_eq'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (1 <= 2 <= 2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__eq'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['_EQ_'], 1, 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__gt'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (3 > 2 > 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench__gt_eq'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (3 >= 2 >= 2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_abs'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['abs'], -42);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_apply'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['apply'], $GLOBALS['_PLUS_'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_assoc'], 100, (call_user_func(function() { $__L_m = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['assoc'], $__L_m, \Clojure\Php\Kw::create('b'), 2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_atom'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['atom'], 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_bit_and'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (255 & 15);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_bit_not'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['bit_not'], 0);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_bit_or'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (240 | 15);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_bit_shift_left'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (1 << 10);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_bit_shift_right'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (1024 >> 4);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_bit_xor'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (255 ^ 15);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_coll_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['coll_QMARK_'], \Clojure\Php\vec(1, 2));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_comp'], 100, (call_user_func(function() { $__L_f = call_user_func($GLOBALS['comp'], $GLOBALS['inc'], $GLOBALS['inc']); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($__L_f, 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_compare'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['compare'], 1, 2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_concat'], 100, (call_user_func(function() { $__L_a = \Clojure\Php\vec(1, 2); $__L_b = \Clojure\Php\vec(3, 4); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['concat'], $__L_a, $__L_b));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_conj'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['conj'], $__L_v, 4);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_cons'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['cons'], 0, $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_constantly'], 100, (call_user_func(function() { $__L_f = call_user_func($GLOBALS['constantly'], 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($__L_f, 1, 2, 3);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_contains_p'], 100, (call_user_func(function() { $__L_m = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2, \Clojure\Php\Kw::create('c'), 3); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['contains_QMARK_'], $__L_m, \Clojure\Php\Kw::create('b'));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_count'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['count'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_dec'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['dec'], 100);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_deref'], 100, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['deref'], $__L_a);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_dissoc'], 100, (call_user_func(function() { $__L_m = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['dissoc'], $__L_m, \Clojure\Php\Kw::create('a'));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_distinct'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 1, 3, 2, 4, 1); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['distinct'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_drop'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 1000)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['drop'], 100, $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_drop_while'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['drop_while'], (function($__L_p1__2443_SHARP_) use (&$__L_v, &$__L__) { return ($__L_p1__2443_SHARP_ < 50);}), $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_empty_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['empty_QMARK_'], \Clojure\Php\vec());
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_even_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['even_QMARK_'], 4);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_filter'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['filter'], $GLOBALS['even_QMARK_'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_filterv'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['filterv'], $GLOBALS['even_QMARK_'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_first'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['first'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_flatten'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(\Clojure\Php\vec(1, \Clojure\Php\vec(2, 3)), \Clojure\Php\vec(4, \Clojure\Php\vec(5, \Clojure\Php\vec(6)))); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['flatten'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_fn_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['fn_QMARK_'], $GLOBALS['inc']);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_frequencies'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('c'), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('a')); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['frequencies'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_get'], 100, (call_user_func(function() { $__L_m = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2, \Clojure\Php\Kw::create('c'), 3); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['get'], $__L_m, \Clojure\Php\Kw::create('b'));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_get_in'], 100, (call_user_func(function() { $__L_m = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), \Clojure\Php\hashMap(\Clojure\Php\Kw::create('c'), 1))); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['get_in'], $__L_m, \Clojure\Php\vec(\Clojure\Php\Kw::create('a'), \Clojure\Php\Kw::create('b'), \Clojure\Php\Kw::create('c')));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_group_by'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['group_by'], $GLOBALS['even_QMARK_'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_hash_map'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['hash_map'], \Clojure\Php\Kw::create('a'), 1, \Clojure\Php\Kw::create('b'), 2, \Clojure\Php\Kw::create('c'), 3, \Clojure\Php\Kw::create('d'), 4, \Clojure\Php\Kw::create('e'), 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_hash_set'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['hash_set'], 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_identity'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['identity'], 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_inc'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['inc'], 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_into'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['into'], \Clojure\Php\vec(), call_user_func($GLOBALS['range'], 100));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_iterate'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['take'], 100, call_user_func($GLOBALS['iterate'], $GLOBALS['inc'], 0)));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_juxt'], 100, (call_user_func(function() { $__L_f = call_user_func($GLOBALS['juxt'], $GLOBALS['first'], $GLOBALS['last']); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($__L_f, \Clojure\Php\vec(1, 2, 3));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_keys'], 100, (call_user_func(function() { $__L_m = call_user_func($GLOBALS['zipmap'], call_user_func($GLOBALS['range'], 100), call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['keys'], $__L_m));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_keyword'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['keyword'], 'foo');
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_keyword_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['keyword_QMARK_'], \Clojure\Php\Kw::create('foo'));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_last'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['last'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_list'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['list'], 1, 2, 3, 4, 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_list_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['list_QMARK_'], \Clojure\Php\plist(1, 2));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_map'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['map'], $GLOBALS['inc'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_map_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['map_QMARK_'], \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_mapcat'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(\Clojure\Php\vec(1, 2), \Clojure\Php\vec(3, 4), \Clojure\Php\vec(5, 6)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['mapcat'], $GLOBALS['identity'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_mapv'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['mapv'], $GLOBALS['inc'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_max'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['max'], 1, 2, 3, 4, 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_merge'], 100, (call_user_func(function() { $__L_m1 = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1); $__L_m2 = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('b'), 2); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['merge'], $__L_m1, $__L_m2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_min'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['min'], 5, 4, 3, 2, 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_mod'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (100 % 7);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_name'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['name'], \Clojure\Php\Kw::create('foo'));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_neg_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['neg_QMARK_'], -1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_next'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['next'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_nil_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['nil_QMARK_'], null);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_nth'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['nth'], $__L_v, 50);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_number_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['number_QMARK_'], 42);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_odd_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['odd_QMARK_'], 3);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_partial'], 100, (call_user_func(function() { $__L_f = call_user_func($GLOBALS['partial'], $GLOBALS['_PLUS_'], 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($__L_f, 3);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_partition'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['partition'], 10, $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_peek'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['peek'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_pop'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['pop'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_pos_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['pos_QMARK_'], 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_quot'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['quot'], 100, 7);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_range'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['range'], 100));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_reduce'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['reduce'], $GLOBALS['_PLUS_'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_reduce_kv'], 100, (call_user_func(function() { $__L_m = call_user_func($GLOBALS['zipmap'], call_user_func($GLOBALS['range'], 50), call_user_func($GLOBALS['range'], 50)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['reduce_kv'], (function($__L_a, $__L_k, $__L_v) use (&$__L__, &$__L_m) { return ($__L_a + $__L_v);}), 0, $__L_m);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_rem'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { (100 % 7);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_remove'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['remove'], $GLOBALS['even_QMARK_'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_repeat'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['take'], 100, call_user_func($GLOBALS['repeat'], 1)));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_reset_bang'], 100, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 1); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['reset_BANG_'], $__L_a, 2);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_rest'], 100, (call_user_func(function() { $__L_v = \Clojure\Php\vec(1, 2, 3, 4, 5); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['rest'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_reverse'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['reverse'], $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_seq_p'], 100, (call_user_func(function() { $__L_s = call_user_func($GLOBALS['seq'], \Clojure\Php\vec(1, 2)); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['seq_QMARK_'], $__L_s);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_set_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['set_QMARK_'], \Clojure\Php\hashSet(1, 2));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_some_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['some_QMARK_'], 1);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_sort'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['shuffle'], call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100))); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['sort'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_sort_by'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['shuffle'], call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100))); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['sort_by'], $GLOBALS['identity'], $__L_v);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_str'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['str'], 'a', 'b', 'c');
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_string_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['string_QMARK_'], 'hello');
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_subs'], 100, (call_user_func(function() { $__L_s = 'hello world'; $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['subs'], $__L_s, 0, 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_swap_bang'], 100, (call_user_func(function() { $__L_a = call_user_func($GLOBALS['atom'], 0); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['swap_BANG_'], $__L_a, $GLOBALS['inc']);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_symbol'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['symbol'], 'foo');
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_symbol_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['symbol_QMARK_'], \Clojure\Php\Sym::create('foo'));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_take'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 1000)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['take'], 100, $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_take_while'], 100, (call_user_func(function() { $__L_v = call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['take_while'], (function($__L_p1__2444_SHARP_) use (&$__L_v, &$__L__) { return ($__L_p1__2444_SHARP_ < 50);}), $__L_v));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_update'], 100, (call_user_func(function() { $__L_m = \Clojure\Php\hashMap(\Clojure\Php\Kw::create('a'), 1); $__L__ = 0;
 while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['update'], $__L_m, \Clojure\Php\Kw::create('a'), $GLOBALS['inc']);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_vals'], 100, (call_user_func(function() { $__L_m = call_user_func($GLOBALS['zipmap'], call_user_func($GLOBALS['range'], 100), call_user_func($GLOBALS['range'], 100)); $__L__ = 0;
 while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['doall'], call_user_func($GLOBALS['vals'], $__L_m));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;} break; }
 })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_vec'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['vec'], call_user_func($GLOBALS['range'], 100));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_vector'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['vector'], 1, 2, 3, 4, 5);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_vector_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['vector_QMARK_'], \Clojure\Php\vec(1, 2, 3));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_zero_p'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 1000)) { call_user_func($GLOBALS['zero_QMARK_'], 0);
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
call_user_func($GLOBALS['defbench'], $GLOBALS['bench_zipmap'], 100, (call_user_func(function() { $__L__ = 0;  while(true) { if (($__L__ < 100)) { call_user_func($GLOBALS['zipmap'], call_user_func($GLOBALS['range'], 50), call_user_func($GLOBALS['range'], 50));
$__L__ = call_user_func($GLOBALS['inc'], $__L__); continue;}
 break; } return $__L__; })));
$GLOBALS['run_parity_suite'] = (function() { call_user_func($GLOBALS['run_tests']);
call_user_func($GLOBALS['run_benchmarks']);
return call_user_func($GLOBALS['print_parity_report']);});
$run_parity_suite = &$GLOBALS['run_parity_suite'];
call_user_func($GLOBALS['run_parity_suite']);
