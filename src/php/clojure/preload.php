<?php

declare(strict_types=1);

/**
 * Opcache Preload Script for ClojurePHP Runtime
 *
 * Configure in php.ini:
 *   opcache.preload=/path/to/ClojurePHP/src/clojure/php/runtime/preload.php
 *   opcache.preload_user=www-data
 *
 * Benefits:
 * - Classes loaded once at server start, shared across all requests
 * - No file stat() calls per request
 * - Reduced memory usage (shared memory)
 * - Faster autoloading (classes already in memory)
 *
 * Note: Only works with PHP-FPM, not CLI.
 */

// Core interfaces and traits
require_once __DIR__ . '/Seq.php';       // ISeq, Seqable, Cons, LazySeq, etc.

// Data structures
require_once __DIR__ . '/Kw.php';        // Keywords
require_once __DIR__ . '/Sym.php';       // Symbols
require_once __DIR__ . '/Vec.php';       // Persistent Vector
require_once __DIR__ . '/Map.php';       // Persistent HashMap
require_once __DIR__ . '/Set.php';       // Persistent HashSet
require_once __DIR__ . '/PList.php';     // Persistent List
require_once __DIR__ . '/Queue.php';     // Persistent Queue
require_once __DIR__ . '/SortedMap.php'; // Sorted Map (TreeMap)
require_once __DIR__ . '/SortedSet.php'; // Sorted Set (TreeSet)
require_once __DIR__ . '/Atom.php';      // Atoms (mutable references)
require_once __DIR__ . '/Var.php';       // Vars
require_once __DIR__ . '/Multi.php';     // Multimethods
require_once __DIR__ . '/Protocol.php';  // Protocols
require_once __DIR__ . '/Meta.php';      // Metadata support
require_once __DIR__ . '/Ex.php';        // Exception info
require_once __DIR__ . '/Ns.php';        // Namespaces

// Core functions
require_once __DIR__ . '/Core.php';      // All clojure.core functions
require_once __DIR__ . '/Printer.php';   // pr-str, print, etc.
require_once __DIR__ . '/Reader.php';    // EDN reader
require_once __DIR__ . '/RT.php';        // Runtime utilities
require_once __DIR__ . '/Transducers.php'; // Transducer support

// Concurrency (Swoole-enhanced)
require_once __DIR__ . '/Ref.php';       // STM refs
require_once __DIR__ . '/Async.php';     // core.async channels

echo "ClojurePHP runtime preloaded.\n";
