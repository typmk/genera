<?php

declare(strict_types=1);

/**
 * Clojure PHP Runtime Bootstrap
 *
 * Loads all runtime files and registers the autoloader.
 */

// Autoloader for Clojure\Php classes
spl_autoload_register(function (string $class): void {
    $prefix = 'Clojure\\Php\\';
    $baseDir = __DIR__ . '/';

    $len = strlen($prefix);
    if (strncmp($prefix, $class, $len) !== 0) {
        return;
    }

    $relativeClass = substr($class, $len);
    $file = $baseDir . str_replace('\\', '/', $relativeClass) . '.php';

    if (file_exists($file)) {
        require $file;
    }
});

// Load core files in dependency order
// Classes first (autoloader handles these, but explicit for clarity)
require_once __DIR__ . '/Kw.php';        // Keywords
require_once __DIR__ . '/Sym.php';       // Symbols
require_once __DIR__ . '/Seq.php';       // ISeq, LazySeq, Cons, ArraySeq, EmptySeq
require_once __DIR__ . '/Vec.php';       // PersistentVector
require_once __DIR__ . '/Map.php';       // PersistentHashMap
require_once __DIR__ . '/Set.php';       // PersistentHashSet
require_once __DIR__ . '/PList.php';     // PersistentList
require_once __DIR__ . '/Queue.php';     // PersistentQueue
require_once __DIR__ . '/SortedMap.php'; // SortedMap (TreeMap)
require_once __DIR__ . '/SortedSet.php'; // SortedSet (TreeSet)
require_once __DIR__ . '/Atom.php';      // Atom + Reduced + Agent + Future + Promise + Delay

// Function files with core operations
require_once __DIR__ . '/RT.php';        // seq, first, rest, conj, etc.
require_once __DIR__ . '/Core.php';      // map, filter, reduce, etc.
require_once __DIR__ . '/Printer.php';   // prStr, println, etc.
require_once __DIR__ . '/Transducers.php'; // transduce, eduction, mapping, filtering, etc.

// Infrastructure
require_once __DIR__ . '/Meta.php';      // IMeta, meta, withMeta
require_once __DIR__ . '/Var.php';       // Var + dynamic binding
require_once __DIR__ . '/Ns.php';        // Namespace registry
require_once __DIR__ . '/Protocol.php';  // Protocol dispatch
require_once __DIR__ . '/Multi.php';     // Multimethod dispatch
require_once __DIR__ . '/Ex.php';        // ExceptionInfo

// Concurrency (Swoole-enhanced)
require_once __DIR__ . '/Ref.php';       // STM refs + dosync + pmap
require_once __DIR__ . '/Async.php';     // core.async channels + go blocks

// Reader (optional - for REPL / dynamic code)
require_once __DIR__ . '/Reader.php';    // Lisp reader
require_once __DIR__ . '/Repl.php';      // REPL

// Globals bridge - maps Clojure function names to $GLOBALS for compiled code
require_once __DIR__ . '/globals.php';

// Register exception handler with source map support
\Clojure\Php\ExceptionHandler::register();
