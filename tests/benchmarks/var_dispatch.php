<?php
/**
 * Benchmark: Variable dispatch methods for REPL mode
 *
 * Tests different approaches for storing and calling redefinable functions:
 * 1. Static class properties (current approach)
 * 2. $GLOBALS array
 * 3. Class constants (not redefinable - baseline)
 * 4. Variable variables
 * 5. ArrayObject
 * 6. Registry pattern (singleton)
 * 7. PHP functions (not redefinable - baseline)
 */

const ITERATIONS = 1000000;
const WARMUP = 10000;

// Test functions
$testFn = function($x) { return $x + 1; };

// 1. Static class properties
class StaticProps {
    public static $inc;
    public static $add;
}
StaticProps::$inc = $testFn;
StaticProps::$add = function($a, $b) { return $a + $b; };

// 2. $GLOBALS
$GLOBALS['__cljp_inc'] = $testFn;
$GLOBALS['__cljp_add'] = function($a, $b) { return $a + $b; };

// 3. Class with __callStatic (dynamic dispatch)
class DynamicStatic {
    private static array $fns = [];

    public static function register(string $name, callable $fn): void {
        self::$fns[$name] = $fn;
    }

    public static function __callStatic(string $name, array $args) {
        return (self::$fns[$name])(...$args);
    }

    public static function call(string $name, ...$args) {
        return (self::$fns[$name])(...$args);
    }
}
DynamicStatic::register('inc', $testFn);
DynamicStatic::register('add', function($a, $b) { return $a + $b; });

// 4. ArrayObject registry
$registry = new ArrayObject();
$registry['inc'] = $testFn;
$registry['add'] = function($a, $b) { return $a + $b; };

// 5. Plain array (fastest possible lookup)
$fnArray = [
    'inc' => $testFn,
    'add' => function($a, $b) { return $a + $b; }
];

// 6. call_user_func with string (current deftype approach before fix)
function cljp_inc($x) { return $x + 1; }
function cljp_add($a, $b) { return $a + $b; }

// 7. Namespaced static (like \Clojure\Core::$inc)
class Ns_Clojure_Core {
    public static $inc;
    public static $add;
}
Ns_Clojure_Core::$inc = $testFn;
Ns_Clojure_Core::$add = function($a, $b) { return $a + $b; };

// 8. Direct closure variable
$directInc = $testFn;
$directAdd = function($a, $b) { return $a + $b; };

// Benchmark function
function benchmark(string $name, callable $test): float {
    global $testFn;

    // Warmup
    for ($i = 0; $i < WARMUP; $i++) {
        $test(42);
    }

    // Actual benchmark
    $start = hrtime(true);
    for ($i = 0; $i < ITERATIONS; $i++) {
        $test(42);
    }
    $end = hrtime(true);

    $ns = $end - $start;
    $ms = $ns / 1_000_000;
    $ops_per_sec = ITERATIONS / ($ns / 1_000_000_000);

    printf("%-40s %8.2f ms  %12.0f ops/sec\n", $name, $ms, $ops_per_sec);
    return $ms;
}

echo "=== REPL Mode Variable Dispatch Benchmark ===\n";
echo "Iterations: " . number_format(ITERATIONS) . "\n\n";

echo "--- Single argument call (inc) ---\n";

// Baseline: direct PHP function
benchmark("PHP function (baseline)", function($x) {
    return cljp_inc($x);
});

// Static property with parens
benchmark("Static property: (Class::\$fn)(\$x)", function($x) {
    return (StaticProps::$inc)($x);
});

// Static property with call_user_func
benchmark("Static property + call_user_func", function($x) {
    return call_user_func(StaticProps::$inc, $x);
});

// Namespaced static property
benchmark("Namespaced static: (\\Ns\\C::\$fn)(\$x)", function($x) {
    return (Ns_Clojure_Core::$inc)($x);
});

// $GLOBALS
benchmark("\$GLOBALS['fn'](\$x)", function($x) {
    return ($GLOBALS['__cljp_inc'])($x);
});

// $GLOBALS with call_user_func
benchmark("\$GLOBALS + call_user_func", function($x) {
    return call_user_func($GLOBALS['__cljp_inc'], $x);
});

// Array lookup
benchmark("Array lookup: \$fns['fn'](\$x)", function($x) {
    global $fnArray;
    return ($fnArray['inc'])($x);
});

// ArrayObject
benchmark("ArrayObject: \$reg['fn'](\$x)", function($x) {
    global $registry;
    return ($registry['inc'])($x);
});

// DynamicStatic::__callStatic
benchmark("__callStatic magic method", function($x) {
    return DynamicStatic::inc($x);
});

// DynamicStatic::call
benchmark("DynamicStatic::call('fn', \$x)", function($x) {
    return DynamicStatic::call('inc', $x);
});

// Direct closure variable
benchmark("Direct closure: \$fn(\$x)", function($x) {
    global $directInc;
    return $directInc($x);
});

// call_user_func with string
benchmark("call_user_func('fn', \$x)", function($x) {
    return call_user_func('cljp_inc', $x);
});

echo "\n--- Two argument call (add) ---\n";

// Baseline: direct PHP function
benchmark("PHP function (baseline)", function($x) {
    return cljp_add($x, $x);
});

// Static property
benchmark("Static property: (Class::\$fn)(\$a,\$b)", function($x) {
    return (StaticProps::$add)($x, $x);
});

// Namespaced static
benchmark("Namespaced static", function($x) {
    return (Ns_Clojure_Core::$add)($x, $x);
});

// $GLOBALS
benchmark("\$GLOBALS", function($x) {
    return ($GLOBALS['__cljp_add'])($x, $x);
});

// Array
benchmark("Array lookup", function($x) {
    global $fnArray;
    return ($fnArray['add'])($x, $x);
});

// Direct closure
benchmark("Direct closure", function($x) {
    global $directAdd;
    return $directAdd($x, $x);
});

echo "\n--- Redefinition test (redefine 1000 times, call after each) ---\n";

$redefStart = hrtime(true);
for ($i = 0; $i < 1000; $i++) {
    StaticProps::$inc = function($x) use ($i) { return $x + $i; };
    $result = (StaticProps::$inc)(42);
}
$redefEnd = hrtime(true);
printf("%-40s %8.2f ms\n", "Static property redefinition", ($redefEnd - $redefStart) / 1_000_000);

$redefStart = hrtime(true);
for ($i = 0; $i < 1000; $i++) {
    $GLOBALS['__cljp_inc'] = function($x) use ($i) { return $x + $i; };
    $result = ($GLOBALS['__cljp_inc'])(42);
}
$redefEnd = hrtime(true);
printf("%-40s %8.2f ms\n", "\$GLOBALS redefinition", ($redefEnd - $redefStart) / 1_000_000);

$redefStart = hrtime(true);
for ($i = 0; $i < 1000; $i++) {
    $fnArray['inc'] = function($x) use ($i) { return $x + $i; };
    $result = ($fnArray['inc'])(42);
}
$redefEnd = hrtime(true);
printf("%-40s %8.2f ms\n", "Array redefinition", ($redefEnd - $redefStart) / 1_000_000);

echo "\n=== Summary ===\n";
echo "For REPL mode (redefinable vars):\n";
echo "  - Static properties offer best balance of speed and redefinability\n";
echo "  - Direct closure calls are fastest but require local variable\n";
echo "  - \$GLOBALS is slightly slower but globally accessible\n";
echo "  - Avoid call_user_func when possible\n";
