<?php
/**
 * Benchmark: Function dispatch mechanisms in PHP 8
 *
 * Tests realistic Clojure-to-PHP emission strategies.
 */

declare(strict_types=1);

const ITERATIONS = 1_000_000;

// Strategy 1: $GLOBALS closure (current approach)
$GLOBALS['add_globals'] = fn(int $a, int $b): int => $a + $b;

// Strategy 2: Static class property (must call via parens)
class Core {
    public static \Closure $add_static;
}
Core::$add_static = fn(int $a, int $b): int => $a + $b;
$core_add = Core::$add_static; // local reference for call syntax

// Strategy 3: Native PHP function (not redefinable)
function add_native(int $a, int $b): int {
    return $a + $b;
}

// Strategy 4: Local variable closure
$add_local = fn(int $a, int $b): int => $a + $b;

// Strategy 5: Object method
class Fns {
    public function add(int $a, int $b): int {
        return $a + $b;
    }
}
$fns = new Fns();

// Warmup
for ($i = 0; $i < 10000; $i++) {
    $GLOBALS['add_globals'](1, 2);
    (Core::$add_static)(1, 2);
    add_native(1, 2);
    $add_local(1, 2);
    $fns->add(1, 2);
}

echo "PHP " . PHP_VERSION . "\n";
echo "Iterations: " . number_format(ITERATIONS) . "\n";
echo str_repeat("-", 50) . "\n";

// Benchmark 1: $GLOBALS closure
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    $GLOBALS['add_globals'](1, 2);
}
$globals_time = (hrtime(true) - $start) / 1e6;
echo "\$GLOBALS closure:     " . number_format($globals_time, 2) . " ms\n";

// Benchmark 2: Static class property
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    (Core::$add_static)(1, 2);
}
$static_time = (hrtime(true) - $start) / 1e6;
echo "Static property:      " . number_format($static_time, 2) . " ms\n";

// Benchmark 3: Native function
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    add_native(1, 2);
}
$native_time = (hrtime(true) - $start) / 1e6;
echo "Native function:      " . number_format($native_time, 2) . " ms\n";

// Benchmark 4: Local variable closure
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    $add_local(1, 2);
}
$local_time = (hrtime(true) - $start) / 1e6;
echo "Local closure:        " . number_format($local_time, 2) . " ms\n";

// Benchmark 5: Object method
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    $fns->add(1, 2);
}
$method_time = (hrtime(true) - $start) / 1e6;
echo "Object method:        " . number_format($method_time, 2) . " ms\n";

echo str_repeat("-", 50) . "\n";
echo "Relative to native function:\n";
echo "  \$GLOBALS:  " . number_format($globals_time / $native_time, 2) . "x\n";
echo "  Static:    " . number_format($static_time / $native_time, 2) . "x\n";
echo "  Local:     " . number_format($local_time / $native_time, 2) . "x\n";
echo "  Method:    " . number_format($method_time / $native_time, 2) . "x\n";

echo str_repeat("-", 50) . "\n";
echo "REPL-compatible (redefinable):\n";
echo "  \$GLOBALS closure: YES\n";
echo "  Static property:  YES\n";
echo "  Native function:  NO (fatal error)\n";
echo "  Local closure:    YES (but scoped)\n";
echo "  Object method:    YES (but OO ceremony)\n";
