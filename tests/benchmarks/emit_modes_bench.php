<?php
/**
 * Benchmark: REPL vs AOT emission mode performance
 */

declare(strict_types=1);

const ITERATIONS = 10_000_000;

// AOT style: native function
function add_aot(int $a, int $b): int {
    return $a + $b;
}

// REPL style: object property closure
class Myapp {
    public \Closure $add_repl;
}
$Myapp = new Myapp();
$Myapp->add_repl = fn(int $a, int $b): int => $a + $b;

// Old style: $GLOBALS
$GLOBALS['add_globals'] = fn(int $a, int $b): int => $a + $b;

// Warmup
for ($i = 0; $i < 100000; $i++) {
    add_aot(1, 2);
    ($Myapp->add_repl)(1, 2);
    call_user_func($GLOBALS['add_globals'], 1, 2);
}

echo "PHP " . PHP_VERSION . "\n";
echo "Iterations: " . number_format(ITERATIONS) . "\n";
echo str_repeat("-", 55) . "\n";
printf("%-35s %10s %8s\n", "Strategy", "ns/call", "vs AOT");
echo str_repeat("-", 55) . "\n";

// AOT: direct function call
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    add_aot(1, 2);
}
$aot_ns = hrtime(true) - $start;
printf("%-35s %10.1f %8s\n", "AOT: add(1, 2)", $aot_ns/ITERATIONS, "1.00x");

// REPL: object property call
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    ($Myapp->add_repl)(1, 2);
}
$repl_ns = hrtime(true) - $start;
printf("%-35s %10.1f %8.2fx\n", "REPL: (\$Ns->add)(1, 2)", $repl_ns/ITERATIONS, $repl_ns/$aot_ns);

// Old: call_user_func + $GLOBALS
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    call_user_func($GLOBALS['add_globals'], 1, 2);
}
$old_ns = hrtime(true) - $start;
printf("%-35s %10.1f %8.2fx\n", "Old: call_user_func(\$GLOBALS[...])", $old_ns/ITERATIONS, $old_ns/$aot_ns);

echo str_repeat("-", 55) . "\n";
echo "Improvement: Old → REPL = " . sprintf("%.1fx", $old_ns/$repl_ns) . " faster\n";
echo "Improvement: Old → AOT  = " . sprintf("%.1fx", $old_ns/$aot_ns) . " faster\n";
