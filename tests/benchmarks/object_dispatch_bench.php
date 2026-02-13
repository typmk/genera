<?php
/**
 * Benchmark: Object-based dispatch strategies
 */

declare(strict_types=1);

const ITERATIONS = 1_000_000;

// Option 1: Object with closures as properties (needs parens to call)
class NsProps {
    public \Closure $add;
}
$ns_props = new NsProps();
$ns_props->add = fn(int $a, int $b): int => $a + $b;

// Option 2: __call magic method (nice syntax, but magic overhead?)
class NsMagic {
    private array $fns = [];

    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
    }

    public function __call(string $name, array $args): mixed {
        return ($this->fns[$name])(...$args);
    }
}
$ns_magic = new NsMagic();
$ns_magic->def('add', fn(int $a, int $b): int => $a + $b);

// Option 3: Registry object with get()
class NsRegistry {
    private array $fns = [];

    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
    }

    public function get(string $name): \Closure {
        return $this->fns[$name];
    }
}
$ns_reg = new NsRegistry();
$ns_reg->def('add', fn(int $a, int $b): int => $a + $b);

// Option 4: ArrayObject style (array syntax)
class NsArray extends ArrayObject {
    public function __construct() {
        parent::__construct([]);
    }
}
$ns_arr = new NsArray();
$ns_arr['add'] = fn(int $a, int $b): int => $a + $b;

// Option 5: Static wrapper methods (nice syntax, redefinable)
class Core {
    public static \Closure $add_impl;

    public static function add(int $a, int $b): int {
        return (self::$add_impl)($a, $b);
    }
}
Core::$add_impl = fn(int $a, int $b): int => $a + $b;

// Baseline: $GLOBALS
$GLOBALS['add'] = fn(int $a, int $b): int => $a + $b;

// Baseline: native function
function add_native(int $a, int $b): int { return $a + $b; }

// Warmup
for ($i = 0; $i < 10000; $i++) {
    ($ns_props->add)(1, 2);
    $ns_magic->add(1, 2);
    ($ns_reg->get('add'))(1, 2);
    ($ns_arr['add'])(1, 2);
    Core::add(1, 2);
    $GLOBALS['add'](1, 2);
    add_native(1, 2);
}

echo "PHP " . PHP_VERSION . " (JIT: " . (function_exists('opcache_get_status') && @opcache_get_status()['jit']['enabled'] ? 'ON' : 'OFF') . ")\n";
echo "Iterations: " . number_format(ITERATIONS) . "\n";
echo str_repeat("-", 60) . "\n";

// Benchmark: Native (baseline)
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    add_native(1, 2);
}
$native_time = (hrtime(true) - $start) / 1e6;
echo "Native function:              " . sprintf("%8.2f", $native_time) . " ms (1.00x)\n";

// Benchmark: $GLOBALS
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    $GLOBALS['add'](1, 2);
}
$globals_time = (hrtime(true) - $start) / 1e6;
echo "\$GLOBALS closure:             " . sprintf("%8.2f", $globals_time) . " ms (" . sprintf("%.2f", $globals_time/$native_time) . "x)\n";

// Benchmark: Object property closure
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    ($ns_props->add)(1, 2);
}
$props_time = (hrtime(true) - $start) / 1e6;
echo "Object property closure:      " . sprintf("%8.2f", $props_time) . " ms (" . sprintf("%.2f", $props_time/$native_time) . "x)\n";

// Benchmark: __call magic
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    $ns_magic->add(1, 2);
}
$magic_time = (hrtime(true) - $start) / 1e6;
echo "__call magic method:          " . sprintf("%8.2f", $magic_time) . " ms (" . sprintf("%.2f", $magic_time/$native_time) . "x)\n";

// Benchmark: Registry get()
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    ($ns_reg->get('add'))(1, 2);
}
$reg_time = (hrtime(true) - $start) / 1e6;
echo "Registry get() call:          " . sprintf("%8.2f", $reg_time) . " ms (" . sprintf("%.2f", $reg_time/$native_time) . "x)\n";

// Benchmark: ArrayObject
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    ($ns_arr['add'])(1, 2);
}
$arr_time = (hrtime(true) - $start) / 1e6;
echo "ArrayObject closure:          " . sprintf("%8.2f", $arr_time) . " ms (" . sprintf("%.2f", $arr_time/$native_time) . "x)\n";

// Benchmark: Static wrapper
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) {
    Core::add(1, 2);
}
$wrapper_time = (hrtime(true) - $start) / 1e6;
echo "Static wrapper method:        " . sprintf("%8.2f", $wrapper_time) . " ms (" . sprintf("%.2f", $wrapper_time/$native_time) . "x)\n";

echo str_repeat("-", 60) . "\n";
echo "REPL-compatible (redefinable at runtime):\n";
echo "  \$GLOBALS closure:      YES\n";
echo "  Object property:       YES\n";
echo "  __call magic:          YES (nicest syntax)\n";
echo "  Registry get():        YES\n";
echo "  ArrayObject:           YES\n";
echo "  Static wrapper:        YES (delegates to closure)\n";
echo "  Native function:       NO\n";
echo str_repeat("-", 60) . "\n";
echo "Call syntax comparison:\n";
echo "  \$GLOBALS:        \$GLOBALS['add'](1, 2)\n";
echo "  Object prop:     (\$ns->add)(1, 2)        <- parens needed\n";
echo "  __call magic:    \$ns->add(1, 2)          <- clean!\n";
echo "  Registry:        (\$ns->get('add'))(1, 2) <- verbose\n";
echo "  ArrayObject:     (\$ns['add'])(1, 2)      <- parens needed\n";
echo "  Static wrapper:  Core::add(1, 2)         <- clean!\n";
