<?php
/**
 * Benchmark: Optimizing __call magic method
 * Shows nanoseconds per call for precision
 */

declare(strict_types=1);

const ITERATIONS = 10_000_000;  // 10M for better precision

// Baseline: native function
function add_native(int $a, int $b): int { return $a + $b; }

// Baseline: $GLOBALS
$GLOBALS['add'] = fn(int $a, int $b): int => $a + $b;

// V1: Naive __call (array + spread)
class MagicV1 {
    private array $fns = [];
    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
    }
    public function __call(string $name, array $args): mixed {
        return ($this->fns[$name])(...$args);
    }
}

// V2: __call with match on arity (avoid spread for common cases)
class MagicV2 {
    private array $fns = [];
    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
    }
    public function __call(string $name, array $args): mixed {
        $fn = $this->fns[$name];
        return match(count($args)) {
            0 => $fn(),
            1 => $fn($args[0]),
            2 => $fn($args[0], $args[1]),
            3 => $fn($args[0], $args[1], $args[2]),
            default => $fn(...$args)
        };
    }
}

// V3: Public properties + __call fallback
class MagicV3 {
    public \Closure $add;  // frequently used = public property
    private array $fns = [];

    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
        if (property_exists($this, $name)) {
            $this->$name = $fn;
        }
    }
    public function __call(string $name, array $args): mixed {
        return ($this->fns[$name])(...$args);
    }
}

// V4: Cached closure in __call
class MagicV4 {
    private array $fns = [];
    private array $cache = [];  // avoid repeated lookup

    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
        unset($this->cache[$name]);
    }
    public function __call(string $name, array $args): mixed {
        $fn = $this->cache[$name] ??= $this->fns[$name];
        return match(count($args)) {
            0 => $fn(),
            1 => $fn($args[0]),
            2 => $fn($args[0], $args[1]),
            3 => $fn($args[0], $args[1], $args[2]),
            default => $fn(...$args)
        };
    }
}

// V5: __get instead of __call (return closure, caller invokes)
class MagicV5 {
    private array $fns = [];
    public function def(string $name, \Closure $fn): void {
        $this->fns[$name] = $fn;
    }
    public function __get(string $name): \Closure {
        return $this->fns[$name];
    }
}

$v1 = new MagicV1(); $v1->def('add', fn(int $a, int $b): int => $a + $b);
$v2 = new MagicV2(); $v2->def('add', fn(int $a, int $b): int => $a + $b);
$v3 = new MagicV3(); $v3->def('add', fn(int $a, int $b): int => $a + $b);
$v4 = new MagicV4(); $v4->def('add', fn(int $a, int $b): int => $a + $b);
$v5 = new MagicV5(); $v5->def('add', fn(int $a, int $b): int => $a + $b);

// Warmup
for ($i = 0; $i < 100000; $i++) {
    add_native(1, 2);
    $GLOBALS['add'](1, 2);
    $v1->add(1, 2);
    $v2->add(1, 2);
    ($v3->add)(1, 2);
    $v4->add(1, 2);
    ($v5->add)(1, 2);
}

echo "PHP " . PHP_VERSION . "\n";
echo "Iterations: " . number_format(ITERATIONS) . "\n";
echo str_repeat("-", 65) . "\n";
printf("%-35s %10s %10s\n", "Strategy", "Total(ms)", "ns/call");
echo str_repeat("-", 65) . "\n";

$results = [];

// Native
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { add_native(1, 2); }
$ns = hrtime(true) - $start;
$results['Native function'] = $ns;
printf("%-35s %10.2f %10.1f\n", "Native function", $ns/1e6, $ns/ITERATIONS);

// $GLOBALS
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { $GLOBALS['add'](1, 2); }
$ns = hrtime(true) - $start;
$results['$GLOBALS closure'] = $ns;
printf("%-35s %10.2f %10.1f\n", "\$GLOBALS closure", $ns/1e6, $ns/ITERATIONS);

// V1: Naive __call
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { $v1->add(1, 2); }
$ns = hrtime(true) - $start;
$results['__call naive (spread)'] = $ns;
printf("%-35s %10.2f %10.1f\n", "__call naive (spread)", $ns/1e6, $ns/ITERATIONS);

// V2: __call with match
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { $v2->add(1, 2); }
$ns = hrtime(true) - $start;
$results['__call match arity'] = $ns;
printf("%-35s %10.2f %10.1f\n", "__call match arity", $ns/1e6, $ns/ITERATIONS);

// V3: Public property (direct)
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { ($v3->add)(1, 2); }
$ns = hrtime(true) - $start;
$results['Public property (direct)'] = $ns;
printf("%-35s %10.2f %10.1f\n", "Public property (direct)", $ns/1e6, $ns/ITERATIONS);

// V4: __call cached + match
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { $v4->add(1, 2); }
$ns = hrtime(true) - $start;
$results['__call cached + match'] = $ns;
printf("%-35s %10.2f %10.1f\n", "__call cached + match", $ns/1e6, $ns/ITERATIONS);

// V5: __get (return closure)
$start = hrtime(true);
for ($i = 0; $i < ITERATIONS; $i++) { ($v5->add)(1, 2); }
$ns = hrtime(true) - $start;
$results['__get (return closure)'] = $ns;
printf("%-35s %10.2f %10.1f\n", "__get (return closure)", $ns/1e6, $ns/ITERATIONS);

echo str_repeat("-", 65) . "\n";
$baseline = $results['Native function'];
echo "Relative to native:\n";
foreach ($results as $name => $ns) {
    printf("  %-33s %.2fx\n", $name, $ns / $baseline);
}
