<?php
/**
 * Benchmark script to test PHP JIT performance on ClojurePHP collections.
 *
 * Usage:
 *   # Without JIT (baseline)
 *   php script/benchmark_jit.php
 *
 *   # With JIT enabled
 *   php -d opcache.enable=1 -d opcache.enable_cli=1 -d opcache.jit=1255 -d opcache.jit_buffer_size=100M script/benchmark_jit.php
 *
 *   # With JIT tracing mode
 *   php -d opcache.enable=1 -d opcache.enable_cli=1 -d opcache.jit=tracing -d opcache.jit_buffer_size=100M script/benchmark_jit.php
 */

declare(strict_types=1);

require_once __DIR__ . '/../vendor/autoload.php';

use Clojure\Lang\Hasher;
use Clojure\Lang\Equalizer;
use Clojure\Lang\Keyword;
use Clojure\Lang\Collections\Vector\PersistentVector;
use Clojure\Lang\Collections\Map\PersistentHashMap;

// Check JIT status
$status = function_exists('opcache_get_status') ? @opcache_get_status() : false;
$jitEnabled = is_array($status) && isset($status['jit']['enabled']) && $status['jit']['enabled'];
echo "JIT Status: " . ($jitEnabled ? "ENABLED" : "DISABLED") . "\n";
if ($jitEnabled && isset($status['jit'])) {
    echo "JIT Buffer: " . number_format($status['jit']['buffer_size'] ?? 0) . " bytes\n";
    echo "JIT Mode: " . ($status['jit']['opt_level'] ?? 'unknown') . "\n";
}
echo str_repeat("-", 60) . "\n\n";

$hasher = Hasher::getInstance();
$equalizer = Equalizer::getInstance();

/**
 * Benchmark helper function
 */
function benchmark(string $name, callable $fn, int $iterations = 100): void {
    // Warmup (important for JIT)
    for ($i = 0; $i < 10; $i++) {
        $fn();
    }

    // Actual benchmark
    $start = hrtime(true);
    for ($i = 0; $i < $iterations; $i++) {
        $fn();
    }
    $elapsed = (hrtime(true) - $start) / 1_000_000; // Convert to ms

    $perIteration = $elapsed / $iterations;
    printf("%-40s %8.3f ms total | %8.3f ms/iter\n", $name, $elapsed, $perIteration);
}

echo "=== Vector Operations ===\n";

// Pre-build vectors for get benchmarks
$v1000 = PersistentVector::fromArray($hasher, $equalizer, range(0, 999));

benchmark("Vector append 1000", function() use ($hasher, $equalizer) {
    $v = PersistentVector::empty($hasher, $equalizer);
    for ($i = 0; $i < 1000; $i++) {
        $v = $v->append($i);
    }
});

benchmark("Vector get 1000", function() use ($v1000) {
    for ($i = 0; $i < 1000; $i++) {
        $v1000->get($i);
    }
});

benchmark("Vector iteration 1000", function() use ($v1000) {
    $sum = 0;
    foreach ($v1000 as $val) {
        $sum += $val;
    }
});

echo "\n=== Map Operations ===\n";

// Pre-build map for get benchmarks
$mapData = [];
for ($i = 0; $i < 1000; $i++) {
    $mapData[] = "key$i";
    $mapData[] = $i;
}
$m1000 = PersistentHashMap::fromArray($hasher, $equalizer, $mapData);

benchmark("Map put 1000 (strings)", function() use ($hasher, $equalizer) {
    $m = PersistentHashMap::empty($hasher, $equalizer);
    for ($i = 0; $i < 1000; $i++) {
        $m = $m->put("key$i", $i);
    }
});

benchmark("Map get 1000 (cold keys)", function() use ($m1000) {
    for ($i = 0; $i < 1000; $i++) {
        $m1000->find("key$i");
    }
});

// Pre-create keys for cached lookup
$cachedKeys = [];
for ($i = 0; $i < 1000; $i++) {
    $cachedKeys[$i] = "key$i";
}

benchmark("Map get 1000 (cached keys)", function() use ($m1000, $cachedKeys) {
    for ($i = 0; $i < 1000; $i++) {
        $m1000->find($cachedKeys[$i]);
    }
});

// Keyword-keyed map
$kwMapData = [];
for ($i = 0; $i < 100; $i++) {
    $kwMapData[] = Keyword::create("key$i");
    $kwMapData[] = $i;
}
$kwMap = PersistentHashMap::fromArray($hasher, $equalizer, $kwMapData);

benchmark("Map put 100 (keywords)", function() use ($hasher, $equalizer) {
    $m = PersistentHashMap::empty($hasher, $equalizer);
    for ($i = 0; $i < 100; $i++) {
        $m = $m->put(Keyword::create("key$i"), $i);
    }
});

benchmark("Map get 1000 (keyword lookup)", function() use ($kwMap) {
    for ($i = 0; $i < 1000; $i++) {
        $kwMap->find(Keyword::create("key" . ($i % 100)));
    }
});

echo "\n=== Transient Operations ===\n";

benchmark("Transient map build 1000", function() use ($hasher, $equalizer) {
    $t = PersistentHashMap::empty($hasher, $equalizer)->asTransient();
    for ($i = 0; $i < 1000; $i++) {
        $t->put("key$i", $i);
    }
    $t->persistent();
});

benchmark("Transient keyword map 1000", function() use ($hasher, $equalizer) {
    $t = PersistentHashMap::empty($hasher, $equalizer)->asTransient();
    for ($i = 0; $i < 1000; $i++) {
        $t->put(Keyword::create("key$i"), $i);
    }
    $t->persistent();
});

echo "\n=== Real-World Scenarios ===\n";

benchmark("Nested map access (get-in)", function() use ($hasher, $equalizer) {
    $inner = PersistentHashMap::empty($hasher, $equalizer)
        ->put(Keyword::create("value"), 42);
    $middle = PersistentHashMap::empty($hasher, $equalizer)
        ->put(Keyword::create("inner"), $inner);
    $outer = PersistentHashMap::empty($hasher, $equalizer)
        ->put(Keyword::create("middle"), $middle);
    $data = PersistentHashMap::empty($hasher, $equalizer)
        ->put(Keyword::create("outer"), $outer);

    // Deep access 100 times
    for ($i = 0; $i < 100; $i++) {
        $o = $data->find(Keyword::create("outer"));
        $m = $o->find(Keyword::create("middle"));
        $in = $m->find(Keyword::create("inner"));
        $val = $in->find(Keyword::create("value"));
    }
});

benchmark("assoc-in simulation", function() use ($hasher, $equalizer) {
    $c = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("value"), 0);
    $b = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("c"), $c);
    $a = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("b"), $b);
    $data = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("a"), $a);

    for ($i = 0; $i < 100; $i++) {
        $a = $data->find(Keyword::create("a"));
        $b = $a->find(Keyword::create("b"));
        $c = $b->find(Keyword::create("c"));
        $newC = $c->put(Keyword::create("value"), $i);
        $newB = $b->put(Keyword::create("c"), $newC);
        $newA = $a->put(Keyword::create("b"), $newB);
        $data = $data->put(Keyword::create("a"), $newA);
    }
});

echo "\n=== Memory Usage ===\n";
echo "Peak memory: " . number_format(memory_get_peak_usage(true)) . " bytes\n";
echo "Current memory: " . number_format(memory_get_usage(true)) . " bytes\n";
