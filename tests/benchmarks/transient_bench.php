<?php

require_once __DIR__ . '/../src/clojure/php/runtime/bootstrap.php';

use Clojure\Php\Vec;
use Clojure\Php\Map;
use function Clojure\Php\vec;
use function Clojure\Php\hashMap;
use function Clojure\Php\subvec;

$sizes = [100, 1000, 10000, 100000];

echo "=== Vector Building Benchmark ===\n\n";

foreach ($sizes as $n) {
    echo "n = $n\n";

    // Method 1: Repeated conj (worst case - creates n vectors)
    $start = hrtime(true);
    $v = vec();
    for ($i = 0; $i < $n; $i++) {
        $v = $v->append($i);
    }
    $conjTime = (hrtime(true) - $start) / 1_000_000;

    // Method 2: PHP array then Vec::from
    $start = hrtime(true);
    $arr = [];
    for ($i = 0; $i < $n; $i++) {
        $arr[] = $i;
    }
    $v2 = Vec::from($arr);
    $arrayTime = (hrtime(true) - $start) / 1_000_000;

    // Method 3: Vec::from(range())
    $start = hrtime(true);
    $v3 = Vec::from(range(0, $n - 1));
    $rangeTime = (hrtime(true) - $start) / 1_000_000;

    printf("  conj loop:     %8.2f ms\n", $conjTime);
    printf("  array + from:  %8.2f ms  (%.1fx faster)\n", $arrayTime, $conjTime / $arrayTime);
    printf("  range + from:  %8.2f ms  (%.1fx faster)\n", $rangeTime, $conjTime / $rangeTime);
    echo "\n";
}

echo "=== Map Building Benchmark ===\n\n";

foreach ($sizes as $n) {
    echo "n = $n\n";

    // Method 1: Repeated put
    $start = hrtime(true);
    $m = hashMap();
    for ($i = 0; $i < $n; $i++) {
        $m = $m->put($i, $i * 2);
    }
    $putTime = (hrtime(true) - $start) / 1_000_000;

    // Method 2: PHP array then Map::from
    $start = hrtime(true);
    $arr = [];
    for ($i = 0; $i < $n; $i++) {
        $arr[] = $i;
        $arr[] = $i * 2;
    }
    $m2 = Map::from($arr);
    $arrayTime = (hrtime(true) - $start) / 1_000_000;

    printf("  put loop:      %8.2f ms\n", $putTime);
    printf("  array + from:  %8.2f ms  (%.1fx vs put)\n", $arrayTime, $putTime / $arrayTime);
    echo "\n";
}

echo "=== SubVector Benchmark ===\n\n";

$v = Vec::from(range(0, 99999));

// Old way: slice (rebuilds)
$start = hrtime(true);
for ($i = 0; $i < 1000; $i++) {
    $sub = $v->slice(1000, 2000);
}
$sliceTime = (hrtime(true) - $start) / 1_000_000;

// New way: subvec (O(1))
$start = hrtime(true);
for ($i = 0; $i < 1000; $i++) {
    $sub = subvec($v, 1000, 2000);
}
$subvecTime = (hrtime(true) - $start) / 1_000_000;

printf("  slice (rebuild): %8.2f ms for 1000 iterations\n", $sliceTime);
printf("  subvec (O(1)):   %8.2f ms for 1000 iterations (%.1fx faster)\n", $subvecTime, $sliceTime / $subvecTime);

echo "\n=== Null Key Benchmark ===\n\n";

// Test null key handling
$m = hashMap(null, "null-value", "a", 1, "b", 2);
echo "Map with null key: " . $m->count() . " entries\n";
echo "  find(null): " . var_export($m->find(null), true) . "\n";
echo "  containsKey(null): " . var_export($m->containsKey(null), true) . "\n";

$m2 = $m->remove(null);
echo "After remove(null): " . $m2->count() . " entries\n";
echo "  containsKey(null): " . var_export($m2->containsKey(null), true) . "\n";

echo "\n=== Memory Usage ===\n\n";

$n = 100000;

// conj loop
gc_collect_cycles();
$before = memory_get_usage();
$v = vec();
for ($i = 0; $i < $n; $i++) {
    $v = $v->append($i);
}
$conjMem = memory_get_usage() - $before;

// array + from
gc_collect_cycles();
$before = memory_get_usage();
$arr = [];
for ($i = 0; $i < $n; $i++) {
    $arr[] = $i;
}
$v2 = Vec::from($arr);
unset($arr);
$arrayMem = memory_get_usage() - $before;

printf("conj loop (n=%d):    %d KB\n", $n, $conjMem / 1024);
printf("array + from:        %d KB\n", $arrayMem / 1024);
