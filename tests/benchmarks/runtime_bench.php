<?php

declare(strict_types=1);

/**
 * ClojurePHP Runtime Benchmarks
 *
 * Run: php benchmarks/runtime_bench.php
 */

require_once __DIR__ . '/../src/clojure/php/runtime/bootstrap.php';

use function Clojure\Php\{
    vec, hashMap, hashSet, plist, kw, sym,
    conj, assoc, dissoc, get, nth, count_,
    first, rest, next_, seq,
    map_, filter_, reduce_, take_, drop_,
    cons, concat_, range_, repeat_,
    equals, hash_
};
use Clojure\Php\{Vec, Map, Set, PList, LazySeq};

// ============================================================
// Benchmark Infrastructure
// ============================================================

function benchmark(string $name, callable $fn, int $iterations = 1000): array {
    // Warmup
    for ($i = 0; $i < min(100, $iterations / 10); $i++) {
        $fn();
    }

    // Timed run
    $start = hrtime(true);
    for ($i = 0; $i < $iterations; $i++) {
        $fn();
    }
    $end = hrtime(true);

    $totalNs = $end - $start;
    $totalMs = $totalNs / 1_000_000;
    $perOpUs = ($totalNs / $iterations) / 1000;

    return [
        'name' => $name,
        'iterations' => $iterations,
        'total_ms' => round($totalMs, 2),
        'per_op_us' => round($perOpUs, 3),
        'ops_per_sec' => round($iterations / ($totalMs / 1000)),
    ];
}

function printResult(array $result): void {
    printf(
        "%-45s %8d iter | %8.2f ms | %8.3f µs/op | %10s ops/sec\n",
        $result['name'],
        $result['iterations'],
        $result['total_ms'],
        $result['per_op_us'],
        number_format($result['ops_per_sec'])
    );
}

function section(string $title): void {
    echo "\n" . str_repeat("=", 70) . "\n";
    echo " $title\n";
    echo str_repeat("=", 70) . "\n";
}

// ============================================================
// Vector Benchmarks
// ============================================================

section("VECTOR BENCHMARKS");

// Construction
printResult(benchmark("vec() empty", fn() => vec()));
printResult(benchmark("vec(1,2,3,4,5)", fn() => vec(1,2,3,4,5)));
printResult(benchmark("vec() 100 elements", fn() => vec(...range(1, 100))));
printResult(benchmark("vec() 1000 elements", fn() => vec(...range(1, 1000)), 100));

// Access
$v100 = vec(...range(1, 100));
$v1000 = vec(...range(1, 1000));
$v10000 = vec(...range(1, 10000));

printResult(benchmark("nth(v100, 50)", fn() => nth($v100, 50)));
printResult(benchmark("nth(v1000, 500)", fn() => nth($v1000, 500)));
printResult(benchmark("nth(v10000, 5000)", fn() => nth($v10000, 5000)));
printResult(benchmark("get(v1000, 500)", fn() => get($v1000, 500)));

// Append
printResult(benchmark("conj(v100, x)", fn() => conj($v100, 999)));
printResult(benchmark("conj(v1000, x)", fn() => conj($v1000, 999)));

// Update
printResult(benchmark("assoc(v100, 50, x)", fn() => assoc($v100, 50, 999)));
printResult(benchmark("assoc(v1000, 500, x)", fn() => assoc($v1000, 500, 999)));

// Iteration
printResult(benchmark("foreach v100", fn() => (function() use ($v100) {
    $sum = 0;
    foreach ($v100 as $x) $sum += $x;
    return $sum;
})(), 500));

printResult(benchmark("foreach v1000", fn() => (function() use ($v1000) {
    $sum = 0;
    foreach ($v1000 as $x) $sum += $x;
    return $sum;
})(), 100));

// ============================================================
// HashMap Benchmarks
// ============================================================

section("HASHMAP BENCHMARKS");

// Construction
printResult(benchmark("hashMap() empty", fn() => hashMap()));
printResult(benchmark("hashMap(k,v,k,v,k,v)", fn() => hashMap(kw('a'), 1, kw('b'), 2, kw('c'), 3)));

// Build map with 100 entries
$buildMap100 = function() {
    $m = hashMap();
    for ($i = 0; $i < 100; $i++) {
        $m = $m->put(kw("k$i"), $i);
    }
    return $m;
};
printResult(benchmark("build map 100 entries", $buildMap100, 100));

// Pre-built maps for access tests
$m100 = $buildMap100();
$m1000 = (function() {
    $m = hashMap();
    for ($i = 0; $i < 1000; $i++) {
        $m = $m->put(kw("k$i"), $i);
    }
    return $m;
})();

printResult(benchmark("get(m100, key) hit", fn() => get($m100, kw('k50'))));
printResult(benchmark("get(m100, key) miss", fn() => get($m100, kw('missing'))));
printResult(benchmark("get(m1000, key) hit", fn() => get($m1000, kw('k500'))));

// Update
printResult(benchmark("assoc(m100, k, v)", fn() => assoc($m100, kw('new'), 999)));
printResult(benchmark("dissoc(m100, k)", fn() => dissoc($m100, kw('k50'))));

// Iteration
printResult(benchmark("foreach m100", fn() => (function() use ($m100) {
    $sum = 0;
    foreach ($m100 as $k => $v) $sum += $v;
    return $sum;
})(), 500));

// ============================================================
// Set Benchmarks
// ============================================================

section("SET BENCHMARKS");

$s100 = hashSet(...range(1, 100));
$s1000 = hashSet(...range(1, 1000));

printResult(benchmark("hashSet() 100 elements", fn() => hashSet(...range(1, 100)), 100));
printResult(benchmark("contains(s100, x) hit", fn() => $s100->contains(50)));
printResult(benchmark("contains(s100, x) miss", fn() => $s100->contains(999)));
printResult(benchmark("add(s100, x)", fn() => $s100->add(999)));
printResult(benchmark("remove(s100, x)", fn() => $s100->remove(50)));
printResult(benchmark("union(s100, s100)", fn() => $s100->union($s100), 100));
printResult(benchmark("intersection(s100, s100)", fn() => $s100->intersection($s100), 100));

// ============================================================
// List Benchmarks
// ============================================================

section("LIST BENCHMARKS");

$l100 = plist(...range(1, 100));
$l1000 = plist(...range(1, 1000));

printResult(benchmark("plist() 100 elements", fn() => plist(...range(1, 100)), 100));
printResult(benchmark("cons(x, l100)", fn() => cons(0, $l100)));
printResult(benchmark("first(l100)", fn() => first($l100)));
printResult(benchmark("rest(l100)", fn() => rest($l100)));
printResult(benchmark("count(l100)", fn() => count_($l100)));
printResult(benchmark("count(l1000)", fn() => count_($l1000)));

// ============================================================
// Sequence Operation Benchmarks
// ============================================================

section("SEQUENCE OPERATIONS");

// map
printResult(benchmark("map(inc, v100) realize", fn() => (function() use ($v100) {
    $result = map_(fn($x) => $x + 1, $v100);
    foreach ($result as $_) {} // force realization
})(), 200));

// filter
printResult(benchmark("filter(even?, v100) realize", fn() => (function() use ($v100) {
    $result = filter_(fn($x) => $x % 2 === 0, $v100);
    foreach ($result as $_) {}
})(), 200));

// reduce
printResult(benchmark("reduce(+, v100)", fn() => reduce_(fn($a, $b) => $a + $b, 0, $v100), 500));
printResult(benchmark("reduce(+, v1000)", fn() => reduce_(fn($a, $b) => $a + $b, 0, $v1000), 100));

// take/drop
printResult(benchmark("take(50, v100) realize", fn() => (function() use ($v100) {
    $result = take_(50, $v100);
    foreach ($result as $_) {}
})(), 500));

printResult(benchmark("drop(50, v100) realize", fn() => (function() use ($v100) {
    $result = drop_(50, $v100);
    foreach ($result as $_) {}
})(), 500));

// Composition
printResult(benchmark("map+filter+take pipeline", fn() => (function() use ($v1000) {
    $result = take_(10, filter_(fn($x) => $x % 2 === 0, map_(fn($x) => $x * 2, $v1000)));
    foreach ($result as $_) {}
})(), 200));

// ============================================================
// Lazy Sequence Benchmarks
// ============================================================

section("LAZY SEQUENCES");

// range (lazy)
printResult(benchmark("range(1000) first 10", fn() => (function() {
    $r = range_(0, 1000);
    $result = take_(10, $r);
    foreach ($result as $_) {}
})(), 500));

printResult(benchmark("range(10000) first 10", fn() => (function() {
    $r = range_(0, 10000);
    $result = take_(10, $r);
    foreach ($result as $_) {}
})(), 500));

// repeat (infinite lazy)
printResult(benchmark("repeat(x) take 100", fn() => (function() {
    $r = repeat_(42);
    $result = take_(100, $r);
    foreach ($result as $_) {}
})(), 200));

// ============================================================
// Keyword/Symbol Benchmarks
// ============================================================

section("KEYWORDS & SYMBOLS");

printResult(benchmark("kw('foo') interned", fn() => kw('foo')));
printResult(benchmark("kw('bar', 'ns') namespaced", fn() => kw('bar', 'myns')));
printResult(benchmark("sym('x')", fn() => sym('x')));
printResult(benchmark("Sym::gen() gensym", fn() => \Clojure\Php\Sym::gen()));

// Keyword as function
$m = hashMap(kw('a'), 1, kw('b'), 2);
$k = kw('a');
printResult(benchmark("(:a m) keyword invoke", fn() => $k($m)));

// ============================================================
// Equality & Hashing
// ============================================================

section("EQUALITY & HASHING");

$va = vec(1, 2, 3, 4, 5);
$vb = vec(1, 2, 3, 4, 5);
$vc = vec(1, 2, 3, 4, 6);

printResult(benchmark("equals(v, v) same", fn() => equals($va, $va)));
printResult(benchmark("equals(va, vb) equal", fn() => equals($va, $vb)));
printResult(benchmark("equals(va, vc) not equal", fn() => equals($va, $vc)));

$ma = hashMap(kw('a'), 1, kw('b'), 2);
$mb = hashMap(kw('a'), 1, kw('b'), 2);
printResult(benchmark("equals(map, map) equal", fn() => equals($ma, $mb)));

printResult(benchmark("hash(vec)", fn() => hash_($va)));
printResult(benchmark("hash(map)", fn() => hash_($ma)));

// ============================================================
// Comparison with PHP Arrays
// ============================================================

section("COMPARISON: Clojure\Php vs PHP Arrays");

$phpArr100 = range(1, 100);
$phpArr1000 = range(1, 1000);

printResult(benchmark("[PHP] array 100 append", fn() => (function() use ($phpArr100) {
    $a = $phpArr100;
    $a[] = 999;
    return $a;
})()));

printResult(benchmark("[Cljp] vec 100 conj", fn() => conj($v100, 999)));

printResult(benchmark("[PHP] array 100 access", fn() => $phpArr100[50]));
printResult(benchmark("[Cljp] vec 100 nth", fn() => nth($v100, 50)));

printResult(benchmark("[PHP] array_map 100", fn() => array_map(fn($x) => $x + 1, $phpArr100), 500));
printResult(benchmark("[Cljp] map 100 realize", fn() => (function() use ($v100) {
    foreach (map_(fn($x) => $x + 1, $v100) as $_) {}
})(), 500));

printResult(benchmark("[PHP] array_filter 100", fn() => array_filter($phpArr100, fn($x) => $x % 2 === 0), 500));
printResult(benchmark("[Cljp] filter 100 realize", fn() => (function() use ($v100) {
    foreach (filter_(fn($x) => $x % 2 === 0, $v100) as $_) {}
})(), 500));

printResult(benchmark("[PHP] array_reduce 100", fn() => array_reduce($phpArr100, fn($a, $b) => $a + $b, 0), 500));
printResult(benchmark("[Cljp] reduce 100", fn() => reduce_(fn($a, $b) => $a + $b, 0, $v100), 500));

// ============================================================
// Memory Usage
// ============================================================

section("MEMORY USAGE");

function measureMemory(string $name, callable $fn): void {
    gc_collect_cycles();
    $before = memory_get_usage(true);
    $result = $fn();
    $after = memory_get_usage(true);
    $used = $after - $before;
    printf("%-45s %12s bytes\n", $name, number_format($used));
    unset($result);
}

measureMemory("Vec 1000 elements", fn() => vec(...range(1, 1000)));
measureMemory("Vec 10000 elements", fn() => vec(...range(1, 10000)));
measureMemory("Map 1000 entries", fn() => (function() {
    $m = hashMap();
    for ($i = 0; $i < 1000; $i++) $m = $m->put(kw("k$i"), $i);
    return $m;
})());
measureMemory("PHP array 1000 elements", fn() => range(1, 1000));
measureMemory("PHP array 10000 elements", fn() => range(1, 10000));

// ============================================================
// Summary
// ============================================================

section("BENCHMARK COMPLETE");
echo "PHP Version: " . PHP_VERSION . "\n";
echo "Platform: " . PHP_OS . "\n";
echo "Memory Limit: " . ini_get('memory_limit') . "\n";
