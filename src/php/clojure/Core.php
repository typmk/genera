<?php

declare(strict_types=1);

/**
 * Clojure Core - Higher-order functions.
 *
 * These are the user-facing clojure.core functions.
 */

namespace Clojure\Php;

// ============================================================
// Sequence Transformations
// ============================================================

/**
 * Lazy map - returns an EagerMappedSeq for Vec/array that uses native array_map on realize.
 */
function map_(callable $f, mixed $coll, mixed ...$colls): ISeq|LazySeq
{
    if (count($colls) === 0) {
        // Fast path: Vec or array - use EagerMappedSeq
        if ($coll instanceof Vec) {
            return new EagerMappedSeq($f, $coll->toArray());
        }
        if (is_array($coll)) {
            return count($coll) === 0
                ? new LazySeq(fn() => null)
                : new EagerMappedSeq($f, $coll);
        }
        // Fallback for other seqables
        return new LazySeq(function () use ($f, $coll) {
            $s = seq($coll);
            if ($s === null) return null;
            return cons($f($s->first()), map_($f, $s->next()));
        });
    }
    // Multi-collection map
    $allColls = [$coll, ...$colls];
    return new LazySeq(function () use ($f, $allColls) {
        $seqs = array_map(fn($c) => seq($c), $allColls);
        foreach ($seqs as $s) {
            if ($s === null) return null;
        }
        $args = array_map(fn($s) => $s->first(), $seqs);
        $rests = array_map(fn($s) => $s->next(), $seqs);
        return cons($f(...$args), map_($f, ...$rests));
    });
}

/**
 * EagerMappedSeq - stores source array, applies f on access.
 * toArray() uses native array_map for speed.
 * Iteration creates minimal objects.
 */
final class EagerMappedSeq implements ISeq
{
    /** @var callable */
    private $f;
    private ?array $realized = null;

    public function __construct(
        callable $f,
        private readonly array $source,
    ) {
        $this->f = $f;
    }

    private function realize(): array
    {
        if ($this->realized === null) {
            $this->realized = \array_map($this->f, $this->source);
        }
        return $this->realized;
    }

    public function seq(): ?ISeq { return count($this->source) > 0 ? $this : null; }
    public function first(): mixed { return ($this->f)($this->source[0]); }

    public function next(): ?ISeq
    {
        if (count($this->source) <= 1) return null;
        return new ArraySeq(\array_slice($this->realize(), 1));
    }

    public function more(): ISeq { return $this->next() ?? EmptySeq::instance(); }
    public function cons(mixed $x): ISeq { return new Cons($x, $this); }
    public function count(): int { return count($this->source); }

    public function toArray(): array
    {
        return $this->realize();
    }

    public function getIterator(): \Traversable
    {
        foreach ($this->realize() as $v) {
            yield $v;
        }
    }
}

/**
 * Lazy filter - returns EagerFilteredSeq for Vec/array.
 */
function filter_(callable $pred, mixed $coll): ISeq|LazySeq
{
    // Fast path: Vec or array - use EagerFilteredSeq
    if ($coll instanceof Vec) {
        return new EagerFilteredSeq($pred, $coll->toArray());
    }
    if (is_array($coll)) {
        return new EagerFilteredSeq($pred, $coll);
    }
    // Fallback for other seqables
    return new LazySeq(function () use ($pred, $coll) {
        $s = seq($coll);
        while ($s !== null) {
            $x = $s->first();
            if ($pred($x)) {
                return cons($x, filter_($pred, $s->next()));
            }
            $s = $s->next();
        }
        return null;
    });
}

/**
 * EagerFilteredSeq - stores source array, filters on realize.
 * toArray() uses native array_filter for speed.
 */
final class EagerFilteredSeq implements ISeq
{
    /** @var callable */
    private $pred;
    private ?array $realized = null;

    public function __construct(
        callable $pred,
        private readonly array $source,
    ) {
        $this->pred = $pred;
    }

    private function realize(): array
    {
        if ($this->realized === null) {
            $this->realized = \array_values(\array_filter($this->source, $this->pred));
        }
        return $this->realized;
    }

    public function seq(): ?ISeq
    {
        $arr = $this->realize();
        return count($arr) > 0 ? $this : null;
    }

    public function first(): mixed
    {
        $arr = $this->realize();
        return $arr[0] ?? null;
    }

    public function next(): ?ISeq
    {
        $arr = $this->realize();
        if (count($arr) <= 1) return null;
        return new ArraySeq(\array_slice($arr, 1));
    }

    public function more(): ISeq { return $this->next() ?? EmptySeq::instance(); }
    public function cons(mixed $x): ISeq { return new Cons($x, $this); }
    public function count(): int { return count($this->realize()); }

    public function toArray(): array
    {
        return $this->realize();
    }

    public function getIterator(): \Traversable
    {
        foreach ($this->realize() as $v) {
            yield $v;
        }
    }
}

function remove_(callable $pred, mixed $coll): LazySeq
{
    return filter_(fn($x) => !$pred($x), $coll);
}

function keep_(callable $f, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($f, $coll) {
        $s = seq($coll);
        while ($s !== null) {
            $x = $f($s->first());
            if ($x !== null) {
                return cons($x, keep_($f, $s->next()));
            }
            $s = $s->next();
        }
        return null;
    });
}

function mapcat(callable $f, mixed ...$colls): LazySeq
{
    return concat_(...toArray(map_($f, ...$colls)));
}

function mapIndexed(callable $f, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($f, $coll) {
        $i = 0;
        $s = seq($coll);
        $step = null;
        $step = function ($i, $s) use ($f, &$step) {
            if ($s === null) return null;
            return cons($f($i, $s->first()), new LazySeq(fn() => $step($i + 1, $s->next())));
        };
        return $step(0, $s);
    });
}

// ============================================================
// Reductions
// ============================================================

/**
 * Optimized reduce with inlined arithmetic for common operations.
 *
 * When $f is a known core function (+, -, *, etc.), we inline the
 * operation instead of going through callable dispatch.
 */
function reduce_(callable $f, mixed ...$args): mixed
{
    if (count($args) === 1) {
        $coll = $args[0];
        $s = seq($coll);
        if ($s === null) return $f();
        return reduce_($f, $s->first(), $s->next());
    }

    [$init, $coll] = $args;

    // Fast path: if collection is array or Vec, use iterator directly
    if (is_array($coll)) {
        return reduceArray_($f, $init, $coll);
    }
    if ($coll instanceof Vec) {
        return reduceVec_($f, $init, $coll);
    }

    $acc = $init;
    $s = seq($coll);
    while ($s !== null) {
        $acc = $f($acc, $s->first());
        if ($acc instanceof Reduced) {
            return $acc->deref();
        }
        $s = $s->next();
    }
    return $acc;
}

/**
 * Fast reduce over PHP arrays - avoids seq overhead.
 */
function reduceArray_(callable $f, mixed $init, array $arr): mixed
{
    $acc = $init;
    foreach ($arr as $x) {
        $acc = $f($acc, $x);
        if ($acc instanceof Reduced) {
            return $acc->deref();
        }
    }
    return $acc;
}

/**
 * Fast reduce over Vec - converts to array to avoid method call overhead.
 * For small-medium vecs, the toArray() cost is offset by faster iteration.
 */
function reduceVec_(callable $f, mixed $init, Vec $v): mixed
{
    // For reduce, converting to array is faster than n method calls
    // toArray() is O(n) but iteration is then native PHP speed
    $arr = $v->toArray();
    $acc = $init;
    foreach ($arr as $x) {
        $acc = $f($acc, $x);
        if ($acc instanceof Reduced) {
            return $acc->deref();
        }
    }
    return $acc;
}

// ============================================================
// Inlined arithmetic reducers - bypass callable dispatch
// ============================================================

/**
 * Sum - inlined + for reduce.
 */
function sum_(mixed $coll): int|float
{
    if (is_array($coll)) {
        return array_sum($coll);
    }
    if ($coll instanceof Vec) {
        $sum = 0;
        foreach ($coll as $x) {
            $sum += $x;
        }
        return $sum;
    }
    $sum = 0;
    $s = seq($coll);
    while ($s !== null) {
        $sum += $s->first();
        $s = $s->next();
    }
    return $sum;
}

/**
 * Product - inlined * for reduce.
 */
function product_(mixed $coll): int|float
{
    if (is_array($coll)) {
        return array_product($coll);
    }
    $product = 1;
    $s = seq($coll);
    while ($s !== null) {
        $product *= $s->first();
        $s = $s->next();
    }
    return $product;
}

/**
 * Max - inlined max for reduce.
 */
function max_(mixed $coll): mixed
{
    if (is_array($coll)) {
        return count($coll) > 0 ? max($coll) : null;
    }
    $s = seq($coll);
    if ($s === null) return null;
    $result = $s->first();
    $s = $s->next();
    while ($s !== null) {
        $x = $s->first();
        if ($x > $result) $result = $x;
        $s = $s->next();
    }
    return $result;
}

/**
 * Min - inlined min for reduce.
 */
function min_(mixed $coll): mixed
{
    if (is_array($coll)) {
        return count($coll) > 0 ? min($coll) : null;
    }
    $s = seq($coll);
    if ($s === null) return null;
    $result = $s->first();
    $s = $s->next();
    while ($s !== null) {
        $x = $s->first();
        if ($x < $result) $result = $x;
        $s = $s->next();
    }
    return $result;
}

function reduceKv(callable $f, mixed $init, mixed $coll): mixed
{
    $acc = $init;
    foreach ($coll as $k => $v) {
        $acc = $f($acc, $k, $v);
        if ($acc instanceof Reduced) {
            return $acc->deref();
        }
    }
    return $acc;
}

function reductions(callable $f, mixed ...$args): LazySeq
{
    if (count($args) === 1) {
        $coll = $args[0];
        return new LazySeq(function () use ($f, $coll) {
            $s = seq($coll);
            if ($s === null) return cons($f(), null);
            return reductions($f, $s->first(), $s->next());
        });
    }

    [$init, $coll] = $args;
    return new LazySeq(function () use ($f, $init, $coll) {
        return cons($init, new LazySeq(function () use ($f, $init, $coll) {
            $s = seq($coll);
            if ($s === null) return null;
            $next = $f($init, $s->first());
            return reductions($f, $next, $s->next());
        }));
    });
}

// ============================================================
// Sequence Slicing
// ============================================================

/**
 * Take first n elements - optimized for Vec/array.
 */
function take_(int $n, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($n, $coll) {
        if ($n <= 0) return null;

        // Fast path: Vec or array - use array_slice
        if ($coll instanceof Vec) {
            $arr = array_slice($coll->toArray(), 0, $n);
            return ChunkedSeq::fromArray($arr);
        }
        if (is_array($coll)) {
            $arr = array_slice($coll, 0, $n);
            return ChunkedSeq::fromArray($arr);
        }

        // Fallback for lazy seqs
        $s = seq($coll);
        if ($s === null) return null;
        return cons($s->first(), take_($n - 1, $s->next()));
    });
}

/**
 * Drop first n elements - optimized for Vec/array.
 */
function drop_(int $n, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($n, $coll) {
        // Fast path: Vec or array - use array_slice
        if ($coll instanceof Vec) {
            $arr = array_slice($coll->toArray(), $n);
            return ChunkedSeq::fromArray($arr);
        }
        if (is_array($coll)) {
            $arr = array_slice($coll, $n);
            return ChunkedSeq::fromArray($arr);
        }

        // Fallback for lazy seqs
        $s = seq($coll);
        for ($i = 0; $i < $n && $s !== null; $i++) {
            $s = $s->next();
        }
        return $s;
    });
}

function takeWhile(callable $pred, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($pred, $coll) {
        $s = seq($coll);
        if ($s === null) return null;
        $x = $s->first();
        if (!$pred($x)) return null;
        return cons($x, takeWhile($pred, $s->next()));
    });
}

function dropWhile(callable $pred, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($pred, $coll) {
        $s = seq($coll);
        while ($s !== null && $pred($s->first())) {
            $s = $s->next();
        }
        return $s;
    });
}

function takeLast(int $n, mixed $coll): LazySeq
{
    $arr = toArray($coll);
    return new LazySeq(fn() => new ArraySeq(array_slice($arr, -$n)));
}

function dropLast(int $n, mixed $coll): LazySeq
{
    $arr = toArray($coll);
    return new LazySeq(fn() => new ArraySeq(array_slice($arr, 0, -$n)));
}

function splitAt(int $n, mixed $coll): Vec
{
    return vec(take_($n, $coll), drop_($n, $coll));
}

function splitWith(callable $pred, mixed $coll): Vec
{
    return vec(takeWhile($pred, $coll), dropWhile($pred, $coll));
}

// ============================================================
// Sequence Combination
// ============================================================

function concat_(mixed ...$colls): LazySeq
{
    return new LazySeq(function () use ($colls) {
        foreach ($colls as $c) {
            $s = seq($c);
            if ($s !== null) {
                $rest = array_slice($colls, 1);
                return cons($s->first(), concat_($s->next(), ...$rest));
            }
            $colls = array_slice($colls, 1);
        }
        return null;
    });
}

function interleave(mixed ...$colls): LazySeq
{
    return new LazySeq(function () use ($colls) {
        $seqs = array_map(fn($c) => seq($c), $colls);
        foreach ($seqs as $s) {
            if ($s === null) return null;
        }
        $firsts = array_map(fn($s) => $s->first(), $seqs);
        $rests = array_map(fn($s) => $s->next(), $seqs);
        return concat_(new ArraySeq($firsts), interleave(...$rests));
    });
}

function interpose(mixed $sep, mixed $coll): LazySeq
{
    return drop_(1, interleave(repeat_($sep), $coll));
}

function flatten_(mixed $coll): LazySeq
{
    return new LazySeq(function () use ($coll) {
        $s = seq($coll);
        if ($s === null) return null;
        $x = $s->first();
        if (isColl($x) && !isMap($x)) {
            return concat_(flatten_($x), flatten_($s->next()));
        }
        return cons($x, flatten_($s->next()));
    });
}

// ============================================================
// Sequence Generation
// ============================================================

/**
 * Repeat x infinitely or n times - optimized for finite case.
 */
function repeat_(mixed $x, ?int $n = null): LazySeq
{
    if ($n !== null) {
        // Finite repeat - use array_fill for speed
        return new LazySeq(fn() => ChunkedSeq::fromArray(array_fill(0, $n, $x)));
    }
    // Infinite - use chunked generation
    return new LazySeq(fn() => repeatChunked_($x));
}

/**
 * Internal chunked infinite repeat.
 */
function repeatChunked_(mixed $x): ISeq
{
    // Generate 32 elements at a time
    $chunk = array_fill(0, 32, $x);
    return ChunkedSeq::fromArrayWithRest($chunk, fn() => repeatChunked_($x));
}

function repeatedly(callable $f, ?int $n = null): LazySeq
{
    if ($n !== null) {
        return take_($n, repeatedly($f));
    }
    return new LazySeq(fn() => cons($f(), repeatedly($f)));
}

function iterate_(callable $f, mixed $x): LazySeq
{
    return new LazySeq(fn() => cons($x, iterate_($f, $f($x))));
}

/**
 * Range - generates numbers lazily in chunks.
 */
function range_(mixed ...$args): LazySeq
{
    $start = 0;
    $end = null;
    $step = 1;

    if (count($args) === 0) {
        $end = PHP_INT_MAX;
    } elseif (count($args) === 1) {
        $end = $args[0];
    } elseif (count($args) === 2) {
        [$start, $end] = $args;
    } else {
        [$start, $end, $step] = $args;
    }

    return new LazySeq(fn() => rangeChunked_($start, $end, $step));
}

/**
 * Internal chunked range - generates 32 numbers at a time.
 */
function rangeChunked_(int|float $start, int|float $end, int|float $step): ?ISeq
{
    if (($step > 0 && $start >= $end) || ($step < 0 && $start <= $end)) {
        return null;
    }

    $chunkSize = 32;
    $chunk = [];

    $val = $start;
    for ($i = 0; $i < $chunkSize; $i++) {
        if (($step > 0 && $val >= $end) || ($step < 0 && $val <= $end)) {
            break;
        }
        $chunk[] = $val;
        $val += $step;
    }

    if (count($chunk) === 0) {
        return null;
    }

    $nextStart = $start + ($chunkSize * $step);
    return ChunkedSeq::fromArrayWithRest(
        $chunk,
        (($step > 0 && $nextStart < $end) || ($step < 0 && $nextStart > $end))
            ? fn() => rangeChunked_($nextStart, $end, $step)
            : null
    );
}

function cycle_(mixed $coll): LazySeq
{
    $arr = toArray($coll);
    if (count($arr) === 0) {
        return new LazySeq(fn() => null);
    }
    return new LazySeq(fn() => concat_(new ArraySeq($arr), cycle_($arr)));
}

// ============================================================
// Sequence Predicates
// ============================================================

function every(callable $pred, mixed $coll): bool
{
    $s = seq($coll);
    while ($s !== null) {
        if (!$pred($s->first())) {
            return false;
        }
        $s = $s->next();
    }
    return true;
}

function some_(callable $pred, mixed $coll): mixed
{
    $s = seq($coll);
    while ($s !== null) {
        $result = $pred($s->first());
        if ($result) {
            return $result;
        }
        $s = $s->next();
    }
    return null;
}

function notEvery(callable $pred, mixed $coll): bool
{
    return !every($pred, $coll);
}

function notAny(callable $pred, mixed $coll): bool
{
    return !some_($pred, $coll);
}

// ============================================================
// Eager operations
// ============================================================

/**
 * Eager map - returns Vec directly.
 * Optimized to use array_map for single-collection case.
 */
function mapv(callable $f, mixed ...$colls): Vec
{
    if (count($colls) === 1) {
        $coll = $colls[0];
        // Fast path: convert to array, use native array_map
        if (is_array($coll)) {
            return Vec::from(array_map($f, $coll));
        }
        if ($coll instanceof Vec) {
            return Vec::from(array_map($f, $coll->toArray()));
        }
    }
    return vec(...toArray(map_($f, ...$colls)));
}

/**
 * Eager filter - returns Vec directly.
 * Optimized to use array_filter.
 */
function filterv(callable $pred, mixed $coll): Vec
{
    if (is_array($coll)) {
        return Vec::from(array_values(array_filter($coll, $pred)));
    }
    if ($coll instanceof Vec) {
        return Vec::from(array_values(array_filter($coll->toArray(), $pred)));
    }
    return vec(...toArray(filter_($pred, $coll)));
}

function intoArray(mixed $coll): array
{
    return toArray($coll);
}

function into(mixed $to, mixed $from): mixed
{
    return reduce_(fn($acc, $x) => conj($acc, $x), $to, $from);
}

// ============================================================
// Grouping & Partitioning
// ============================================================

function groupBy(callable $f, mixed $coll): Map
{
    return reduce_(function ($m, $x) use ($f) {
        $k = $f($x);
        $existing = get($m, $k, vec());
        return assoc($m, $k, conj($existing, $x));
    }, hashMap(), $coll);
}

function partition_(int $n, mixed $coll, ?int $step = null, mixed $pad = null): LazySeq
{
    $step ??= $n;
    return new LazySeq(function () use ($n, $step, $pad, $coll) {
        $s = seq($coll);
        if ($s === null) return null;

        $chunk = toArray(take_($n, $s));
        if (count($chunk) === $n) {
            return cons(vec(...$chunk), partition_($n, drop_($step, $s), $step, $pad));
        }
        if ($pad !== null) {
            $padded = array_merge($chunk, array_slice(toArray($pad), 0, $n - count($chunk)));
            return cons(vec(...$padded), null);
        }
        return null;
    });
}

function partitionAll(int $n, mixed $coll, ?int $step = null): LazySeq
{
    $step ??= $n;
    return new LazySeq(function () use ($n, $step, $coll) {
        $s = seq($coll);
        if ($s === null) return null;
        $chunk = toArray(take_($n, $s));
        return cons(vec(...$chunk), partitionAll($n, drop_($step, $s), $step));
    });
}

function partitionBy(callable $f, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($f, $coll) {
        $s = seq($coll);
        if ($s === null) return null;

        $x = $s->first();
        $v = $f($x);
        $run = [$x];
        $s = $s->next();

        while ($s !== null) {
            $x = $s->first();
            if ($f($x) !== $v) {
                return cons(vec(...$run), partitionBy($f, cons($x, $s->next())));
            }
            $run[] = $x;
            $s = $s->next();
        }

        return cons(vec(...$run), null);
    });
}

// ============================================================
// Sorting
// ============================================================

function sort_(mixed $coll, ?callable $cmp = null): Vec
{
    $arr = toArray($coll);
    if ($cmp !== null) {
        usort($arr, $cmp);
    } else {
        usort($arr, fn($a, $b) => $a <=> $b);
    }
    return vec(...$arr);
}

function sortBy(callable $keyfn, mixed $coll, ?callable $cmp = null): Vec
{
    $arr = toArray($coll);
    $cmp ??= fn($a, $b) => $a <=> $b;
    usort($arr, fn($a, $b) => $cmp($keyfn($a), $keyfn($b)));
    return vec(...$arr);
}

function reverse_(mixed $coll): mixed
{
    if (is_string($coll)) {
        // Reverse string (handles multibyte)
        return implode('', array_reverse(mb_str_split($coll)));
    }
    return vec(...array_reverse(toArray($coll)));
}

// ============================================================
// Misc
// ============================================================

function distinct_(mixed $coll): LazySeq
{
    $seen = hashSet();
    return new LazySeq(function () use ($coll, &$seen) {
        $step = null;
        $step = function ($s) use (&$seen, &$step) {
            while ($s !== null) {
                $x = $s->first();
                if (!$seen->contains($x)) {
                    $seen = $seen->add($x);
                    return cons($x, new LazySeq(fn() => $step($s->next())));
                }
                $s = $s->next();
            }
            return null;
        };
        return $step(seq($coll));
    });
}

function dedupe(mixed $coll): LazySeq
{
    return new LazySeq(function () use ($coll) {
        $s = seq($coll);
        if ($s === null) return null;

        $prev = new \stdClass(); // unique sentinel
        $step = null;
        $step = function ($s, $prev) use (&$step) {
            while ($s !== null) {
                $x = $s->first();
                if (!equals($x, $prev)) {
                    return cons($x, new LazySeq(fn() => $step($s->next(), $x)));
                }
                $s = $s->next();
            }
            return null;
        };
        return $step($s, $prev);
    });
}

function frequencies(mixed $coll): Map
{
    return reduce_(function ($m, $x) {
        return assoc($m, $x, (get($m, $x, 0)) + 1);
    }, hashMap(), $coll);
}

function zipmap(mixed $keys, mixed $vals): Map
{
    $m = hashMap();
    $ks = seq($keys);
    $vs = seq($vals);
    while ($ks !== null && $vs !== null) {
        $m = $m->put($ks->first(), $vs->first());
        $ks = $ks->next();
        $vs = $vs->next();
    }
    return $m;
}

// ============================================================
// Additional Sequence Operations
// ============================================================

/**
 * Returns all but the last element.
 */
function butlast(mixed $coll): ?LazySeq
{
    $arr = toArray($coll);
    if (count($arr) <= 1) return null;
    return new LazySeq(fn() => new ArraySeq(array_slice($arr, 0, -1)));
}

/**
 * Returns every nth element.
 */
function takeNth(int $n, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($n, $coll) {
        $s = seq($coll);
        if ($s === null) return null;
        return cons($s->first(), takeNth($n, drop_($n, $s)));
    });
}

/**
 * Returns a random permutation of the collection.
 */
function shuffle_(mixed $coll): Vec
{
    $arr = toArray($coll);
    shuffle($arr);
    return vec(...$arr);
}

/**
 * Returns a random element from the collection.
 */
function randNth(mixed $coll): mixed
{
    $arr = toArray($coll);
    if (count($arr) === 0) return null;
    return $arr[array_rand($arr)];
}

/**
 * Returns the nth rest of coll, or nil if exhausted.
 */
function nthrest(mixed $coll, int $n): mixed
{
    $s = $coll;
    for ($i = 0; $i < $n && $s !== null; $i++) {
        $s = rest($s);
    }
    return $s;
}

/**
 * Returns the nth next of coll, or nil if exhausted.
 */
function nthnext(mixed $coll, int $n): ?ISeq
{
    $s = seq($coll);
    for ($i = 0; $i < $n && $s !== null; $i++) {
        $s = $s->next();
    }
    return $s;
}

/**
 * Returns a lazy seq with items replaced according to smap.
 */
function replace_(mixed $smap, mixed $coll): LazySeq
{
    return map_(fn($x) => contains($smap, $x) ? get($smap, $x) : $x, $coll);
}

/**
 * Returns the second element.
 */
function second(mixed $coll): mixed
{
    return first(next_($coll));
}

/**
 * Returns the third element.
 */
function third(mixed $coll): mixed
{
    return first(next_(next_($coll)));
}

/**
 * Returns the fourth element.
 */
function fourth(mixed $coll): mixed
{
    return first(next_(next_(next_($coll))));
}

/**
 * Returns the last element (eager - walks entire seq).
 */
function last_(mixed $coll): mixed
{
    $s = seq($coll);
    if ($s === null) return null;
    while (true) {
        $next = $s->next();
        if ($next === null) return $s->first();
        $s = $next;
    }
}

/**
 * keep-indexed - keeps non-nil results of (f index item).
 */
function keepIndexed(callable $f, mixed $coll): LazySeq
{
    return new LazySeq(function () use ($f, $coll) {
        $step = null;
        $step = function ($i, $s) use ($f, &$step) {
            while ($s !== null) {
                $x = $f($i, $s->first());
                if ($x !== null) {
                    return cons($x, new LazySeq(fn() => $step($i + 1, $s->next())));
                }
                $i++;
                $s = $s->next();
            }
            return null;
        };
        return $step(0, seq($coll));
    });
}

// ============================================================
// Higher-Order Functions
// ============================================================

/**
 * Returns the identity of its argument.
 */
function identity(mixed $x): mixed
{
    return $x;
}

/**
 * Returns a function that always returns x.
 */
function constantly(mixed $x): callable
{
    return fn(mixed ...$_) => $x;
}

/**
 * Compose functions right to left.
 * (comp f g h) => fn($x) => f(g(h($x)))
 */
function comp(callable ...$fns): callable
{
    if (count($fns) === 0) {
        return fn(mixed $x) => $x;
    }
    if (count($fns) === 1) {
        return $fns[0];
    }
    return function (mixed ...$args) use ($fns) {
        $fns = array_reverse($fns);
        $result = ($fns[0])(...$args);
        for ($i = 1; $i < count($fns); $i++) {
            $result = ($fns[$i])($result);
        }
        return $result;
    };
}

/**
 * Partial application - returns a function with some args pre-filled.
 */
function partial(callable $f, mixed ...$args): callable
{
    return fn(mixed ...$more) => $f(...$args, ...$more);
}

/**
 * Returns a function that is the complement (logical not) of f.
 */
function complement(callable $f): callable
{
    return fn(mixed ...$args) => !$f(...$args);
}

/**
 * Returns a function that calls each fn with args and returns a vector of results.
 */
function juxt(callable ...$fns): callable
{
    return fn(mixed ...$args) => vec(...array_map(fn($f) => $f(...$args), $fns));
}

/**
 * Returns a function that replaces nil arguments with defaults.
 * (fnil f default) => fn($x) => f($x ?? default)
 */
function fnil(callable $f, mixed ...$defaults): callable
{
    return function (mixed ...$args) use ($f, $defaults) {
        $fixed = [];
        for ($i = 0; $i < count($args); $i++) {
            $fixed[] = $args[$i] ?? ($defaults[$i] ?? null);
        }
        return $f(...$fixed);
    };
}

/**
 * Returns a memoized version of f.
 */
function memoize(callable $f): callable
{
    $cache = [];
    return function (mixed ...$args) use ($f, &$cache) {
        $key = serialize($args);
        if (!array_key_exists($key, $cache)) {
            $cache[$key] = $f(...$args);
        }
        return $cache[$key];
    };
}

/**
 * Apply function to arguments.
 * Last arg should be a collection that gets spread.
 */
function apply(callable $f, mixed ...$args): mixed
{
    if (count($args) === 0) {
        return $f();
    }
    $last = array_pop($args);
    $spread = toArray($last);
    return $f(...$args, ...$spread);
}

/**
 * Returns the result of threading x through fns.
 * Equivalent to (-> x (f1) (f2) ...).
 */
function threadFirst(mixed $x, callable ...$fns): mixed
{
    foreach ($fns as $f) {
        $x = $f($x);
    }
    return $x;
}

/**
 * Returns a fn that calls f, then returns its first arg.
 * Useful for side-effectful operations.
 */
function doto(mixed $x, callable ...$fns): mixed
{
    foreach ($fns as $f) {
        $f($x);
    }
    return $x;
}

/**
 * Returns a function that returns true if all predicates return true.
 */
function everyPred(callable ...$preds): callable
{
    return function (mixed ...$args) use ($preds) {
        foreach ($preds as $p) {
            foreach ($args as $arg) {
                if (!$p($arg)) return false;
            }
        }
        return true;
    };
}

/**
 * Returns a function that returns true if any predicate returns true.
 */
function someFn(callable ...$preds): callable
{
    return function (mixed ...$args) use ($preds) {
        foreach ($preds as $p) {
            foreach ($args as $arg) {
                if ($p($arg)) return true;
            }
        }
        return false;
    };
}

/**
 * Calls f with the supplied arguments.
 */
function trampoline(callable $f, mixed ...$args): mixed
{
    $result = $f(...$args);
    while (is_callable($result)) {
        $result = $result();
    }
    return $result;
}

/**
 * Calls f once per unique set of arguments (caches result).
 * Similar to memoize but uses Atom for thread-safety.
 */
function memoize1(callable $f): callable
{
    $cache = atom(hashMap());
    return function (mixed ...$args) use ($f, $cache) {
        $key = vec(...$args);
        $c = deref($cache);
        if (contains($c, $key)) {
            return get($c, $key);
        }
        $result = $f(...$args);
        swap($cache, fn($c) => assoc($c, $key, $result));
        return $result;
    };
}

// ============================================================
// Math Functions
// ============================================================

function inc(int|float $n): int|float
{
    return $n + 1;
}

function dec(int|float $n): int|float
{
    return $n - 1;
}

/**
 * Quotient of dividing numerator by denominator.
 */
function quot(int $num, int $div): int
{
    return intdiv($num, $div);
}

/**
 * Remainder of dividing numerator by denominator.
 */
function rem(int|float $num, int|float $div): int|float
{
    return $num % $div;
}

/**
 * Modulus (always positive).
 */
function mod(int|float $num, int|float $div): int|float
{
    $r = $num % $div;
    return ($r < 0) ? $r + abs($div) : $r;
}

function abs_(int|float $n): int|float
{
    return abs($n);
}

function rand_(float $max = 1.0): float
{
    return mt_rand() / mt_getrandmax() * $max;
}

function randInt(int $n): int
{
    return mt_rand(0, $n - 1);
}

// Note: max_ and min_ are defined above for collection aggregation.
// Use PHP's max() / min() for variadic comparison.

/**
 * Returns the greatest common divisor.
 */
function gcd(int $a, int $b): int
{
    while ($b !== 0) {
        $t = $b;
        $b = $a % $b;
        $a = $t;
    }
    return abs($a);
}

/**
 * Returns the least common multiple.
 */
function lcm(int $a, int $b): int
{
    if ($a === 0 || $b === 0) return 0;
    return abs($a * $b) / gcd($a, $b);
}

function pow_(int|float $base, int|float $exp): int|float
{
    return pow($base, $exp);
}

function sqrt_(int|float $n): float
{
    return sqrt($n);
}

function floor_(int|float $n): int
{
    return (int) floor($n);
}

function ceil_(int|float $n): int
{
    return (int) ceil($n);
}

function round_(int|float $n, int $precision = 0): float
{
    return round($n, $precision);
}

// ============================================================
// Comparison Functions
// ============================================================

/**
 * Compare two values (like Java's compareTo).
 * Returns negative, zero, or positive.
 */
function compare(mixed $x, mixed $y): int
{
    if ($x === $y) return 0;
    if ($x === null) return -1;
    if ($y === null) return 1;
    if (is_numeric($x) && is_numeric($y)) {
        return $x <=> $y;
    }
    if (is_string($x) && is_string($y)) {
        return strcmp($x, $y);
    }
    if ($x instanceof Kw && $y instanceof Kw) {
        return strcmp($x->fullName(), $y->fullName());
    }
    if ($x instanceof Sym && $y instanceof Sym) {
        return strcmp($x->fullName(), $y->fullName());
    }
    if ($x instanceof Vec && $y instanceof Vec) {
        $lenCmp = count_($x) <=> count_($y);
        if ($lenCmp !== 0) return $lenCmp;
        $n = count_($x);
        for ($i = 0; $i < $n; $i++) {
            $c = compare(nth($x, $i), nth($y, $i));
            if ($c !== 0) return $c;
        }
        return 0;
    }
    return gettype($x) <=> gettype($y);
}

/**
 * Identity comparison (same object).
 */
function identical(mixed $x, mixed $y): bool
{
    return $x === $y;
}

/**
 * Not equal.
 */
function notEquals(mixed $x, mixed $y): bool
{
    return !equals($x, $y);
}

// ============================================================
// String Functions
// ============================================================

/**
 * Substring.
 */
function subs(string $s, int $start, ?int $end = null): string
{
    if ($end === null) {
        return mb_substr($s, $start);
    }
    return mb_substr($s, $start, $end - $start);
}

/**
 * Split string by regex or string.
 */
function split(string $s, string $re, ?int $limit = null): Vec
{
    $parts = $limit !== null
        ? preg_split($re, $s, $limit)
        : preg_split($re, $s);
    return vec(...$parts);
}

/**
 * Join collection with separator.
 */
function join_(mixed $separator, mixed $coll = null): string
{
    if ($coll === null) {
        $coll = $separator;
        $separator = '';
    }
    return implode($separator, toArray($coll));
}

/**
 * Replace occurrences of match with replacement.
 */
function strReplace(string $s, string $match, string $replacement): string
{
    // If match looks like regex
    if (preg_match('/^\/.*\/[a-z]*$/', $match)) {
        return preg_replace($match, $replacement, $s);
    }
    return str_replace($match, $replacement, $s);
}

/**
 * Replace first occurrence.
 */
function strReplaceFirst(string $s, string $match, string $replacement): string
{
    if (preg_match('/^\/.*\/[a-z]*$/', $match)) {
        return preg_replace($match, $replacement, $s, 1);
    }
    $pos = strpos($s, $match);
    if ($pos === false) return $s;
    return substr_replace($s, $replacement, $pos, strlen($match));
}

function upperCase(string $s): string
{
    return mb_strtoupper($s);
}

function lowerCase(string $s): string
{
    return mb_strtolower($s);
}

function capitalize(string $s): string
{
    if ($s === '') return $s;
    return mb_strtoupper(mb_substr($s, 0, 1)) . mb_strtolower(mb_substr($s, 1));
}

function trim_(string $s): string
{
    return trim($s);
}

function trimL(string $s): string
{
    return ltrim($s);
}

function trimR(string $s): string
{
    return rtrim($s);
}

function isBlank(?string $s): bool
{
    return $s === null || trim($s) === '';
}

function startsWith(string $s, string $prefix): bool
{
    return str_starts_with($s, $prefix);
}

function endsWith(string $s, string $suffix): bool
{
    return str_ends_with($s, $suffix);
}

function includes(string $s, string $substr): bool
{
    return str_contains($s, $substr);
}

// Note: reverse_ defined above in Sorting section

// ============================================================
// I/O Functions
// ============================================================

/**
 * Read entire file as string.
 */
function slurp(string $path, ?string $encoding = null): string
{
    $content = file_get_contents($path);
    if ($content === false) {
        throw new \RuntimeException("Cannot read file: $path");
    }
    if ($encoding !== null && strtolower($encoding) !== 'utf-8') {
        $content = mb_convert_encoding($content, 'UTF-8', $encoding);
    }
    return $content;
}

/**
 * Write string to file.
 */
function spit(string $path, string $content, ?array $opts = null): void
{
    $append = $opts['append'] ?? false;
    $flags = $append ? FILE_APPEND : 0;
    $result = file_put_contents($path, $content, $flags);
    if ($result === false) {
        throw new \RuntimeException("Cannot write file: $path");
    }
}

// ============================================================
// clojure.walk functions
// ============================================================

/**
 * Recursively transforms all map keys from strings to keywords.
 */
function keywordizeKeys(mixed $m): mixed
{
    return postwalk(function ($x) {
        if ($x instanceof Map) {
            $result = hashMap();
            foreach ($x as $k => $v) {
                if (is_string($k)) {
                    $result = assoc($result, kw($k), $v);
                } else {
                    $result = assoc($result, $k, $v);
                }
            }
            return $result;
        }
        return $x;
    }, $m);
}

/**
 * Recursively transforms all map keys from keywords to strings.
 */
function stringifyKeys(mixed $m): mixed
{
    return postwalk(function ($x) {
        if ($x instanceof Map) {
            $result = hashMap();
            foreach ($x as $k => $v) {
                if ($k instanceof Kw) {
                    $result = assoc($result, $k->name(), $v);
                } else {
                    $result = assoc($result, $k, $v);
                }
            }
            return $result;
        }
        return $x;
    }, $m);
}

/**
 * Traverses form depth-first, calls f on each element, returns modified form.
 * Applies f after visiting children (post-order).
 */
function postwalk(callable $f, mixed $form): mixed
{
    return walk(fn($x) => postwalk($f, $x), $f, $form);
}

/**
 * Like postwalk but applies f before visiting children (pre-order).
 */
function prewalk(callable $f, mixed $form): mixed
{
    return walk(fn($x) => prewalk($f, $x), identity(...), $f($form));
}

/**
 * Core walk function. Traverses form.
 * inner is applied to each element before outer is applied to the result.
 */
function walk(callable $inner, callable $outer, mixed $form): mixed
{
    if ($form instanceof Map) {
        $result = hashMap();
        // Map iterator yields $k => $v
        foreach ($form as $k => $v) {
            $result = assoc($result, $inner($k), $inner($v));
        }
        return $outer($result);
    }
    if ($form instanceof Vec) {
        $arr = [];
        foreach ($form as $x) {
            $arr[] = $inner($x);
        }
        return $outer(vec(...$arr));
    }
    if ($form instanceof Set) {
        $arr = [];
        foreach ($form as $x) {
            $arr[] = $inner($x);
        }
        return $outer(hashSet(...$arr));
    }
    if ($form instanceof PList) {
        $arr = [];
        foreach ($form as $x) {
            $arr[] = $inner($x);
        }
        return $outer(plist(...$arr));
    }
    if ($form instanceof ISeq) {
        // For lazy seqs, keep it lazy
        return $outer(map_($inner, $form));
    }
    if (is_array($form)) {
        return $outer(array_map($inner, $form));
    }
    // Scalar or unknown - return as-is after applying outer
    return $outer($form);
}

/**
 * Replaces all occurrences of smap keys with their values in form.
 */
function postwalkReplace(mixed $smap, mixed $form): mixed
{
    return postwalk(fn($x) => contains($smap, $x) ? get($smap, $x) : $x, $form);
}

/**
 * Like postwalk-replace but pre-order.
 */
function prewalkReplace(mixed $smap, mixed $form): mixed
{
    return prewalk(fn($x) => contains($smap, $x) ? get($smap, $x) : $x, $form);
}

/**
 * Macroexpand all macros in form (used by compiler).
 */
function macroexpandAll(mixed $form, ?callable $expander = null): mixed
{
    if ($expander === null) {
        return $form;
    }
    return prewalk(fn($x) => $expander($x), $form);
}

// I/O functions are in Printer.php
