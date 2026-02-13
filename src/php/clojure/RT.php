<?php

declare(strict_types=1);

/**
 * Clojure PHP Runtime - Primitive Operations
 *
 * Core functions that the emitter calls directly.
 * These are the building blocks for clojure.core.
 */

namespace Clojure\Php;

// ============================================================
// Sequence Operations
// ============================================================

function seq(mixed $x): ?ISeq
{
    if ($x === null) {
        return null;
    }
    if ($x instanceof ISeq) {
        return $x->seq();
    }
    if ($x instanceof Seqable) {
        return $x->seq();
    }
    if (is_array($x)) {
        return count($x) === 0 ? null : new ArraySeq($x);
    }
    if (is_string($x)) {
        return $x === '' ? null : new ArraySeq(mb_str_split($x));
    }
    if ($x instanceof \Traversable) {
        $arr = iterator_to_array($x, false);
        return count($arr) === 0 ? null : new ArraySeq($arr);
    }
    throw new \InvalidArgumentException("Don't know how to create seq from " . gettype($x));
}

function first(mixed $x): mixed
{
    $s = seq($x);
    return $s?->first();
}

function rest(mixed $x): ISeq
{
    $s = seq($x);
    return $s?->more() ?? EmptySeq::instance();
}

function next_(mixed $x): ?ISeq
{
    $s = seq($x);
    return $s?->next();
}

function conj(mixed $coll, mixed ...$xs): mixed
{
    if ($coll === null) {
        $coll = plist();
    }
    foreach ($xs as $x) {
        if ($coll instanceof Vec) {
            $coll = $coll->append($x);
        } elseif ($coll instanceof Map) {
            if ($x instanceof Vec && count_($x) === 2) {
                $coll = $coll->put(nth($x, 0), nth($x, 1));
            }
        } elseif ($coll instanceof Set) {
            $coll = $coll->add($x);
        } elseif ($coll instanceof PList) {
            $coll = $coll->cons($x);
        } elseif ($coll instanceof Queue) {
            $coll = $coll->conj($x);
        } elseif ($coll instanceof ISeq) {
            $coll = new Cons($x, $coll);
        } elseif (is_array($coll)) {
            $coll[] = $x;
        }
    }
    return $coll;
}

function count_(mixed $x): int
{
    if ($x === null) {
        return 0;
    }
    if ($x instanceof \Countable) {
        return count($x);
    }
    if (is_array($x)) {
        return count($x);
    }
    if (is_string($x)) {
        return mb_strlen($x);
    }
    // For seqs, iterate
    $count = 0;
    $s = seq($x);
    while ($s !== null) {
        $count++;
        $s = $s->next();
    }
    return $count;
}

function nth(mixed $coll, int $n, mixed $notFound = null): mixed
{
    if ($coll === null) {
        return $notFound;
    }
    if ($coll instanceof Vec) {
        return $n >= 0 && $n < count($coll) ? $coll->get($n) : $notFound;
    }
    if (is_array($coll)) {
        return $coll[$n] ?? $notFound;
    }
    if (is_string($coll)) {
        return $n >= 0 && $n < mb_strlen($coll) ? mb_substr($coll, $n, 1) : $notFound;
    }
    // For seqs, drop n elements
    $s = seq($coll);
    for ($i = 0; $i < $n && $s !== null; $i++) {
        $s = $s->next();
    }
    return $s !== null ? $s->first() : $notFound;
}

// ============================================================
// Associative Operations
// ============================================================

function get(mixed $coll, mixed $key, mixed $notFound = null): mixed
{
    if ($coll === null) {
        return $notFound;
    }
    if ($coll instanceof Map) {
        return $coll->find($key) ?? $notFound;
    }
    if ($coll instanceof Vec) {
        return is_int($key) && $key >= 0 && $key < count($coll)
            ? $coll->get($key)
            : $notFound;
    }
    if ($coll instanceof Set) {
        return $coll->contains($key) ? $key : $notFound;
    }
    if (is_array($coll)) {
        return array_key_exists($key, $coll) ? $coll[$key] : $notFound;
    }
    if (is_object($coll) && $coll instanceof \ArrayAccess) {
        return isset($coll[$key]) ? $coll[$key] : $notFound;
    }
    return $notFound;
}

function assoc(mixed $coll, mixed ...$kvs): mixed
{
    if ($coll === null) {
        $coll = hashMap();
    }
    for ($i = 0; $i < count($kvs); $i += 2) {
        $k = $kvs[$i];
        $v = $kvs[$i + 1] ?? null;
        if ($coll instanceof Map) {
            $coll = $coll->put($k, $v);
        } elseif ($coll instanceof Vec) {
            $coll = $coll->update($k, $v);
        }
    }
    return $coll;
}

function dissoc(mixed $coll, mixed ...$keys): mixed
{
    if ($coll === null) {
        return null;
    }
    if ($coll instanceof Map) {
        foreach ($keys as $k) {
            $coll = $coll->remove($k);
        }
    }
    return $coll;
}

// ============================================================
// Nested Access - get-in, assoc-in, update-in, update
// ============================================================

/**
 * Returns the value in a nested associative structure.
 * (get-in {:a {:b 1}} [:a :b]) => 1
 */
function getIn(mixed $m, mixed $ks, mixed $notFound = null): mixed
{
    $s = seq($ks);
    $result = $m;
    while ($s !== null) {
        if ($result === null) {
            return $notFound;
        }
        $result = get($result, $s->first());
        $s = $s->next();
    }
    return $result ?? $notFound;
}

/**
 * Associates a value in a nested associative structure.
 * (assoc-in {} [:a :b] 1) => {:a {:b 1}}
 */
function assocIn(mixed $m, mixed $ks, mixed $v): mixed
{
    $keys = toArray($ks);
    if (count($keys) === 0) {
        return $v;
    }
    if (count($keys) === 1) {
        return assoc($m ?? hashMap(), $keys[0], $v);
    }
    $k = $keys[0];
    $restKeys = array_slice($keys, 1);
    return assoc($m ?? hashMap(), $k, assocIn(get($m, $k), $restKeys, $v));
}

/**
 * Updates a value in a nested associative structure by applying f.
 * (update-in {:a {:b 1}} [:a :b] inc) => {:a {:b 2}}
 */
function updateIn(mixed $m, mixed $ks, callable $f, mixed ...$args): mixed
{
    $keys = toArray($ks);
    if (count($keys) === 0) {
        return $f($m, ...$args);
    }
    if (count($keys) === 1) {
        return update($m ?? hashMap(), $keys[0], $f, ...$args);
    }
    $k = $keys[0];
    $restKeys = array_slice($keys, 1);
    return assoc($m ?? hashMap(), $k, updateIn(get($m, $k), $restKeys, $f, ...$args));
}

/**
 * Updates a value at key by applying f to the old value.
 * (update {:a 1} :a inc) => {:a 2}
 */
function update(mixed $m, mixed $k, callable $f, mixed ...$args): mixed
{
    if ($m === null) {
        $m = hashMap();
    }
    return assoc($m, $k, $f(get($m, $k), ...$args));
}

/**
 * Returns a map containing only those entries whose key is in keys.
 */
function selectKeys(mixed $m, mixed $ks): Map
{
    $result = hashMap();
    $s = seq($ks);
    while ($s !== null) {
        $k = $s->first();
        if (contains($m, $k)) {
            $result = $result->put($k, get($m, $k));
        }
        $s = $s->next();
    }
    return $result;
}

/**
 * Returns an empty collection of the same type.
 */
function empty_(mixed $coll): mixed
{
    if ($coll instanceof Vec) return Vec::empty();
    if ($coll instanceof Map) return Map::empty();
    if ($coll instanceof Set) return Set::empty();
    if ($coll instanceof PList) return PList::empty();
    if (is_array($coll)) return [];
    return null;
}

/**
 * Merge maps. Later values win.
 */
function merge(mixed ...$maps): ?Map
{
    $maps = array_filter($maps, fn($m) => $m !== null);
    if (count($maps) === 0) return null;

    $result = hashMap();
    foreach ($maps as $m) {
        foreach ($m as $k => $v) {
            $result = $result->put($k, $v);
        }
    }
    return $result;
}

/**
 * Merge maps with a function to resolve conflicts.
 * (merge-with + {:a 1} {:a 2}) => {:a 3}
 */
function mergeWith(callable $f, mixed ...$maps): ?Map
{
    $maps = array_filter($maps, fn($m) => $m !== null);
    if (count($maps) === 0) return null;

    $result = hashMap();
    foreach ($maps as $m) {
        foreach ($m as $k => $v) {
            if (contains($result, $k)) {
                $result = $result->put($k, $f(get($result, $k), $v));
            } else {
                $result = $result->put($k, $v);
            }
        }
    }
    return $result;
}

/**
 * Returns a map with the keys renamed.
 * (rename-keys {:a 1} {:a :b}) => {:b 1}
 */
function renameKeys(mixed $m, mixed $kmap): Map
{
    $result = $m;
    foreach ($kmap as $oldK => $newK) {
        if (contains($m, $oldK)) {
            $result = assoc(dissoc($result, $oldK), $newK, get($m, $oldK));
        }
    }
    return $result;
}

/**
 * Returns a set with val removed.
 */
function disj(Set $s, mixed ...$vals): Set
{
    foreach ($vals as $v) {
        $s = $s->remove($v);
    }
    return $s;
}

/**
 * For vector/list: returns the first/last item.
 */
function peek(mixed $coll): mixed
{
    if ($coll === null) return null;
    if ($coll instanceof Vec) {
        return count_($coll) > 0 ? $coll->get(count_($coll) - 1) : null;
    }
    if ($coll instanceof PList) {
        return $coll->first();
    }
    return first($coll);
}

/**
 * For vector: returns vector without last item.
 * For list: returns list without first item.
 * For queue: returns queue without front item.
 */
function pop_(mixed $coll): mixed
{
    if ($coll === null) {
        throw new \RuntimeException("Can't pop empty collection");
    }
    if ($coll instanceof Vec) {
        if (count_($coll) === 0) {
            throw new \RuntimeException("Can't pop empty vector");
        }
        return $coll->pop();
    }
    if ($coll instanceof PList) {
        return $coll->rest();
    }
    if ($coll instanceof Queue) {
        return $coll->pop();
    }
    throw new \RuntimeException("pop not supported on " . gettype($coll));
}

/**
 * Find - returns [key value] for key in map, or nil.
 */
function find_(mixed $m, mixed $k): ?Vec
{
    if ($m === null) return null;
    if ($m instanceof Map && $m->containsKey($k)) {
        return vec($k, $m->find($k));
    }
    return null;
}

function contains(mixed $coll, mixed $key): bool
{
    if ($coll === null) {
        return false;
    }
    if ($coll instanceof Map) {
        return $coll->find($key) !== null;
    }
    if ($coll instanceof Set) {
        return $coll->contains($key);
    }
    if ($coll instanceof Vec) {
        return is_int($key) && $key >= 0 && $key < count($coll);
    }
    if (is_array($coll)) {
        return array_key_exists($key, $coll);
    }
    return false;
}

function keys_(mixed $m): ?Vec
{
    if ($m === null) {
        return null;
    }
    $result = [];
    foreach ($m as $k => $v) {
        $result[] = $k;
    }
    return vec(...$result);
}

function vals(mixed $m): ?Vec
{
    if ($m === null) {
        return null;
    }
    $result = [];
    foreach ($m as $v) {
        $result[] = $v;
    }
    return vec(...$result);
}

// ============================================================
// Equality & Hashing
// ============================================================

function equals(mixed $a, mixed $b): bool
{
    if ($a === $b) {
        return true;
    }
    if ($a === null || $b === null) {
        return false;
    }
    if (is_object($a) && method_exists($a, 'equals')) {
        return $a->equals($b);
    }
    if (is_object($b) && method_exists($b, 'equals')) {
        return $b->equals($a);
    }
    return $a == $b;
}

function hash_(mixed $x): int
{
    if ($x === null) {
        return 0;
    }
    if (is_object($x) && method_exists($x, 'hash')) {
        return $x->hash();
    }
    if (is_string($x)) {
        return crc32($x);
    }
    if (is_int($x)) {
        return $x;
    }
    if (is_float($x)) {
        return (int)$x;
    }
    if (is_bool($x)) {
        return $x ? 1 : 0;
    }
    if (is_array($x)) {
        return crc32(serialize($x));
    }
    return crc32(spl_object_hash($x));
}

// ============================================================
// String Operations
// ============================================================

function str_(mixed ...$xs): string
{
    $result = '';
    foreach ($xs as $x) {
        if ($x === null) {
            continue;
        }
        if (is_string($x)) {
            $result .= $x;
        } elseif (is_bool($x)) {
            $result .= $x ? 'true' : 'false';
        } elseif (is_numeric($x)) {
            $result .= (string)$x;
        } elseif ($x instanceof Kw) {
            $result .= ':' . $x->fullName();
        } elseif ($x instanceof \Stringable) {
            $result .= (string)$x;
        } else {
            $result .= prStr($x);
        }
    }
    return $result;
}

// ============================================================
// Type Predicates
// ============================================================

function isNil(mixed $x): bool
{
    return $x === null;
}

function isSome(mixed $x): bool
{
    return $x !== null;
}

function isSeq(mixed $x): bool
{
    return $x instanceof ISeq;
}

function isVector(mixed $x): bool
{
    return $x instanceof Vec;
}

function isMap(mixed $x): bool
{
    return $x instanceof Map;
}

function isHashSet(mixed $x): bool
{
    return $x instanceof Set;
}

function isList(mixed $x): bool
{
    return $x instanceof PList || $x instanceof Cons || $x instanceof LazySeq;
}

function isKeyword(mixed $x): bool
{
    return $x instanceof Kw;
}

function isSymbol(mixed $x): bool
{
    return $x instanceof Sym;
}

function isFn(mixed $x): bool
{
    return is_callable($x);
}

// ============================================================
// Type Predicates (Clojure parity)
// ============================================================

function isNumber(mixed $x): bool
{
    return is_int($x) || is_float($x);
}

function isString(mixed $x): bool
{
    return is_string($x);
}

function isBoolean(mixed $x): bool
{
    return is_bool($x);
}

function isInt(mixed $x): bool
{
    return is_int($x);
}

function isFloat(mixed $x): bool
{
    return is_float($x);
}

function isPos(mixed $x): bool
{
    return is_numeric($x) && $x > 0;
}

function isNeg(mixed $x): bool
{
    return is_numeric($x) && $x < 0;
}

function isZero(mixed $x): bool
{
    return $x === 0 || $x === 0.0;
}

function isEven(int $n): bool
{
    return ($n & 1) === 0;
}

function isOdd(int $n): bool
{
    return ($n & 1) === 1;
}

function isTrue(mixed $x): bool
{
    return $x === true;
}

function isFalse(mixed $x): bool
{
    return $x === false;
}

// Note: isNil and isSome already defined above in Type Predicates section

function isNat(mixed $x): bool
{
    return is_int($x) && $x >= 0;
}

function isAssoc(mixed $x): bool
{
    return $x instanceof Map || $x instanceof Vec;
}

function isSequential(mixed $x): bool
{
    return $x instanceof Vec || $x instanceof PList || $x instanceof ISeq || is_array($x);
}

function isSorted(mixed $x): bool
{
    // PHP doesn't have sorted collections by default
    return false;
}

function isCounted(mixed $x): bool
{
    return $x instanceof \Countable || is_array($x) || is_string($x);
}

function isIndexed(mixed $x): bool
{
    return $x instanceof Vec || is_array($x);
}

function isSeqable(mixed $x): bool
{
    return $x === null || $x instanceof Seqable || $x instanceof ISeq
        || is_array($x) || is_string($x) || $x instanceof \Traversable;
}

function isReversible(mixed $x): bool
{
    return $x instanceof Vec || is_array($x);
}

function isAtom(mixed $x): bool
{
    return $x instanceof Atom;
}

function isRational(mixed $x): bool
{
    return is_int($x) || is_float($x);
}

function isInteger(mixed $x): bool
{
    return is_int($x);
}

function isDouble(mixed $x): bool
{
    return is_float($x);
}

function isColl(mixed $x): bool
{
    return $x instanceof Vec || $x instanceof Map || $x instanceof Set
        || $x instanceof PList || $x instanceof ISeq || is_array($x);
}

function isEmpty(mixed $x): bool
{
    return seq($x) === null;
}

// ============================================================
// Invocation
// ============================================================

function invoke(mixed $f, mixed ...$args): mixed
{
    if (is_callable($f)) {
        return $f(...$args);
    }
    if ($f instanceof Kw) {
        return get($args[0] ?? null, $f, $args[1] ?? null);
    }
    if ($f instanceof Map || $f instanceof Vec) {
        return get($f, $args[0] ?? null, $args[1] ?? null);
    }
    throw new \InvalidArgumentException("Cannot invoke " . gettype($f));
}

// ============================================================
// Reduced (for early termination in reduce)
// ============================================================

final class Reduced
{
    public function __construct(private readonly mixed $val)
    {
    }

    public function deref(): mixed
    {
        return $this->val;
    }
}

function reduced(mixed $val): Reduced
{
    return new Reduced($val);
}

function isReduced(mixed $x): bool
{
    return $x instanceof Reduced;
}

function unreduced(mixed $x): mixed
{
    return $x instanceof Reduced ? $x->deref() : $x;
}

// ============================================================
// Array/Collection Conversion
// ============================================================

function toArray(mixed $coll): array
{
    if ($coll === null) {
        return [];
    }
    if (is_array($coll)) {
        return $coll;
    }
    if ($coll instanceof ISeq) {
        return $coll->toArray();
    }
    if ($coll instanceof \Traversable) {
        return iterator_to_array($coll, false);
    }
    throw new \InvalidArgumentException("Cannot convert to array: " . gettype($coll));
}

// ============================================================
// Timing / Benchmarking
// ============================================================

/**
 * Get current time in microseconds (for benchmarking).
 */
function microtime_us(): float
{
    return microtime(true) * 1000000;
}

/**
 * Get current time in milliseconds.
 */
function microtime_ms(): float
{
    return microtime(true) * 1000;
}

/**
 * Get current Unix timestamp in seconds.
 */
function now_seconds(): int
{
    return time();
}

// ============================================================
// Placeholder declarations for collection types
// These will be properly defined in their respective files
// ============================================================

// Forward declarations - these classes are defined in Vec.php, Map.php, etc.
// The autoloader will load them when needed
