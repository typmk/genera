<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;
use Countable;
use IteratorAggregate;

/**
 * Persistent Hash Set - backed by a Map.
 */
final class Set implements Seqable, Countable, IteratorAggregate
{
    private ?Map $meta = null;

    private function __construct(
        private readonly Map $impl,
    ) {
    }

    public static function empty(): self
    {
        static $empty = null;
        return $empty ??= new self(Map::empty());
    }

    public static function from(array $vals): self
    {
        if (count($vals) === 0) {
            return self::empty();
        }
        // Build map directly instead of n individual adds
        $pairs = [];
        foreach ($vals as $v) {
            $pairs[] = $v;
            $pairs[] = $v;
        }
        return new self(Map::from($pairs));
    }

    /**
     * Create from an existing Map (internal use).
     */
    public static function fromMap(Map $m): self
    {
        return $m->count() === 0 ? self::empty() : new self($m);
    }

    public function count(): int
    {
        return $this->impl->count();
    }

    public function add(mixed $val): self
    {
        $newImpl = $this->impl->put($val, $val);
        if ($newImpl === $this->impl) {
            return $this;
        }
        return new self($newImpl);
    }

    public function remove(mixed $val): self
    {
        $newImpl = $this->impl->remove($val);
        if ($newImpl === $this->impl) {
            return $this;
        }
        return new self($newImpl);
    }

    public function contains(mixed $val): bool
    {
        return $this->impl->containsKey($val);
    }

    public function get(mixed $val, mixed $notFound = null): mixed
    {
        return $this->contains($val) ? $val : $notFound;
    }

    // ============================================================
    // Seq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        if ($this->count() === 0) {
            return null;
        }
        return new SetSeq($this->impl->keys(), 0);
    }

    // ============================================================
    // Set operations
    // ============================================================

    public function union(Set $other): self
    {
        if ($this->count() === 0) return $other;
        if ($other->count() === 0) return $this;

        // Collect all elements and build once
        $all = array_merge($this->toArray(), $other->toArray());
        return self::from($all);
    }

    public function intersection(Set $other): self
    {
        // Iterate smaller set for efficiency
        $smaller = $this->count() <= $other->count() ? $this : $other;
        $larger = $smaller === $this ? $other : $this;

        // Collect matches, then build once
        $matches = [];
        foreach ($smaller as $v) {
            if ($larger->contains($v)) {
                $matches[] = $v;
            }
        }

        if (count($matches) === 0) {
            return self::empty();
        }

        return self::from($matches);
    }

    public function difference(Set $other): self
    {
        $s = $this;
        foreach ($other as $v) {
            $s = $s->remove($v);
        }
        return $s;
    }

    public function isSubset(Set $other): bool
    {
        foreach ($this as $v) {
            if (!$other->contains($v)) {
                return false;
            }
        }
        return true;
    }

    public function isSuperset(Set $other): bool
    {
        return $other->isSubset($this);
    }

    // ============================================================
    // Equality & Hashing
    // ============================================================

    public function hash(): int
    {
        $h = 0;
        foreach ($this as $v) {
            $h += hash_($v);
        }
        return $h;
    }

    public function equals(mixed $other): bool
    {
        if ($this === $other) return true;
        if (!$other instanceof Set) return false;
        if ($this->count() !== $other->count()) return false;
        foreach ($this as $v) {
            if (!$other->contains($v)) {
                return false;
            }
        }
        return true;
    }

    // ============================================================
    // Meta
    // ============================================================

    public function meta(): ?Map
    {
        return $this->meta;
    }

    public function withMeta(?Map $meta): self
    {
        if ($meta === $this->meta) return $this;
        $new = clone $this;
        $new->meta = $meta;
        return $new;
    }

    // ============================================================
    // IteratorAggregate
    // ============================================================

    public function getIterator(): Traversable
    {
        foreach ($this->impl as $k => $v) {
            yield $k;
        }
    }

    public function toArray(): array
    {
        return iterator_to_array($this, false);
    }

    public function __toString(): string
    {
        return prStr($this);
    }

    /**
     * Set invocation: (#{:a :b} :a) returns :a or nil
     */
    public function __invoke(mixed $val, mixed $notFound = null): mixed
    {
        return $this->get($val, $notFound);
    }
}

/**
 * Seq over Set elements.
 * @internal
 */
final class SetSeq implements ISeq
{
    public function __construct(
        private readonly Vec $keys,
        private readonly int $i,
    ) {
    }

    public function seq(): ?ISeq
    {
        return $this->i < count_($this->keys) ? $this : null;
    }

    public function first(): mixed
    {
        return $this->keys->get($this->i);
    }

    public function next(): ?ISeq
    {
        return $this->i + 1 < count_($this->keys) ? new SetSeq($this->keys, $this->i + 1) : null;
    }

    public function more(): ISeq
    {
        return $this->next() ?? EmptySeq::instance();
    }

    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    public function count(): int
    {
        return count_($this->keys) - $this->i;
    }

    public function toArray(): array
    {
        return array_slice($this->keys->toArray(), $this->i);
    }

    public function getIterator(): Traversable
    {
        for ($j = $this->i; $j < count_($this->keys); $j++) {
            yield $this->keys->get($j);
        }
    }
}

// Constructor function
function hashSet(mixed ...$vals): Set
{
    return Set::from($vals);
}

// ============================================================
// clojure.set functions
// ============================================================

/**
 * Union of sets.
 */
function union(?Set $s1 = null, ?Set ...$sets): Set
{
    if ($s1 === null) {
        return Set::empty();
    }
    $result = $s1;
    foreach ($sets as $s) {
        if ($s !== null) {
            foreach ($s as $val) {
                $result = $result->add($val);
            }
        }
    }
    return $result;
}

/**
 * Intersection of sets.
 */
function intersection(Set $s1, Set ...$sets): Set
{
    if (count($sets) === 0) {
        return $s1;
    }

    // Find smallest set
    $smallest = $s1;
    $others = $sets;
    foreach ($sets as $s) {
        if (count_($s) < count_($smallest)) {
            $others = array_filter(
                array_merge([$smallest], $sets),
                fn($x) => $x !== $s
            );
            $smallest = $s;
        }
    }

    $result = Set::empty();
    foreach ($smallest as $val) {
        $inAll = true;
        foreach ($others as $other) {
            if (!$other->contains($val)) {
                $inAll = false;
                break;
            }
        }
        if ($inAll) {
            $result = $result->add($val);
        }
    }
    return $result;
}

/**
 * Difference of sets (s1 - s2 - s3 ...).
 */
function difference(Set $s1, Set ...$sets): Set
{
    if (count($sets) === 0) {
        return $s1;
    }

    $result = $s1;
    foreach ($sets as $s) {
        foreach ($s as $val) {
            $result = $result->remove($val);
        }
    }
    return $result;
}

/**
 * Is s1 a subset of s2?
 */
function isSubset(Set $s1, Set $s2): bool
{
    if (count_($s1) > count_($s2)) {
        return false;
    }
    foreach ($s1 as $val) {
        if (!$s2->contains($val)) {
            return false;
        }
    }
    return true;
}

/**
 * Is s1 a superset of s2?
 */
function isSuperset(Set $s1, Set $s2): bool
{
    return isSubset($s2, $s1);
}

/**
 * Select elements from set that match predicate.
 */
function selectSet(callable $pred, Set $s): Set
{
    $result = Set::empty();
    foreach ($s as $val) {
        if ($pred($val)) {
            $result = $result->add($val);
        }
    }
    return $result;
}

/**
 * Project - given a set of maps, return set of maps with only specified keys.
 */
function project(Set $xrel, mixed $ks): Set
{
    $keys = is_array($ks) ? $ks : toArray($ks);
    $result = Set::empty();
    foreach ($xrel as $map) {
        $result = $result->add(selectKeys($map, $keys));
    }
    return $result;
}

/**
 * Rename keys in a set of maps.
 */
function renameSetKeys(Set $xrel, mixed $kmap): Set
{
    $result = Set::empty();
    foreach ($xrel as $map) {
        $result = $result->add(renameKeys($map, $kmap));
    }
    return $result;
}

/**
 * Index - returns a map of val to set of maps having that val for k.
 */
function index(Set $xrel, mixed $ks): Map
{
    $keys = is_array($ks) ? $ks : toArray($ks);
    $result = hashMap();
    foreach ($xrel as $map) {
        $indexKey = selectKeys($map, $keys);
        $existing = get($result, $indexKey, Set::empty());
        $result = assoc($result, $indexKey, $existing->add($map));
    }
    return $result;
}

/**
 * Map invert - swap keys and values.
 */
function mapInvert(mixed $m): Map
{
    $result = hashMap();
    foreach (seq($m) as $entry) {
        $k = first($entry);
        $v = second($entry);
        $result = assoc($result, $v, $k);
    }
    return $result;
}

/**
 * Join - relational join of two sets of maps.
 * Named relJoin to avoid conflict with clojure.string/join.
 */
function relJoin(Set $xrel, Set $yrel, ?Map $km = null): Set
{
    if ($km === null) {
        // Natural join - find common keys
        $xFirst = first($xrel);
        $yFirst = first($yrel);
        if ($xFirst === null || $yFirst === null) {
            return Set::empty();
        }
        $xKeys = hashSet(...keys($xFirst));
        $yKeys = hashSet(...keys($yFirst));
        $commonKeys = intersection($xKeys, $yKeys);

        if (count_($commonKeys) === 0) {
            // Cross product
            $result = Set::empty();
            foreach ($xrel as $xm) {
                foreach ($yrel as $ym) {
                    $result = $result->add(merge($xm, $ym));
                }
            }
            return $result;
        }

        $km = hashMap();
        foreach ($commonKeys as $k) {
            $km = assoc($km, $k, $k);
        }
    }

    // Use smaller relation for index
    $r = count_($xrel) <= count_($yrel) ? $xrel : $yrel;
    $s = $r === $xrel ? $yrel : $xrel;
    $kSwapped = $r === $yrel;

    $k = $kSwapped ? mapInvert($km) : $km;
    $idx = index($r, keys($k));

    $result = Set::empty();
    foreach ($s as $row) {
        $projectKey = selectKeys(renameKeys($row, $k), keys($k));
        $found = get($idx, $projectKey);
        if ($found !== null) {
            foreach ($found as $match) {
                $result = $result->add(merge($row, $match));
            }
        }
    }
    return $result;
}
