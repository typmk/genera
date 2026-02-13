<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;
use ArrayAccess;
use Countable;
use IteratorAggregate;

/**
 * Persistent Vector - bit-partitioned trie with tail optimization.
 *
 * @see https://hypirion.com/musings/understanding-persistent-vector-pt-1
 */
final class Vec implements Seqable, Countable, IteratorAggregate, ArrayAccess
{
    private const BITS = 5;
    private const WIDTH = 32; // 2^5
    private const MASK = 31;  // WIDTH - 1

    private ?Map $meta = null;
    private ?int $hashCache = null;

    // Cached tail offset - avoids count($tail) on every access
    private readonly int $tailOff;

    private function __construct(
        private readonly int $cnt,
        private readonly int $shift,
        private readonly array $root,
        private readonly array $tail,
    ) {
        $this->tailOff = $cnt - \count($tail);
    }

    public static function empty(): self
    {
        static $empty = null;
        return $empty ??= new self(0, self::BITS, [], []);
    }

    public static function from(array $vals): self
    {
        $cnt = count($vals);
        if ($cnt === 0) {
            return self::empty();
        }

        // Fast path: small vectors fit entirely in tail
        if ($cnt <= self::WIDTH) {
            return new self($cnt, self::BITS, [], array_values($vals));
        }

        // Build tree efficiently for larger vectors
        // Split into full chunks + tail
        $tailStart = $cnt - (($cnt - 1) & self::MASK) - 1;
        $tail = array_slice($vals, $tailStart);
        $treeVals = array_slice($vals, 0, $tailStart);

        // Build leaf nodes (chunks of 32)
        $leaves = array_chunk($treeVals, self::WIDTH);

        // Build tree from leaves up
        $shift = self::BITS;
        $nodes = $leaves;

        while (count($nodes) > self::WIDTH) {
            $nodes = array_chunk($nodes, self::WIDTH);
            $shift += self::BITS;
        }

        // Handle case where nodes fit in single root
        $root = count($nodes) === 1 && $shift === self::BITS
            ? $nodes[0]
            : $nodes;

        return new self($cnt, $shift, $root, $tail);
    }

    public function count(): int
    {
        return $this->cnt;
    }

    public function get(int $i): mixed
    {
        // Inline everything to avoid method call overhead
        // Fast path: small vectors (≤32) have everything in tail
        if ($this->cnt <= self::WIDTH) {
            if ($i < 0 || $i >= $this->cnt) {
                throw new \OutOfBoundsException("Index $i out of bounds [0, {$this->cnt})");
            }
            return $this->tail[$i];
        }

        // Bounds check for larger vectors
        if ($i < 0 || $i >= $this->cnt) {
            throw new \OutOfBoundsException("Index $i out of bounds [0, {$this->cnt})");
        }

        // Check if in tail
        if ($i >= $this->tailOff) {
            return $this->tail[$i - $this->tailOff];
        }

        // Tree traversal for elements before tail
        $node = $this->root;
        for ($level = $this->shift; $level > 0; $level -= self::BITS) {
            $node = $node[($i >> $level) & self::MASK];
        }
        return $node[$i & self::MASK];
    }

    /**
     * Get without bounds check - for internal use and hot paths.
     */
    public function getUnsafe(int $i): mixed
    {
        // Fast path: small vectors (≤32) have everything in tail
        if ($this->cnt <= self::WIDTH) {
            return $this->tail[$i];
        }

        // Check if in tail (most common case, especially for small vecs)
        if ($i >= $this->tailOff) {
            return $this->tail[$i - $this->tailOff];
        }

        // Tree traversal for elements before tail
        $node = $this->root;
        for ($level = $this->shift; $level > 0; $level -= self::BITS) {
            $node = $node[($i >> $level) & self::MASK];
        }
        return $node[$i & self::MASK];
    }

    // ============================================================
    // Inlined operations - bypass seq abstraction for speed
    // ============================================================

    /**
     * Inlined reduce - uses cached array for native iteration speed.
     */
    public function reduce(callable $f, mixed $init): mixed
    {
        $arr = $this->asArray();
        $acc = $init;
        foreach ($arr as $x) {
            $acc = $f($acc, $x);
        }
        return $acc;
    }

    /**
     * Inlined map - returns new Vec.
     */
    public function map(callable $f): self
    {
        return self::from(\array_map($f, $this->asArray()));
    }

    /**
     * Inlined filter - returns new Vec.
     */
    public function filter(callable $pred): self
    {
        return self::from(\array_values(\array_filter($this->asArray(), $pred)));
    }

    /**
     * Inlined sum - uses native array_sum.
     */
    public function sum(): int|float
    {
        return \array_sum($this->asArray());
    }

    /**
     * Inlined product - uses native array_product.
     */
    public function product(): int|float
    {
        return \array_product($this->asArray());
    }

    /**
     * Inlined max.
     */
    public function max(): mixed
    {
        $arr = $this->asArray();
        return $arr ? \max($arr) : null;
    }

    /**
     * Inlined min.
     */
    public function min(): mixed
    {
        $arr = $this->asArray();
        return $arr ? \min($arr) : null;
    }

    /**
     * Inlined forEach - fastest iteration.
     */
    public function each(callable $f): void
    {
        foreach ($this->asArray() as $x) {
            $f($x);
        }
    }

    // Cached array for repeated operations
    private ?array $arrayCache = null;

    /**
     * Get as array with caching - for repeated operations on same Vec.
     */
    private function asArray(): array
    {
        return $this->arrayCache ??= $this->toArray();
    }

    public function append(mixed $val): self
    {
        if (count($this->tail) < self::WIDTH) {
            return new self(
                $this->cnt + 1,
                $this->shift,
                $this->root,
                [...$this->tail, $val],
            );
        }
        // Tail full, push into tree
        $tailNode = $this->tail;
        $newShift = $this->shift;
        if ($this->cnt >> self::BITS > (1 << $this->shift)) {
            // Overflow root
            $newRoot = [$this->root, $this->newPath($this->shift, $tailNode)];
            $newShift += self::BITS;
        } else {
            $newRoot = $this->pushTail($this->shift, $this->root, $tailNode);
        }
        return new self($this->cnt + 1, $newShift, $newRoot, [$val]);
    }

    public function update(int $i, mixed $val): self
    {
        if ($i < 0 || $i > $this->cnt) {
            throw new \OutOfBoundsException("Index $i out of bounds");
        }
        if ($i === $this->cnt) {
            return $this->append($val);
        }
        if ($i >= $this->tailOffset()) {
            $newTail = $this->tail;
            $newTail[$i & self::MASK] = $val;
            return new self($this->cnt, $this->shift, $this->root, $newTail);
        }
        return new self(
            $this->cnt,
            $this->shift,
            $this->doUpdate($this->shift, $this->root, $i, $val),
            $this->tail,
        );
    }

    public function pop(): self
    {
        if ($this->cnt === 0) {
            throw new \RuntimeException("Can't pop empty vector");
        }
        if ($this->cnt === 1) {
            return self::empty();
        }
        if (count($this->tail) > 1) {
            return new self(
                $this->cnt - 1,
                $this->shift,
                $this->root,
                array_slice($this->tail, 0, -1),
            );
        }
        $newTail = $this->arrayFor($this->cnt - 2);
        $newRoot = $this->popTail($this->shift, $this->root) ?? [];
        $newShift = $this->shift;
        if ($this->shift > self::BITS && ($newRoot[1] ?? null) === null) {
            $newRoot = $newRoot[0];
            $newShift -= self::BITS;
        }
        return new self($this->cnt - 1, $newShift, $newRoot, $newTail);
    }

    // ============================================================
    // Seq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        return $this->cnt > 0 ? new VecSeq($this, 0) : null;
    }

    public function first(): mixed
    {
        return $this->cnt > 0 ? $this->get(0) : null;
    }

    public function rest(): self
    {
        return $this->cnt > 1 ? $this->slice(1) : self::empty();
    }

    public function cons(mixed $x): self
    {
        // For vectors, cons prepends (different from append)
        // But Clojure's conj on vector appends. For cons we need to rebuild.
        $arr = $this->toArray();
        array_unshift($arr, $x);
        return self::from($arr);
    }

    // ============================================================
    // Collection operations
    // ============================================================

    public function slice(int $start, ?int $end = null): self
    {
        $end ??= $this->cnt;
        if ($start < 0) $start = max(0, $this->cnt + $start);
        if ($end < 0) $end = max(0, $this->cnt + $end);
        $start = min($start, $this->cnt);
        $end = min($end, $this->cnt);
        if ($start >= $end) {
            return self::empty();
        }
        // Simple implementation - rebuild
        $result = self::empty();
        for ($i = $start; $i < $end; $i++) {
            $result = $result->append($this->get($i));
        }
        return $result;
    }

    public function toArray(): array
    {
        $result = [];
        $this->collectArray($this->root, $this->shift, $result);
        return array_merge($result, $this->tail);
    }

    // ============================================================
    // Equality & Hashing
    // ============================================================

    public function hash(): int
    {
        if ($this->hashCache === null) {
            $h = 1;
            foreach ($this as $v) {
                $h = 31 * $h + hash_($v);
            }
            $this->hashCache = $h;
        }
        return $this->hashCache;
    }

    public function equals(mixed $other): bool
    {
        if ($this === $other) return true;
        if (!$other instanceof Vec) return false;
        if ($this->cnt !== $other->cnt) return false;
        for ($i = 0; $i < $this->cnt; $i++) {
            if (!equals($this->get($i), $other->get($i))) {
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
    // ArrayAccess
    // ============================================================

    public function offsetExists(mixed $offset): bool
    {
        return is_int($offset) && $offset >= 0 && $offset < $this->cnt;
    }

    public function offsetGet(mixed $offset): mixed
    {
        return $this->get($offset);
    }

    public function offsetSet(mixed $offset, mixed $value): void
    {
        throw new \RuntimeException("Vectors are immutable");
    }

    public function offsetUnset(mixed $offset): void
    {
        throw new \RuntimeException("Vectors are immutable");
    }

    // ============================================================
    // IteratorAggregate
    // ============================================================

    public function getIterator(): Traversable
    {
        // Fast path: small vectors - iterate tail directly (no method calls)
        if ($this->cnt <= self::WIDTH) {
            foreach ($this->tail as $i => $v) {
                yield $i => $v;
            }
            return;
        }

        // For larger vectors, use cached array to avoid per-element get() calls
        foreach ($this->asArray() as $i => $v) {
            yield $i => $v;
        }
    }

    public function __toString(): string
    {
        return prStr($this);
    }

    // ============================================================
    // Internal helpers
    // ============================================================

    private function tailOffset(): int
    {
        return $this->cnt < self::WIDTH ? 0 : $this->cnt - count($this->tail);
    }

    private function arrayFor(int $i): array
    {
        if ($i >= $this->tailOffset()) {
            return $this->tail;
        }
        $node = $this->root;
        for ($level = $this->shift; $level > 0; $level -= self::BITS) {
            $node = $node[($i >> $level) & self::MASK];
        }
        return $node;
    }

    private function pushTail(int $level, array $parent, array $tailNode): array
    {
        $ret = $parent;
        if ($level === self::BITS) {
            $ret[] = $tailNode;
            return $ret;
        }
        $subIdx = ($this->cnt - 1 >> $level) & self::MASK;
        if (count($parent) > $subIdx) {
            $ret[$subIdx] = $this->pushTail($level - self::BITS, $parent[$subIdx], $tailNode);
        } else {
            $ret[] = $this->newPath($level - self::BITS, $tailNode);
        }
        return $ret;
    }

    private function newPath(int $level, array $node): array
    {
        return $level === 0 ? $node : [$this->newPath($level - self::BITS, $node)];
    }

    private function doUpdate(int $level, array $node, int $i, mixed $val): array
    {
        $ret = $node;
        if ($level === 0) {
            $ret[$i & self::MASK] = $val;
        } else {
            $subIdx = ($i >> $level) & self::MASK;
            $ret[$subIdx] = $this->doUpdate($level - self::BITS, $node[$subIdx], $i, $val);
        }
        return $ret;
    }

    private function popTail(int $level, array $node): ?array
    {
        $subIdx = (($this->cnt - 2) >> $level) & self::MASK;
        if ($level > self::BITS) {
            $newChild = $this->popTail($level - self::BITS, $node[$subIdx]);
            if ($newChild === null && $subIdx === 0) {
                return null;
            }
            $ret = $node;
            $ret[$subIdx] = $newChild;
            return $ret;
        }
        if ($subIdx === 0) {
            return null;
        }
        $ret = $node;
        unset($ret[$subIdx]);
        return $ret;
    }

    private function collectArray(array $node, int $shift, array &$out): void
    {
        if ($shift === 0) {
            $out = array_merge($out, $node);
        } else {
            foreach ($node as $child) {
                $this->collectArray($child, $shift - self::BITS, $out);
            }
        }
    }
}

/**
 * Seq view over a Vec.
 */
final class VecSeq implements ISeq
{
    public function __construct(
        private readonly Vec $vec,
        private readonly int $i,
    ) {
    }

    public function seq(): ?ISeq
    {
        return $this->i < count_($this->vec) ? $this : null;
    }

    public function first(): mixed
    {
        return $this->vec->get($this->i);
    }

    public function next(): ?ISeq
    {
        return $this->i + 1 < count_($this->vec) ? new VecSeq($this->vec, $this->i + 1) : null;
    }

    public function more(): ISeq
    {
        return $this->next() ?? EmptySeq::instance();
    }

    public function rest(): ISeq
    {
        return $this->more();
    }

    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    public function count(): int
    {
        return count_($this->vec) - $this->i;
    }

    public function toArray(): array
    {
        return array_slice($this->vec->toArray(), $this->i);
    }

    public function getIterator(): Traversable
    {
        for ($j = $this->i; $j < count_($this->vec); $j++) {
            yield $this->vec->get($j);
        }
    }
}

// Constructor function
function vec(mixed ...$xs): Vec
{
    return Vec::from($xs);
}

// ============================================================
// SubVector - O(1) slice view over a Vec
// ============================================================

/**
 * SubVector - O(1) view over a slice of a Vec.
 *
 * Instead of copying elements, stores offset/end into underlying Vec.
 * Mutations return a new Vec (not SubVector).
 */
final class SubVector implements Seqable, \Countable, \IteratorAggregate, \ArrayAccess
{
    public function __construct(
        private readonly Vec $vec,
        private readonly int $start,
        private readonly int $end,
    ) {}

    public function count(): int
    {
        return $this->end - $this->start;
    }

    public function get(int $i): mixed
    {
        if ($i < 0 || $i >= $this->count()) {
            throw new \OutOfBoundsException("Index $i out of bounds [0, {$this->count()})");
        }
        return $this->vec->get($this->start + $i);
    }

    public function first(): mixed
    {
        return $this->count() > 0 ? $this->get(0) : null;
    }

    public function rest(): Vec
    {
        return $this->count() > 1 ? $this->slice(1)->toVec() : Vec::empty();
    }

    public function seq(): ?ISeq
    {
        return $this->count() > 0 ? new SubVecSeq($this, 0) : null;
    }

    public function slice(int $start, ?int $end = null): SubVector
    {
        $len = $this->count();
        $end ??= $len;
        if ($start < 0) $start = max(0, $len + $start);
        if ($end < 0) $end = max(0, $len + $end);
        $start = min($start, $len);
        $end = min($end, $len);
        if ($start >= $end) {
            return new SubVector(Vec::empty(), 0, 0);
        }
        return new SubVector($this->vec, $this->start + $start, $this->start + $end);
    }

    public function append(mixed $val): Vec
    {
        return Vec::from([...$this->toArray(), $val]);
    }

    public function update(int $i, mixed $val): Vec
    {
        $arr = $this->toArray();
        $arr[$i] = $val;
        return Vec::from($arr);
    }

    public function pop(): Vec
    {
        if ($this->count() === 0) {
            throw new \RuntimeException("Can't pop empty vector");
        }
        return Vec::from(array_slice($this->toArray(), 0, -1));
    }

    public function toArray(): array
    {
        $result = [];
        for ($i = $this->start; $i < $this->end; $i++) {
            $result[] = $this->vec->get($i);
        }
        return $result;
    }

    public function toVec(): Vec
    {
        return Vec::from($this->toArray());
    }

    // ArrayAccess
    public function offsetExists(mixed $offset): bool
    {
        return is_int($offset) && $offset >= 0 && $offset < $this->count();
    }

    public function offsetGet(mixed $offset): mixed
    {
        return $this->get($offset);
    }

    public function offsetSet(mixed $offset, mixed $value): void
    {
        throw new \RuntimeException("Vectors are immutable");
    }

    public function offsetUnset(mixed $offset): void
    {
        throw new \RuntimeException("Vectors are immutable");
    }

    // IteratorAggregate
    public function getIterator(): Traversable
    {
        for ($i = 0; $i < $this->count(); $i++) {
            yield $i => $this->get($i);
        }
    }

    public function __toString(): string
    {
        return prStr($this);
    }
}

/**
 * Seq view over a SubVector.
 */
final class SubVecSeq implements ISeq
{
    public function __construct(
        private readonly SubVector $vec,
        private readonly int $i,
    ) {}

    public function seq(): ?ISeq
    {
        return $this->i < $this->vec->count() ? $this : null;
    }

    public function first(): mixed
    {
        return $this->vec->get($this->i);
    }

    public function next(): ?ISeq
    {
        return $this->i + 1 < $this->vec->count() ? new SubVecSeq($this->vec, $this->i + 1) : null;
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
        return $this->vec->count() - $this->i;
    }

    public function toArray(): array
    {
        return array_slice($this->vec->toArray(), $this->i);
    }

    public function getIterator(): Traversable
    {
        for ($j = $this->i; $j < $this->vec->count(); $j++) {
            yield $this->vec->get($j);
        }
    }
}

/**
 * Create a subvector (O(1) slice).
 */
function subvec(Vec $v, int $start, ?int $end = null): SubVector
{
    $end ??= $v->count();
    if ($start < 0 || $start > $v->count()) {
        throw new \OutOfBoundsException("Start $start out of bounds");
    }
    if ($end < $start || $end > $v->count()) {
        throw new \OutOfBoundsException("End $end out of bounds");
    }
    return new SubVector($v, $start, $end);
}
