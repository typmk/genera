<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;
use ArrayAccess;
use Countable;
use IteratorAggregate;

/**
 * Persistent Hash Map - Hash Array Mapped Trie (HAMT).
 *
 * Uses 32-way branching with bitmap compression for sparse nodes.
 */
final class Map implements Seqable, Countable, IteratorAggregate, ArrayAccess
{
    private ?Map $meta = null;
    private ?int $hashCache = null;

    private function __construct(
        private readonly int $cnt,
        private readonly ?MapNode $root,
        private readonly bool $hasNull = false,
        private readonly mixed $nullValue = null,
    ) {
    }

    public static function empty(): self
    {
        static $empty = null;
        return $empty ??= new self(0, null);
    }

    public static function from(array $kvs): self
    {
        $m = self::empty();
        $len = count($kvs);
        for ($i = 0; $i < $len; $i += 2) {
            $m = $m->put($kvs[$i], $kvs[$i + 1] ?? null);
        }
        return $m;
    }

    public function count(): int
    {
        return $this->cnt;
    }

    public function put(mixed $key, mixed $val): self
    {
        // Special case: null key
        if ($key === null) {
            if ($this->hasNull && equals($val, $this->nullValue)) {
                return $this;
            }
            return new self(
                $this->hasNull ? $this->cnt : $this->cnt + 1,
                $this->root,
                true,
                $val
            );
        }

        $added = new Box(false);
        $hash = hash_($key);
        $newRoot = $this->root === null
            ? (new IndexedNode([]))->put(0, $hash, $key, $val, $added)
            : $this->root->put(0, $hash, $key, $val, $added);

        if ($newRoot === $this->root && !$added->val) {
            return $this;
        }
        return new self(
            $added->val ? $this->cnt + 1 : $this->cnt,
            $newRoot,
            $this->hasNull,
            $this->nullValue
        );
    }

    public function remove(mixed $key): self
    {
        // Special case: null key
        if ($key === null) {
            if (!$this->hasNull) {
                return $this;
            }
            return new self($this->cnt - 1, $this->root, false, null);
        }

        if ($this->root === null) {
            return $this;
        }
        $hash = hash_($key);
        $newRoot = $this->root->remove(0, $hash, $key);
        if ($newRoot === $this->root) {
            return $this;
        }
        return new self($this->cnt - 1, $newRoot, $this->hasNull, $this->nullValue);
    }

    public function find(mixed $key, mixed $notFound = null): mixed
    {
        // Special case: null key
        if ($key === null) {
            return $this->hasNull ? $this->nullValue : $notFound;
        }

        if ($this->root === null) {
            return $notFound;
        }
        return $this->root->find(0, hash_($key), $key, $notFound);
    }

    public function containsKey(mixed $key): bool
    {
        // Special case: null key
        if ($key === null) {
            return $this->hasNull;
        }

        $sentinel = new \stdClass();
        return $this->find($key, $sentinel) !== $sentinel;
    }

    // ============================================================
    // Seq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        if ($this->root === null) {
            return null;
        }
        return $this->root->nodeSeq();
    }

    // ============================================================
    // Collection operations
    // ============================================================

    public function keys(): Vec
    {
        $ks = [];
        foreach ($this as $k => $v) {
            $ks[] = $k;
        }
        return vec(...$ks);
    }

    public function vals(): Vec
    {
        $vs = [];
        foreach ($this as $v) {
            $vs[] = $v;
        }
        return vec(...$vs);
    }

    public function merge(Map $other): self
    {
        $m = $this;
        foreach ($other as $k => $v) {
            $m = $m->put($k, $v);
        }
        return $m;
    }

    // ============================================================
    // Inlined operations - bypass seq abstraction for speed
    // ============================================================

    /**
     * Inlined reduce over key-value pairs.
     */
    public function reduce(callable $f, mixed $init): mixed
    {
        $acc = $init;
        foreach ($this as $k => $v) {
            $acc = $f($acc, $k, $v);
        }
        return $acc;
    }

    /**
     * Inlined map over values - returns new Map.
     */
    public function mapVals(callable $f): self
    {
        $pairs = [];
        foreach ($this as $k => $v) {
            $pairs[] = $k;
            $pairs[] = $f($v);
        }
        return self::from($pairs);
    }

    /**
     * Inlined filter by predicate on key-value pairs.
     */
    public function filter(callable $pred): self
    {
        $pairs = [];
        foreach ($this as $k => $v) {
            if ($pred($k, $v)) {
                $pairs[] = $k;
                $pairs[] = $v;
            }
        }
        return self::from($pairs);
    }

    /**
     * Inlined forEach over key-value pairs.
     */
    public function each(callable $f): void
    {
        foreach ($this as $k => $v) {
            $f($k, $v);
        }
    }

    // ============================================================
    // Equality & Hashing
    // ============================================================

    public function hash(): int
    {
        if ($this->hashCache === null) {
            $h = 0;
            foreach ($this as $k => $v) {
                $h += hash_($k) ^ hash_($v);
            }
            $this->hashCache = $h;
        }
        return $this->hashCache;
    }

    public function equals(mixed $other): bool
    {
        if ($this === $other) return true;
        if (!$other instanceof Map) return false;
        if ($this->cnt !== $other->cnt) return false;
        foreach ($this as $k => $v) {
            $sentinel = new \stdClass();
            $ov = $other->find($k, $sentinel);
            if ($ov === $sentinel || !equals($v, $ov)) {
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
        return $this->containsKey($offset);
    }

    public function offsetGet(mixed $offset): mixed
    {
        return $this->find($offset);
    }

    public function offsetSet(mixed $offset, mixed $value): void
    {
        throw new \RuntimeException("Maps are immutable");
    }

    public function offsetUnset(mixed $offset): void
    {
        throw new \RuntimeException("Maps are immutable");
    }

    // ============================================================
    // IteratorAggregate
    // ============================================================

    public function getIterator(): Traversable
    {
        // Yield null key first if present
        if ($this->hasNull) {
            yield null => $this->nullValue;
        }
        if ($this->root !== null) {
            yield from $this->root->getIterator();
        }
    }

    public function __toString(): string
    {
        return prStr($this);
    }

    /**
     * Keyword/Map invocation: (m :key) or (:key m)
     */
    public function __invoke(mixed $key, mixed $notFound = null): mixed
    {
        return $this->find($key, $notFound);
    }
}

// ============================================================
// Internal Node Types
// ============================================================

/** @internal */
interface MapNode
{
    public function put(int $shift, int $hash, mixed $key, mixed $val, Box $added): MapNode;
    public function remove(int $shift, int $hash, mixed $key): ?MapNode;
    public function find(int $shift, int $hash, mixed $key, mixed $notFound): mixed;
    public function getIterator(): Traversable;
    public function nodeSeq(): ?ISeq;
}

/** @internal */
final class Box
{
    public function __construct(public mixed $val) {}
}

/**
 * Sparse node with up to 16 entries before splitting to ArrayNode.
 * @internal
 */
final class IndexedNode implements MapNode
{
    public function __construct(private array $entries) {}

    public function put(int $shift, int $hash, mixed $key, mixed $val, Box $added): MapNode
    {
        $idx = ($hash >> $shift) & 0x1f;
        $entry = $this->entries[$idx] ?? null;

        if ($entry === null) {
            // Empty slot
            if (count($this->entries) >= 16) {
                return $this->expand($idx, $shift, $hash, $key, $val, $added);
            }
            $new = $this->entries;
            $new[$idx] = [$key, $val];
            $added->val = true;
            return new IndexedNode($new);
        }

        [$eKey, $eVal] = $entry;

        if ($eKey === null) {
            // Child node
            $child = $eVal;
            $newChild = $child->put($shift + 5, $hash, $key, $val, $added);
            if ($newChild === $child) return $this;
            $new = $this->entries;
            $new[$idx] = [null, $newChild];
            return new IndexedNode($new);
        }

        // Existing key
        if ($key === $eKey || equals($key, $eKey)) {
            if ($val === $eVal || equals($val, $eVal)) return $this;
            $new = $this->entries;
            $new[$idx] = [$key, $val];
            return new IndexedNode($new);
        }

        // Collision - create child
        $added->val = true;
        $new = $this->entries;
        $new[$idx] = [null, $this->createNode($shift + 5, $eKey, $eVal, $hash, $key, $val)];
        return new IndexedNode($new);
    }

    public function remove(int $shift, int $hash, mixed $key): ?MapNode
    {
        $idx = ($hash >> $shift) & 0x1f;
        $entry = $this->entries[$idx] ?? null;
        if ($entry === null) return $this;

        [$eKey, $eVal] = $entry;

        if ($eKey === null) {
            $child = $eVal;
            $newChild = $child->remove($shift + 5, $hash, $key);
            if ($newChild === $child) return $this;
            if ($newChild === null) {
                if (count($this->entries) === 1) return null;
                $new = $this->entries;
                unset($new[$idx]);
                return new IndexedNode($new);
            }
            $new = $this->entries;
            $new[$idx] = [null, $newChild];
            return new IndexedNode($new);
        }

        if ($key === $eKey || equals($key, $eKey)) {
            if (count($this->entries) === 1) return null;
            $new = $this->entries;
            unset($new[$idx]);
            return new IndexedNode($new);
        }

        return $this;
    }

    public function find(int $shift, int $hash, mixed $key, mixed $notFound): mixed
    {
        $entry = $this->entries[($hash >> $shift) & 0x1f] ?? null;
        if ($entry === null) return $notFound;

        [$eKey, $eVal] = $entry;
        if ($eKey === null) return $eVal->find($shift + 5, $hash, $key, $notFound);
        if ($key === $eKey || equals($key, $eKey)) return $eVal;
        return $notFound;
    }

    public function getIterator(): Traversable
    {
        foreach ($this->entries as [$k, $v]) {
            if ($k === null) {
                yield from $v->getIterator();
            } else {
                yield $k => $v;
            }
        }
    }

    public function nodeSeq(): ?ISeq
    {
        return new NodeSeq($this->entries, 0);
    }

    private function createNode(int $shift, mixed $k1, mixed $v1, int $h2, mixed $k2, mixed $v2): MapNode
    {
        $h1 = hash_($k1);
        if ($h1 === $h2) {
            return new CollisionNode($h1, [[$k1, $v1], [$k2, $v2]]);
        }
        $added = new Box(false);
        return (new IndexedNode([]))
            ->put($shift, $h1, $k1, $v1, $added)
            ->put($shift, $h2, $k2, $v2, $added);
    }

    private function expand(int $idx, int $shift, int $hash, mixed $key, mixed $val, Box $added): MapNode
    {
        $nodes = [];
        $empty = new IndexedNode([]);
        foreach ($this->entries as $i => [$k, $v]) {
            $nodes[$i] = $k === null ? $v : $empty->put($shift + 5, hash_($k), $k, $v, new Box(false));
        }
        $nodes[$idx] = $empty->put($shift + 5, $hash, $key, $val, $added);
        return new ArrayNode(count($this->entries) + 1, $nodes);
    }
}

/**
 * Full 32-way node (used when IndexedNode grows too large).
 * @internal
 */
final class ArrayNode implements MapNode
{
    public function __construct(
        private int $cnt,
        private array $nodes,
    ) {}

    public function put(int $shift, int $hash, mixed $key, mixed $val, Box $added): MapNode
    {
        $idx = ($hash >> $shift) & 0x1f;
        $node = $this->nodes[$idx] ?? null;

        if ($node === null) {
            $added->val = true;
            $new = $this->nodes;
            $new[$idx] = (new IndexedNode([]))->put($shift + 5, $hash, $key, $val, $added);
            return new ArrayNode($this->cnt + 1, $new);
        }

        $newNode = $node->put($shift + 5, $hash, $key, $val, $added);
        if ($newNode === $node) return $this;
        $new = $this->nodes;
        $new[$idx] = $newNode;
        return new ArrayNode($this->cnt, $new);
    }

    public function remove(int $shift, int $hash, mixed $key): ?MapNode
    {
        $idx = ($hash >> $shift) & 0x1f;
        $node = $this->nodes[$idx] ?? null;
        if ($node === null) return $this;

        $newNode = $node->remove($shift + 5, $hash, $key);
        if ($newNode === $node) return $this;

        if ($newNode === null) {
            if ($this->cnt <= 8) {
                return $this->pack($idx);
            }
            $new = $this->nodes;
            unset($new[$idx]);
            return new ArrayNode($this->cnt - 1, $new);
        }

        $new = $this->nodes;
        $new[$idx] = $newNode;
        return new ArrayNode($this->cnt, $new);
    }

    public function find(int $shift, int $hash, mixed $key, mixed $notFound): mixed
    {
        $node = $this->nodes[($hash >> $shift) & 0x1f] ?? null;
        return $node?->find($shift + 5, $hash, $key, $notFound) ?? $notFound;
    }

    public function getIterator(): Traversable
    {
        foreach ($this->nodes as $node) {
            if ($node !== null) {
                yield from $node->getIterator();
            }
        }
    }

    public function nodeSeq(): ?ISeq
    {
        foreach ($this->nodes as $node) {
            if ($node !== null) {
                $s = $node->nodeSeq();
                if ($s !== null) return $s;
            }
        }
        return null;
    }

    private function pack(int $excludeIdx): MapNode
    {
        $entries = [];
        foreach ($this->nodes as $i => $node) {
            if ($i !== $excludeIdx && $node !== null) {
                $entries[$i] = [null, $node];
            }
        }
        return new IndexedNode($entries);
    }
}

/**
 * Node for hash collisions (same hash, different keys).
 * @internal
 */
final class CollisionNode implements MapNode
{
    public function __construct(
        private int $hash,
        private array $pairs,
    ) {}

    public function put(int $shift, int $hash, mixed $key, mixed $val, Box $added): MapNode
    {
        if ($hash === $this->hash) {
            foreach ($this->pairs as $i => [$k, $v]) {
                if ($key === $k || equals($key, $k)) {
                    if ($val === $v || equals($val, $v)) return $this;
                    $new = $this->pairs;
                    $new[$i] = [$key, $val];
                    return new CollisionNode($this->hash, $new);
                }
            }
            $added->val = true;
            return new CollisionNode($this->hash, [...$this->pairs, [$key, $val]]);
        }
        // Different hash - wrap in IndexedNode
        $added->val = true;
        $idx = ($this->hash >> $shift) & 0x1f;
        return (new IndexedNode([$idx => [null, $this]]))->put($shift, $hash, $key, $val, $added);
    }

    public function remove(int $shift, int $hash, mixed $key): ?MapNode
    {
        if ($hash !== $this->hash) return $this;
        foreach ($this->pairs as $i => [$k, $v]) {
            if ($key === $k || equals($key, $k)) {
                if (count($this->pairs) === 1) return null;
                if (count($this->pairs) === 2) {
                    $other = $this->pairs[$i === 0 ? 1 : 0];
                    return new IndexedNode([($this->hash >> $shift) & 0x1f => $other]);
                }
                $new = $this->pairs;
                array_splice($new, $i, 1);
                return new CollisionNode($this->hash, $new);
            }
        }
        return $this;
    }

    public function find(int $shift, int $hash, mixed $key, mixed $notFound): mixed
    {
        if ($hash !== $this->hash) return $notFound;
        foreach ($this->pairs as [$k, $v]) {
            if ($key === $k || equals($key, $k)) return $v;
        }
        return $notFound;
    }

    public function getIterator(): Traversable
    {
        foreach ($this->pairs as [$k, $v]) {
            yield $k => $v;
        }
    }

    public function nodeSeq(): ?ISeq
    {
        return new ArraySeq(array_map(fn($p) => vec($p[0], $p[1]), $this->pairs));
    }
}

/**
 * Seq over IndexedNode entries.
 * @internal
 */
final class NodeSeq implements ISeq
{
    public function __construct(
        private array $entries,
        private int $i,
        private ?ISeq $childSeq = null,
    ) {}

    public function seq(): ?ISeq
    {
        return $this;
    }

    public function first(): mixed
    {
        if ($this->childSeq !== null) {
            return $this->childSeq->first();
        }
        [$k, $v] = array_values($this->entries)[$this->i];
        return vec($k, $v);
    }

    public function next(): ?ISeq
    {
        if ($this->childSeq !== null) {
            $ns = $this->childSeq->next();
            if ($ns !== null) {
                return new NodeSeq($this->entries, $this->i, $ns);
            }
            return $this->advance($this->i + 1);
        }
        return $this->advance($this->i + 1);
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
        $c = 0;
        $s = $this;
        while ($s !== null) {
            $c++;
            $s = $s->next();
        }
        return $c;
    }

    public function toArray(): array
    {
        $arr = [];
        $s = $this;
        while ($s !== null) {
            $arr[] = $s->first();
            $s = $s->next();
        }
        return $arr;
    }

    public function getIterator(): Traversable
    {
        $s = $this;
        while ($s !== null) {
            yield $s->first();
            $s = $s->next();
        }
    }

    private function advance(int $i): ?ISeq
    {
        $vals = array_values($this->entries);
        while ($i < count($vals)) {
            [$k, $v] = $vals[$i];
            if ($k === null && $v instanceof MapNode) {
                $ns = $v->nodeSeq();
                if ($ns !== null) {
                    return new NodeSeq($this->entries, $i, $ns);
                }
            } elseif ($k !== null) {
                return new NodeSeq($this->entries, $i);
            }
            $i++;
        }
        return null;
    }
}

// Constructor function
function hashMap(mixed ...$kvs): Map
{
    return Map::from($kvs);
}

