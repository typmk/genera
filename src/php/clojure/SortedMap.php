<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;

/**
 * PersistentTreeMap - sorted immutable map using a Red-Black tree.
 *
 * Keys are kept in sorted order according to the comparator.
 * Supports sorted-map operations: subseq, rsubseq, etc.
 */
final class SortedMap implements Seqable, \Countable, \IteratorAggregate, \ArrayAccess
{
    private const RED = true;
    private const BLACK = false;

    private static ?SortedMap $empty = null;

    /** @var callable */
    private $comparator;

    private function __construct(
        private readonly ?array $root,  // [key, val, color, left, right]
        private readonly int $count,
        ?callable $comparator = null,
        private readonly ?Map $meta = null,
    ) {
        $this->comparator = $comparator ?? fn($a, $b) => compare($a, $b);
    }

    public static function empty(?callable $comparator = null): self
    {
        if ($comparator === null && self::$empty !== null) {
            return self::$empty;
        }
        $map = new self(null, 0, $comparator);
        if ($comparator === null) {
            self::$empty = $map;
        }
        return $map;
    }

    public static function from(iterable $items, ?callable $comparator = null): self
    {
        $map = self::empty($comparator);
        foreach ($items as $k => $v) {
            $map = $map->put($k, $v);
        }
        return $map;
    }

    // ============================================================
    // Core operations
    // ============================================================

    public function put(mixed $key, mixed $val): self
    {
        $newRoot = $this->insertNode($this->root, $key, $val);
        // Root is always black
        $newRoot[2] = self::BLACK;
        return new self($newRoot, $this->count + 1, $this->comparator, $this->meta);
    }

    public function find(mixed $key): mixed
    {
        $node = $this->findNode($this->root, $key);
        return $node !== null ? $node[1] : null;
    }

    public function containsKey(mixed $key): bool
    {
        return $this->findNode($this->root, $key) !== null;
    }

    public function remove(mixed $key): self
    {
        if (!$this->containsKey($key)) {
            return $this;
        }
        $newRoot = $this->removeNode($this->root, $key);
        if ($newRoot !== null) {
            $newRoot[2] = self::BLACK;
        }
        return new self($newRoot, max(0, $this->count - 1), $this->comparator, $this->meta);
    }

    public function count(): int
    {
        return $this->count;
    }

    // ============================================================
    // Red-Black tree operations
    // ============================================================

    private function isRed(?array $node): bool
    {
        return $node !== null && $node[2] === self::RED;
    }

    private function findNode(?array $node, mixed $key): ?array
    {
        while ($node !== null) {
            $cmp = ($this->comparator)($key, $node[0]);
            if ($cmp === 0) {
                return $node;
            }
            $node = $cmp < 0 ? $node[3] : $node[4];
        }
        return null;
    }

    private function insertNode(?array $node, mixed $key, mixed $val): array
    {
        if ($node === null) {
            return [$key, $val, self::RED, null, null];
        }

        $cmp = ($this->comparator)($key, $node[0]);
        if ($cmp === 0) {
            // Update value
            return [$key, $val, $node[2], $node[3], $node[4]];
        }

        if ($cmp < 0) {
            $newLeft = $this->insertNode($node[3], $key, $val);
            $node = [$node[0], $node[1], $node[2], $newLeft, $node[4]];
        } else {
            $newRight = $this->insertNode($node[4], $key, $val);
            $node = [$node[0], $node[1], $node[2], $node[3], $newRight];
        }

        // Balance the tree
        return $this->balance($node);
    }

    private function balance(array $node): array
    {
        // Right red, left black -> rotate left
        if ($this->isRed($node[4]) && !$this->isRed($node[3])) {
            $node = $this->rotateLeft($node);
        }

        // Left red, left-left red -> rotate right
        if ($this->isRed($node[3]) && $node[3] !== null && $this->isRed($node[3][3])) {
            $node = $this->rotateRight($node);
        }

        // Both children red -> flip colors
        if ($this->isRed($node[3]) && $this->isRed($node[4])) {
            $node = $this->flipColors($node);
        }

        return $node;
    }

    private function rotateLeft(array $node): array
    {
        $x = $node[4];
        $newNode = [$x[0], $x[1], $node[2], [$node[0], $node[1], self::RED, $node[3], $x[3]], $x[4]];
        return $newNode;
    }

    private function rotateRight(array $node): array
    {
        $x = $node[3];
        $newNode = [$x[0], $x[1], $node[2], $x[3], [$node[0], $node[1], self::RED, $x[4], $node[4]]];
        return $newNode;
    }

    private function flipColors(array $node): array
    {
        return [
            $node[0],
            $node[1],
            !$node[2],
            $node[3] !== null ? [$node[3][0], $node[3][1], !$node[3][2], $node[3][3], $node[3][4]] : null,
            $node[4] !== null ? [$node[4][0], $node[4][1], !$node[4][2], $node[4][3], $node[4][4]] : null,
        ];
    }

    private function removeNode(?array $node, mixed $key): ?array
    {
        if ($node === null) {
            return null;
        }

        $cmp = ($this->comparator)($key, $node[0]);

        if ($cmp < 0) {
            // Go left
            if ($node[3] !== null && !$this->isRed($node[3]) && !$this->isRed($node[3][3] ?? null)) {
                $node = $this->moveRedLeft($node);
            }
            $node = [$node[0], $node[1], $node[2], $this->removeNode($node[3], $key), $node[4]];
        } else {
            if ($this->isRed($node[3])) {
                $node = $this->rotateRight($node);
            }
            if ($cmp === 0 && $node[4] === null) {
                return null;
            }
            if ($node[4] !== null && !$this->isRed($node[4]) && !$this->isRed($node[4][3] ?? null)) {
                $node = $this->moveRedRight($node);
            }
            if (($this->comparator)($key, $node[0]) === 0) {
                // Replace with min from right subtree
                $min = $this->minNode($node[4]);
                $node = [$min[0], $min[1], $node[2], $node[3], $this->removeMin($node[4])];
            } else {
                $node = [$node[0], $node[1], $node[2], $node[3], $this->removeNode($node[4], $key)];
            }
        }

        return $this->balance($node);
    }

    private function moveRedLeft(array $node): array
    {
        $node = $this->flipColors($node);
        if ($node[4] !== null && $this->isRed($node[4][3] ?? null)) {
            $newRight = $this->rotateRight($node[4]);
            $node = [$node[0], $node[1], $node[2], $node[3], $newRight];
            $node = $this->rotateLeft($node);
            $node = $this->flipColors($node);
        }
        return $node;
    }

    private function moveRedRight(array $node): array
    {
        $node = $this->flipColors($node);
        if ($node[3] !== null && $this->isRed($node[3][3] ?? null)) {
            $node = $this->rotateRight($node);
            $node = $this->flipColors($node);
        }
        return $node;
    }

    private function minNode(?array $node): ?array
    {
        while ($node !== null && $node[3] !== null) {
            $node = $node[3];
        }
        return $node;
    }

    private function removeMin(?array $node): ?array
    {
        if ($node[3] === null) {
            return null;
        }
        if (!$this->isRed($node[3]) && !$this->isRed($node[3][3] ?? null)) {
            $node = $this->moveRedLeft($node);
        }
        $node = [$node[0], $node[1], $node[2], $this->removeMin($node[3]), $node[4]];
        return $this->balance($node);
    }

    // ============================================================
    // Sorted operations
    // ============================================================

    /**
     * Get the minimum key.
     */
    public function minKey(): mixed
    {
        $min = $this->minNode($this->root);
        return $min !== null ? $min[0] : null;
    }

    /**
     * Get the maximum key.
     */
    public function maxKey(): mixed
    {
        $node = $this->root;
        while ($node !== null && $node[4] !== null) {
            $node = $node[4];
        }
        return $node !== null ? $node[0] : null;
    }

    /**
     * Get entries >= key.
     */
    public function subseq(mixed $key): ?ISeq
    {
        $result = [];
        $this->collectFrom($this->root, $key, $result, true);
        return count($result) > 0 ? new ArraySeq($result) : null;
    }

    /**
     * Get entries in reverse order from key.
     */
    public function rsubseq(mixed $key): ?ISeq
    {
        $result = [];
        $this->collectFrom($this->root, $key, $result, false);
        return count($result) > 0 ? new ArraySeq(array_reverse($result)) : null;
    }

    private function collectFrom(?array $node, mixed $key, array &$result, bool $ascending): void
    {
        if ($node === null) {
            return;
        }

        $cmp = ($this->comparator)($key, $node[0]);

        if ($ascending) {
            if ($cmp <= 0) {
                $this->collectFrom($node[3], $key, $result, $ascending);
            }
            if ($cmp <= 0) {
                $result[] = vec($node[0], $node[1]);
            }
            $this->collectFrom($node[4], $key, $result, $ascending);
        } else {
            $this->collectFrom($node[4], $key, $result, $ascending);
            if ($cmp >= 0) {
                $result[] = vec($node[0], $node[1]);
            }
            if ($cmp >= 0) {
                $this->collectFrom($node[3], $key, $result, $ascending);
            }
        }
    }

    // ============================================================
    // Seq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        if ($this->root === null) {
            return null;
        }
        $entries = [];
        $this->inorder($this->root, $entries);
        return new ArraySeq($entries);
    }

    private function inorder(?array $node, array &$result): void
    {
        if ($node === null) {
            return;
        }
        $this->inorder($node[3], $result);
        $result[] = vec($node[0], $node[1]);
        $this->inorder($node[4], $result);
    }

    public function keys(): Vec
    {
        $keys = [];
        $this->collectKeys($this->root, $keys);
        return vec(...$keys);
    }

    private function collectKeys(?array $node, array &$result): void
    {
        if ($node === null) {
            return;
        }
        $this->collectKeys($node[3], $result);
        $result[] = $node[0];
        $this->collectKeys($node[4], $result);
    }

    public function vals(): Vec
    {
        $vals = [];
        $this->collectVals($this->root, $vals);
        return vec(...$vals);
    }

    private function collectVals(?array $node, array &$result): void
    {
        if ($node === null) {
            return;
        }
        $this->collectVals($node[3], $result);
        $result[] = $node[1];
        $this->collectVals($node[4], $result);
    }

    public function getIterator(): Traversable
    {
        $stack = [];
        $node = $this->root;

        while ($node !== null || !empty($stack)) {
            while ($node !== null) {
                $stack[] = $node;
                $node = $node[3];
            }
            $node = array_pop($stack);
            yield $node[0] => $node[1];
            $node = $node[4];
        }
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
        throw new \RuntimeException("SortedMap is immutable");
    }

    public function offsetUnset(mixed $offset): void
    {
        throw new \RuntimeException("SortedMap is immutable");
    }

    // ============================================================
    // Equality
    // ============================================================

    public function equals(mixed $other): bool
    {
        if ($other === $this) {
            return true;
        }
        if (!$other instanceof SortedMap) {
            return false;
        }
        if ($this->count !== $other->count) {
            return false;
        }
        foreach ($this as $k => $v) {
            if (!$other->containsKey($k) || !equals($v, $other->find($k))) {
                return false;
            }
        }
        return true;
    }

    public function hash(): int
    {
        $hash = 0;
        foreach ($this as $k => $v) {
            $hash ^= (hash_($k) * 31 + hash_($v));
        }
        return $hash & 0x7FFFFFFF;
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
        if ($meta === $this->meta) {
            return $this;
        }
        return new self($this->root, $this->count, $this->comparator, $meta);
    }

    public function __toString(): string
    {
        $pairs = [];
        foreach ($this as $k => $v) {
            $pairs[] = prStr($k) . ' ' . prStr($v);
        }
        return '{' . implode(', ', $pairs) . '}';
    }
}

// Constructor functions
function sortedMap(mixed ...$kvs): SortedMap
{
    $map = SortedMap::empty();
    for ($i = 0; $i < count($kvs); $i += 2) {
        $map = $map->put($kvs[$i], $kvs[$i + 1] ?? null);
    }
    return $map;
}

function sortedMapBy(callable $comparator, mixed ...$kvs): SortedMap
{
    $map = SortedMap::empty($comparator);
    for ($i = 0; $i < count($kvs); $i += 2) {
        $map = $map->put($kvs[$i], $kvs[$i + 1] ?? null);
    }
    return $map;
}
