<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;

/**
 * PersistentTreeSet - sorted immutable set.
 *
 * Backed by a SortedMap where each key maps to itself.
 */
final class SortedSet implements Seqable, \Countable, \IteratorAggregate
{
    private static ?SortedSet $empty = null;

    private function __construct(
        private readonly SortedMap $impl,
        private readonly ?Map $meta = null,
    ) {
    }

    public static function empty(?callable $comparator = null): self
    {
        if ($comparator === null && self::$empty !== null) {
            return self::$empty;
        }
        $set = new self(SortedMap::empty($comparator));
        if ($comparator === null) {
            self::$empty = $set;
        }
        return $set;
    }

    public static function from(iterable $items, ?callable $comparator = null): self
    {
        $set = self::empty($comparator);
        foreach ($items as $item) {
            $set = $set->add($item);
        }
        return $set;
    }

    // ============================================================
    // Core operations
    // ============================================================

    public function add(mixed $x): self
    {
        $newImpl = $this->impl->put($x, $x);
        return new self($newImpl, $this->meta);
    }

    public function remove(mixed $x): self
    {
        if (!$this->contains($x)) {
            return $this;
        }
        $newImpl = $this->impl->remove($x);
        return new self($newImpl, $this->meta);
    }

    public function contains(mixed $x): bool
    {
        return $this->impl->containsKey($x);
    }

    public function count(): int
    {
        return $this->impl->count();
    }

    // ============================================================
    // Sorted operations
    // ============================================================

    /**
     * Get the minimum element.
     */
    public function first(): mixed
    {
        return $this->impl->minKey();
    }

    /**
     * Get the maximum element.
     */
    public function last(): mixed
    {
        return $this->impl->maxKey();
    }

    /**
     * Get elements >= x.
     */
    public function subseq(mixed $x): ?ISeq
    {
        $result = $this->impl->subseq($x);
        if ($result === null) {
            return null;
        }
        // Extract just the keys (values in sorted-set)
        $values = [];
        $s = $result;
        while ($s !== null) {
            $entry = $s->first();
            $values[] = $entry->get(0);
            $s = $s->next();
        }
        return count($values) > 0 ? new ArraySeq($values) : null;
    }

    /**
     * Get elements in reverse order from x.
     */
    public function rsubseq(mixed $x): ?ISeq
    {
        $result = $this->impl->rsubseq($x);
        if ($result === null) {
            return null;
        }
        $values = [];
        $s = $result;
        while ($s !== null) {
            $entry = $s->first();
            $values[] = $entry->get(0);
            $s = $s->next();
        }
        return count($values) > 0 ? new ArraySeq($values) : null;
    }

    // ============================================================
    // Seq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        if ($this->impl->count() === 0) {
            return null;
        }
        $keys = [];
        foreach ($this->impl as $k => $_) {
            $keys[] = $k;
        }
        return new ArraySeq($keys);
    }

    public function toArray(): array
    {
        $result = [];
        foreach ($this->impl as $k => $_) {
            $result[] = $k;
        }
        return $result;
    }

    public function getIterator(): Traversable
    {
        foreach ($this->impl as $k => $_) {
            yield $k;
        }
    }

    // ============================================================
    // Equality
    // ============================================================

    public function equals(mixed $other): bool
    {
        if ($other === $this) {
            return true;
        }
        if (!$other instanceof SortedSet) {
            return false;
        }
        if ($this->count() !== $other->count()) {
            return false;
        }
        foreach ($this as $x) {
            if (!$other->contains($x)) {
                return false;
            }
        }
        return true;
    }

    public function hash(): int
    {
        $hash = 0;
        foreach ($this as $x) {
            $hash ^= hash_($x);
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
        return new self($this->impl, $meta);
    }

    public function __toString(): string
    {
        $items = [];
        foreach ($this as $x) {
            $items[] = prStr($x);
        }
        return '#{' . implode(' ', $items) . '}';
    }
}

// Constructor functions
function sortedSet(mixed ...$items): SortedSet
{
    return SortedSet::from($items);
}

function sortedSetBy(callable $comparator, mixed ...$items): SortedSet
{
    return SortedSet::from($items, $comparator);
}
