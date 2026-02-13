<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;

/**
 * PersistentQueue - immutable FIFO queue.
 *
 * Implements a functional queue using two lists:
 * - front: items ready to be dequeued (in order)
 * - rear: items added (in reverse order)
 *
 * When front is empty, rear is reversed to become the new front.
 * This gives O(1) amortized time for both conj (enqueue) and pop (dequeue).
 */
final class Queue implements Seqable, \Countable, \IteratorAggregate
{
    private static ?Queue $empty = null;

    private function __construct(
        private readonly array $front,
        private readonly array $rear,
        private readonly int $count,
        private readonly ?Map $meta = null,
    ) {
    }

    public static function empty(): self
    {
        return self::$empty ??= new self([], [], 0);
    }

    public static function from(iterable $items): self
    {
        $queue = self::empty();
        foreach ($items as $item) {
            $queue = $queue->conj($item);
        }
        return $queue;
    }

    /**
     * Add an item to the rear of the queue.
     */
    public function conj(mixed $x): self
    {
        // If front is empty, put item directly in front
        if (empty($this->front)) {
            return new self([$x], [], $this->count + 1, $this->meta);
        }
        // Otherwise add to rear
        return new self($this->front, [$x, ...$this->rear], $this->count + 1, $this->meta);
    }

    /**
     * Remove the front item (oldest).
     */
    public function pop(): self
    {
        if ($this->count === 0) {
            throw new \RuntimeException("Can't pop empty queue");
        }

        $newFront = array_slice($this->front, 1);

        // If front is now empty, reverse rear to become new front
        if (empty($newFront)) {
            if (empty($this->rear)) {
                return self::empty();
            }
            return new self(array_reverse($this->rear), [], $this->count - 1, $this->meta);
        }

        return new self($newFront, $this->rear, $this->count - 1, $this->meta);
    }

    /**
     * Get the front item without removing it.
     */
    public function peek(): mixed
    {
        if ($this->count === 0) {
            return null;
        }
        return $this->front[0];
    }

    public function count(): int
    {
        return $this->count;
    }

    public function isEmpty(): bool
    {
        return $this->count === 0;
    }

    // ============================================================
    // Seq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        if ($this->count === 0) {
            return null;
        }
        // Combine front and reversed rear
        $all = [...$this->front, ...array_reverse($this->rear)];
        return new ArraySeq($all);
    }

    public function toArray(): array
    {
        return [...$this->front, ...array_reverse($this->rear)];
    }

    public function getIterator(): Traversable
    {
        foreach ($this->front as $item) {
            yield $item;
        }
        foreach (array_reverse($this->rear) as $item) {
            yield $item;
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
        if (!$other instanceof Queue) {
            return false;
        }
        if ($this->count !== $other->count) {
            return false;
        }
        $a = $this->toArray();
        $b = $other->toArray();
        for ($i = 0; $i < $this->count; $i++) {
            if (!equals($a[$i], $b[$i])) {
                return false;
            }
        }
        return true;
    }

    public function hash(): int
    {
        $hash = 0;
        foreach ($this as $item) {
            $hash = (31 * $hash + hash_($item)) & 0x7FFFFFFF;
        }
        return $hash;
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
        return new self($this->front, $this->rear, $this->count, $meta);
    }

    public function __toString(): string
    {
        return '#queue ' . prStr(vec(...$this->toArray()));
    }
}

// Constructor function
function queue(mixed ...$items): Queue
{
    if (count($items) === 0) {
        return Queue::empty();
    }
    return Queue::from($items);
}

// Extend conj to handle Queue
// Note: conj is defined in RT.php - we'll need to add Queue support there
