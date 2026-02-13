<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Traversable;

/**
 * A sequence backed by a PHP array.
 * Provides ISeq interface over array values.
 *
 * @template T
 * @implements ISeq<T>
 */
final class ArraySeq implements ISeq
{
    /** @var array<int, T> */
    private readonly array $array;
    private readonly int $offset;
    private readonly int $count;

    /**
     * @param array<T> $array The backing array
     * @param int $offset Starting offset (for efficient rest)
     */
    public function __construct(array $array, int $offset = 0)
    {
        $this->array = array_values($array);
        $this->offset = $offset;
        $this->count = count($array) - $offset;
    }

    /**
     * @inheritDoc
     */
    public function seq(): ?ISeq
    {
        return $this->count > 0 ? $this : null;
    }

    /**
     * @inheritDoc
     */
    public function first(): mixed
    {
        return $this->count > 0 ? $this->array[$this->offset] : null;
    }

    /**
     * @inheritDoc
     */
    public function next(): ?ISeq
    {
        if ($this->count <= 1) {
            return null;
        }
        return new ArraySeq($this->array, $this->offset + 1);
    }

    /**
     * @inheritDoc
     */
    public function more(): ISeq
    {
        $n = $this->next();
        return $n ?? new EmptySeq();
    }

    /**
     * Alias for more() - returns the rest of the sequence.
     */
    public function rest(): ISeq
    {
        return $this->more();
    }

    /**
     * @inheritDoc
     */
    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    /**
     * @inheritDoc
     */
    public function count(): int
    {
        return $this->count;
    }

    /**
     * @inheritDoc
     */
    public function toArray(): array
    {
        return array_slice($this->array, $this->offset);
    }

    /**
     * @inheritDoc
     */
    public function getIterator(): Traversable
    {
        for ($i = $this->offset; $i < count($this->array); $i++) {
            yield $this->array[$i];
        }
    }
}
