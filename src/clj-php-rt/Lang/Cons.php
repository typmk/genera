<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Traversable;

/**
 * A cons cell - prepends a value to a sequence.
 * Immutable pair of (first, rest).
 *
 * @template T
 * @implements ISeq<T>
 */
final class Cons implements ISeq
{
    /**
     * @param T $first The first element
     * @param ISeq<T>|array<T>|null $rest The rest of the sequence
     */
    public function __construct(
        private readonly mixed $first,
        private readonly mixed $rest
    ) {
    }

    /**
     * @inheritDoc
     */
    public function seq(): ?ISeq
    {
        return $this;
    }

    /**
     * @inheritDoc
     */
    public function first(): mixed
    {
        return $this->first;
    }

    /**
     * @inheritDoc
     */
    public function next(): ?ISeq
    {
        if ($this->rest === null) {
            return null;
        }

        if ($this->rest instanceof ISeq) {
            return $this->rest->seq();
        }

        if (is_array($this->rest)) {
            return count($this->rest) > 0 ? new ArraySeq($this->rest) : null;
        }

        return null;
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
        $count = 1;
        $seq = $this->next();
        while ($seq !== null) {
            $count++;
            $seq = $seq->next();
        }
        return $count;
    }

    /**
     * @inheritDoc
     */
    public function toArray(): array
    {
        $result = [$this->first];
        $seq = $this->next();
        while ($seq !== null) {
            $result[] = $seq->first();
            $seq = $seq->next();
        }
        return $result;
    }

    /**
     * @inheritDoc
     */
    public function getIterator(): Traversable
    {
        yield $this->first;
        $seq = $this->next();
        while ($seq !== null) {
            yield $seq->first();
            $seq = $seq->next();
        }
    }
}
