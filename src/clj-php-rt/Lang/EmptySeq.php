<?php

declare(strict_types=1);

namespace Clojure\Lang;

use EmptyIterator;
use Traversable;

/**
 * The empty sequence singleton.
 * Returned by more() when there are no more elements.
 *
 * @implements ISeq<never>
 */
final class EmptySeq implements ISeq
{
    /**
     * @inheritDoc
     */
    public function seq(): ?ISeq
    {
        return null;
    }

    /**
     * @inheritDoc
     */
    public function first(): mixed
    {
        return null;
    }

    /**
     * @inheritDoc
     */
    public function next(): ?ISeq
    {
        return null;
    }

    /**
     * @inheritDoc
     */
    public function more(): ISeq
    {
        return $this;
    }

    /**
     * Alias for more() - returns the rest of the sequence.
     */
    public function rest(): ISeq
    {
        return $this;
    }

    /**
     * @inheritDoc
     */
    public function cons(mixed $x): ISeq
    {
        return new Cons($x, null);
    }

    /**
     * @inheritDoc
     */
    public function count(): int
    {
        return 0;
    }

    /**
     * @inheritDoc
     */
    public function toArray(): array
    {
        return [];
    }

    /**
     * @inheritDoc
     */
    public function getIterator(): Traversable
    {
        return new EmptyIterator();
    }
}
