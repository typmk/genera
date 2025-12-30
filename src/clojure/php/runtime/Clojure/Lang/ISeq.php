<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Core sequence interface - mirrors clojure.lang.ISeq.
 *
 * In Clojure:
 * - first() returns the first element
 * - next() returns the rest as a seq, or null if empty (nil-punning)
 * - more() returns the rest as a seq, or empty seq if empty (never null)
 * - cons() prepends an element
 *
 * @template T
 */
interface ISeq extends Seqable, \Countable, \IteratorAggregate
{
    /**
     * Returns the first element of this sequence.
     *
     * @return T|null
     */
    public function first(): mixed;

    /**
     * Returns the rest of the sequence, or null if empty.
     * This is Clojure's nil-punning behavior.
     *
     * @return ISeq<T>|null
     */
    public function next(): ?ISeq;

    /**
     * Returns the rest of the sequence, never null.
     * Returns an empty seq if this is the last element.
     *
     * @return ISeq<T>
     */
    public function more(): ISeq;

    /**
     * Prepends an element to this sequence.
     *
     * @param T $x
     * @return ISeq<T>
     */
    public function cons(mixed $x): ISeq;

    /**
     * Convert to PHP array.
     *
     * @return array<int, T>
     */
    public function toArray(): array;
}
