<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Traversable;

/**
 * A lazy sequence that wraps a thunk (closure).
 * The thunk is called once on first access, and the result is cached.
 *
 * This mirrors Clojure's LazySeq behavior:
 * - Thunk returns either nil, a value, or a seq
 * - Result is memoized after first realization
 * - Supports ISeq interface for first/next/more
 *
 * Usage in Clojure:
 *   (lazy-seq (cons 1 (lazy-seq (cons 2 nil))))
 *
 * @template T
 * @implements ISeq<T>
 */
final class LazySeq implements ISeq
{
    /** @var callable|null The thunk to call for producing the sequence */
    private $thunk;

    /** @var ISeq<T>|array<T>|null The realized sequence (cached) */
    private mixed $realized = null;

    /** @var bool Whether the thunk has been called */
    private bool $isRealized = false;

    /**
     * @param callable $thunk A function that returns a seq (or null)
     */
    public function __construct(callable $thunk)
    {
        $this->thunk = $thunk;
    }

    /**
     * Force realization of the lazy sequence.
     * Calls the thunk once and caches the result.
     *
     * @return ISeq<T>|array<T>|null
     */
    private function realize(): mixed
    {
        if (!$this->isRealized) {
            $this->isRealized = true;
            $result = ($this->thunk)();
            $this->thunk = null; // Release thunk for GC

            // Unwrap nested LazySeqs
            while ($result instanceof LazySeq) {
                $result = $result->realize();
            }

            $this->realized = $result;
        }
        return $this->realized;
    }

    /**
     * Get the underlying seq (forces realization).
     *
     * @return ISeq<T>|null
     */
    public function seq(): ?ISeq
    {
        $result = $this->realize();

        if ($result === null) {
            return null;
        }

        if ($result instanceof ISeq) {
            return $result;
        }

        // If result is an array, wrap first element in this and track rest
        if (is_array($result)) {
            if (count($result) === 0) {
                return null;
            }
            return $this; // Return self since we have the array
        }

        return $this;
    }

    /**
     * @inheritDoc
     */
    public function first(): mixed
    {
        $result = $this->realize();

        if ($result === null) {
            return null;
        }

        // Handle anything with a first() method (ISeq, SeqInterface, PersistentList, etc.)
        if (is_object($result) && method_exists($result, 'first')) {
            return $result->first();
        }

        if (is_array($result)) {
            return count($result) > 0 ? reset($result) : null;
        }

        // Single value
        return $result;
    }

    /**
     * @inheritDoc
     */
    public function next(): ?ISeq
    {
        $result = $this->realize();

        if ($result === null) {
            return null;
        }

        // Handle anything with cdr() (Clojure-style next that returns null when empty)
        if (is_object($result) && method_exists($result, 'cdr')) {
            $cdr = $result->cdr();
            if ($cdr === null) {
                return null;
            }
            // Wrap non-ISeq in ArraySeq if needed
            if ($cdr instanceof ISeq) {
                return $cdr;
            }
            // PersistentList returns PersistentList from cdr()
            return new LazySeq(fn() => $cdr);
        }

        if (is_object($result) && method_exists($result, 'next')) {
            return $result->next();
        }

        if (is_array($result)) {
            $rest = array_slice($result, 1);
            return count($rest) > 0 ? new ArraySeq($rest) : null;
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
     * Used by runtime.cljc which calls .rest() on sequences.
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
        // Warning: This forces full realization!
        $count = 0;
        $seq = $this->seq();
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
        $result = [];
        $seq = $this->seq();
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
        $seq = $this->seq();
        while ($seq !== null) {
            yield $seq->first();
            $seq = $seq->next();
        }
    }

    /**
     * Check if the sequence has been realized.
     */
    public function isRealized(): bool
    {
        return $this->isRealized;
    }
}
