<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Interface for types that can produce a sequence.
 * Mirrors clojure.lang.Seqable.
 *
 * @template T
 */
interface Seqable
{
    /**
     * Returns a seq on this collection.
     * Returns null if the collection is empty (nil-punning).
     *
     * @return ISeq<T>|null
     */
    public function seq(): ?ISeq;
}
