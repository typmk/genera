<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * @template T of ConcatInterface
 */
interface ConcatInterface
{
    /**
     * Concatenates a value to the data structure.
     *
     * @param array<int, mixed> $xs The value to concatenate
     *
     * @return T
     */
    public function concat($xs);
}
