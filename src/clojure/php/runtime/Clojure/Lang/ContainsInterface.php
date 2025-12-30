<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * @template K
 */
interface ContainsInterface
{
    /**
     * @param K $key
     */
    public function contains($key): bool;
}
