<?php

declare(strict_types=1);

namespace Clojure\Lang;

interface EqualsInterface
{
    /**
     * @return bool True, if this is equals $other
     */
    public function equals(mixed $other): bool;
}
