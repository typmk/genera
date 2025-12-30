<?php

declare(strict_types=1);

namespace Clojure\Lang;

interface HashableInterface
{
    /**
     * @return int Return the hash of this object
     */
    public function hash(): int;
}
