<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections;

/**
 * @template TransientType
 */
interface AsTransientInterface
{
    /**
     * @return TransientType
     */
    public function asTransient();
}
