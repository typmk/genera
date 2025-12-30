<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Clojure\Lang\Collections\Map\PersistentMapInterface;

/**
 * @template TSelf
 */
interface MetaInterface
{
    public function getMeta(): ?PersistentMapInterface;

    /**
     * @return TSelf
     */
    public function withMeta(?PersistentMapInterface $meta);
}
