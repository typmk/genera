<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Clojure\Lang\Collections\Map\PersistentMapInterface;

trait MetaTrait
{
    private ?PersistentMapInterface $meta = null;

    public function getMeta(): ?PersistentMapInterface
    {
        return $this->meta;
    }

    public function withMeta(?PersistentMapInterface $meta)
    {
        $this->meta = $meta;
        return $this;
    }
}
