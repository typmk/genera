<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\HashSet;

use Countable;
use Clojure\Lang\ContainsInterface;

/**
 * @template V
 *
 * @extends ContainsInterface<V>
 */
interface TransientHashSetInterface extends Countable, ContainsInterface
{
    /**
     * @param V $value
     */
    public function add($value): self;

    /**
     * @param V $value
     */
    public function remove($value): self;

    public function persistent(): PersistentHashSetInterface;
}
