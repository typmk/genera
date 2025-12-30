<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\HashSet;

use Countable;
use Clojure\Lang\Collections\AsTransientInterface;
use Clojure\Lang\ConcatInterface;
use Clojure\Lang\ContainsInterface;
use Clojure\Lang\FnInterface;

/**
 * @template V
 *
 * @extends AsTransientInterface<TransientHashSetInterface>
 * @extends ContainsInterface<V>
 */
interface PersistentHashSetInterface extends Countable, AsTransientInterface, FnInterface, ConcatInterface, ContainsInterface
{
    /**
     * @param V $value
     */
    public function add($value): self;

    /**
     * @param V $value
     */
    public function remove($value): self;

    public function toPhpArray(): array;
}
