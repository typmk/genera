<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\Map;

use ArrayAccess;
use Countable;
use IteratorAggregate;
use Clojure\Lang\Collections\AsTransientInterface;
use Clojure\Lang\ContainsInterface;
use Clojure\Lang\FnInterface;
use Clojure\Lang\TypeInterface;

/**
 * @template K
 * @template V
 *
 * @extends IteratorAggregate<K, V>
 * @extends ArrayAccess<K,V>
 * @extends TypeInterface<PersistentMapInterface<K, V>>
 * @extends AsTransientInterface<TransientMapInterface>
 * @extends ContainsInterface<K>
 */
interface PersistentMapInterface extends TypeInterface, Countable, IteratorAggregate, ArrayAccess, AsTransientInterface, FnInterface, ContainsInterface
{
    /**
     * @param K $key
     * @param V $value
     */
    public function put($key, $value): self;

    /**
     * @param K $key
     */
    public function remove($key): self;

    /**
     * @param K $key
     *
     * @return V
     */
    public function find($key);

    public function merge(self $other): self;
}
