<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\Vector;

use ArrayAccess;
use Countable;
use IteratorAggregate;
use Clojure\Lang\Collections\AsTransientInterface;
use Clojure\Lang\ConcatInterface;
use Clojure\Lang\ContainsInterface;
use Clojure\Lang\FnInterface;
use Clojure\Lang\PushInterface;
use Clojure\Lang\SeqInterface;
use Clojure\Lang\SliceInterface;
use Clojure\Lang\TypeInterface;

/**
 * @template T
 *
 * @extends TypeInterface<PersistentVectorInterface<T>>
 * @extends SeqInterface<T, PersistentVectorInterface<T>>
 * @extends IteratorAggregate<T>
 * @extends ArrayAccess<T>
 * @extends ConcatInterface<PersistentVectorInterface<T>>
 * @extends PushInterface<PersistentVectorInterface<T>>
 * @extends AsTransientInterface<TransientVectorInterface>
 * @extends ContainsInterface<int>
 */
interface PersistentVectorInterface extends TypeInterface, SeqInterface, IteratorAggregate, Countable, ArrayAccess, ConcatInterface, PushInterface, SliceInterface, AsTransientInterface, FnInterface, ContainsInterface
{
    public const BRANCH_FACTOR = 32;

    public const INDEX_MASK = self::BRANCH_FACTOR - 1;

    public const SHIFT = 5;

    /**
     * @param T $value
     */
    public function append($value): self;

    /**
     * @param T $value
     */
    public function update(int $i, $value): self;

    /**
     * @return T
     */
    public function get(int $i);

    public function pop(): self;
}
