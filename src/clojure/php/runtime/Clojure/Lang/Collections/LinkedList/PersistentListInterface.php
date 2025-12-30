<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\LinkedList;

use ArrayAccess;
use Countable;
use IteratorAggregate;
use Clojure\Lang\Collections\Exceptions\IndexOutOfBoundsException;
use Clojure\Lang\ConcatInterface;
use Clojure\Lang\ConsInterface;
use Clojure\Lang\ContainsInterface;
use Clojure\Lang\SeqInterface;
use Clojure\Lang\TypeInterface;

/**
 * @template TValue
 *
 * @extends SeqInterface<TValue, PersistentListInterface<TValue>>
 * @extends TypeInterface<PersistentListInterface<TValue>>
 * @extends IteratorAggregate<TValue>
 * @extends ConsInterface<PersistentListInterface<TValue>>
 * @extends ArrayAccess<TValue>
 * @extends ConcatInterface<PersistentListInterface<TValue>>
 * @extends ContainsInterface<int>
 */
interface PersistentListInterface extends TypeInterface, SeqInterface, IteratorAggregate, Countable, ConsInterface, ArrayAccess, ConcatInterface, ContainsInterface
{
    /**
     * @param TValue $value
     *
     * @return PersistentListInterface<TValue>
     */
    public function prepend($value): self;

    /**
     * @throws IndexOutOfBoundsException
     *
     * @return TValue
     */
    public function get(int $i);

    /**
     * @return PersistentListInterface<TValue>
     */
    public function pop(): self;
}
