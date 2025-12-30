<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\Map;

use Clojure\Lang\AbstractType;
use Clojure\Lang\Collections\Exceptions\MethodNotSupportedException;
use Clojure\Lang\EqualizerInterface;
use Clojure\Lang\Equalizer;
use Clojure\Lang\HasherInterface;
use Clojure\Lang\Hasher;

/**
 * @template K
 * @template V
 *
 * @implements PersistentMapInterface<K, V>
 *
 * @extends AbstractType<AbstractPersistentMap<K, V>>
 */
abstract class AbstractPersistentMap extends AbstractType implements PersistentMapInterface
{
    /** @var ?int Use null as sentinel - 0 can be a valid hash */
    private ?int $hashCache = null;

    public function __construct(
        protected HasherInterface $hasher,
        protected EqualizerInterface $equalizer,
        protected ?PersistentMapInterface $meta,
    ) {
    }

    /**
     * @param K $key
     *
     * @return ?V
     */
    public function __invoke($key)
    {
        return $this->find($key);
    }

    public function getMeta(): ?PersistentMapInterface
    {
        return $this->meta;
    }

    public function hash(): int
    {
        if ($this->hashCache === null) {
            $hash = 1;
            foreach ($this as $key => $value) {
                $hash += Hasher::hashValue($key) ^ Hasher::hashValue($value);
            }
            $this->hashCache = $hash;
        }

        return $this->hashCache;
    }

    public function equals(mixed $other): bool
    {
        if (!$other instanceof PersistentMapInterface) {
            return false;
        }

        if ($this->count() !== $other->count()) {
            return false;
        }

        foreach ($this as $key => $value) {
            if (!$other->contains($key)) {
                return false;
            }

            if (!Equalizer::areEqual($value, $other->find($key))) {
                return false;
            }
        }

        return true;
    }

    public function merge(PersistentMapInterface $other): PersistentMapInterface
    {
        $m = $this;
        foreach ($other as $k => $v) {
            $m = $m->put($k, $v);
        }

        return $m;
    }

    /**
     * @param K $offset
     *
     * @return V|null
     */
    public function offsetGet($offset): mixed
    {
        return $this->find($offset);
    }

    /**
     * @param K $offset
     */
    public function offsetExists($offset): bool
    {
        return $this->contains($offset);
    }

    public function offsetSet($offset, $value): void
    {
        throw new MethodNotSupportedException('Method offsetSet is not supported on PersistentMap');
    }

    public function offsetUnset($offset): void
    {
        throw new MethodNotSupportedException('Method offsetUnset is not supported on PersistentMap');
    }
}
