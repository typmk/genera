<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\Map;

use Clojure\Lang\EqualizerInterface;
use Clojure\Lang\HasherInterface;
use Traversable;

use function array_key_exists;
use function count;

/**
 * @template K
 * @template V
 *
 * @implements HashMapNodeInterface<K, V>
 */
final class IndexedNode implements HashMapNodeInterface
{
    // Cache empty node per hasher/equalizer pair
    private static array $emptyCache = [];

    /**
     * @param list<array{0: K|null, 1: V|HashMapNodeInterface<K, V>}> $objects
     */
    public function __construct(
        private readonly HasherInterface $hasher,
        private readonly EqualizerInterface $equalizer,
        private array $objects,
    ) {
    }

    public static function empty(HasherInterface $hasher, EqualizerInterface $equalizer): self
    {
        // Cache empty nodes - they're immutable so safe to reuse
        $key = spl_object_id($hasher) . ':' . spl_object_id($equalizer);
        return self::$emptyCache[$key] ??= new self($hasher, $equalizer, []);
    }

    /**
     * @param K $key
     * @param V $value
     *
     * @return HashMapNodeInterface<K, V>
     */
    public function put(int $shift, int $hash, $key, $value, Box $addedLeaf): HashMapNodeInterface
    {
        // Inline mask calculation for hot path
        $index = ($hash >> $shift) & 0x01f;
        $entry = $this->objects[$index] ?? null;

        if ($entry !== null) {
            $currentKey = $entry[0];
            $currentValue = $entry[1];

            if ($currentKey === null) {
                // Child node - delegate
                /** @var HashMapNodeInterface $childNode */
                $childNode = $currentValue;
                $newChild = $childNode->put($shift + 5, $hash, $key, $value, $addedLeaf);
                if ($childNode === $newChild) {
                    return $this;
                }
                $newObjects = $this->objects;
                $newObjects[$index][1] = $newChild;
                return new self($this->hasher, $this->equalizer, $newObjects);
            }

            // Fast path: identical key check before equals()
            if ($key === $currentKey || $this->equalizer->equals($key, $currentKey)) {
                // Update existing key - check if value changed
                if ($currentValue === $value || $this->equalizer->equals($value, $currentValue)) {
                    return $this;
                }
                $newObjects = $this->objects;
                $newObjects[$index][1] = $value;
                return new self($this->hasher, $this->equalizer, $newObjects);
            }

            // Hash collision at this level - create child node
            $addedLeaf->value = true;
            $newObjects = $this->objects;
            $newObjects[$index] = [null, $this->createNode($shift + 5, $currentKey, $currentValue, $hash, $key, $value)];
            return new self($this->hasher, $this->equalizer, $newObjects);
        }

        // Empty slot - insert new key
        if (count($this->objects) >= 16) {
            return $this->splitNode($index, $shift, $hash, $key, $value, $addedLeaf);
        }

        $newObjects = $this->objects;
        $newObjects[$index] = [$key, $value];
        $addedLeaf->value = true;
        return new self($this->hasher, $this->equalizer, $newObjects);
    }

    /**
     * @param mixed $key
     */
    public function remove(int $shift, int $hash, $key): ?HashMapNodeInterface
    {
        // Inline mask calculation
        $index = ($hash >> $shift) & 0x01f;
        $entry = $this->objects[$index] ?? null;

        if ($entry === null) {
            return $this;
        }

        $currentKey = $entry[0];
        $currentValue = $entry[1];

        if ($currentKey === null) {
            // Child node
            /** @var HashMapNodeInterface $node */
            $node = $currentValue;
            $n = $node->remove($shift + 5, $hash, $key);

            if ($n === $node) {
                return $this;
            }

            if ($n instanceof HashMapNodeInterface) {
                $newObjects = $this->objects;
                $newObjects[$index][1] = $n;
                return new self($this->hasher, $this->equalizer, $newObjects);
            }

            if (count($this->objects) === 1) {
                return null;
            }

            $newObjects = $this->objects;
            unset($newObjects[$index]);
            return new self($this->hasher, $this->equalizer, $newObjects);
        }

        // Fast path: identical key check
        if ($key === $currentKey || $this->equalizer->equals($key, $currentKey)) {
            if (count($this->objects) === 1) {
                return null;
            }

            $newObjects = $this->objects;
            unset($newObjects[$index]);
            return new self($this->hasher, $this->equalizer, $newObjects);
        }

        return $this;
    }

    /**
     * @param mixed $key
     * @param mixed $notFound
     *
     * @return ?mixed
     */
    public function find(int $shift, int $hash, $key, $notFound)
    {
        $entry = $this->objects[($hash >> $shift) & 0x01f] ?? null;

        if ($entry === null) {
            return $notFound;
        }

        $currentKey = $entry[0];

        if ($currentKey === null) {
            // Child node
            return $entry[1]->find($shift + 5, $hash, $key, $notFound);
        }

        // Fast path: identical keys (same string interned, same int, etc)
        if ($key === $currentKey || $this->equalizer->equals($key, $currentKey)) {
            return $entry[1];
        }

        return $notFound;
    }

    public function getIterator(): Traversable
    {
        return new IndexedNodeIterator($this->objects);
    }

    /**
     * @param K $key1
     * @param V $value1
     * @param K $key2
     * @param V $value2
     *
     * @return HashMapNodeInterface<K, V>
     */
    private function createNode(int $shift, mixed $key1, $value1, int $key2Hash, $key2, $value2): HashMapNodeInterface
    {
        $key1Hash = $this->hasher->hash($key1);
        if ($key1Hash === $key2Hash) {
            return new HashCollisionNode($this->hasher, $this->equalizer, $key1Hash, 2, [$key1, $value1, $key2, $value2]);
        }

        $addedLeaf = new Box(null);
        return self::empty($this->hasher, $this->equalizer)
            ->put($shift, $key1Hash, $key1, $value1, $addedLeaf)
            ->put($shift, $key2Hash, $key2, $value2, $addedLeaf);
    }

    /**
     * @param K $key
     * @param V $value
     *
     * @return HashMapNodeInterface<K, V>
     */
    private function splitNode(int $idx, int $shift, int $hash, $key, $value, Box $addedLeaf): HashMapNodeInterface
    {
        $nodes = []; // array_fill(0, 32, null);
        $empty = self::empty($this->hasher, $this->equalizer);
        $nodes[$idx] = $empty->put($shift + 5, $hash, $key, $value, $addedLeaf);
        for ($i = 0; $i < 32; ++$i) {
            if (array_key_exists($i, $this->objects)) {
                /** @var V $v */
                [$k, $v] = $this->objects[$i];
                $nodes[$i] = ($k === null) ? $v : $empty->put($shift + 5, $this->hasher->hash($k), $k, $v, $addedLeaf);
            }
        }

        return new ArrayNode($this->hasher, $this->equalizer, count($this->objects) + 1, $nodes);
    }
}
