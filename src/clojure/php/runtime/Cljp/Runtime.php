<?php

namespace Cljp;

use Clojure\Lang\Hasher;
use Clojure\Lang\Equalizer;
use Clojure\Lang\Printer;
use Clojure\Lang\Collections\Vector\PersistentVector;
use Clojure\Lang\Collections\Vector\PersistentVectorInterface;
use Clojure\Lang\Collections\Map\PersistentHashMap;
use Clojure\Lang\Collections\Map\PersistentMapInterface;
use Clojure\Lang\Collections\HashSet\PersistentHashSet;
use Clojure\Lang\Collections\HashSet\PersistentHashSetInterface;
use Clojure\Lang\Collections\HashSet\TransientHashSet;
use Clojure\Lang\Collections\LinkedList\PersistentList;
use Clojure\Lang\Collections\LinkedList\PersistentListInterface;
use Clojure\Lang\Keyword;
use Clojure\Lang\ExceptionInfo;
use Clojure\Lang\IExceptionInfoInterface;

class Runtime {
    private static ?Hasher $hasher = null;
    private static ?Equalizer $equalizer = null;

    public static function getHasher(): Hasher {
        if (self::$hasher === null) {
            self::$hasher = new Hasher();
        }
        return self::$hasher;
    }

    public static function getEqualizer(): Equalizer {
        if (self::$equalizer === null) {
            self::$equalizer = new Equalizer();
        }
        return self::$equalizer;
    }

    public static function vector(array $data): PersistentVectorInterface {
        return PersistentVector::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $data
        );
    }

    public static function map(array $data): PersistentMapInterface {
        return PersistentHashMap::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $data
        );
    }

    public static function set(array $data): PersistentHashSetInterface {
        $hasher = self::getHasher();
        $equalizer = self::getEqualizer();
        $map = PersistentHashMap::empty($hasher, $equalizer)->asTransient();
        $set = new TransientHashSet($hasher, $map);
        foreach ($data as $item) {
            $set->add($item);
        }
        return $set->persistent();
    }

    public static function list(array $data): PersistentListInterface {
        return PersistentList::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $data
        );
    }

    public static function keyword(string $name): Keyword {
        if ($name[0] === ':') {
            $name = substr($name, 1);
        }
        return Keyword::create($name);
    }

    public static function conj($coll, $x) {
        if ($coll instanceof PersistentVectorInterface) {
            return $coll->append($x);
        }
        if ($coll instanceof PersistentMapInterface) {
            // Assume $x is [k, v] or map?
            // Clojure conj on map expects map or entry.
            // PersistentMap put expects key, value.
            // For simplicitly, let's assume $x is a vector of two elements [k, v]
            if ($x instanceof PersistentVectorInterface && count($x) === 2) {
                return $coll->put($x->get(0), $x->get(1));
            }
        }
        if ($coll instanceof PersistentHashSetInterface) {
            return $coll->add($x);
        }
        if ($coll instanceof PersistentListInterface) {
            return $coll->cons($x);
        }
        return $coll;
    }

    public static function cons($x, $coll) {
        if ($coll instanceof PersistentListInterface) {
            return $coll->cons($x);
        }
        $arr = self::toArray($coll);
        array_unshift($arr, $x);
        return self::list($arr);
    }

    public static function str($x): string {
        if ($x === null) {
            return '';
        }
        if (is_string($x)) {
            return $x;
        }
        if (is_bool($x)) {
            return $x ? 'true' : 'false';
        }
        if (is_numeric($x)) {
            return (string)$x;
        }
        if ($x instanceof Keyword) {
            return ':' . $x->getName();
        }
        if ($x instanceof \Stringable) {
            return (string)$x;
        }
        if (is_array($x) || $x instanceof \Traversable) {
            $items = [];
            foreach ($x as $item) {
                $items[] = self::str($item);
            }
            return '[' . implode(' ', $items) . ']';
        }
        return (string)$x;
    }

    public static function toArray($coll): array {
        if ($coll === null) {
            return [];
        }
        if (is_array($coll)) {
            return $coll;
        }
        if ($coll instanceof \Traversable) {
            return iterator_to_array($coll, false);
        }
        throw new \Exception("Cannot convert to array: " . gettype($coll));
    }

    public static function equals($a, $b): bool {
        return self::getEqualizer()->equals($a, $b);
    }

    public static function assoc($map, $key, $val) {
        if ($map instanceof PersistentMapInterface) {
            return $map->put($key, $val);
        }
        if ($map instanceof PersistentVectorInterface) {
            return $map->update($key, $val);
        }
        throw new \Exception("assoc requires a map or vector");
    }

    public static function get($coll, $key) {
        if ($coll === null) {
            return null;
        }
        if ($coll instanceof PersistentMapInterface) {
            return $coll->find($key);
        }
        if ($coll instanceof PersistentVectorInterface) {
            return $coll->get($key);
        }
        if (is_array($coll)) {
            return $coll[$key] ?? null;
        }
        return null;
    }

    public static function hashMap(array $kvs): PersistentMapInterface {
        return PersistentHashMap::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $kvs
        );
    }

    public static function keys($m): ?PersistentVectorInterface {
        if ($m === null) {
            return null;
        }
        if ($m instanceof PersistentMapInterface) {
            $result = [];
            foreach ($m as $k => $v) {
                $result[] = $k;
            }
            return self::vector($result);
        }
        if (is_array($m)) {
            return self::vector(array_keys($m));
        }
        throw new \Exception("keys requires a map");
    }

    public static function vals($m): ?PersistentVectorInterface {
        if ($m === null) {
            return null;
        }
        if ($m instanceof PersistentMapInterface) {
            $result = [];
            foreach ($m as $k => $v) {
                $result[] = $v;
            }
            return self::vector($result);
        }
        if (is_array($m)) {
            return self::vector(array_values($m));
        }
        throw new \Exception("vals requires a map");
    }

    public static function contains($coll, $key): bool {
        if ($coll === null) {
            return false;
        }
        if ($coll instanceof PersistentMapInterface) {
            return $coll->find($key) !== null;
        }
        if ($coll instanceof PersistentHashSetInterface) {
            return $coll->contains($key);
        }
        if ($coll instanceof PersistentVectorInterface) {
            return $key >= 0 && $key < count($coll);
        }
        if (is_array($coll)) {
            return array_key_exists($key, $coll);
        }
        return false;
    }

    public static function dissoc($m, $key) {
        if ($m === null) {
            return null;
        }
        if ($m instanceof PersistentMapInterface) {
            return $m->remove($key);
        }
        throw new \Exception("dissoc requires a map");
    }

    /**
     * Returns a readable string representation of a value (EDN format).
     */
    public static function prStr(mixed $x): string {
        return Printer::readable()->print($x);
    }

    /**
     * Returns a non-readable string representation (for display).
     */
    public static function printStr(mixed $x): string {
        return Printer::nonReadable()->print($x);
    }

    // ============================================================
    // Exception Functions (ex-info, ex-data, ex-message, ex-cause)
    // ============================================================

    /**
     * Create an ExceptionInfo with message, data map, and optional cause.
     * (ex-info msg map) or (ex-info msg map cause)
     */
    public static function exInfo(string $msg, ?PersistentMapInterface $data = null, ?\Throwable $cause = null): ExceptionInfo {
        return new ExceptionInfo($msg, $data, $cause);
    }

    /**
     * Returns the data map from an ExceptionInfo, or nil for other exceptions.
     * (ex-data ex)
     */
    public static function exData(?\Throwable $ex): ?PersistentMapInterface {
        if ($ex instanceof IExceptionInfoInterface) {
            return $ex->getData();
        }
        return null;
    }

    /**
     * Returns the message from an exception.
     * (ex-message ex)
     */
    public static function exMessage(?\Throwable $ex): ?string {
        if ($ex === null) {
            return null;
        }
        return $ex->getMessage();
    }

    /**
     * Returns the cause (previous exception) from an exception.
     * (ex-cause ex)
     */
    public static function exCause(?\Throwable $ex): ?\Throwable {
        if ($ex === null) {
            return null;
        }
        return $ex->getPrevious();
    }

    // ============================================================
    // Reduced - for early termination in reduce
    // ============================================================

    /**
     * Wraps a value to signal early termination from reduce.
     */
    public static function reduced(mixed $val): Reduced {
        return new Reduced($val);
    }

    /**
     * Returns true if x is a Reduced value.
     */
    public static function isReduced(mixed $x): bool {
        return $x instanceof Reduced;
    }

    /**
     * Unwraps a Reduced value, returning its inner value.
     */
    public static function unreduced(mixed $x): mixed {
        if ($x instanceof Reduced) {
            return $x->deref();
        }
        return $x;
    }
}

/**
 * Reduced - wrapper class for early termination in reduce.
 * Implements IDeref for consistency with Clojure.
 */
class Reduced {
    private mixed $val;

    public function __construct(mixed $val) {
        $this->val = $val;
    }

    public function deref(): mixed {
        return $this->val;
    }
}
