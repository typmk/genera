<?php

declare(strict_types=1);

namespace Clojure\Lang;

use RuntimeException;

use function gettype;
use function is_int;
use function is_object;
use function is_scalar;
use function is_string;

/**
 * This Hasher is inspired by the Clojurescript implementation.
 * These constants are the same hash value as in clojure.
 *
 * Uses singleton pattern for performance - collections don't need to store hasher instances.
 */
final class Hasher implements HasherInterface
{
    private const NULL_HASH_VALUE = 0;

    private const TRUE_HASH_VALUE = 1231;

    private const FALSE_HASH_VALUE = 1237;

    private const POSITIVE_INF_HASH_VALUE = 2_146_435_072;

    private const NEGATIVE_INF_HASH_VALUE = -1_048_576;

    private const DEFAULT_FLOAT_HASH_VALUE = 2_146_959_360;

    // Cache size limit to prevent memory bloat
    private const STRING_CACHE_MAX = 10000;

    private static ?self $instance = null;

    // String hash cache - strings are frequently used as map keys
    private array $stringCache = [];
    private int $stringCacheCount = 0;

    /**
     * Get the singleton instance.
     */
    public static function getInstance(): self
    {
        return self::$instance ??= new self();
    }

    /**
     * Static convenience method for hashing values.
     */
    public static function hashValue(mixed $value): int
    {
        return self::getInstance()->hash($value);
    }

    /**
     * @param mixed $value The value to hash
     *
     * @return int The hash of the given value
     */
    public function hash(mixed $value): int
    {
        // Fast path: integers are their own hash (most common for array-like access)
        if (is_int($value)) {
            return $value;
        }

        // Fast path: strings with cache
        if (is_string($value)) {
            return $this->stringCache[$value] ?? $this->hashAndCacheString($value);
        }

        if ($value === null) {
            return self::NULL_HASH_VALUE;
        }

        if ($value instanceof HashableInterface) {
            return $value->hash();
        }

        if ($value === true) {
            return self::TRUE_HASH_VALUE;
        }

        if ($value === false) {
            return self::FALSE_HASH_VALUE;
        }

        if (is_float($value)) {
            return $this->hashFloat($value);
        }

        if (is_object($value)) {
            return crc32(spl_object_hash($value));
        }

        throw new RuntimeException('This type is not hashable: ' . gettype($value));
    }

    private function hashAndCacheString(string $value): int
    {
        $hash = crc32($value);

        if ($this->stringCacheCount < self::STRING_CACHE_MAX) {
            $this->stringCache[$value] = $hash;
            $this->stringCacheCount++;
        }

        return $hash;
    }

    private function hashFloat(float $value): int
    {
        if (is_finite($value)) {
            return (int)($value);
        }

        if ($value === INF) {
            return self::POSITIVE_INF_HASH_VALUE;
        }

        if ($value === -INF) {
            return self::NEGATIVE_INF_HASH_VALUE;
        }

        return self::DEFAULT_FLOAT_HASH_VALUE;
    }
}
