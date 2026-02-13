<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Value equality checker.
 *
 * Uses singleton pattern for performance - collections don't need to store equalizer instances.
 */
final class Equalizer implements EqualizerInterface
{
    private static ?self $instance = null;

    /**
     * Get the singleton instance.
     */
    public static function getInstance(): self
    {
        return self::$instance ??= new self();
    }

    /**
     * Static convenience method for equality checks.
     */
    public static function areEqual(mixed $a, mixed $b): bool
    {
        return self::getInstance()->equals($a, $b);
    }

    /**
     * @param mixed $a Left value
     * @param mixed $b Right value
     *
     * @return bool True, if $a is equals $b
     */
    public function equals(mixed $a, mixed $b): bool
    {
        // Fast path: identical values (covers most cases)
        if ($a === $b) {
            return true;
        }

        // Only check EqualsInterface if not identical
        if ($a instanceof EqualsInterface) {
            return $a->equals($b);
        }

        return false;
    }
}
