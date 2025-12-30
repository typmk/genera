<?php

declare(strict_types=1);

namespace Clojure\Lang\Collections\Map;

final class Box
{
    // Reusable instances to avoid allocations
    private static ?self $true = null;
    private static ?self $false = null;

    public function __construct(public mixed $value)
    {
    }

    /**
     * Get a reusable Box initialized to false.
     * Use reset() before reusing.
     */
    public static function reusableFalse(): self
    {
        if (self::$false === null) {
            self::$false = new self(false);
        }
        self::$false->value = false;
        return self::$false;
    }

    public function getValue(): mixed
    {
        return $this->value;
    }

    public function setValue(mixed $value): void
    {
        $this->value = $value;
    }
}
