<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Clojure Multimethod - runtime polymorphic dispatch.
 *
 * Multimethods provide ad-hoc polymorphism based on arbitrary dispatch values,
 * not just types. The dispatch function determines which method implementation
 * to call.
 *
 * Usage:
 *   // Define multimethod with dispatch function
 *   $area = new MultiFn(fn($shape) => $shape['type']);
 *
 *   // Register methods for dispatch values
 *   $area->addMethod('circle', fn($s) => M_PI * $s['r'] ** 2);
 *   $area->addMethod('rect', fn($s) => $s['w'] * $s['h']);
 *   $area->addMethod(':default', fn($s) => 0);
 *
 *   // Invoke
 *   $area(['type' => 'circle', 'r' => 5]);  // 78.54...
 *   $area(['type' => 'rect', 'w' => 3, 'h' => 4]);  // 12
 */
final class MultiFn
{
    /** @var callable */
    private $dispatchFn;

    /** @var array<string|int, callable> */
    private array $methods = [];

    /** @var callable|null */
    private $defaultMethod = null;

    /** @var string */
    private string $name;

    /** @var array<string|int, array<string|int, bool>> Hierarchy for isa? relationships */
    private static array $hierarchy = [];

    /**
     * @param callable $dispatchFn Function that returns the dispatch value
     * @param string $name Optional name for error messages
     */
    public function __construct(callable $dispatchFn, string $name = 'anonymous')
    {
        $this->dispatchFn = $dispatchFn;
        $this->name = $name;
    }

    /**
     * Add a method implementation for a dispatch value.
     *
     * @param string|int|null $dispatchVal The dispatch value (null for default)
     * @param callable $method The implementation
     */
    public function addMethod(string|int|null $dispatchVal, callable $method): self
    {
        if ($dispatchVal === null || $dispatchVal === ':default' || $dispatchVal === 'default') {
            $this->defaultMethod = $method;
        } else {
            $this->methods[$dispatchVal] = $method;
        }
        return $this;
    }

    /**
     * Remove a method for a dispatch value.
     */
    public function removeMethod(string|int $dispatchVal): self
    {
        unset($this->methods[$dispatchVal]);
        return $this;
    }

    /**
     * Get the method for a dispatch value, or null.
     */
    public function getMethod(string|int $dispatchVal): ?callable
    {
        return $this->methods[$dispatchVal] ?? null;
    }

    /**
     * Get all registered dispatch values.
     *
     * @return array<string|int>
     */
    public function getMethods(): array
    {
        return array_keys($this->methods);
    }

    /**
     * Prefer one dispatch value over another for ambiguous cases.
     * (Simplified - full Clojure has prefer-method)
     */
    public function preferMethod(string|int $preferred, string|int $over): self
    {
        // Store preference in hierarchy
        self::$hierarchy[$preferred][$over] = true;
        return $this;
    }

    /**
     * Invoke the multimethod.
     */
    public function __invoke(mixed ...$args): mixed
    {
        $dispatchVal = ($this->dispatchFn)(...$args);

        // Direct match
        if (isset($this->methods[$dispatchVal])) {
            return ($this->methods[$dispatchVal])(...$args);
        }

        // Try isa? hierarchy match
        foreach ($this->methods as $methodVal => $method) {
            if ($this->isa($dispatchVal, $methodVal)) {
                return $method(...$args);
            }
        }

        // Default method
        if ($this->defaultMethod !== null) {
            return ($this->defaultMethod)(...$args);
        }

        // No method found
        throw new \RuntimeException(
            "No method in multimethod '{$this->name}' for dispatch value: " .
            (is_string($dispatchVal) ? $dispatchVal : var_export($dispatchVal, true))
        );
    }

    /**
     * Check if child isa? parent in the hierarchy.
     * Simplified version - full Clojure has derive/underive.
     */
    private function isa(mixed $child, mixed $parent): bool
    {
        if ($child === $parent) {
            return true;
        }

        // Check explicit hierarchy
        if (isset(self::$hierarchy[$child][$parent])) {
            return true;
        }

        // For keywords represented as strings starting with :
        if (is_string($child) && is_string($parent)) {
            // Check class hierarchy for class names
            if (class_exists($child) && class_exists($parent)) {
                return is_subclass_of($child, $parent);
            }
        }

        return false;
    }

    /**
     * Derive - establish an isa? relationship.
     */
    public static function derive(string|int $child, string|int $parent): void
    {
        self::$hierarchy[$child][$parent] = true;
    }

    /**
     * Underive - remove an isa? relationship.
     */
    public static function underive(string|int $child, string|int $parent): void
    {
        unset(self::$hierarchy[$child][$parent]);
    }
}
