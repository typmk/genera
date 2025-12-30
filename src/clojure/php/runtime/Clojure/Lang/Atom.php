<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Clojure Atom - thread-safe mutable reference.
 *
 * Atoms provide a way to manage shared, synchronous, independent state.
 * They support swap! (apply function to current value) and reset! (set new value).
 *
 * In PHP (single-threaded), we don't need CAS, but we maintain the API
 * for compatibility with Clojure code.
 *
 * Usage:
 *   $a = new Atom(0);
 *   $a->deref();           // 0
 *   $a->reset(42);         // 42
 *   $a->swap(fn($x) => $x + 1);  // 43
 *
 * @template T
 */
final class Atom
{
    /** @var T */
    private mixed $value;

    /** @var array<callable> */
    private array $watches = [];

    /** @var array<callable> */
    private array $validators = [];

    /**
     * @param T $value
     */
    public function __construct(mixed $value)
    {
        $this->value = $value;
    }

    /**
     * Returns the current value.
     *
     * @return T
     */
    public function deref(): mixed
    {
        return $this->value;
    }

    /**
     * Sets the value to newval without checking the current value.
     * Returns the new value.
     *
     * @param T $newval
     * @return T
     */
    public function reset(mixed $newval): mixed
    {
        $oldval = $this->value;

        // Run validators
        foreach ($this->validators as $validator) {
            if (!$validator($newval)) {
                throw new \InvalidArgumentException("Invalid atom value");
            }
        }

        $this->value = $newval;

        // Notify watches
        foreach ($this->watches as $key => $watch) {
            $watch($key, $this, $oldval, $newval);
        }

        return $newval;
    }

    /**
     * Atomically swaps the value by applying f to current value and args.
     * Returns the new value.
     *
     * swap!(atom, f) = reset!(atom, f(deref(atom)))
     * swap!(atom, f, x) = reset!(atom, f(deref(atom), x))
     * swap!(atom, f, x, y) = reset!(atom, f(deref(atom), x, y))
     *
     * @param callable $f
     * @param mixed ...$args
     * @return T
     */
    public function swap(callable $f, mixed ...$args): mixed
    {
        $oldval = $this->value;
        $newval = $f($oldval, ...$args);
        return $this->reset($newval);
    }

    /**
     * Atomically swaps the value, returning [old-value, new-value].
     *
     * @param callable $f
     * @param mixed ...$args
     * @return array{0: T, 1: T}
     */
    public function swapVals(callable $f, mixed ...$args): array
    {
        $oldval = $this->value;
        $newval = $this->swap($f, ...$args);
        return [$oldval, $newval];
    }

    /**
     * Atomically resets the value, returning [old-value, new-value].
     *
     * @param T $newval
     * @return array{0: T, 1: T}
     */
    public function resetVals(mixed $newval): array
    {
        $oldval = $this->value;
        $this->reset($newval);
        return [$oldval, $newval];
    }

    /**
     * Compare-and-set: set value to newval if current value is oldval.
     * Returns true if set succeeded.
     *
     * @param T $oldval
     * @param T $newval
     * @return bool
     */
    public function compareAndSet(mixed $oldval, mixed $newval): bool
    {
        if ($this->value === $oldval) {
            $this->reset($newval);
            return true;
        }
        return false;
    }

    /**
     * Add a watch function that will be called when the atom's value changes.
     *
     * @param string|int $key Unique key for this watch
     * @param callable $fn fn(key, atom, old-val, new-val)
     */
    public function addWatch(string|int $key, callable $fn): self
    {
        $this->watches[$key] = $fn;
        return $this;
    }

    /**
     * Remove a watch by key.
     */
    public function removeWatch(string|int $key): self
    {
        unset($this->watches[$key]);
        return $this;
    }

    /**
     * Add a validator function that must return true for new values.
     *
     * @param callable $fn fn(new-val) -> bool
     */
    public function setValidator(callable $fn): self
    {
        $this->validators[] = $fn;
        return $this;
    }
}
