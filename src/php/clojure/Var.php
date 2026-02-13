<?php

declare(strict_types=1);

/**
 * Var - a mutable container for values with dynamic binding support.
 */

namespace Clojure\Php;

/**
 * Var - holds a root value and supports thread-local (stack-based) bindings.
 */
final class CljVar
{
    private static array $registry = [];
    private static array $bindingStack = [];

    private mixed $root;
    private ?Map $meta = null;
    private bool $dynamic = false;
    private ?string $ns = null;
    private ?string $name = null;

    private function __construct(mixed $root, ?string $ns = null, ?string $name = null)
    {
        $this->root = $root;
        $this->ns = $ns;
        $this->name = $name;
    }

    /**
     * Create or get a Var in a namespace.
     */
    public static function intern(string $ns, string $name, mixed $root = null): self
    {
        $key = "$ns/$name";
        if (!isset(self::$registry[$key])) {
            self::$registry[$key] = new self($root, $ns, $name);
        } elseif ($root !== null) {
            self::$registry[$key]->root = $root;
        }
        return self::$registry[$key];
    }

    /**
     * Find a Var by namespace and name.
     */
    public static function find(string $ns, string $name): ?self
    {
        $key = "$ns/$name";
        return self::$registry[$key] ?? null;
    }

    /**
     * Get all Vars in a namespace.
     */
    public static function nsVars(string $ns): array
    {
        $vars = [];
        $prefix = "$ns/";
        foreach (self::$registry as $key => $var) {
            if (str_starts_with($key, $prefix)) {
                $vars[substr($key, strlen($prefix))] = $var;
            }
        }
        return $vars;
    }

    // ============================================================
    // Value access
    // ============================================================

    /**
     * Get the current value (checks bindings first if dynamic).
     */
    public function deref(): mixed
    {
        if ($this->dynamic && !empty(self::$bindingStack)) {
            $key = $this->qualifiedName();
            $frame = self::$bindingStack[count(self::$bindingStack) - 1];
            if (array_key_exists($key, $frame)) {
                return $frame[$key];
            }
        }
        return $this->root;
    }

    /**
     * Get the root value (ignores bindings).
     */
    public function getRawRoot(): mixed
    {
        return $this->root;
    }

    /**
     * Set the root value.
     */
    public function setRoot(mixed $val): self
    {
        $this->root = $val;
        return $this;
    }

    /**
     * Alter the root value with a function.
     */
    public function alterRoot(callable $f, mixed ...$args): mixed
    {
        $this->root = $f($this->root, ...$args);
        return $this->root;
    }

    // ============================================================
    // Dynamic binding
    // ============================================================

    /**
     * Mark this Var as dynamic.
     */
    public function setDynamic(bool $dynamic = true): self
    {
        $this->dynamic = $dynamic;
        return $this;
    }

    /**
     * Check if this Var is dynamic.
     */
    public function isDynamic(): bool
    {
        return $this->dynamic;
    }

    /**
     * Push a new binding frame.
     */
    public static function pushBindings(array $bindings): void
    {
        $frame = [];
        foreach ($bindings as $var => $val) {
            if ($var instanceof CljVar) {
                if (!$var->isDynamic()) {
                    throw new \Exception("Can't bind non-dynamic var: " . $var->qualifiedName());
                }
                $frame[$var->qualifiedName()] = $val;
            } else {
                $frame[(string)$var] = $val;
            }
        }
        self::$bindingStack[] = $frame;
    }

    /**
     * Pop the current binding frame.
     */
    public static function popBindings(): void
    {
        if (empty(self::$bindingStack)) {
            throw new \Exception("No binding frame to pop");
        }
        array_pop(self::$bindingStack);
    }

    /**
     * Get the current thread binding for this var.
     */
    public function getThreadBinding(): mixed
    {
        if (!empty(self::$bindingStack)) {
            $key = $this->qualifiedName();
            $frame = self::$bindingStack[count(self::$bindingStack) - 1];
            if (array_key_exists($key, $frame)) {
                return $frame[$key];
            }
        }
        return null;
    }

    /**
     * Set the thread binding for this var.
     */
    public function setThreadBinding(mixed $val): void
    {
        if (empty(self::$bindingStack)) {
            throw new \Exception("No binding frame for set!");
        }
        if (!$this->dynamic) {
            throw new \Exception("Can't set! non-dynamic var: " . $this->qualifiedName());
        }
        $key = $this->qualifiedName();
        self::$bindingStack[count(self::$bindingStack) - 1][$key] = $val;
    }

    // ============================================================
    // Identity
    // ============================================================

    public function qualifiedName(): string
    {
        if ($this->ns !== null && $this->name !== null) {
            return "{$this->ns}/{$this->name}";
        }
        return $this->name ?? '#<Var@' . spl_object_id($this) . '>';
    }

    public function ns(): ?string
    {
        return $this->ns;
    }

    public function name(): ?string
    {
        return $this->name;
    }

    // ============================================================
    // Callable
    // ============================================================

    public function __invoke(mixed ...$args): mixed
    {
        $val = $this->deref();
        if (!is_callable($val)) {
            throw new \Exception("Var {$this->qualifiedName()} is not callable");
        }
        return $val(...$args);
    }

    // ============================================================
    // Meta
    // ============================================================

    public function meta(): ?Map
    {
        return $this->meta;
    }

    public function setMeta(Map $meta): self
    {
        $this->meta = $meta;
        return $this;
    }

    public function alterMeta(callable $f, mixed ...$args): Map
    {
        $this->meta = $f($this->meta ?? hashMap(), ...$args);
        return $this->meta;
    }

    public function __toString(): string
    {
        return "#'" . $this->qualifiedName();
    }
}

// ============================================================
// Var functions
// ============================================================

/**
 * Intern a Var in a namespace.
 */
function defvar(string $ns, string $name, mixed $val = null): CljVar
{
    return CljVar::intern($ns, $name, $val);
}

/**
 * Find a Var by qualified name.
 */
function findVar(string $ns, string $name): ?CljVar
{
    return CljVar::find($ns, $name);
}

/**
 * Execute a function with dynamic bindings in effect.
 */
function withBindings(array $bindings, callable $f): mixed
{
    CljVar::pushBindings($bindings);
    try {
        return $f();
    } finally {
        CljVar::popBindings();
    }
}

/**
 * Set thread-local binding for a dynamic var.
 */
function varSet(CljVar $var, mixed $val): mixed
{
    $var->setThreadBinding($val);
    return $val;
}

/**
 * Get the root value of a Var.
 */
function varGet(CljVar $var): mixed
{
    return $var->deref();
}
