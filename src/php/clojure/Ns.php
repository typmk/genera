<?php

declare(strict_types=1);

/**
 * Namespace registry - symbol tables for Clojure namespaces.
 */

namespace Clojure\Php;

/**
 * Namespace - holds mappings from symbols to Vars/classes/aliases.
 */
final class Ns
{
    private static array $registry = [];
    private static ?self $current = null;

    private string $name;
    private array $mappings = [];     // symbol-name => Var
    private array $aliases = [];      // alias => Ns name
    private array $refers = [];       // symbol-name => Var (from other ns)
    private ?Map $meta = null;

    private function __construct(string $name)
    {
        $this->name = $name;
    }

    /**
     * Find or create a namespace.
     */
    public static function findOrCreate(string $name): self
    {
        if (!isset(self::$registry[$name])) {
            self::$registry[$name] = new self($name);
        }
        return self::$registry[$name];
    }

    /**
     * Find a namespace (returns null if not found).
     */
    public static function find(string $name): ?self
    {
        return self::$registry[$name] ?? null;
    }

    /**
     * Get all namespaces.
     */
    public static function all(): array
    {
        return array_values(self::$registry);
    }

    /**
     * Remove a namespace.
     */
    public static function remove(string $name): ?self
    {
        $ns = self::$registry[$name] ?? null;
        unset(self::$registry[$name]);
        return $ns;
    }

    /**
     * Get/set the current namespace.
     */
    public static function current(): ?self
    {
        return self::$current;
    }

    public static function setCurrent(self $ns): void
    {
        self::$current = $ns;
    }

    // ============================================================
    // Identity
    // ============================================================

    public function name(): string
    {
        return $this->name;
    }

    // ============================================================
    // Mappings (intern)
    // ============================================================

    /**
     * Intern a Var in this namespace.
     */
    public function intern(string $name, mixed $val = null): CljVar
    {
        $var = CljVar::intern($this->name, $name, $val);
        $this->mappings[$name] = $var;
        return $var;
    }

    /**
     * Get a mapping (Var) by name.
     */
    public function getMapping(string $name): ?CljVar
    {
        return $this->mappings[$name] ?? $this->refers[$name] ?? null;
    }

    /**
     * Get all mappings.
     */
    public function getMappings(): array
    {
        return array_merge($this->refers, $this->mappings);
    }

    /**
     * Unmap a symbol.
     */
    public function unmap(string $name): void
    {
        unset($this->mappings[$name]);
        unset($this->refers[$name]);
    }

    // ============================================================
    // Refers (use symbols from other namespaces)
    // ============================================================

    /**
     * Refer a Var from another namespace.
     */
    public function refer(string $localName, CljVar $var): void
    {
        $this->refers[$localName] = $var;
    }

    /**
     * Refer all public Vars from another namespace.
     */
    public function referAll(string $nsName): void
    {
        $otherNs = self::find($nsName);
        if ($otherNs === null) {
            throw new \Exception("No namespace: $nsName");
        }
        foreach ($otherNs->getMappings() as $name => $var) {
            // Only refer public vars (no private metadata check for simplicity)
            $this->refers[$name] = $var;
        }
    }

    /**
     * Get refers.
     */
    public function getRefers(): array
    {
        return $this->refers;
    }

    // ============================================================
    // Aliases
    // ============================================================

    /**
     * Add an alias for another namespace.
     */
    public function addAlias(string $alias, string $nsName): void
    {
        if (isset($this->aliases[$alias]) && $this->aliases[$alias] !== $nsName) {
            throw new \Exception("Alias $alias already refers to {$this->aliases[$alias]}");
        }
        $this->aliases[$alias] = $nsName;
    }

    /**
     * Remove an alias.
     */
    public function removeAlias(string $alias): void
    {
        unset($this->aliases[$alias]);
    }

    /**
     * Get the namespace for an alias.
     */
    public function getAlias(string $alias): ?self
    {
        $nsName = $this->aliases[$alias] ?? null;
        return $nsName !== null ? self::find($nsName) : null;
    }

    /**
     * Get all aliases.
     */
    public function getAliases(): array
    {
        $result = [];
        foreach ($this->aliases as $alias => $nsName) {
            $result[$alias] = self::find($nsName);
        }
        return $result;
    }

    // ============================================================
    // Resolve
    // ============================================================

    /**
     * Resolve a symbol to a Var.
     */
    public function resolve(Sym $sym): ?CljVar
    {
        $ns = $sym->ns();
        $name = $sym->name();

        if ($ns !== null) {
            // Qualified symbol
            $targetNs = $this->getAlias($ns) ?? self::find($ns);
            if ($targetNs === null) {
                return null;
            }
            return $targetNs->getMapping($name);
        }

        // Unqualified - check local mappings and refers
        return $this->getMapping($name);
    }

    // ============================================================
    // Meta
    // ============================================================

    public function meta(): ?Map
    {
        return $this->meta;
    }

    public function resetMeta(Map $meta): Map
    {
        $this->meta = $meta;
        return $meta;
    }

    public function alterMeta(callable $f, mixed ...$args): Map
    {
        $this->meta = $f($this->meta ?? hashMap(), ...$args);
        return $this->meta;
    }

    public function __toString(): string
    {
        return $this->name;
    }
}

// ============================================================
// Namespace functions
// ============================================================

/**
 * Switch to or create a namespace (like Clojure's in-ns).
 */
function inNs(string $name): Ns
{
    $ns = Ns::findOrCreate($name);
    Ns::setCurrent($ns);
    return $ns;
}

/**
 * Get the current namespace.
 */
function currentNs(): ?Ns
{
    return Ns::current();
}

/**
 * Find a namespace by name.
 */
function findNs(string $name): ?Ns
{
    return Ns::find($name);
}

/**
 * Get all namespaces.
 */
function allNs(): array
{
    return Ns::all();
}

/**
 * Create an alias for a namespace.
 */
function aliasNs(string $alias, string $nsName): void
{
    $current = Ns::current();
    if ($current === null) {
        throw new \Exception("No current namespace");
    }
    $current->addAlias($alias, $nsName);
}

/**
 * Resolve a symbol in the current namespace.
 */
function resolveVar(Sym $sym): ?CljVar
{
    $current = Ns::current();
    if ($current === null) {
        return null;
    }
    return $current->resolve($sym);
}

/**
 * Get the name of a namespace.
 */
function nsName(Ns $ns): string
{
    return $ns->name();
}

/**
 * Get all mappings in a namespace.
 */
function nsMap(Ns $ns): array
{
    return $ns->getMappings();
}

/**
 * Get all aliases in a namespace.
 */
function nsAliases(Ns $ns): array
{
    return $ns->getAliases();
}

/**
 * Get all refers in a namespace.
 */
function nsRefers(Ns $ns): array
{
    return $ns->getRefers();
}
