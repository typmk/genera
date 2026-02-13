<?php

declare(strict_types=1);

namespace Clojure\Php;

use Stringable;

/**
 * Clojure Symbol - interned, identity-comparable.
 */
final class Sym implements Stringable
{
    private readonly int $hash;

    /** @var array<string, Sym> */
    private static array $intern = [];

    private static int $genCounter = 1;

    private ?Map $meta = null;

    private function __construct(
        private readonly ?string $ns,
        private readonly string $name,
    ) {
        $this->hash = $ns !== null && $ns !== ''
            ? crc32($ns . '/' . $name)
            : crc32($name);
    }

    public static function create(string $name): self
    {
        if (isset(self::$intern[$name])) {
            return self::$intern[$name];
        }

        $pos = strpos($name, '/');
        $sym = ($pos === false || $name === '/')
            ? new self(null, $name)
            : new self(substr($name, 0, $pos), substr($name, $pos + 1));

        self::$intern[$name] = $sym;
        return $sym;
    }

    public static function createNs(?string $ns, string $name): self
    {
        $key = ($ns !== null && $ns !== '')
            ? $ns . '/' . $name
            : $name;

        if (isset(self::$intern[$key])) {
            return self::$intern[$key];
        }

        $sym = new self($ns, $name);
        self::$intern[$key] = $sym;
        return $sym;
    }

    public static function gen(string $prefix = '__gensym_'): self
    {
        return new self(null, $prefix . (self::$genCounter++));
    }

    public static function resetGen(): void
    {
        self::$genCounter = 1;
    }

    public function name(): string
    {
        return $this->name;
    }

    public function ns(): ?string
    {
        return $this->ns;
    }

    public function fullName(): string
    {
        return $this->ns !== null && $this->ns !== ''
            ? $this->ns . '/' . $this->name
            : $this->name;
    }

    public function hash(): int
    {
        return $this->hash;
    }

    public function equals(mixed $other): bool
    {
        return $other instanceof self
            && $this->name === $other->name
            && $this->ns === $other->ns;
    }

    public function meta(): ?Map
    {
        return $this->meta;
    }

    public function withMeta(?Map $meta): self
    {
        if ($meta === $this->meta) {
            return $this;
        }
        $new = clone $this;
        $new->meta = $meta;
        return $new;
    }

    public function __toString(): string
    {
        return $this->fullName();
    }
}

// Constructor function
function sym(string $name, ?string $ns = null): Sym
{
    return $ns !== null ? Sym::createNs($ns, $name) : Sym::create($name);
}
