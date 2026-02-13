<?php

declare(strict_types=1);

namespace Clojure\Php;

use Stringable;

/**
 * Clojure Keyword - interned, identity-comparable.
 */
final class Kw implements Stringable
{
    private readonly int $hash;

    /** @var array<string, Kw> */
    private static array $intern = [];

    private ?Map $meta = null;

    private function __construct(
        private readonly ?string $ns,
        private readonly string $name,
    ) {
        $this->hash = $ns !== null && $ns !== ''
            ? crc32(':' . $ns . '/' . $name)
            : crc32(':' . $name);
    }

    public static function create(string $name): self
    {
        if (!isset(self::$intern[$name])) {
            self::$intern[$name] = new self(null, $name);
        }
        return self::$intern[$name];
    }

    public static function createNs(string $ns, string $name): self
    {
        $key = $ns . '/' . $name;
        if (!isset(self::$intern[$key])) {
            self::$intern[$key] = new self($ns, $name);
        }
        return self::$intern[$key];
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
        return $this === $other;
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

    /**
     * Keywords are callable - (:key map) returns (get map :key)
     */
    public function __invoke(mixed $map, mixed $notFound = null): mixed
    {
        return get($map, $this, $notFound);
    }

    public function __toString(): string
    {
        return ':' . $this->fullName();
    }
}

// Constructor function
function kw(string $name, ?string $ns = null): Kw
{
    return $ns !== null ? Kw::createNs($ns, $name) : Kw::create($name);
}
