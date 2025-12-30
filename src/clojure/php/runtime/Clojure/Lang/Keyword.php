<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Clojure\Lang\Collections\Map\PersistentMapInterface;

final class Keyword extends AbstractType implements IdenticalInterface, FnInterface, NamedInterface
{
    use MetaTrait;

    private readonly int $hash;

    /** @var array<string, Keyword> */
    private static array $refStore = [];

    private function __construct(
        private readonly ?string $namespace,
        private readonly string $name,
    ) {
        $this->hash = $namespace !== null && $namespace !== ''
            ? crc32(':' . $namespace . '/' . $name)
            : crc32(':' . $name);
    }

    public function __invoke(PersistentMapInterface $obj, float|bool|int|string|TypeInterface $default = null)
    {
        return $obj[$this] ?? $default;
    }

    public static function create(string $name): self
    {
        if (!isset(self::$refStore[$name])) {
            self::$refStore[$name] = new self(null, $name);
        }
        return self::$refStore[$name];
    }

    public static function createForNamespace(string $namespace, string $name): self
    {
        $key = $namespace . '/' . $name;
        if (!isset(self::$refStore[$key])) {
            self::$refStore[$key] = new self($namespace, $name);
        }
        return self::$refStore[$key];
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getNamespace(): ?string
    {
        return $this->namespace;
    }

    public function getFullName(): string
    {
        if ($this->namespace !== null && $this->namespace !== '') {
            return $this->namespace . '/' . $this->name;
        }

        return $this->name;
    }

    public function hash(): int
    {
        return $this->hash;
    }

    public function equals(mixed $other): bool
    {
        return $this->identical($other);
    }

    public function identical(mixed $other): bool
    {
        // With interning, keywords can be compared by reference
        return $this === $other;
    }
}
