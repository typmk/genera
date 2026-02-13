<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;
use Countable;
use IteratorAggregate;

/**
 * Persistent Linked List.
 *
 * Efficient prepend (cons), but O(n) for count and random access.
 */
final class PList implements ISeq, Countable, IteratorAggregate
{
    private ?Map $meta = null;
    private ?int $cachedCount = null;

    private function __construct(
        private readonly mixed $head,
        private readonly ?PList $tail,
    ) {
    }

    public static function empty(): EmptyList
    {
        static $empty = null;
        return $empty ??= new EmptyList();
    }

    public static function from(array $vals): ISeq
    {
        if (count($vals) === 0) {
            return self::empty();
        }
        $list = null;
        for ($i = count($vals) - 1; $i >= 0; $i--) {
            $list = new PList($vals[$i], $list);
        }
        return $list;
    }

    public static function of(mixed $head, ?PList $tail = null): self
    {
        return new self($head, $tail);
    }

    public function count(): int
    {
        if ($this->cachedCount === null) {
            $c = 1;
            $t = $this->tail;
            while ($t !== null) {
                $c++;
                $t = $t->tail;
            }
            $this->cachedCount = $c;
        }
        return $this->cachedCount;
    }

    // ============================================================
    // ISeq interface
    // ============================================================

    public function seq(): ?ISeq
    {
        return $this;
    }

    public function first(): mixed
    {
        return $this->head;
    }

    public function next(): ?ISeq
    {
        return $this->tail;
    }

    public function more(): ISeq
    {
        return $this->tail ?? self::empty();
    }

    public function cons(mixed $x): ISeq
    {
        return new PList($x, $this);
    }

    public function toArray(): array
    {
        $arr = [];
        $l = $this;
        while ($l !== null) {
            $arr[] = $l->head;
            $l = $l->tail;
        }
        return $arr;
    }

    // ============================================================
    // Equality & Hashing
    // ============================================================

    public function hash(): int
    {
        $h = 1;
        foreach ($this as $v) {
            $h = 31 * $h + hash_($v);
        }
        return $h;
    }

    public function equals(mixed $other): bool
    {
        if ($this === $other) return true;
        if (!$other instanceof ISeq) return false;
        $s1 = $this->seq();
        $s2 = seq($other);
        while ($s1 !== null && $s2 !== null) {
            if (!equals($s1->first(), $s2->first())) {
                return false;
            }
            $s1 = $s1->next();
            $s2 = $s2->next();
        }
        return $s1 === null && $s2 === null;
    }

    // ============================================================
    // Meta
    // ============================================================

    public function meta(): ?Map
    {
        return $this->meta;
    }

    public function withMeta(?Map $meta): self
    {
        if ($meta === $this->meta) return $this;
        $new = clone $this;
        $new->meta = $meta;
        return $new;
    }

    // ============================================================
    // IteratorAggregate
    // ============================================================

    public function getIterator(): Traversable
    {
        $l = $this;
        while ($l !== null) {
            yield $l->head;
            $l = $l->tail;
        }
    }

    public function __toString(): string
    {
        return prStr($this);
    }
}

/**
 * Empty list singleton.
 */
final class EmptyList implements ISeq, Countable
{
    public function seq(): ?ISeq
    {
        return null;
    }

    public function first(): mixed
    {
        return null;
    }

    public function next(): ?ISeq
    {
        return null;
    }

    public function more(): ISeq
    {
        return $this;
    }

    public function cons(mixed $x): ISeq
    {
        return PList::of($x, null);
    }

    public function count(): int
    {
        return 0;
    }

    public function toArray(): array
    {
        return [];
    }

    public function getIterator(): Traversable
    {
        return new \EmptyIterator();
    }

    public function hash(): int
    {
        return 1;
    }

    public function equals(mixed $other): bool
    {
        return $other instanceof EmptyList || (is_iterable($other) && isEmpty($other));
    }

    public function __toString(): string
    {
        return '()';
    }
}

// Constructor function
function plist(mixed ...$vals): ISeq
{
    return PList::from($vals);
}
