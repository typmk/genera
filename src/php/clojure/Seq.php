<?php

declare(strict_types=1);

namespace Clojure\Php;

use Traversable;
use EmptyIterator;

/**
 * Seqable - anything that can produce a sequence.
 */
interface Seqable
{
    public function seq(): ?ISeq;
}

/**
 * Core sequence interface - mirrors clojure.lang.ISeq.
 *
 * @template T
 */
interface ISeq extends Seqable, \Countable, \IteratorAggregate
{
    /** @return T|null */
    public function first(): mixed;

    /** @return ISeq<T>|null */
    public function next(): ?ISeq;

    /** @return ISeq<T> */
    public function more(): ISeq;

    /** @return ISeq<T> */
    public function cons(mixed $x): ISeq;

    /** @return array<int, T> */
    public function toArray(): array;
}

/**
 * The empty sequence singleton.
 */
final class EmptySeq implements ISeq
{
    private static ?EmptySeq $instance = null;

    public static function instance(): EmptySeq
    {
        return self::$instance ??= new EmptySeq();
    }

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
        return new Cons($x, null);
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
        return new EmptyIterator();
    }
}

/**
 * A cons cell - prepends a value to a sequence.
 *
 * @template T
 * @implements ISeq<T>
 */
final class Cons implements ISeq
{
    public function __construct(
        private readonly mixed $first,
        private readonly mixed $rest
    ) {
    }

    public function seq(): ?ISeq
    {
        return $this;
    }

    public function first(): mixed
    {
        return $this->first;
    }

    public function next(): ?ISeq
    {
        if ($this->rest === null) {
            return null;
        }
        if ($this->rest instanceof ISeq) {
            return $this->rest->seq();
        }
        if (is_array($this->rest)) {
            return count($this->rest) > 0 ? new ArraySeq($this->rest) : null;
        }
        return null;
    }

    public function more(): ISeq
    {
        return $this->next() ?? EmptySeq::instance();
    }

    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    public function count(): int
    {
        $count = 1;
        $seq = $this->next();
        while ($seq !== null) {
            $count++;
            $seq = $seq->next();
        }
        return $count;
    }

    public function toArray(): array
    {
        $result = [$this->first];
        $seq = $this->next();
        while ($seq !== null) {
            $result[] = $seq->first();
            $seq = $seq->next();
        }
        return $result;
    }

    public function getIterator(): Traversable
    {
        yield $this->first;
        $seq = $this->next();
        while ($seq !== null) {
            yield $seq->first();
            $seq = $seq->next();
        }
    }
}

/**
 * A sequence backed by a PHP array.
 *
 * @template T
 * @implements ISeq<T>
 */
final class ArraySeq implements ISeq
{
    private readonly array $array;
    private readonly int $offset;
    private readonly int $count;

    public function __construct(array $array, int $offset = 0)
    {
        $this->array = array_values($array);
        $this->offset = $offset;
        $this->count = count($array) - $offset;
    }

    public function seq(): ?ISeq
    {
        return $this->count > 0 ? $this : null;
    }

    public function first(): mixed
    {
        return $this->count > 0 ? $this->array[$this->offset] : null;
    }

    public function next(): ?ISeq
    {
        if ($this->count <= 1) {
            return null;
        }
        return new ArraySeq($this->array, $this->offset + 1);
    }

    public function more(): ISeq
    {
        return $this->next() ?? EmptySeq::instance();
    }

    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    public function count(): int
    {
        return $this->count;
    }

    public function toArray(): array
    {
        return array_slice($this->array, $this->offset);
    }

    public function getIterator(): Traversable
    {
        for ($i = $this->offset; $i < count($this->array); $i++) {
            yield $this->array[$i];
        }
    }
}

/**
 * A lazy sequence that wraps a thunk (closure).
 * The thunk is called once on first access, and the result is cached.
 *
 * @template T
 * @implements ISeq<T>
 */
final class LazySeq implements ISeq
{
    private $thunk;
    private mixed $realized = null;
    private bool $isRealized = false;

    public function __construct(callable $thunk)
    {
        $this->thunk = $thunk;
    }

    private function realize(): mixed
    {
        if (!$this->isRealized) {
            $this->isRealized = true;
            $result = ($this->thunk)();
            $this->thunk = null;

            while ($result instanceof LazySeq) {
                $result = $result->realize();
            }
            $this->realized = $result;
        }
        return $this->realized;
    }

    public function seq(): ?ISeq
    {
        $result = $this->realize();
        if ($result === null) {
            return null;
        }
        if ($result instanceof ISeq) {
            return $result->seq();
        }
        if (is_array($result)) {
            return count($result) === 0 ? null : $this;
        }
        return $this;
    }

    public function first(): mixed
    {
        $result = $this->realize();
        if ($result === null) {
            return null;
        }
        if (is_object($result) && method_exists($result, 'first')) {
            return $result->first();
        }
        if (is_array($result)) {
            return count($result) > 0 ? reset($result) : null;
        }
        return $result;
    }

    public function next(): ?ISeq
    {
        $result = $this->realize();
        if ($result === null) {
            return null;
        }
        if ($result instanceof ISeq) {
            return $result->next();
        }
        if (is_array($result)) {
            $rest = array_slice($result, 1);
            return count($rest) > 0 ? new ArraySeq($rest) : null;
        }
        return null;
    }

    public function more(): ISeq
    {
        return $this->next() ?? EmptySeq::instance();
    }

    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    public function count(): int
    {
        $count = 0;
        $seq = $this->seq();
        while ($seq !== null) {
            $count++;
            $seq = $seq->next();
        }
        return $count;
    }

    public function toArray(): array
    {
        $result = [];
        $seq = $this->seq();
        while ($seq !== null) {
            $result[] = $seq->first();
            $seq = $seq->next();
        }
        return $result;
    }

    public function getIterator(): Traversable
    {
        $seq = $this->seq();
        while ($seq !== null) {
            yield $seq->first();
            $seq = $seq->next();
        }
    }

    public function isRealized(): bool
    {
        return $this->isRealized;
    }
}

// Constructor functions

function cons(mixed $x, mixed $coll): Cons
{
    return new Cons($x, $coll);
}

function lazySeq(callable $thunk): LazySeq
{
    return new LazySeq($thunk);
}

// ============================================================
// Chunked Sequences - process 32 elements per thunk
// ============================================================

/**
 * ChunkedSeq - A sequence backed by chunks of 32 elements.
 *
 * Instead of 1 thunk per element (100 thunks for 100 elements),
 * we use 1 thunk per 32 elements (4 thunks for 100 elements).
 * This dramatically reduces overhead for map/filter operations.
 */
final class ChunkedSeq implements ISeq
{
    private const CHUNK_SIZE = 32;

    public function __construct(
        private readonly array $chunk,      // current chunk (up to 32 elements)
        private readonly int $offset,       // offset within chunk
        private readonly mixed $restThunk,  // thunk for remaining chunks
    ) {
    }

    /**
     * Create a chunked seq from an array.
     */
    public static function fromArray(array $arr): ?ISeq
    {
        if (count($arr) === 0) {
            return null;
        }
        if (count($arr) <= self::CHUNK_SIZE) {
            return new self(array_values($arr), 0, null);
        }

        $firstChunk = array_slice($arr, 0, self::CHUNK_SIZE);
        $rest = array_slice($arr, self::CHUNK_SIZE);

        return new self($firstChunk, 0, fn() => self::fromArray($rest));
    }

    /**
     * Create a chunked seq from a pre-chunked array with explicit rest thunk.
     * Used by chunked map/filter for efficiency.
     */
    public static function fromArrayWithRest(array $chunk, ?callable $restThunk): ?ISeq
    {
        if (count($chunk) === 0) {
            // Empty chunk - try rest
            if ($restThunk !== null) {
                return $restThunk();
            }
            return null;
        }
        return new self(array_values($chunk), 0, $restThunk);
    }

    public function seq(): ?ISeq
    {
        return $this->offset < count($this->chunk) ? $this : null;
    }

    public function first(): mixed
    {
        return $this->chunk[$this->offset] ?? null;
    }

    public function next(): ?ISeq
    {
        // More elements in this chunk?
        if ($this->offset + 1 < count($this->chunk)) {
            return new self($this->chunk, $this->offset + 1, $this->restThunk);
        }
        // Get next chunk
        if ($this->restThunk !== null) {
            $rest = ($this->restThunk)();
            return $rest instanceof ISeq ? $rest->seq() : null;
        }
        return null;
    }

    public function more(): ISeq
    {
        return $this->next() ?? EmptySeq::instance();
    }

    public function cons(mixed $x): ISeq
    {
        return new Cons($x, $this);
    }

    public function count(): int
    {
        $count = count($this->chunk) - $this->offset;
        if ($this->restThunk !== null) {
            $rest = ($this->restThunk)();
            if ($rest instanceof ISeq) {
                $count += $rest->count();
            }
        }
        return $count;
    }

    public function toArray(): array
    {
        $result = array_slice($this->chunk, $this->offset);
        if ($this->restThunk !== null) {
            $rest = ($this->restThunk)();
            if ($rest instanceof ISeq) {
                $result = array_merge($result, $rest->toArray());
            }
        }
        return $result;
    }

    public function getIterator(): Traversable
    {
        // Yield from current chunk
        for ($i = $this->offset; $i < count($this->chunk); $i++) {
            yield $this->chunk[$i];
        }
        // Yield from rest
        if ($this->restThunk !== null) {
            $rest = ($this->restThunk)();
            if ($rest instanceof ISeq) {
                foreach ($rest as $v) {
                    yield $v;
                }
            }
        }
    }

    /**
     * Get remaining elements in current chunk (for efficient processing).
     */
    public function chunkRest(): array
    {
        return array_slice($this->chunk, $this->offset + 1);
    }

    /**
     * Get the thunk for remaining chunks.
     */
    public function restChunks(): mixed
    {
        return $this->restThunk;
    }
}

/**
 * Apply a function to a collection in chunks.
 * Much faster than element-by-element LazySeq.
 */
function chunkedMap(callable $f, mixed $coll): ?ISeq
{
    $arr = [];
    $s = seq($coll);
    while ($s !== null) {
        $arr[] = $s->first();
        $s = $s->next();
    }

    if (count($arr) === 0) {
        return null;
    }

    return ChunkedSeq::fromArray(array_map($f, $arr));
}

/**
 * Filter a collection in chunks.
 */
function chunkedFilter(callable $pred, mixed $coll): ?ISeq
{
    $arr = [];
    $s = seq($coll);
    while ($s !== null) {
        $arr[] = $s->first();
        $s = $s->next();
    }

    if (count($arr) === 0) {
        return null;
    }

    return ChunkedSeq::fromArray(array_filter($arr, $pred));
}
