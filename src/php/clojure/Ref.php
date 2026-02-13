<?php

declare(strict_types=1);

namespace Clojure\Php;

/**
 * Ref - coordinated synchronous reference for STM.
 *
 * Refs provide coordinated, synchronous changes to shared state via
 * software transactional memory (STM). Multiple refs can be modified
 * in a single atomic transaction using dosync.
 *
 * In Swoole, we use coroutine-safe locks for MVCC-like semantics.
 * In non-Swoole, we use a simpler lock-based approach.
 */
final class Ref
{
    private static int $txnId = 0;
    private static ?int $currentTxn = null;
    private static array $txnWrites = [];
    private static array $txnReads = [];

    private mixed $value;
    private int $version = 0;
    private ?Map $meta = null;
    /** @var callable|null */
    private $validator = null;

    /** @var \Swoole\Lock|null */
    private ?object $lock = null;

    public function __construct(mixed $val, ?callable $validator = null)
    {
        if ($validator !== null && !$validator($val)) {
            throw new \InvalidArgumentException("Validator rejected initial value");
        }
        $this->value = $val;
        $this->validator = $validator;

        if (class_exists(\Swoole\Lock::class)) {
            $this->lock = new \Swoole\Lock(SWOOLE_RWLOCK);
        }
    }

    public function deref(): mixed
    {
        // In a transaction, return the txn-local value if written
        if (self::$currentTxn !== null) {
            $id = spl_object_id($this);
            if (isset(self::$txnWrites[$id])) {
                return self::$txnWrites[$id];
            }
            // Track reads for conflict detection
            self::$txnReads[$id] = $this->version;
        }
        return $this->value;
    }

    /**
     * Alter ref value with function (must be in transaction).
     */
    public function alter(callable $f, mixed ...$args): mixed
    {
        $this->ensureInTransaction();

        $id = spl_object_id($this);
        $current = isset(self::$txnWrites[$id]) ? self::$txnWrites[$id] : $this->value;
        $newVal = $f($current, ...$args);

        if ($this->validator !== null && !($this->validator)($newVal)) {
            throw new \InvalidArgumentException("Validator rejected value");
        }

        self::$txnWrites[$id] = $newVal;
        return $newVal;
    }

    /**
     * Set ref value directly (must be in transaction).
     */
    public function refSet(mixed $val): mixed
    {
        $this->ensureInTransaction();

        if ($this->validator !== null && !($this->validator)($val)) {
            throw new \InvalidArgumentException("Validator rejected value");
        }

        $id = spl_object_id($this);
        self::$txnWrites[$id] = $val;
        return $val;
    }

    /**
     * Commute - like alter but allows more concurrency.
     * The function may be called multiple times during retry.
     */
    public function commute(callable $f, mixed ...$args): mixed
    {
        // For simplicity, commute behaves like alter
        // A full implementation would defer the computation
        return $this->alter($f, ...$args);
    }

    private function ensureInTransaction(): void
    {
        if (self::$currentTxn === null) {
            throw new \RuntimeException("No transaction running (missing dosync?)");
        }
    }

    // ============================================================
    // Transaction support
    // ============================================================

    /**
     * Start a new transaction.
     */
    public static function startTransaction(): int
    {
        if (self::$currentTxn !== null) {
            // Nested transaction - just return current
            return self::$currentTxn;
        }
        self::$txnId++;
        self::$currentTxn = self::$txnId;
        self::$txnWrites = [];
        self::$txnReads = [];
        return self::$currentTxn;
    }

    /**
     * Commit the current transaction.
     */
    public static function commitTransaction(): bool
    {
        if (self::$currentTxn === null) {
            throw new \RuntimeException("No transaction to commit");
        }

        // Validate reads haven't changed
        foreach (self::$txnReads as $id => $expectedVersion) {
            $ref = self::findRefById($id);
            if ($ref !== null && $ref->version !== $expectedVersion) {
                // Conflict detected
                self::abortTransaction();
                return false;
            }
        }

        // Apply all writes atomically
        foreach (self::$txnWrites as $id => $val) {
            $ref = self::findRefById($id);
            if ($ref !== null) {
                if ($ref->lock) {
                    $ref->lock->lock();
                }
                try {
                    $ref->value = $val;
                    $ref->version++;
                } finally {
                    if ($ref->lock) {
                        $ref->lock->unlock();
                    }
                }
            }
        }

        self::$currentTxn = null;
        self::$txnWrites = [];
        self::$txnReads = [];
        return true;
    }

    /**
     * Abort the current transaction.
     */
    public static function abortTransaction(): void
    {
        self::$currentTxn = null;
        self::$txnWrites = [];
        self::$txnReads = [];
    }

    /**
     * Find ref by object ID (for transaction commit).
     */
    private static array $refRegistry = [];

    public static function findRefById(int $id): ?Ref
    {
        return self::$refRegistry[$id] ?? null;
    }

    public function register(): void
    {
        self::$refRegistry[spl_object_id($this)] = $this;
    }

    public function unregister(): void
    {
        unset(self::$refRegistry[spl_object_id($this)]);
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

    public function setValidator(?callable $fn): self
    {
        if ($fn !== null && !$fn($this->value)) {
            throw new \InvalidArgumentException("Validator rejected current value");
        }
        $this->validator = $fn;
        return $this;
    }

    public function __toString(): string
    {
        return '#<Ref@' . spl_object_id($this) . ': ' . prStr($this->value) . '>';
    }

    public function __destruct()
    {
        $this->unregister();
    }
}

// ============================================================
// Ref functions
// ============================================================

function ref(mixed $val, ?callable $validator = null): Ref
{
    $r = new Ref($val, $validator);
    $r->register();
    return $r;
}

function alter(Ref $ref, callable $f, mixed ...$args): mixed
{
    return $ref->alter($f, ...$args);
}

function refSet(Ref $ref, mixed $val): mixed
{
    return $ref->refSet($val);
}

function commute(Ref $ref, callable $f, mixed ...$args): mixed
{
    return $ref->commute($f, ...$args);
}

function ensure_(Ref $ref): mixed
{
    // Mark ref as read in current transaction
    return $ref->deref();
}

/**
 * Execute body in a transaction with automatic retry on conflict.
 */
function dosync(callable $body): mixed
{
    $maxRetries = 100;
    $retries = 0;

    while ($retries < $maxRetries) {
        Ref::startTransaction();
        try {
            $result = $body();
            if (Ref::commitTransaction()) {
                return $result;
            }
            // Commit failed, retry
            $retries++;
            if (function_exists('Swoole\Coroutine::sleep')) {
                \Swoole\Coroutine::sleep(0.001 * $retries);
            } else {
                usleep(1000 * $retries);
            }
        } catch (\Throwable $e) {
            Ref::abortTransaction();
            throw $e;
        }
    }

    throw new \RuntimeException("Transaction failed after $maxRetries retries");
}

// ============================================================
// Parallel operations
// ============================================================

/**
 * Parallel map - execute f on each element concurrently.
 * Returns a lazy sequence that realizes in parallel.
 *
 * Requires Swoole for true parallelism; falls back to sequential without it.
 */
function pmap(callable $f, mixed $coll, mixed ...$colls): LazySeq
{
    if (!function_exists('go')) {
        // No Swoole - fall back to regular map
        return map_($f, $coll, ...$colls);
    }

    $items = toArray($coll);
    if (count($colls) > 0) {
        // Multi-collection - zip and apply
        $allColls = [$items, ...array_map(fn($c) => toArray($c), $colls)];
        $minLen = min(array_map(fn($a) => count($a), $allColls));
        $items = [];
        for ($i = 0; $i < $minLen; $i++) {
            $args = array_map(fn($c) => $c[$i], $allColls);
            $items[] = $args;
        }
        $applyF = fn($args) => $f(...$args);
    } else {
        $applyF = $f;
    }

    return new LazySeq(function () use ($items, $applyF) {
        $results = array_fill(0, count($items), null);
        $channel = new \Swoole\Coroutine\Channel(count($items));

        foreach ($items as $i => $item) {
            go(function () use ($i, $item, $applyF, $channel) {
                try {
                    $result = $applyF($item);
                    $channel->push(['ok', $i, $result]);
                } catch (\Throwable $e) {
                    $channel->push(['error', $i, $e]);
                }
            });
        }

        $errors = [];
        for ($j = 0; $j < count($items); $j++) {
            $msg = $channel->pop();
            if ($msg[0] === 'ok') {
                $results[$msg[1]] = $msg[2];
            } else {
                $errors[$msg[1]] = $msg[2];
            }
        }

        if (!empty($errors)) {
            throw reset($errors);
        }

        return new ArraySeq($results);
    });
}

/**
 * Parallel calls - execute multiple thunks concurrently.
 */
function pcalls(callable ...$fns): LazySeq
{
    return pmap(fn($f) => $f(), $fns);
}

/**
 * Parallel values macro helper.
 * Usage: pvalues(fn() => expr1, fn() => expr2, ...)
 */
function pvalues(callable ...$thunks): LazySeq
{
    return pcalls(...$thunks);
}

/**
 * pvec - force parallel evaluation and return vector.
 */
function pvec(callable $f, mixed $coll, mixed ...$colls): Vec
{
    $result = pmap($f, $coll, ...$colls);
    return vec(...toArray($result));
}
