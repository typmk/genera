<?php

declare(strict_types=1);

namespace Clojure\Php;

/**
 * Atom - mutable reference with compare-and-swap semantics.
 *
 * Coroutine-safe when using Swoole - uses spinlock for CAS.
 * For standard PHP (single-threaded), CAS always succeeds on first try.
 */
final class Atom
{
    private mixed $state;
    private ?Map $meta = null;
    private array $watches = [];
    /** @var callable|null */
    private mixed $validator = null;

    /** @var \Swoole\Lock|null Spinlock for Swoole coroutine safety */
    private ?object $lock = null;

    /** Max CAS retries before yielding to scheduler */
    private const MAX_SPINS = 100;

    public function __construct(mixed $val, ?callable $validator = null)
    {
        if ($validator !== null && !$validator($val)) {
            throw new \InvalidArgumentException("Validator rejected initial value");
        }
        $this->state = $val;
        $this->validator = $validator;

        // Initialize Swoole lock if available (for coroutine safety)
        if (class_exists(\Swoole\Lock::class)) {
            $this->lock = new \Swoole\Lock(SWOOLE_SPINLOCK);
        }
    }

    public function deref(): mixed
    {
        return $this->state;
    }

    public function reset(mixed $newVal): mixed
    {
        $this->validate($newVal);

        if ($this->lock) {
            $this->lock->lock();
            try {
                $oldVal = $this->state;
                $this->state = $newVal;
            } finally {
                $this->lock->unlock();
            }
        } else {
            $oldVal = $this->state;
            $this->state = $newVal;
        }

        $this->notifyWatches($oldVal, $newVal);
        return $newVal;
    }

    public function swap(callable $f, mixed ...$args): mixed
    {
        $spins = 0;
        while (true) {
            $oldVal = $this->state;
            $newVal = $f($oldVal, ...$args);
            $this->validate($newVal);

            if ($this->compareAndSet($oldVal, $newVal)) {
                return $newVal;
            }

            // Yield to Swoole scheduler after too many spins
            if (++$spins >= self::MAX_SPINS && function_exists('Swoole\Coroutine::sched_yield')) {
                \Swoole\Coroutine::sched_yield();
                $spins = 0;
            }
        }
    }

    public function compareAndSet(mixed $oldVal, mixed $newVal): bool
    {
        if ($this->lock) {
            $this->lock->lock();
            try {
                if ($this->state === $oldVal || equals($this->state, $oldVal)) {
                    $this->validate($newVal);
                    $actualOld = $this->state;
                    $this->state = $newVal;
                    // Notify outside lock to prevent deadlock
                    $needNotify = true;
                } else {
                    return false;
                }
            } finally {
                $this->lock->unlock();
            }
            if ($needNotify) {
                $this->notifyWatches($actualOld, $newVal);
            }
            return true;
        }

        // Non-Swoole path (single-threaded, always succeeds)
        if ($this->state === $oldVal || equals($this->state, $oldVal)) {
            $this->validate($newVal);
            $actualOld = $this->state;
            $this->state = $newVal;
            $this->notifyWatches($actualOld, $newVal);
            return true;
        }
        return false;
    }

    // ============================================================
    // Watches
    // ============================================================

    public function addWatch(mixed $key, callable $fn): self
    {
        $this->watches[$this->watchKey($key)] = [$key, $fn];
        return $this;
    }

    public function removeWatch(mixed $key): self
    {
        unset($this->watches[$this->watchKey($key)]);
        return $this;
    }

    private function notifyWatches(mixed $oldVal, mixed $newVal): void
    {
        foreach ($this->watches as [$key, $fn]) {
            $fn($key, $this, $oldVal, $newVal);
        }
    }

    private function watchKey(mixed $key): string
    {
        if ($key instanceof Kw) {
            return ':' . $key->fullName();
        }
        return (string)$key;
    }

    // ============================================================
    // Validator
    // ============================================================

    public function setValidator(?callable $fn): self
    {
        if ($fn !== null && !$fn($this->state)) {
            throw new \InvalidArgumentException("Validator rejected current value");
        }
        $this->validator = $fn;
        return $this;
    }

    public function getValidator(): ?callable
    {
        return $this->validator;
    }

    private function validate(mixed $val): void
    {
        if ($this->validator !== null && !($this->validator)($val)) {
            throw new \InvalidArgumentException("Validator rejected value");
        }
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
        $this->meta = $f($this->meta, ...$args);
        return $this->meta;
    }

    public function __toString(): string
    {
        return '#<Atom@' . spl_object_id($this) . ': ' . prStr($this->state) . '>';
    }
}

// Constructor functions

function atom(mixed $val, ?callable $validator = null): Atom
{
    return new Atom($val, $validator);
}

function deref(mixed $ref): mixed
{
    if ($ref instanceof Atom) {
        return $ref->deref();
    }
    if ($ref instanceof Agent) {
        return $ref->deref();
    }
    if ($ref instanceof Reduced) {
        return $ref->deref();
    }
    throw new \InvalidArgumentException("Cannot deref " . gettype($ref));
}

function reset_(Atom $atom, mixed $val): mixed
{
    return $atom->reset($val);
}

function swap(Atom $atom, callable $f, mixed ...$args): mixed
{
    return $atom->swap($f, ...$args);
}

function compareAndSet(Atom $atom, mixed $oldVal, mixed $newVal): bool
{
    return $atom->compareAndSet($oldVal, $newVal);
}

function addWatch(Atom $atom, mixed $key, callable $fn): Atom
{
    return $atom->addWatch($key, $fn);
}

function removeWatch(Atom $atom, mixed $key): Atom
{
    return $atom->removeWatch($key);
}

function setValidator(Atom $atom, ?callable $fn): Atom
{
    return $atom->setValidator($fn);
}

function getValidator(Atom $atom): ?callable
{
    return $atom->getValidator();
}

// ============================================================
// Agent - Async state updates (Swoole only)
// ============================================================

/**
 * Agent - asynchronous state updates via message queue.
 *
 * Like Clojure agents: send actions to update state asynchronously.
 * Requires Swoole for coroutine-based processing.
 *
 * Usage:
 *   $a = agent(vec());
 *   send($a, fn($v) => $v->append('item'));
 *   // ... later ...
 *   deref($a); // includes 'item' once processed
 */
final class Agent
{
    private Atom $state;
    /** @var \Swoole\Coroutine\Channel|null */
    private ?object $channel = null;
    private bool $running = false;
    private ?\Throwable $error = null;

    public function __construct(mixed $val)
    {
        $this->state = new Atom($val);

        if (class_exists(\Swoole\Coroutine\Channel::class)) {
            $this->channel = new \Swoole\Coroutine\Channel(10000);
            $this->startProcessor();
        }
    }

    private function startProcessor(): void
    {
        if ($this->running || !function_exists('go')) {
            return;
        }
        $this->running = true;

        go(function () {
            while (true) {
                $action = $this->channel->pop();
                if ($action === false) {
                    break; // Channel closed
                }

                try {
                    $this->state->swap($action);
                    $this->error = null;
                } catch (\Throwable $e) {
                    $this->error = $e;
                    // State unchanged on error (like Clojure)
                }
            }
            $this->running = false;
        });
    }

    public function send(callable $f): self
    {
        if ($this->channel) {
            $this->channel->push($f);
        } else {
            // Fallback: synchronous update when not in Swoole
            $this->state->swap($f);
        }
        return $this;
    }

    public function deref(): mixed
    {
        return $this->state->deref();
    }

    public function error(): ?\Throwable
    {
        return $this->error;
    }

    public function await(): self
    {
        if ($this->channel && function_exists('Swoole\Coroutine::sleep')) {
            // Wait for queue to drain
            while (!$this->channel->isEmpty()) {
                \Swoole\Coroutine::sleep(0.001);
            }
        }
        return $this;
    }

    public function shutdown(): void
    {
        if ($this->channel) {
            $this->channel->close();
        }
    }

    public function __toString(): string
    {
        $status = $this->error ? ' ERROR' : '';
        return '#<Agent@' . spl_object_id($this) . $status . ': ' . prStr($this->state->deref()) . '>';
    }
}

function agent(mixed $val): Agent
{
    return new Agent($val);
}

function send(Agent $agent, callable $f): Agent
{
    return $agent->send($f);
}

function agentError(Agent $agent): ?\Throwable
{
    return $agent->error();
}

function await(Agent $agent): Agent
{
    return $agent->await();
}

// ============================================================
// Promise - Single-assignment mutable reference
// ============================================================

/**
 * Promise - a single-assignment value that can be delivered once.
 *
 * Unlike Clojure's future, this is just a container that blocks on deref
 * until a value is delivered. Use with Swoole coroutines.
 */
final class Promise
{
    private mixed $value = null;
    private bool $delivered = false;
    /** @var \Swoole\Coroutine\Channel|null */
    private ?object $channel = null;

    public function __construct()
    {
        if (class_exists(\Swoole\Coroutine\Channel::class)) {
            $this->channel = new \Swoole\Coroutine\Channel(1);
        }
    }

    public function deliver(mixed $val): bool
    {
        if ($this->delivered) {
            return false;
        }
        $this->value = $val;
        $this->delivered = true;
        if ($this->channel) {
            $this->channel->push($val);
        }
        return true;
    }

    public function deref(?float $timeout = null, mixed $timeoutVal = null): mixed
    {
        if ($this->delivered) {
            return $this->value;
        }

        if ($this->channel) {
            // Block until value is delivered or timeout
            if ($timeout !== null) {
                $result = $this->channel->pop($timeout);
                if ($this->channel->errCode === SWOOLE_CHANNEL_TIMEOUT) {
                    return $timeoutVal;
                }
                return $result;
            }
            // Block indefinitely
            return $this->channel->pop();
        }

        // Non-Swoole: busy-wait (not recommended)
        while (!$this->delivered) {
            usleep(1000);
        }
        return $this->value;
    }

    public function isRealized(): bool
    {
        return $this->delivered;
    }

    public function __toString(): string
    {
        if ($this->delivered) {
            return '#<Promise: ' . prStr($this->value) . '>';
        }
        return '#<Promise: pending>';
    }
}

function promise(): Promise
{
    return new Promise();
}

function deliver(Promise $p, mixed $val): bool
{
    return $p->deliver($val);
}

function realized(mixed $ref): bool
{
    if ($ref instanceof Promise) {
        return $ref->isRealized();
    }
    if ($ref instanceof Future) {
        return $ref->isRealized();
    }
    if ($ref instanceof LazySeq) {
        return $ref->isRealized();
    }
    return true;
}

// ============================================================
// Future - Async computation (Swoole coroutine)
// ============================================================

/**
 * Future - executes a function asynchronously.
 *
 * In Swoole, spawns a coroutine. In non-Swoole, executes synchronously.
 */
final class Future
{
    private mixed $value = null;
    private ?\Throwable $error = null;
    private bool $realized = false;
    /** @var \Swoole\Coroutine\Channel|null */
    private ?object $channel = null;

    public function __construct(callable $f)
    {
        if (function_exists('go')) {
            $this->channel = new \Swoole\Coroutine\Channel(1);
            go(function () use ($f) {
                try {
                    $this->value = $f();
                    $this->channel->push(['ok', $this->value]);
                } catch (\Throwable $e) {
                    $this->error = $e;
                    $this->channel->push(['error', $e]);
                }
                $this->realized = true;
            });
        } else {
            // Synchronous fallback
            try {
                $this->value = $f();
            } catch (\Throwable $e) {
                $this->error = $e;
            }
            $this->realized = true;
        }
    }

    public function deref(?float $timeout = null, mixed $timeoutVal = null): mixed
    {
        if ($this->realized) {
            if ($this->error) {
                throw $this->error;
            }
            return $this->value;
        }

        if ($this->channel) {
            if ($timeout !== null) {
                $result = $this->channel->pop($timeout);
                if ($this->channel->errCode === SWOOLE_CHANNEL_TIMEOUT) {
                    return $timeoutVal;
                }
            } else {
                $result = $this->channel->pop();
            }

            if ($result[0] === 'error') {
                throw $result[1];
            }
            return $result[1];
        }

        // Already realized in sync mode
        if ($this->error) {
            throw $this->error;
        }
        return $this->value;
    }

    public function isRealized(): bool
    {
        return $this->realized;
    }

    public function __toString(): string
    {
        if (!$this->realized) {
            return '#<Future: pending>';
        }
        if ($this->error) {
            return '#<Future: ERROR ' . $this->error->getMessage() . '>';
        }
        return '#<Future: ' . prStr($this->value) . '>';
    }
}

function future(callable $f): Future
{
    return new Future($f);
}

/**
 * Deref with optional timeout.
 * Works with Atom, Agent, Promise, Future, Reduced.
 */
function derefTimeout(mixed $ref, float $timeout, mixed $timeoutVal = null): mixed
{
    if ($ref instanceof Promise) {
        return $ref->deref($timeout, $timeoutVal);
    }
    if ($ref instanceof Future) {
        return $ref->deref($timeout, $timeoutVal);
    }
    // For Atom/Agent, deref is immediate
    return deref($ref);
}

// ============================================================
// Delay - Lazy computation (computed once on first deref)
// ============================================================

/**
 * Delay - lazily computes a value on first deref.
 */
final class Delay
{
    /** @var callable|null */
    private $f;
    private mixed $value = null;
    private bool $realized = false;

    public function __construct(callable $f)
    {
        $this->f = $f;
    }

    public function deref(): mixed
    {
        if (!$this->realized) {
            $this->value = ($this->f)();
            $this->f = null;
            $this->realized = true;
        }
        return $this->value;
    }

    public function isRealized(): bool
    {
        return $this->realized;
    }

    public function __toString(): string
    {
        if ($this->realized) {
            return '#<Delay: ' . prStr($this->value) . '>';
        }
        return '#<Delay: pending>';
    }
}

function delay(callable $f): Delay
{
    return new Delay($f);
}

function force(mixed $x): mixed
{
    if ($x instanceof Delay) {
        return $x->deref();
    }
    return $x;
}

function isDelay(mixed $x): bool
{
    return $x instanceof Delay;
}

// ============================================================
// Volatile - Mutable reference without CAS (for perf-critical code)
// ============================================================

/**
 * Volatile - simple mutable box without CAS overhead.
 * Use when you know there's no contention.
 */
final class Volatile
{
    public function __construct(private mixed $val)
    {
    }

    public function deref(): mixed
    {
        return $this->val;
    }

    public function reset(mixed $val): mixed
    {
        $old = $this->val;
        $this->val = $val;
        return $old;
    }

    public function swap(callable $f, mixed ...$args): mixed
    {
        $this->val = $f($this->val, ...$args);
        return $this->val;
    }

    public function __toString(): string
    {
        return '#<Volatile: ' . prStr($this->val) . '>';
    }
}

function volatile(mixed $val): Volatile
{
    return new Volatile($val);
}

function vReset(Volatile $v, mixed $val): mixed
{
    return $v->reset($val);
}

function vSwap(Volatile $v, callable $f, mixed ...$args): mixed
{
    return $v->swap($f, ...$args);
}
