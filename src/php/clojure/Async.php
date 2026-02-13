<?php

declare(strict_types=1);

namespace Clojure\Php;

/**
 * core.async-like channels for ClojurePHP.
 *
 * Provides CSP-style (Communicating Sequential Processes) concurrency:
 * - Channels for message passing
 * - go blocks (coroutines in Swoole)
 * - alts! for non-deterministic choice
 *
 * Requires Swoole for async operation; degrades to synchronous without it.
 */

// ============================================================
// Channel
// ============================================================

/**
 * Chan - an async channel for message passing.
 *
 * Like Clojure's core.async channels:
 * - Buffered or unbuffered
 * - Blocking put/take when buffer full/empty
 * - Can be closed
 */
final class Chan
{
    private bool $closed = false;
    /** @var \Swoole\Coroutine\Channel|null */
    private ?object $impl = null;
    /** @var array Buffer for non-Swoole mode */
    private array $buffer = [];
    private int $bufferSize;

    public function __construct(int $bufferSize = 0)
    {
        $this->bufferSize = max(1, $bufferSize); // At least 1 for synchronization

        if (class_exists(\Swoole\Coroutine\Channel::class)) {
            $this->impl = new \Swoole\Coroutine\Channel($this->bufferSize);
        }
    }

    /**
     * Put a value onto the channel.
     * Blocks if the channel is full.
     * Returns false if channel is closed.
     */
    public function put(mixed $val): bool
    {
        if ($this->closed) {
            return false;
        }

        if ($this->impl) {
            return $this->impl->push($val);
        }

        // Non-Swoole: add to buffer (may overflow)
        if (count($this->buffer) < $this->bufferSize) {
            $this->buffer[] = $val;
            return true;
        }

        return false;
    }

    /**
     * Take a value from the channel.
     * Blocks if the channel is empty.
     * Returns null if channel is closed and empty.
     */
    public function take(): mixed
    {
        if ($this->impl) {
            $result = $this->impl->pop();
            if ($this->impl->errCode === SWOOLE_CHANNEL_CLOSED) {
                return null;
            }
            return $result;
        }

        // Non-Swoole: take from buffer
        if (!empty($this->buffer)) {
            return array_shift($this->buffer);
        }

        return null;
    }

    /**
     * Try to take without blocking.
     * Returns [true, value] on success, [false, null] if empty.
     */
    public function poll(): array
    {
        if ($this->impl) {
            if ($this->impl->isEmpty()) {
                return [false, null];
            }
            return [true, $this->impl->pop(0.0)];
        }

        if (!empty($this->buffer)) {
            return [true, array_shift($this->buffer)];
        }

        return [false, null];
    }

    /**
     * Try to put without blocking.
     * Returns true on success, false if full.
     */
    public function offer(mixed $val): bool
    {
        if ($this->closed) {
            return false;
        }

        if ($this->impl) {
            if ($this->impl->isFull()) {
                return false;
            }
            return $this->impl->push($val, 0.0);
        }

        if (count($this->buffer) < $this->bufferSize) {
            $this->buffer[] = $val;
            return true;
        }

        return false;
    }

    /**
     * Close the channel.
     */
    public function close(): void
    {
        $this->closed = true;
        if ($this->impl) {
            $this->impl->close();
        }
    }

    public function isClosed(): bool
    {
        return $this->closed;
    }

    public function isEmpty(): bool
    {
        if ($this->impl) {
            return $this->impl->isEmpty();
        }
        return empty($this->buffer);
    }

    public function isFull(): bool
    {
        if ($this->impl) {
            return $this->impl->isFull();
        }
        return count($this->buffer) >= $this->bufferSize;
    }

    public function __toString(): string
    {
        $status = $this->closed ? 'closed' : 'open';
        return "#<Chan[$status]>";
    }
}

// ============================================================
// Channel functions
// ============================================================

/**
 * Create a channel.
 */
function chan(int $bufferSize = 1): Chan
{
    return new Chan($bufferSize);
}

/**
 * Put value onto channel (blocking).
 */
function putAsync(Chan $ch, mixed $val): bool
{
    return $ch->put($val);
}

/**
 * Take value from channel (blocking).
 */
function takeAsync(Chan $ch): mixed
{
    return $ch->take();
}

/**
 * Close a channel.
 */
function closeAsync(Chan $ch): void
{
    $ch->close();
}

// ============================================================
// Go blocks
// ============================================================

/**
 * Launch a go block (coroutine).
 *
 * The callback receives a helper object with <! and >! methods
 * for channel operations within the go block.
 *
 * Returns a channel that will receive the result when done.
 */
function goBlock(callable $f): Chan
{
    $resultCh = chan(1);

    if (function_exists('go')) {
        go(function () use ($f, $resultCh) {
            try {
                $result = $f();
                $resultCh->put($result);
            } catch (\Throwable $e) {
                $resultCh->put(['error' => $e]);
            } finally {
                $resultCh->close();
            }
        });
    } else {
        // Non-Swoole: execute synchronously
        try {
            $result = $f();
            $resultCh->put($result);
        } catch (\Throwable $e) {
            $resultCh->put(['error' => $e]);
        } finally {
            $resultCh->close();
        }
    }

    return $resultCh;
}

/**
 * Take from channel (for use in go blocks).
 * Alias for takeAsync.
 */
function lessBang(Chan $ch): mixed
{
    return $ch->take();
}

/**
 * Put to channel (for use in go blocks).
 * Alias for putAsync.
 */
function greaterBang(Chan $ch, mixed $val): bool
{
    return $ch->put($val);
}

// ============================================================
// Alts
// ============================================================

/**
 * Non-deterministic choice over multiple channel operations.
 *
 * Takes an array of operations:
 * - [channel] - take from channel
 * - [channel, value] - put value to channel
 *
 * Returns [value, channel] where value is the result and channel
 * is the channel that was selected.
 *
 * Options:
 * - 'default' => value - return this if no operation ready
 * - 'priority' => true - try channels in order
 */
function alts(array $ops, array $opts = []): array
{
    $default = $opts['default'] ?? null;
    $hasDefault = array_key_exists('default', $opts);
    $priority = $opts['priority'] ?? false;

    if (!$priority) {
        shuffle($ops);
    }

    // First pass: try each operation without blocking
    foreach ($ops as $op) {
        if (count($op) === 1) {
            // Take operation
            $ch = $op[0];
            [$success, $val] = $ch->poll();
            if ($success) {
                return [$val, $ch];
            }
        } else {
            // Put operation
            [$ch, $val] = $op;
            if ($ch->offer($val)) {
                return [true, $ch];
            }
        }
    }

    // No operation ready
    if ($hasDefault) {
        return [$default, 'default'];
    }

    // Block on first available
    // For Swoole, we'd use select; for now, busy-wait
    if (function_exists('Swoole\Coroutine::sleep')) {
        while (true) {
            foreach ($ops as $op) {
                if (count($op) === 1) {
                    $ch = $op[0];
                    [$success, $val] = $ch->poll();
                    if ($success) {
                        return [$val, $ch];
                    }
                } else {
                    [$ch, $val] = $op;
                    if ($ch->offer($val)) {
                        return [true, $ch];
                    }
                }
            }
            \Swoole\Coroutine::sleep(0.001);
        }
    }

    // Non-Swoole: return default or throw
    throw new \RuntimeException("alts! would block but no Swoole available");
}

// ============================================================
// Pipeline
// ============================================================

/**
 * Pipeline - process values through a transducer in parallel.
 */
function pipeline(int $n, Chan $to, callable $xf, Chan $from, bool $close = true): void
{
    $jobs = chan($n);
    $results = chan($n);

    // Start workers
    for ($i = 0; $i < $n; $i++) {
        goBlock(function () use ($jobs, $results, $xf) {
            while (true) {
                $job = $jobs->take();
                if ($job === null) {
                    break;
                }
                $rf = $xf(fn($acc, $x) => $x);
                $result = $rf(null, $job);
                $results->put($result);
            }
        });
    }

    // Feeder
    goBlock(function () use ($from, $jobs, $n) {
        while (true) {
            $val = $from->take();
            if ($val === null) {
                break;
            }
            $jobs->put($val);
        }
        $jobs->close();
    });

    // Collector
    goBlock(function () use ($from, $results, $to, $close, $n) {
        while (true) {
            $result = $results->take();
            if ($result === null) {
                break;
            }
            $to->put($result);
        }
        if ($close) {
            $to->close();
        }
    });
}

/**
 * Pipeline-blocking - like pipeline but with blocking operations.
 */
function pipelineBlocking(int $n, Chan $to, callable $f, Chan $from, bool $close = true): void
{
    pipeline($n, $to, mapping($f), $from, $close);
}

/**
 * Pipeline-async - like pipeline but f returns a channel.
 */
function pipelineAsync(int $n, Chan $to, callable $af, Chan $from, bool $close = true): void
{
    $xf = fn($rf) => fn($acc, $x) => ($af($x))->take();
    pipeline($n, $to, $xf, $from, $close);
}

// ============================================================
// Utilities
// ============================================================

/**
 * Onto-chan - put values from collection onto channel.
 */
function ontoChan(Chan $ch, mixed $coll, bool $close = true): Chan
{
    return goBlock(function () use ($ch, $coll, $close) {
        $s = seq($coll);
        while ($s !== null) {
            $ch->put($s->first());
            $s = $s->next();
        }
        if ($close) {
            $ch->close();
        }
        return null;
    });
}

/**
 * To-chan - create a channel and put collection values onto it.
 */
function toChan(mixed $coll): Chan
{
    $ch = chan(32);
    ontoChan($ch, $coll);
    return $ch;
}

/**
 * Into-chan - take all values from channel into a collection.
 */
function intoChan(Chan $ch): Vec
{
    $result = [];
    while (true) {
        $val = $ch->take();
        if ($val === null) {
            break;
        }
        $result[] = $val;
    }
    return vec(...$result);
}

/**
 * Timeout - a channel that closes after ms milliseconds.
 */
function timeout(int $ms): Chan
{
    $ch = chan(1);

    if (function_exists('go')) {
        go(function () use ($ch, $ms) {
            \Swoole\Coroutine::sleep($ms / 1000);
            $ch->close();
        });
    } else {
        // Non-Swoole: immediate close (no async)
        $ch->close();
    }

    return $ch;
}

/**
 * Merge - take from multiple channels and put to one.
 */
function mergeChans(array $chs): Chan
{
    $out = chan(count($chs));

    foreach ($chs as $ch) {
        goBlock(function () use ($ch, $out) {
            while (true) {
                $val = $ch->take();
                if ($val === null) {
                    break;
                }
                $out->put($val);
            }
        });
    }

    return $out;
}

/**
 * Pub/Sub - simple publish-subscribe.
 */
final class Pub
{
    private array $subs = [];
    private Chan $source;

    public function __construct(Chan $source, callable $topicFn)
    {
        $this->source = $source;

        goBlock(function () use ($topicFn) {
            while (true) {
                $val = $this->source->take();
                if ($val === null) {
                    break;
                }
                $topic = $topicFn($val);
                if (isset($this->subs[$topic])) {
                    foreach ($this->subs[$topic] as $ch) {
                        $ch->put($val);
                    }
                }
            }
            // Close all subscribers
            foreach ($this->subs as $chs) {
                foreach ($chs as $ch) {
                    $ch->close();
                }
            }
        });
    }

    public function sub(mixed $topic, Chan $ch): void
    {
        $key = is_object($topic) ? spl_object_id($topic) : (string)$topic;
        if (!isset($this->subs[$key])) {
            $this->subs[$key] = [];
        }
        $this->subs[$key][] = $ch;
    }

    public function unsub(mixed $topic, Chan $ch): void
    {
        $key = is_object($topic) ? spl_object_id($topic) : (string)$topic;
        if (isset($this->subs[$key])) {
            $this->subs[$key] = array_filter(
                $this->subs[$key],
                fn($c) => $c !== $ch
            );
        }
    }
}

function pub(Chan $source, callable $topicFn): Pub
{
    return new Pub($source, $topicFn);
}

function sub(Pub $pub, mixed $topic, ?Chan $ch = null): Chan
{
    $ch ??= chan(32);
    $pub->sub($topic, $ch);
    return $ch;
}

function unsub(Pub $pub, mixed $topic, Chan $ch): void
{
    $pub->unsub($topic, $ch);
}
