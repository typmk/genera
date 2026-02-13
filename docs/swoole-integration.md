# Swoole Integration for ClojurePHP

Swoole is a coroutine-based async runtime for PHP. Combined with ClojurePHP's immutable data structures, it enables:

- **JIT stays warm** - long-running process, no cold starts
- **Safe concurrency** - immutable data = no locks needed
- **STM-like semantics** - Atoms + coroutines for coordinated state
- **Async I/O** - non-blocking database, HTTP, file operations
- **Channels** - CSP-style communication (like core.async)

## Installation

```bash
pecl install swoole
# or
pecl install openswoole
```

php.ini:
```ini
extension=swoole
opcache.enable=1
opcache.jit_buffer_size=100M
opcache.jit=1255
```

## Basic Server

```php
<?php
require_once 'src/clojure/php/runtime/bootstrap.php';

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;
use function Clojure\Php\{atom, deref, swap, hashMap, assoc, kw, vec};

// Shared state via Atom (thread-safe with immutable values)
$appState = atom(hashMap(
    kw('requests'), 0,
    kw('users'), vec()
));

$server = new Server('0.0.0.0', 9501);

$server->on('request', function (Request $req, Response $res) use ($appState) {
    // Atomically increment request counter
    swap($appState, fn($state) => assoc($state, kw('requests'), $state->get(kw('requests')) + 1));

    $state = deref($appState);
    $res->header('Content-Type', 'application/json');
    $res->end(json_encode([
        'requests' => $state->get(kw('requests')),
        'path' => $req->server['request_uri'],
    ]));
});

$server->start();
```

## Atoms for Concurrent State (STM-lite)

Atoms provide compare-and-swap semantics. With immutable data, this gives you safe concurrent updates:

```php
use function Clojure\Php\{atom, deref, swap, reset, compareAndSet};

// Create shared state
$counter = atom(0);
$cache = atom(hashMap());

// In coroutine 1:
swap($counter, fn($n) => $n + 1);

// In coroutine 2 (concurrent):
swap($counter, fn($n) => $n + 1);

// Both succeed without locks! Atom retries on conflict.

// Cache pattern
function memoizedFetch($cache, $key, $fetcher) {
    $cached = deref($cache)->get($key);
    if ($cached !== null) {
        return $cached;
    }

    $value = $fetcher($key);
    swap($cache, fn($c) => $c->put($key, $value));
    return $value;
}
```

## Channels (CSP / core.async style)

```php
use Swoole\Coroutine\Channel;
use function Clojure\Php\{vec, conj, map_, take_};

// Create channel
$ch = new Channel(100);

// Producer coroutine
go(function () use ($ch) {
    foreach (range(1, 1000) as $i) {
        $ch->push(vec($i, $i * 2, $i * 3));
    }
    $ch->close();
});

// Consumer coroutine
go(function () use ($ch) {
    while ($data = $ch->pop()) {
        // $data is immutable Vec - safe to pass between coroutines
        $sum = $data->sum();
        echo "Sum: $sum\n";
    }
});
```

## Connection Pool with Immutable Config

```php
use Swoole\Database\PDOPool;
use Swoole\Database\PDOConfig;

// Immutable config
$dbConfig = hashMap(
    kw('host'), 'localhost',
    kw('port'), 3306,
    kw('database'), 'myapp',
    kw('username'), 'root',
    kw('password'), 'secret',
    kw('pool-size'), 64
);

$pool = new PDOPool(
    (new PDOConfig)
        ->withHost($dbConfig->get(kw('host')))
        ->withPort($dbConfig->get(kw('port')))
        ->withDbName($dbConfig->get(kw('database')))
        ->withUsername($dbConfig->get(kw('username')))
        ->withPassword($dbConfig->get(kw('password'))),
    $dbConfig->get(kw('pool-size'))
);
```

## Parallel Processing with Immutable Data

```php
use Swoole\Coroutine;
use function Clojure\Php\{vec, map_, reduce_, partition_};

function parallelMap(callable $f, $coll, int $concurrency = 10) {
    $results = atom(vec());
    $chunks = partition_($concurrency, $coll);

    foreach ($chunks as $chunk) {
        $wg = new Coroutine\WaitGroup();

        foreach ($chunk as $idx => $item) {
            $wg->add();
            go(function () use ($f, $item, $idx, $results, $wg) {
                $result = $f($item);
                // Safe concurrent append via atom
                swap($results, fn($v) => $v->append([$idx, $result]));
                $wg->done();
            });
        }

        $wg->wait();
    }

    // Sort by original index and extract values
    return vec(...array_column(
        collect(deref($results))->sortBy(0)->toArray(),
        1
    ));
}

// Usage
$urls = vec('https://api.example.com/1', 'https://api.example.com/2', ...);
$responses = parallelMap(fn($url) => httpGet($url), $urls, 50);
```

## Agent-style Async Updates

```php
/**
 * Agent - like Clojure agents, async state updates.
 */
class Agent {
    private $state;
    private Channel $actions;

    public function __construct(mixed $initial) {
        $this->state = atom($initial);
        $this->actions = new Channel(1000);
        $this->startProcessor();
    }

    private function startProcessor(): void {
        go(function () {
            while ($action = $this->actions->pop()) {
                try {
                    swap($this->state, $action);
                } catch (\Throwable $e) {
                    // Log error, state unchanged
                }
            }
        });
    }

    public function send(callable $f): void {
        $this->actions->push($f);
    }

    public function deref(): mixed {
        return deref($this->state);
    }
}

// Usage
$logger = new Agent(vec());
$logger->send(fn($logs) => $logs->append(['time' => time(), 'msg' => 'Started']));
$logger->send(fn($logs) => $logs->append(['time' => time(), 'msg' => 'Processing']));
```

## Performance Tips

1. **JIT stays warm** - Swoole workers are long-lived, JIT compiles hot paths once
2. **Preload runtime** - Use opcache.preload for instant class availability
3. **Use Atoms, not locks** - Immutable data + CAS = lock-free concurrency
4. **Share nothing** - Pass immutable data between coroutines freely
5. **Pool connections** - Swoole provides connection pools for DB, Redis, etc.

## php.ini for Swoole + ClojurePHP

```ini
; Swoole
extension=swoole

; Opcache + JIT (critical for performance)
opcache.enable=1
opcache.enable_cli=1
opcache.jit_buffer_size=100M
opcache.jit=1255
opcache.memory_consumption=256
opcache.interned_strings_buffer=64

; Preload ClojurePHP runtime
opcache.preload=/path/to/ClojurePHP/src/clojure/php/runtime/preload.php

; Memory for immutable data structures
memory_limit=512M
```
