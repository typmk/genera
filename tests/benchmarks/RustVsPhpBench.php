<?php

declare(strict_types=1);

namespace Benchmarks;

use PhpBench\Benchmark\Metadata\Annotations\Revs;
use PhpBench\Benchmark\Metadata\Annotations\Iterations;
use PhpBench\Attributes as Bench;
use Clojure\Lang\Collections\Vector\PersistentVector as PhpVector;
use Clojure\Lang\Collections\Map\PersistentHashMap as PhpMap;
use Clojure\Lang\Hasher;
use Clojure\Lang\Equalizer;
use Clojure\Lang\Keyword;
use Cljp\PersistentVector as RustVector;
use Cljp\PersistentHashMap as RustMap;
use Cljp\Keyword as RustKeyword;

/**
 * Benchmark comparing PHP and Rust implementations
 */
class RustVsPhpBench
{
    // ========== Vector Append Benchmarks ==========

    #[Bench\Revs(50)]
    #[Bench\Iterations(5)]
    public function benchPhpVectorAppend1000(): void
    {
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();
        $v = PhpVector::empty($hasher, $equalizer);
        for ($i = 0; $i < 1000; $i++) {
            $v = $v->append($i);
        }
    }

    #[Bench\Revs(50)]
    #[Bench\Iterations(5)]
    public function benchRustVectorAppend1000(): void
    {
        $v = RustVector::empty();
        for ($i = 0; $i < 1000; $i++) {
            $v = $v->conj($i);
        }
    }

    // ========== Vector Get Benchmarks ==========

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchPhpVectorGet1000(): void
    {
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();
        $v = PhpVector::empty($hasher, $equalizer);
        for ($i = 0; $i < 1000; $i++) {
            $v = $v->append($i);
        }

        // Now benchmark the gets
        for ($i = 0; $i < 1000; $i++) {
            $v->get($i);
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchRustVectorGet1000(): void
    {
        $v = RustVector::empty();
        for ($i = 0; $i < 1000; $i++) {
            $v = $v->conj($i);
        }

        // Now benchmark the gets
        for ($i = 0; $i < 1000; $i++) {
            $v->nth($i);
        }
    }

    // ========== Map Put Benchmarks ==========

    #[Bench\Revs(50)]
    #[Bench\Iterations(5)]
    public function benchPhpMapPut1000(): void
    {
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();
        $m = PhpMap::empty($hasher, $equalizer);
        for ($i = 0; $i < 1000; $i++) {
            $m = $m->put("key$i", $i);
        }
    }

    #[Bench\Revs(50)]
    #[Bench\Iterations(5)]
    public function benchRustMapPut1000(): void
    {
        $m = RustMap::empty();
        for ($i = 0; $i < 1000; $i++) {
            $m = $m->assoc("key$i", $i);
        }
    }

    // ========== Map Get Benchmarks ==========

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchPhpMapGet1000(): void
    {
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();
        $m = PhpMap::empty($hasher, $equalizer);
        for ($i = 0; $i < 1000; $i++) {
            $m = $m->put("key$i", $i);
        }

        // Now benchmark the gets
        for ($i = 0; $i < 1000; $i++) {
            $m->find("key$i");
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchRustMapGet1000(): void
    {
        $m = RustMap::empty();
        for ($i = 0; $i < 1000; $i++) {
            $m = $m->assoc("key$i", $i);
        }

        // Now benchmark the gets
        for ($i = 0; $i < 1000; $i++) {
            $m->get("key$i");
        }
    }

    // ========== Keyword Benchmarks ==========

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchPhpKeywordCreation(): void
    {
        // Create 100 keywords
        for ($i = 0; $i < 100; $i++) {
            Keyword::create("key$i");
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchRustKeywordCreation(): void
    {
        // Create 100 keywords
        for ($i = 0; $i < 100; $i++) {
            RustKeyword::create("key$i");
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchPhpKeywordEquality(): void
    {
        $k1 = Keyword::create("test");
        $k2 = Keyword::create("test");
        for ($i = 0; $i < 1000; $i++) {
            $k1->equals($k2);
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchRustKeywordEquality(): void
    {
        $k1 = RustKeyword::create("test");
        $k2 = RustKeyword::create("test");
        for ($i = 0; $i < 1000; $i++) {
            $k1->equals($k2);
        }
    }
}
