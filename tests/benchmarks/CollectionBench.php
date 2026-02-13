<?php

declare(strict_types=1);

namespace Benchmarks;

use Clojure\Lang\Hasher;
use Clojure\Lang\Equalizer;
use Clojure\Lang\Keyword;
use Clojure\Lang\Symbol;
use Clojure\Lang\Collections\Vector\PersistentVector;
use Clojure\Lang\Collections\Map\PersistentHashMap;
use Clojure\Lang\Collections\HashSet\PersistentHashSet;
use Clojure\Lang\Collections\HashSet\TransientHashSet;
use PhpBench\Attributes as Bench;

/**
 * Benchmark suite for ClojurePHP persistent data structures.
 *
 * Run with: vendor/bin/phpbench run benchmarks/ --report=aggregate
 */
class CollectionBench
{
    private PersistentVector $prebuiltVector;
    private PersistentHashMap $prebuiltMap;

    public function __construct()
    {
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        // Pre-build collections for lookup benchmarks
        $this->prebuiltVector = PersistentVector::fromArray($hasher, $equalizer, range(0, 999));

        // PersistentHashMap::fromArray expects [k1, v1, k2, v2, ...] format
        $mapData = [];
        for ($i = 0; $i < 1000; $i++) {
            $mapData[] = "key$i";
            $mapData[] = $i;
        }
        $this->prebuiltMap = PersistentHashMap::fromArray($hasher, $equalizer, $mapData);
    }

    // ========== Vector Benchmarks ==========

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchVectorAppend1000(): void
    {
        $v = PersistentVector::empty(Hasher::getInstance(), Equalizer::getInstance());
        for ($i = 0; $i < 1000; $i++) {
            $v = $v->append($i);
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchVectorGet1000(): void
    {
        for ($i = 0; $i < 1000; $i++) {
            $this->prebuiltVector->get($i);
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchVectorIteration(): void
    {
        $sum = 0;
        foreach ($this->prebuiltVector as $val) {
            $sum += $val;
        }
    }

    // ========== Map Benchmarks ==========

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchMapPut1000(): void
    {
        $m = PersistentHashMap::empty(Hasher::getInstance(), Equalizer::getInstance());
        for ($i = 0; $i < 1000; $i++) {
            $m = $m->put("key$i", $i);
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchMapGet1000(): void
    {
        for ($i = 0; $i < 1000; $i++) {
            $this->prebuiltMap->find("key$i");
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchMapKeywordGet1000(): void
    {
        // Build a map with keyword keys
        $m = PersistentHashMap::empty(Hasher::getInstance(), Equalizer::getInstance());
        for ($i = 0; $i < 100; $i++) {
            $m = $m->put(Keyword::create("key$i"), $i);
        }

        // Lookup with keywords - should benefit from interning
        for ($i = 0; $i < 1000; $i++) {
            $key = Keyword::create("key" . ($i % 100));
            $m->find($key);
        }
    }

    // ========== Keyword/Symbol Interning Benchmarks ==========

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchKeywordCreation(): void
    {
        // Same keyword created 100 times - should be interned
        for ($i = 0; $i < 100; $i++) {
            Keyword::create("test-keyword");
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchSymbolCreation(): void
    {
        // Same symbol created 100 times - should be interned
        for ($i = 0; $i < 100; $i++) {
            Symbol::create("test-symbol");
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchKeywordEquality(): void
    {
        $k1 = Keyword::create("test");
        $k2 = Keyword::create("test");

        for ($i = 0; $i < 100; $i++) {
            $k1->equals($k2);
        }
    }

    // ========== Hashing Benchmarks ==========

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchHashString(): void
    {
        for ($i = 0; $i < 100; $i++) {
            Hasher::hashValue("test-string-$i");
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchHashKeyword(): void
    {
        $k = Keyword::create("test");
        for ($i = 0; $i < 100; $i++) {
            Hasher::hashValue($k);
        }
    }

    #[Bench\Revs(1000)]
    #[Bench\Iterations(5)]
    public function benchHashInteger(): void
    {
        for ($i = 0; $i < 100; $i++) {
            Hasher::hashValue($i);
        }
    }

    // ========== Set Benchmarks ==========

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchSetAdd1000(): void
    {
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();
        $map = PersistentHashMap::empty($hasher, $equalizer)->asTransient();
        $set = new TransientHashSet($hasher, $map);

        for ($i = 0; $i < 1000; $i++) {
            $set->add($i);
        }
        $set->persistent();
    }

    // ========== Realistic Workload Benchmarks ==========

    #[Bench\Revs(50)]
    #[Bench\Iterations(5)]
    public function benchMapTransformPipeline(): void
    {
        // Simulates: (->> data (map transform) (filter pred) (reduce acc))
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        // Build initial data: vector of maps
        $data = PersistentVector::empty($hasher, $equalizer);
        for ($i = 0; $i < 100; $i++) {
            $record = PersistentHashMap::empty($hasher, $equalizer)
                ->put(Keyword::create("id"), $i)
                ->put(Keyword::create("value"), $i * 10)
                ->put(Keyword::create("active"), $i % 2 === 0);
            $data = $data->append($record);
        }

        // Transform: double each value
        $transformed = PersistentVector::empty($hasher, $equalizer);
        foreach ($data as $record) {
            $newVal = $record->find(Keyword::create("value")) * 2;
            $transformed = $transformed->append(
                $record->put(Keyword::create("value"), $newVal)
            );
        }

        // Filter: keep active records
        $filtered = PersistentVector::empty($hasher, $equalizer);
        foreach ($transformed as $record) {
            if ($record->find(Keyword::create("active")) === true) {
                $filtered = $filtered->append($record);
            }
        }

        // Reduce: sum values
        $sum = 0;
        foreach ($filtered as $record) {
            $sum += $record->find(Keyword::create("value"));
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchNestedMapAccess(): void
    {
        // Simulates: (get-in data [:users 0 :profile :settings :theme])
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        // Build nested structure
        $theme = PersistentHashMap::empty($hasher, $equalizer)
            ->put(Keyword::create("name"), "dark")
            ->put(Keyword::create("primary"), "#333");

        $settings = PersistentHashMap::empty($hasher, $equalizer)
            ->put(Keyword::create("theme"), $theme)
            ->put(Keyword::create("notifications"), true);

        $profile = PersistentHashMap::empty($hasher, $equalizer)
            ->put(Keyword::create("settings"), $settings)
            ->put(Keyword::create("bio"), "Hello");

        $user = PersistentHashMap::empty($hasher, $equalizer)
            ->put(Keyword::create("id"), 1)
            ->put(Keyword::create("profile"), $profile);

        $users = PersistentVector::empty($hasher, $equalizer)->append($user);

        $data = PersistentHashMap::empty($hasher, $equalizer)
            ->put(Keyword::create("users"), $users);

        // Deep access 100 times
        for ($i = 0; $i < 100; $i++) {
            $users = $data->find(Keyword::create("users"));
            $user = $users->get(0);
            $profile = $user->find(Keyword::create("profile"));
            $settings = $profile->find(Keyword::create("settings"));
            $theme = $settings->find(Keyword::create("theme"));
            $name = $theme->find(Keyword::create("name"));
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchAssocInSimulation(): void
    {
        // Simulates: (assoc-in data [:a :b :c] value)
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        $c = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("value"), 0);
        $b = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("c"), $c);
        $a = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("b"), $b);
        $data = PersistentHashMap::empty($hasher, $equalizer)->put(Keyword::create("a"), $a);

        // Update nested value 100 times (each creates new path)
        for ($i = 0; $i < 100; $i++) {
            $a = $data->find(Keyword::create("a"));
            $b = $a->find(Keyword::create("b"));
            $c = $b->find(Keyword::create("c"));
            $newC = $c->put(Keyword::create("value"), $i);
            $newB = $b->put(Keyword::create("c"), $newC);
            $newA = $a->put(Keyword::create("b"), $newB);
            $data = $data->put(Keyword::create("a"), $newA);
        }
    }

    #[Bench\Revs(50)]
    #[Bench\Iterations(5)]
    public function benchTransientBatchBuild(): void
    {
        // Simulates building a large map using transients
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        $transient = PersistentHashMap::empty($hasher, $equalizer)->asTransient();
        for ($i = 0; $i < 1000; $i++) {
            $transient->put(Keyword::create("key$i"), $i);
        }
        $persistent = $transient->persistent();
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchVectorConcat(): void
    {
        // Simulates: (into [] (concat v1 v2 v3))
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        $v1 = PersistentVector::fromArray($hasher, $equalizer, range(0, 99));
        $v2 = PersistentVector::fromArray($hasher, $equalizer, range(100, 199));
        $v3 = PersistentVector::fromArray($hasher, $equalizer, range(200, 299));

        // Concat via iteration
        $result = $v1;
        foreach ($v2 as $item) {
            $result = $result->append($item);
        }
        foreach ($v3 as $item) {
            $result = $result->append($item);
        }
    }

    #[Bench\Revs(100)]
    #[Bench\Iterations(5)]
    public function benchMergeMap(): void
    {
        // Simulates: (merge m1 m2 m3)
        $hasher = Hasher::getInstance();
        $equalizer = Equalizer::getInstance();

        // Build 3 maps with 50 entries each
        $m1 = PersistentHashMap::empty($hasher, $equalizer);
        $m2 = PersistentHashMap::empty($hasher, $equalizer);
        $m3 = PersistentHashMap::empty($hasher, $equalizer);

        for ($i = 0; $i < 50; $i++) {
            $m1 = $m1->put(Keyword::create("a$i"), $i);
            $m2 = $m2->put(Keyword::create("b$i"), $i);
            $m3 = $m3->put(Keyword::create("c$i"), $i);
        }

        // Merge via iteration
        $result = $m1;
        foreach ($m2 as $k => $v) {
            $result = $result->put($k, $v);
        }
        foreach ($m3 as $k => $v) {
            $result = $result->put($k, $v);
        }
    }
}
