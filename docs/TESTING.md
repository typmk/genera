# Testing

## Running Tests

### Clojure Tests

```bash
# Run all tests
clojure -M:test

# Run specific test namespace
clojure -M:test -n genera.compiler-test
```

### PHP Tests

```bash
# Install PHP dependencies
composer install

# Run PHPUnit tests
./vendor/bin/phpunit

# Run PHPStan static analysis
./vendor/bin/phpstan analyse

# Run PHP benchmarks
./vendor/bin/phpbench run
```

### Parity Tests

Test that compiled PHP output matches Clojure/JVM behavior:

```bash
# Generate parity test results from JVM
clojure -M:parity-jvm > parity_results_jvm.edn

# Run same tests in PHP and compare
php parity_test.php
```

## Test Structure

```
tests/
├── Cljp/                   # PHP runtime tests
├── clojure/                # Clojure compiler tests
└── benchmarks/             # Performance benchmarks
```

## Writing Tests

### Clojure Tests

```clojure
(ns genera.my-test
  (:require [clojure.test :refer :all]
            [genera.compiler :as compiler]))

(deftest compile-simple-form
  (is (= "(1 + 2)" (compiler/emit '(+ 1 2)))))
```

### PHP Tests

```php
<?php
use PHPUnit\Framework\TestCase;
use Clojure\Lang\PersistentVector;

class VectorTest extends TestCase
{
    public function testConj(): void
    {
        $v = PersistentVector::create([1, 2, 3]);
        $v2 = $v->conj(4);
        $this->assertEquals(4, $v2->count());
    }
}
```

## Continuous Integration

Tests run on:
- Push to main branch
- Pull requests
- Nightly builds

## Coverage

Generate coverage reports:

```bash
# Clojure coverage (with cloverage)
clojure -M:coverage

# PHP coverage
./vendor/bin/phpunit --coverage-html coverage/
```
