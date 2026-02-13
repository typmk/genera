<?php

declare(strict_types=1);

/**
 * Test: Can Psalm/PHPStan analyze closure property calls?
 */

class Core {
    /** @var \Closure(mixed): mixed */
    public \Closure $first;

    /** @var \Closure(callable, iterable): iterable */
    public \Closure $map;
}

$core = new Core();
$core->first = fn(mixed $coll): mixed => $coll[0] ?? null;
$core->map = fn(callable $f, iterable $coll): array => array_map($f, [...$coll]);

// Can static analysis track types through this?
$result = ($core->first)([1, 2, 3]);
$mapped = ($core->map)(fn($x) => $x * 2, [1, 2, 3]);

// Compare to native - this is easy for analyzers
function first_native(mixed $coll): mixed {
    return $coll[0] ?? null;
}
$result2 = first_native([1, 2, 3]);
