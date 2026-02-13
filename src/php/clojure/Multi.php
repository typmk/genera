<?php

declare(strict_types=1);

/**
 * Multimethod dispatch - dispatch on arbitrary function of arguments.
 */

namespace Clojure\Php;

// Multimethod dispatch functions: multi-name => callable
$GLOBALS['__cljp_multis'] = [];
// Multimethod implementations: multi-name => dispatch-value => callable
$GLOBALS['__cljp_multi_methods'] = [];

/**
 * Define a multimethod with a dispatch function.
 */
function defMulti(string $name, callable $dispatchFn): void
{
    $GLOBALS['__cljp_multis'][$name] = $dispatchFn;
    if (!isset($GLOBALS['__cljp_multi_methods'][$name])) {
        $GLOBALS['__cljp_multi_methods'][$name] = [];
    }
}

/**
 * Add a method implementation for a dispatch value.
 */
function addMethod(string $name, mixed $dispatchVal, callable $fn): void
{
    $key = dispatchKey($dispatchVal);
    $GLOBALS['__cljp_multi_methods'][$name][$key] = $fn;
}

/**
 * Convert a dispatch value to a string key.
 */
function dispatchKey(mixed $val): string
{
    if ($val === null) return ':nil:';
    if (is_string($val)) return $val;
    if ($val instanceof Kw) return ':' . $val->fullName();
    if (is_int($val) || is_float($val)) return (string)$val;
    if (is_bool($val)) return $val ? ':true:' : ':false:';
    // For complex values, use serialize
    return md5(serialize($val));
}

/**
 * Dispatch a multimethod call.
 */
function multimethodDispatch(string $name, array $args): mixed
{
    if (!isset($GLOBALS['__cljp_multis'][$name])) {
        throw new \Exception("No multimethod defined: $name");
    }

    // Get dispatch value
    $dispatchVal = ($GLOBALS['__cljp_multis'][$name])(...$args);
    $key = dispatchKey($dispatchVal);

    // Find implementation
    if (isset($GLOBALS['__cljp_multi_methods'][$name][$key])) {
        return ($GLOBALS['__cljp_multi_methods'][$name][$key])(...$args);
    }

    // Check for default (keyword :default becomes ':default')
    if (isset($GLOBALS['__cljp_multi_methods'][$name][':default'])) {
        return ($GLOBALS['__cljp_multi_methods'][$name][':default'])(...$args);
    }

    throw new \Exception("No method in multimethod '$name' for dispatch value: " . prStr($dispatchVal));
}

/**
 * Get all dispatch values for a multimethod.
 */
function getMethods(string $name): Map
{
    if (!isset($GLOBALS['__cljp_multi_methods'][$name])) {
        return hashMap();
    }
    $m = hashMap();
    foreach ($GLOBALS['__cljp_multi_methods'][$name] as $key => $fn) {
        $m = $m->put($key, $fn);
    }
    return $m;
}

/**
 * Remove a method from a multimethod.
 */
function removeMethod(string $name, mixed $dispatchVal): void
{
    $key = dispatchKey($dispatchVal);
    unset($GLOBALS['__cljp_multi_methods'][$name][$key]);
}

/**
 * Remove all methods from a multimethod.
 */
function removeAllMethods(string $name): void
{
    $GLOBALS['__cljp_multi_methods'][$name] = [];
}

/**
 * Get the preferred method for a dispatch value.
 */
function preferMethod(string $name, mixed $dispatchValX, mixed $dispatchValY): void
{
    // TODO: Implement prefer-method hierarchy
    // For now, just no-op
}
