<?php

declare(strict_types=1);

namespace Clojure\Php;

/**
 * Transducers - composable algorithmic transformations.
 *
 * A transducer is a function that takes a reducing function and returns
 * a new reducing function. This allows transformations to be composed
 * without creating intermediate collections.
 *
 * Example:
 *   $xf = comp(mapping(fn($x) => $x * 2), filtering(fn($x) => $x > 5));
 *   transduce($xf, fn($acc, $x) => $acc + $x, 0, [1, 2, 3, 4, 5]);
 */

// ============================================================
// Core transducer protocol
// ============================================================

/**
 * Transduce - apply a transducer to a collection with a reducing function.
 *
 * @param callable $xform The transducer
 * @param callable $f The reducing function (arity 0 for init, 2 for step, 1 for complete)
 * @param mixed $init Initial value
 * @param mixed $coll Collection to process
 */
function transduce(callable $xform, callable $f, mixed $init, mixed $coll): mixed
{
    // Apply transducer to reducing function
    $rf = $xform($f);

    // Reduce over collection
    $acc = $init;
    $s = seq($coll);
    while ($s !== null) {
        $acc = $rf($acc, $s->first());
        if ($acc instanceof Reduced) {
            return unreduced($acc);
        }
        $s = $s->next();
    }

    // Complete - call arity-1 form if it's a completing transducer
    return $acc;
}

/**
 * Into with transducer - pour transformed values into a collection.
 */
function intoXf(mixed $to, callable $xform, mixed $from): mixed
{
    $rf = $xform(fn($acc, $x) => conj($acc, $x));
    return reduce_($rf, $to, $from);
}

/**
 * Sequence with transducer - return lazy sequence of transformed values.
 */
function sequenceXf(callable $xform, mixed $coll): LazySeq
{
    $buffer = [];
    $bufferedRf = $xform(function ($acc, $x) use (&$buffer) {
        $buffer[] = $x;
        return $acc;
    });

    return new LazySeq(function () use ($xform, $coll, &$buffer, &$bufferedRf) {
        $s = seq($coll);

        $step = function ($s) use (&$buffer, &$bufferedRf, &$step) {
            // If buffer has items, return them
            if (!empty($buffer)) {
                $x = array_shift($buffer);
                return cons($x, new LazySeq(fn() => $step($s)));
            }

            // Process more input
            while ($s !== null && empty($buffer)) {
                $result = $bufferedRf(null, $s->first());
                if ($result instanceof Reduced) {
                    break;
                }
                $s = $s->next();
            }

            if (!empty($buffer)) {
                $x = array_shift($buffer);
                return cons($x, new LazySeq(fn() => $step($s)));
            }

            return null;
        };

        return $step($s);
    });
}

/**
 * Eduction - a reducible/iterable of transformed values.
 * Delays transformation until iterated or reduced.
 */
final class Eduction implements \IteratorAggregate, Seqable
{
    /** @var callable */
    private $xform;
    private mixed $coll;

    public function __construct(callable $xform, mixed $coll)
    {
        $this->xform = $xform;
        $this->coll = $coll;
    }

    public function reduce(callable $f, mixed $init): mixed
    {
        return transduce($this->xform, $f, $init, $this->coll);
    }

    public function seq(): ?ISeq
    {
        return sequenceXf($this->xform, $this->coll)->seq();
    }

    public function getIterator(): \Traversable
    {
        $buffer = [];
        $rf = ($this->xform)(function ($acc, $x) use (&$buffer) {
            $buffer[] = $x;
            return $acc;
        });

        $s = seq($this->coll);
        while ($s !== null) {
            $result = $rf(null, $s->first());
            while (!empty($buffer)) {
                yield array_shift($buffer);
            }
            if ($result instanceof Reduced) {
                break;
            }
            $s = $s->next();
        }
    }
}

function eduction(callable $xform, mixed $coll): Eduction
{
    return new Eduction($xform, $coll);
}

// ============================================================
// Transducer factories
// ============================================================

/**
 * Map transducer - apply f to each element.
 */
function mapping(callable $f): callable
{
    return fn(callable $rf) => fn(mixed $acc, mixed $x) => $rf($acc, $f($x));
}

/**
 * Filter transducer - keep elements satisfying pred.
 */
function filtering(callable $pred): callable
{
    return fn(callable $rf) => fn(mixed $acc, mixed $x) =>
        $pred($x) ? $rf($acc, $x) : $acc;
}

/**
 * Remove transducer - remove elements satisfying pred.
 */
function removing(callable $pred): callable
{
    return filtering(fn($x) => !$pred($x));
}

/**
 * Take transducer - take first n elements.
 */
function taking(int $n): callable
{
    return function (callable $rf) use ($n) {
        $count = 0;
        return function (mixed $acc, mixed $x) use ($rf, $n, &$count) {
            if ($count < $n) {
                $count++;
                $result = $rf($acc, $x);
                if ($count >= $n) {
                    return reduced($result);
                }
                return $result;
            }
            return reduced($acc);
        };
    };
}

/**
 * Drop transducer - skip first n elements.
 */
function dropping(int $n): callable
{
    return function (callable $rf) use ($n) {
        $count = 0;
        return function (mixed $acc, mixed $x) use ($rf, $n, &$count) {
            if ($count < $n) {
                $count++;
                return $acc;
            }
            return $rf($acc, $x);
        };
    };
}

/**
 * Take-while transducer - take while pred is true.
 */
function takingWhile(callable $pred): callable
{
    return function (callable $rf) use ($pred) {
        return function (mixed $acc, mixed $x) use ($rf, $pred) {
            if ($pred($x)) {
                return $rf($acc, $x);
            }
            return reduced($acc);
        };
    };
}

/**
 * Drop-while transducer - drop while pred is true.
 */
function droppingWhile(callable $pred): callable
{
    return function (callable $rf) use ($pred) {
        $dropping = true;
        return function (mixed $acc, mixed $x) use ($rf, $pred, &$dropping) {
            if ($dropping) {
                if ($pred($x)) {
                    return $acc;
                }
                $dropping = false;
            }
            return $rf($acc, $x);
        };
    };
}

/**
 * Mapcat transducer - map then concatenate.
 */
function mapcatting(callable $f): callable
{
    return function (callable $rf) use ($f) {
        return function (mixed $acc, mixed $x) use ($rf, $f) {
            $result = $f($x);
            $s = seq($result);
            while ($s !== null) {
                $acc = $rf($acc, $s->first());
                if ($acc instanceof Reduced) {
                    return $acc;
                }
                $s = $s->next();
            }
            return $acc;
        };
    };
}

/**
 * Distinct transducer - remove duplicates.
 */
function distincting(): callable
{
    return function (callable $rf) {
        $seen = hashSet();
        return function (mixed $acc, mixed $x) use ($rf, &$seen) {
            if ($seen->contains($x)) {
                return $acc;
            }
            $seen = $seen->add($x);
            return $rf($acc, $x);
        };
    };
}

/**
 * Dedupe transducer - remove consecutive duplicates.
 */
function deduping(): callable
{
    return function (callable $rf) {
        $prev = new \stdClass(); // unique sentinel
        return function (mixed $acc, mixed $x) use ($rf, &$prev) {
            if (equals($x, $prev)) {
                return $acc;
            }
            $prev = $x;
            return $rf($acc, $x);
        };
    };
}

/**
 * Partition-all transducer - partition into chunks of n.
 */
function partitioningAll(int $n): callable
{
    return function (callable $rf) use ($n) {
        $buffer = [];
        return function (mixed $acc, mixed $x) use ($rf, $n, &$buffer) {
            $buffer[] = $x;
            if (count($buffer) >= $n) {
                $chunk = $buffer;
                $buffer = [];
                return $rf($acc, vec(...$chunk));
            }
            return $acc;
        };
    };
}

/**
 * Keep transducer - keep non-nil results of f.
 */
function keeping(callable $f): callable
{
    return function (callable $rf) use ($f) {
        return function (mixed $acc, mixed $x) use ($rf, $f) {
            $result = $f($x);
            return $result !== null ? $rf($acc, $result) : $acc;
        };
    };
}

/**
 * Keep-indexed transducer - keep non-nil results of (f index item).
 */
function keepingIndexed(callable $f): callable
{
    return function (callable $rf) use ($f) {
        $i = 0;
        return function (mixed $acc, mixed $x) use ($rf, $f, &$i) {
            $result = $f($i++, $x);
            return $result !== null ? $rf($acc, $result) : $acc;
        };
    };
}

/**
 * Map-indexed transducer - apply (f index item).
 */
function mappingIndexed(callable $f): callable
{
    return function (callable $rf) use ($f) {
        $i = 0;
        return function (mixed $acc, mixed $x) use ($rf, $f, &$i) {
            return $rf($acc, $f($i++, $x));
        };
    };
}

/**
 * Interpose transducer - insert sep between elements.
 */
function interposing(mixed $sep): callable
{
    return function (callable $rf) use ($sep) {
        $first = true;
        return function (mixed $acc, mixed $x) use ($rf, $sep, &$first) {
            if ($first) {
                $first = false;
                return $rf($acc, $x);
            }
            $acc = $rf($acc, $sep);
            if ($acc instanceof Reduced) {
                return $acc;
            }
            return $rf($acc, $x);
        };
    };
}

/**
 * Replace transducer - replace values according to smap.
 */
function replacing(mixed $smap): callable
{
    return mapping(fn($x) => contains($smap, $x) ? get($smap, $x) : $x);
}

// ============================================================
// Completing transducers
// ============================================================

/**
 * Completing - wrap a 2-arity function with optional 1-arity completion.
 */
function completing(callable $f, ?callable $cf = null): callable
{
    $cf ??= fn($x) => $x;
    return function (...$args) use ($f, $cf) {
        return match (count($args)) {
            1 => $cf($args[0]),
            2 => $f($args[0], $args[1]),
            default => throw new \InvalidArgumentException("Invalid arity"),
        };
    };
}

// ============================================================
// Helpers
// ============================================================

/**
 * Cat - concatenate reducing function.
 * Used with mapcat: (mapcat identity) == cat
 */
function cat(): callable
{
    return mapcatting(fn($x) => $x);
}

/**
 * Halt-when transducer - halt when pred becomes true.
 */
function haltWhen(callable $pred, ?callable $retf = null): callable
{
    $retf ??= fn($acc, $x) => $acc;
    return function (callable $rf) use ($pred, $retf) {
        return function (mixed $acc, mixed $x) use ($rf, $pred, $retf) {
            if ($pred($x)) {
                return reduced($retf($acc, $x));
            }
            return $rf($acc, $x);
        };
    };
}
