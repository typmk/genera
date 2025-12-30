<?php

declare(strict_types=1);

namespace Clojure;

use Clojure\Lang\Atom;
use Clojure\Lang\Keyword;
use Clojure\Lang\MultiFn;
use Clojure\Lang\Symbol;

/**
 * Clojure Core Functions for PHP Runtime.
 *
 * These functions enable Clojure code compiled to PHP to run correctly.
 * They mirror clojure.core as closely as possible.
 *
 * Naming: PHP doesn't allow ? or ! in function names, so:
 *   nil?    -> nil_QMARK_
 *   swap!   -> swap_BANG_
 *   first   -> first (no change)
 */
final class Core
{
    // ============================================================
    // Atoms
    // ============================================================

    /**
     * Creates and returns an Atom with an initial value.
     */
    public static function atom(mixed $x): Atom
    {
        return new Atom($x);
    }

    /**
     * Returns the current value of an atom.
     */
    public static function deref(Atom $ref): mixed
    {
        return $ref->deref();
    }

    /**
     * Sets the value of atom to newval. Returns newval.
     */
    public static function reset_BANG_(Atom $atom, mixed $newval): mixed
    {
        return $atom->reset($newval);
    }

    /**
     * Atomically swaps the value of atom by applying f to current value and args.
     */
    public static function swap_BANG_(Atom $atom, callable $f, mixed ...$args): mixed
    {
        return $atom->swap($f, ...$args);
    }

    /**
     * Atomically compares and sets the value of atom.
     */
    public static function compare_and_set_BANG_(Atom $atom, mixed $oldval, mixed $newval): bool
    {
        return $atom->compareAndSet($oldval, $newval);
    }

    // ============================================================
    // Multimethods
    // ============================================================

    /**
     * Creates a new multimethod with the given dispatch function.
     */
    public static function defmulti(callable $dispatchFn, string $name = 'anonymous'): MultiFn
    {
        return new MultiFn($dispatchFn, $name);
    }

    /**
     * Creates and associates a new method with a multimethod.
     */
    public static function defmethod(MultiFn $multiFn, string|int|null $dispatchVal, callable $method): MultiFn
    {
        return $multiFn->addMethod($dispatchVal, $method);
    }

    // ============================================================
    // Predicates
    // ============================================================

    public static function nil_QMARK_(mixed $x): bool
    {
        return $x === null;
    }

    public static function some_QMARK_(mixed $x): bool
    {
        return $x !== null;
    }

    public static function true_QMARK_(mixed $x): bool
    {
        return $x === true;
    }

    public static function false_QMARK_(mixed $x): bool
    {
        return $x === false;
    }

    public static function boolean_QMARK_(mixed $x): bool
    {
        return is_bool($x);
    }

    public static function string_QMARK_(mixed $x): bool
    {
        return is_string($x);
    }

    public static function number_QMARK_(mixed $x): bool
    {
        return is_int($x) || is_float($x);
    }

    public static function integer_QMARK_(mixed $x): bool
    {
        return is_int($x);
    }

    public static function float_QMARK_(mixed $x): bool
    {
        return is_float($x);
    }

    public static function keyword_QMARK_(mixed $x): bool
    {
        return $x instanceof Keyword;
    }

    public static function symbol_QMARK_(mixed $x): bool
    {
        return $x instanceof Symbol;
    }

    public static function fn_QMARK_(mixed $x): bool
    {
        return is_callable($x);
    }

    public static function map_QMARK_(mixed $x): bool
    {
        return is_array($x) && (empty($x) || !array_is_list($x));
    }

    public static function vector_QMARK_(mixed $x): bool
    {
        return is_array($x) && (empty($x) || array_is_list($x));
    }

    public static function seq_QMARK_(mixed $x): bool
    {
        return is_array($x) || is_iterable($x);
    }

    public static function sequential_QMARK_(mixed $x): bool
    {
        return is_array($x) && (empty($x) || array_is_list($x));
    }

    public static function coll_QMARK_(mixed $x): bool
    {
        return is_array($x) || is_iterable($x);
    }

    public static function empty_QMARK_(mixed $x): bool
    {
        if ($x === null) return true;
        if (is_array($x)) return count($x) === 0;
        if (is_string($x)) return strlen($x) === 0;
        return false;
    }

    public static function contains_QMARK_(mixed $coll, mixed $key): bool
    {
        if (is_array($coll)) {
            return array_key_exists($key instanceof Keyword ? $key->getName() : $key, $coll);
        }
        return false;
    }

    // ============================================================
    // Sequences
    // ============================================================

    public static function seq(mixed $x): ?array
    {
        if ($x === null) return null;
        if (is_array($x)) return count($x) > 0 ? array_values($x) : null;
        if (is_string($x)) return strlen($x) > 0 ? str_split($x) : null;
        if (is_iterable($x)) {
            $arr = iterator_to_array($x);
            return count($arr) > 0 ? array_values($arr) : null;
        }
        return null;
    }

    public static function first(mixed $x): mixed
    {
        if ($x === null) return null;
        if (is_array($x)) return count($x) > 0 ? reset($x) : null;
        if (is_string($x)) return strlen($x) > 0 ? $x[0] : null;
        return null;
    }

    public static function second(mixed $x): mixed
    {
        if ($x === null) return null;
        if (is_array($x)) {
            $values = array_values($x);
            return $values[1] ?? null;
        }
        return null;
    }

    public static function rest(mixed $x): array
    {
        if ($x === null) return [];
        if (is_array($x)) return array_slice(array_values($x), 1);
        if (is_string($x)) return strlen($x) > 1 ? str_split(substr($x, 1)) : [];
        return [];
    }

    public static function next(mixed $x): ?array
    {
        $r = self::rest($x);
        return count($r) > 0 ? $r : null;
    }

    public static function last(mixed $x): mixed
    {
        if ($x === null) return null;
        if (is_array($x)) return count($x) > 0 ? end($x) : null;
        if (is_string($x)) return strlen($x) > 0 ? $x[strlen($x) - 1] : null;
        return null;
    }

    public static function butlast(mixed $x): ?array
    {
        if ($x === null) return null;
        if (is_array($x)) {
            $count = count($x);
            return $count > 1 ? array_slice(array_values($x), 0, $count - 1) : null;
        }
        return null;
    }

    public static function cons(mixed $x, mixed $seq): array
    {
        $arr = self::seq($seq) ?? [];
        array_unshift($arr, $x);
        return $arr;
    }

    public static function conj(mixed $coll, mixed ...$xs): array
    {
        $result = $coll ?? [];
        foreach ($xs as $x) {
            $result[] = $x;
        }
        return $result;
    }

    public static function concat(mixed ...$colls): array
    {
        $result = [];
        foreach ($colls as $coll) {
            if ($coll !== null) {
                foreach ($coll as $item) {
                    $result[] = $item;
                }
            }
        }
        return $result;
    }

    public static function count(mixed $x): int
    {
        if ($x === null) return 0;
        if (is_array($x)) return \count($x);
        if (is_string($x)) return strlen($x);
        if (is_countable($x)) return \count($x);
        return 0;
    }

    public static function nth(mixed $coll, int $n, mixed $notFound = null): mixed
    {
        if ($coll === null) return $notFound;
        if (is_array($coll)) {
            $values = array_values($coll);
            return $values[$n] ?? $notFound;
        }
        if (is_string($coll)) {
            return $coll[$n] ?? $notFound;
        }
        return $notFound;
    }

    public static function take(int $n, mixed $coll): array
    {
        if ($coll === null || $n <= 0) return [];
        if (is_array($coll)) return array_slice(array_values($coll), 0, $n);
        return [];
    }

    public static function drop(int $n, mixed $coll): array
    {
        if ($coll === null) return [];
        if (is_array($coll)) return array_slice(array_values($coll), $n);
        return [];
    }

    public static function take_while(callable $pred, mixed $coll): array
    {
        if ($coll === null) return [];
        $result = [];
        foreach ($coll as $item) {
            if (!$pred($item)) break;
            $result[] = $item;
        }
        return $result;
    }

    public static function drop_while(callable $pred, mixed $coll): array
    {
        if ($coll === null) return [];
        $result = [];
        $dropping = true;
        foreach ($coll as $item) {
            if ($dropping && $pred($item)) continue;
            $dropping = false;
            $result[] = $item;
        }
        return $result;
    }

    public static function partition(int $n, mixed $coll): array
    {
        if ($coll === null) return [];
        $arr = is_array($coll) ? array_values($coll) : iterator_to_array($coll);
        return array_chunk($arr, $n);
    }

    public static function partition_all(int $n, mixed $coll): array
    {
        return self::partition($n, $coll);
    }

    public static function reverse(mixed $coll): array
    {
        if ($coll === null) return [];
        if (is_array($coll)) return array_reverse(array_values($coll));
        return [];
    }

    public static function sort(mixed $coll, ?callable $comp = null): array
    {
        if ($coll === null) return [];
        $arr = is_array($coll) ? array_values($coll) : iterator_to_array($coll);
        if ($comp) {
            usort($arr, $comp);
        } else {
            sort($arr);
        }
        return $arr;
    }

    public static function distinct(mixed $coll): array
    {
        if ($coll === null) return [];
        return array_values(array_unique(is_array($coll) ? $coll : iterator_to_array($coll)));
    }

    // ============================================================
    // Higher-Order Functions
    // ============================================================

    public static function map(callable $f, mixed ...$colls): array
    {
        if (count($colls) === 0) return [];
        if (count($colls) === 1) {
            $coll = $colls[0];
            if ($coll === null) return [];
            return array_map($f, is_array($coll) ? array_values($coll) : iterator_to_array($coll));
        }

        // Multi-collection map
        $arrays = array_map(fn($c) => $c === null ? [] : (is_array($c) ? array_values($c) : iterator_to_array($c)), $colls);
        $minLen = min(array_map('count', $arrays));
        $result = [];
        for ($i = 0; $i < $minLen; $i++) {
            $args = array_map(fn($arr) => $arr[$i], $arrays);
            $result[] = $f(...$args);
        }
        return $result;
    }

    public static function mapv(callable $f, mixed ...$colls): array
    {
        return self::map($f, ...$colls);
    }

    public static function filter(callable $pred, mixed $coll): array
    {
        if ($coll === null) return [];
        return array_values(array_filter(
            is_array($coll) ? $coll : iterator_to_array($coll),
            $pred
        ));
    }

    public static function filterv(callable $pred, mixed $coll): array
    {
        return self::filter($pred, $coll);
    }

    public static function remove(callable $pred, mixed $coll): array
    {
        return self::filter(fn($x) => !$pred($x), $coll);
    }

    public static function reduce(callable $f, mixed ...$args): mixed
    {
        if (count($args) === 1) {
            // (reduce f coll)
            $coll = $args[0];
            if ($coll === null || (is_array($coll) && count($coll) === 0)) {
                return $f();
            }
            $arr = is_array($coll) ? array_values($coll) : iterator_to_array($coll);
            return array_reduce(array_slice($arr, 1), $f, $arr[0]);
        } else {
            // (reduce f init coll)
            [$init, $coll] = $args;
            if ($coll === null) return $init;
            return array_reduce(
                is_array($coll) ? array_values($coll) : iterator_to_array($coll),
                $f,
                $init
            );
        }
    }

    public static function reduce_kv(callable $f, mixed $init, mixed $coll): mixed
    {
        if ($coll === null) return $init;
        $result = $init;
        foreach ($coll as $k => $v) {
            $result = $f($result, $k, $v);
        }
        return $result;
    }

    public static function some(callable $pred, mixed $coll): mixed
    {
        if ($coll === null) return null;
        foreach ($coll as $item) {
            $result = $pred($item);
            if ($result) return $result;
        }
        return null;
    }

    public static function every_QMARK_(callable $pred, mixed $coll): bool
    {
        if ($coll === null) return true;
        foreach ($coll as $item) {
            if (!$pred($item)) return false;
        }
        return true;
    }

    public static function not_every_QMARK_(callable $pred, mixed $coll): bool
    {
        return !self::every_QMARK_($pred, $coll);
    }

    public static function not_any_QMARK_(callable $pred, mixed $coll): bool
    {
        return self::some($pred, $coll) === null;
    }

    public static function keep(callable $f, mixed $coll): array
    {
        if ($coll === null) return [];
        $result = [];
        foreach ($coll as $item) {
            $v = $f($item);
            if ($v !== null) $result[] = $v;
        }
        return $result;
    }

    public static function mapcat(callable $f, mixed ...$colls): array
    {
        return self::concat(...self::map($f, ...$colls));
    }

    public static function into(mixed $to, mixed $from): mixed
    {
        if ($from === null) return $to;
        if ($to === null) $to = [];

        if (is_array($to) && self::map_QMARK_($to)) {
            // Into map
            foreach ($from as $item) {
                if (is_array($item) && count($item) >= 2) {
                    $to[$item[0]] = $item[1];
                }
            }
            return $to;
        }

        // Into vector/list
        foreach ($from as $item) {
            $to[] = $item;
        }
        return $to;
    }

    public static function group_by(callable $f, mixed $coll): array
    {
        if ($coll === null) return [];
        $result = [];
        foreach ($coll as $item) {
            $key = $f($item);
            $result[$key][] = $item;
        }
        return $result;
    }

    public static function frequencies(mixed $coll): array
    {
        if ($coll === null) return [];
        return array_count_values(is_array($coll) ? $coll : iterator_to_array($coll));
    }

    // ============================================================
    // Maps (Associative)
    // ============================================================

    public static function get(mixed $map, mixed $key, mixed $notFound = null): mixed
    {
        if ($map === null) return $notFound;
        if ($key instanceof Keyword) {
            $key = $key->getName();
        }
        if (is_array($map)) {
            return array_key_exists($key, $map) ? $map[$key] : $notFound;
        }
        return $notFound;
    }

    public static function get_in(mixed $m, array $ks, mixed $notFound = null): mixed
    {
        $result = $m;
        foreach ($ks as $k) {
            $result = self::get($result, $k);
            if ($result === null) return $notFound;
        }
        return $result;
    }

    public static function assoc(mixed $map, mixed ...$kvs): array
    {
        $result = $map ?? [];
        for ($i = 0; $i < count($kvs); $i += 2) {
            $k = $kvs[$i];
            $v = $kvs[$i + 1] ?? null;
            if ($k instanceof Keyword) {
                $k = $k->getName();
            }
            $result[$k] = $v;
        }
        return $result;
    }

    public static function assoc_in(mixed $m, array $ks, mixed $v): array
    {
        if (count($ks) === 0) return $v;
        if (count($ks) === 1) return self::assoc($m, $ks[0], $v);

        $k = $ks[0];
        $restKs = array_slice($ks, 1);
        return self::assoc($m, $k, self::assoc_in(self::get($m, $k), $restKs, $v));
    }

    public static function dissoc(mixed $map, mixed ...$keys): array
    {
        if ($map === null) return [];
        $result = $map;
        foreach ($keys as $k) {
            if ($k instanceof Keyword) {
                $k = $k->getName();
            }
            unset($result[$k]);
        }
        return $result;
    }

    public static function update(mixed $m, mixed $k, callable $f, mixed ...$args): array
    {
        $v = self::get($m, $k);
        return self::assoc($m, $k, $f($v, ...$args));
    }

    public static function update_in(mixed $m, array $ks, callable $f, mixed ...$args): array
    {
        if (count($ks) === 0) return $f($m, ...$args);
        if (count($ks) === 1) return self::update($m, $ks[0], $f, ...$args);

        $k = $ks[0];
        $restKs = array_slice($ks, 1);
        return self::assoc($m, $k, self::update_in(self::get($m, $k), $restKs, $f, ...$args));
    }

    public static function merge(mixed ...$maps): array
    {
        $result = [];
        foreach ($maps as $m) {
            if ($m !== null) {
                $result = array_merge($result, $m);
            }
        }
        return $result;
    }

    public static function select_keys(mixed $map, array $keys): array
    {
        if ($map === null) return [];
        $result = [];
        foreach ($keys as $k) {
            $key = $k instanceof Keyword ? $k->getName() : $k;
            if (array_key_exists($key, $map)) {
                $result[$key] = $map[$key];
            }
        }
        return $result;
    }

    public static function keys(mixed $map): array
    {
        if ($map === null) return [];
        return array_keys($map);
    }

    public static function vals(mixed $map): array
    {
        if ($map === null) return [];
        return array_values($map);
    }

    public static function key(mixed $entry): mixed
    {
        if (is_array($entry) && count($entry) >= 1) {
            return array_key_first($entry);
        }
        return null;
    }

    public static function val(mixed $entry): mixed
    {
        if (is_array($entry) && count($entry) >= 1) {
            return $entry[array_key_first($entry)];
        }
        return null;
    }

    public static function zipmap(array $keys, array $vals): array
    {
        return array_combine($keys, array_slice($vals, 0, count($keys)));
    }

    // ============================================================
    // Sets
    // ============================================================

    public static function set(mixed $coll): array
    {
        if ($coll === null) return [];
        return array_values(array_unique(is_array($coll) ? $coll : iterator_to_array($coll)));
    }

    public static function union(array ...$sets): array
    {
        return self::distinct(self::concat(...$sets));
    }

    public static function intersection(array $s1, array ...$sets): array
    {
        $result = $s1;
        foreach ($sets as $s) {
            $result = array_values(array_intersect($result, $s));
        }
        return $result;
    }

    public static function difference(array $s1, array ...$sets): array
    {
        $result = $s1;
        foreach ($sets as $s) {
            $result = array_values(array_diff($result, $s));
        }
        return $result;
    }

    // ============================================================
    // Strings
    // ============================================================

    public static function str(mixed ...$args): string
    {
        $result = '';
        foreach ($args as $arg) {
            if ($arg === null) continue;
            if ($arg === true) $result .= 'true';
            elseif ($arg === false) $result .= 'false';
            elseif (is_string($arg)) $result .= $arg;
            elseif (is_int($arg) || is_float($arg)) $result .= (string)$arg;
            elseif ($arg instanceof Keyword) $result .= ':' . $arg->getName();
            elseif ($arg instanceof Symbol) $result .= $arg->getName();
            else $result .= (string)$arg;
        }
        return $result;
    }

    public static function subs(string $s, int $start, ?int $end = null): string
    {
        if ($end === null) {
            return substr($s, $start);
        }
        return substr($s, $start, $end - $start);
    }

    public static function name(mixed $x): string
    {
        if (is_string($x)) return $x;
        if ($x instanceof Keyword) return $x->getName();
        if ($x instanceof Symbol) return $x->getName();
        return (string)$x;
    }

    public static function namespace_(mixed $x): ?string
    {
        if ($x instanceof Keyword) return $x->getNamespace();
        if ($x instanceof Symbol) return $x->getNamespace();
        return null;
    }

    // ============================================================
    // clojure.string functions
    // ============================================================

    /**
     * Returns a string of all elements in coll, separated by separator.
     * (str/join ", " ["a" "b" "c"]) => "a, b, c"
     */
    public static function str_join(string $separator, iterable $coll): string
    {
        $arr = is_array($coll) ? $coll : iterator_to_array($coll);
        return implode($separator, array_map(fn($x) => self::str($x), $arr));
    }

    /**
     * Replaces all instances of match with replacement in string s.
     * (str/replace "hello world" "o" "0") => "hell0 w0rld"
     */
    public static function str_replace(string $s, string $match, string $replacement): string
    {
        return str_replace($match, $replacement, $s);
    }

    /**
     * True if s starts with substr.
     * (str/starts-with? "hello" "he") => true
     */
    public static function str_starts_with_QMARK_(string $s, string $substr): bool
    {
        return str_starts_with($s, $substr);
    }

    /**
     * True if s ends with substr.
     * (str/ends-with? "hello" "lo") => true
     */
    public static function str_ends_with_QMARK_(string $s, string $substr): bool
    {
        return str_ends_with($s, $substr);
    }

    /**
     * True if s includes substr.
     * (str/includes? "hello" "ell") => true
     */
    public static function str_includes_QMARK_(string $s, string $substr): bool
    {
        return str_contains($s, $substr);
    }

    /**
     * Splits string on a regular expression or string.
     * (str/split "a,b,c" ",") => ["a" "b" "c"]
     */
    public static function str_split(string $s, string $re, ?int $limit = null): array
    {
        // If it looks like a regex (starts with /), use preg_split
        if (str_starts_with($re, '/')) {
            return $limit !== null
                ? preg_split($re, $s, $limit)
                : preg_split($re, $s);
        }
        // Otherwise treat as literal string
        return $limit !== null
            ? explode($re, $s, $limit)
            : explode($re, $s);
    }

    /**
     * Removes whitespace from both ends of string.
     * (str/trim "  hello  ") => "hello"
     */
    public static function str_trim(string $s): string
    {
        return trim($s);
    }

    /**
     * Removes whitespace from the left side of string.
     */
    public static function str_triml(string $s): string
    {
        return ltrim($s);
    }

    /**
     * Removes whitespace from the right side of string.
     */
    public static function str_trimr(string $s): string
    {
        return rtrim($s);
    }

    /**
     * Converts string to all upper-case.
     */
    public static function str_upper_case(string $s): string
    {
        return strtoupper($s);
    }

    /**
     * Converts string to all lower-case.
     */
    public static function str_lower_case(string $s): string
    {
        return strtolower($s);
    }

    /**
     * Capitalizes the first character of a string.
     */
    public static function str_capitalize(string $s): string
    {
        return ucfirst(strtolower($s));
    }

    /**
     * True if s is nil, empty, or contains only whitespace.
     */
    public static function str_blank_QMARK_(?string $s): bool
    {
        return $s === null || trim($s) === '';
    }

    /**
     * Reverses a string.
     */
    public static function str_reverse(string $s): string
    {
        return strrev($s);
    }

    // ============================================================
    // Comparison
    // ============================================================

    public static function _EQ_(mixed $x, mixed $y, mixed ...$more): bool
    {
        if ($x !== $y && !self::equals($x, $y)) return false;
        foreach ($more as $z) {
            if (!self::equals($y, $z)) return false;
            $y = $z;
        }
        return true;
    }

    public static function not_EQ_(mixed $x, mixed $y): bool
    {
        return !self::_EQ_($x, $y);
    }

    private static function equals(mixed $a, mixed $b): bool
    {
        if ($a === $b) return true;
        if (is_array($a) && is_array($b)) {
            if (count($a) !== count($b)) return false;
            foreach ($a as $k => $v) {
                if (!array_key_exists($k, $b)) return false;
                if (!self::equals($v, $b[$k])) return false;
            }
            return true;
        }
        if ($a instanceof Keyword && $b instanceof Keyword) {
            return $a->equals($b);
        }
        if ($a instanceof Symbol && $b instanceof Symbol) {
            return $a->equals($b);
        }
        return $a == $b;
    }

    public static function identical_QMARK_(mixed $x, mixed $y): bool
    {
        return $x === $y;
    }

    public static function compare(mixed $x, mixed $y): int
    {
        return $x <=> $y;
    }

    // ============================================================
    // Logic
    // ============================================================

    public static function not(mixed $x): bool
    {
        return $x === false || $x === null;
    }

    public static function and_(mixed ...$args): mixed
    {
        $result = true;
        foreach ($args as $arg) {
            if ($arg === false || $arg === null) return $arg;
            $result = $arg;
        }
        return $result;
    }

    public static function or_(mixed ...$args): mixed
    {
        foreach ($args as $arg) {
            if ($arg !== false && $arg !== null) return $arg;
        }
        return end($args) ?: null;
    }

    // ============================================================
    // Misc
    // ============================================================

    public static function identity(mixed $x): mixed
    {
        return $x;
    }

    public static function constantly(mixed $x): callable
    {
        return fn() => $x;
    }

    public static function comp(callable ...$fns): callable
    {
        if (count($fns) === 0) return fn($x) => $x;
        if (count($fns) === 1) return $fns[0];

        return function(...$args) use ($fns) {
            $fns = array_reverse($fns);
            $result = ($fns[0])(...$args);
            for ($i = 1; $i < count($fns); $i++) {
                $result = ($fns[$i])($result);
            }
            return $result;
        };
    }

    public static function partial(callable $f, mixed ...$args): callable
    {
        return fn(...$moreArgs) => $f(...$args, ...$moreArgs);
    }

    public static function apply(callable $f, mixed ...$args): mixed
    {
        if (count($args) === 0) return $f();

        $lastArg = array_pop($args);
        if (is_array($lastArg)) {
            return $f(...$args, ...$lastArg);
        }
        return $f(...$args, $lastArg);
    }

    public static function juxt(callable ...$fns): callable
    {
        return fn(...$args) => array_map(fn($f) => $f(...$args), $fns);
    }

    public static function complement(callable $f): callable
    {
        return fn(...$args) => !$f(...$args);
    }

    public static function fnil(callable $f, mixed ...$defaults): callable
    {
        return function(...$args) use ($f, $defaults) {
            for ($i = 0; $i < count($defaults) && $i < count($args); $i++) {
                if ($args[$i] === null) {
                    $args[$i] = $defaults[$i];
                }
            }
            return $f(...$args);
        };
    }

    public static function memoize(callable $f): callable
    {
        $cache = [];
        return function(...$args) use ($f, &$cache) {
            $key = serialize($args);
            if (!array_key_exists($key, $cache)) {
                $cache[$key] = $f(...$args);
            }
            return $cache[$key];
        };
    }

    public static function iterate(callable $f, mixed $x): \Generator
    {
        while (true) {
            yield $x;
            $x = $f($x);
        }
    }

    public static function repeatedly(callable $f, ?int $n = null): array|\Generator
    {
        if ($n !== null) {
            $result = [];
            for ($i = 0; $i < $n; $i++) {
                $result[] = $f();
            }
            return $result;
        }
        return (function() use ($f) {
            while (true) {
                yield $f();
            }
        })();
    }

    public static function repeat(mixed $x, ?int $n = null): array|\Generator
    {
        if ($n !== null) {
            return array_fill(0, $n, $x);
        }
        return (function() use ($x) {
            while (true) {
                yield $x;
            }
        })();
    }

    public static function range(int $start = 0, ?int $end = null, int $step = 1): array
    {
        if ($end === null) {
            // (range n) => 0 to n-1
            return range(0, $start - 1);
        }
        return range($start, $end - 1, $step);
    }

    public static function vec(mixed $coll): array
    {
        if ($coll === null) return [];
        return array_values(is_array($coll) ? $coll : iterator_to_array($coll));
    }

    public static function vector(mixed ...$args): array
    {
        return $args;
    }

    public static function hash_map(mixed ...$kvs): array
    {
        $result = [];
        for ($i = 0; $i < count($kvs); $i += 2) {
            $k = $kvs[$i];
            if ($k instanceof Keyword) $k = $k->getName();
            $result[$k] = $kvs[$i + 1] ?? null;
        }
        return $result;
    }

    public static function sorted_map(mixed ...$kvs): array
    {
        $result = self::hash_map(...$kvs);
        ksort($result);
        return $result;
    }

    public static function keyword(string $name, ?string $ns = null): Keyword
    {
        if ($ns !== null) {
            return Keyword::createForNamespace($ns, $name);
        }
        return Keyword::create($name);
    }

    public static function symbol(string $name, ?string $ns = null): Symbol
    {
        if ($ns !== null) {
            return Symbol::createForNamespace($ns, $name);
        }
        return Symbol::create($name);
    }

    public static function gensym(?string $prefix = null): Symbol
    {
        static $counter = 0;
        $counter++;
        return Symbol::create(($prefix ?? 'G__') . $counter);
    }

    public static function pr_str(mixed $x): string
    {
        if ($x === null) return 'nil';
        if ($x === true) return 'true';
        if ($x === false) return 'false';
        if (is_string($x)) return '"' . addslashes($x) . '"';
        if (is_int($x) || is_float($x)) return (string)$x;
        if ($x instanceof Keyword) return ':' . ($x->getNamespace() ? $x->getNamespace() . '/' : '') . $x->getName();
        if ($x instanceof Symbol) return ($x->getNamespace() ? $x->getNamespace() . '/' : '') . $x->getName();
        if (is_array($x)) {
            if (self::map_QMARK_($x)) {
                $pairs = [];
                foreach ($x as $k => $v) {
                    $pairs[] = self::pr_str($k) . ' ' . self::pr_str($v);
                }
                return '{' . implode(', ', $pairs) . '}';
            }
            return '[' . implode(' ', array_map([self::class, 'pr_str'], $x)) . ']';
        }
        return var_export($x, true);
    }

    public static function println(mixed ...$args): void
    {
        echo implode(' ', array_map([self::class, 'pr_str'], $args)) . "\n";
    }

    public static function print_(mixed ...$args): void
    {
        echo implode(' ', array_map([self::class, 'pr_str'], $args));
    }

    public static function prn(mixed ...$args): void
    {
        echo implode(' ', array_map([self::class, 'pr_str'], $args)) . "\n";
    }

    public static function type(mixed $x): string
    {
        if ($x === null) return 'nil';
        if (is_bool($x)) return 'boolean';
        if (is_int($x)) return 'integer';
        if (is_float($x)) return 'float';
        if (is_string($x)) return 'string';
        if (is_array($x)) return 'array';
        if ($x instanceof Keyword) return 'clojure.lang.Keyword';
        if ($x instanceof Symbol) return 'clojure.lang.Symbol';
        if (is_object($x)) return get_class($x);
        return gettype($x);
    }

    public static function class_(mixed $x): ?string
    {
        if (is_object($x)) return get_class($x);
        return null;
    }
}
