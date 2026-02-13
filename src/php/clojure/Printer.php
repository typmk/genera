<?php

declare(strict_types=1);

/**
 * Printer for ClojurePHP data structures.
 *
 * Provides EDN-compatible string representations of values.
 */

namespace Clojure\Php;

/**
 * Print a value to an EDN-readable string.
 */
function prStr(mixed ...$xs): string
{
    if (count($xs) === 0) return '';
    $parts = [];
    foreach ($xs as $x) {
        $parts[] = printValue($x, true);
    }
    return implode(' ', $parts);
}

// str_ is defined in RT.php

/**
 * Print to stdout with a newline.
 */
function println_(mixed ...$xs): void
{
    $parts = [];
    foreach ($xs as $x) {
        $parts[] = printValue($x, false);
    }
    echo implode(' ', $parts) . "\n";
}

/**
 * Print to stdout (no newline).
 */
function print_(mixed ...$xs): void
{
    $parts = [];
    foreach ($xs as $x) {
        $parts[] = printValue($x, false);
    }
    echo implode(' ', $parts);
}

/**
 * Print readably to stdout.
 */
function prn(mixed ...$xs): void
{
    echo prStr(...$xs) . "\n";
}

/**
 * Print readably to stdout (no newline).
 */
function pr(mixed ...$xs): void
{
    echo prStr(...$xs);
}

/**
 * Print a single value.
 */
function printValue(mixed $value, bool $readable = true): string
{
    if ($value === null) {
        return 'nil';
    }

    if ($value === true) {
        return 'true';
    }

    if ($value === false) {
        return 'false';
    }

    if (is_int($value) || is_float($value)) {
        if (is_nan($value)) return '##NaN';
        if (is_infinite($value)) return $value > 0 ? '##Inf' : '##-Inf';
        return (string) $value;
    }

    if (is_string($value)) {
        return $readable ? printReadableString($value) : $value;
    }

    if ($value instanceof Kw) {
        return printKeyword($value);
    }

    if ($value instanceof Sym) {
        return printSymbol($value);
    }

    if ($value instanceof Vec) {
        return printVector($value, $readable);
    }

    if ($value instanceof PList || $value instanceof EmptyList) {
        return printList($value, $readable);
    }

    if ($value instanceof ISeq) {
        return printSeq($value, $readable);
    }

    if ($value instanceof Map) {
        return printMap($value, $readable);
    }

    if ($value instanceof Set) {
        return printSet($value, $readable);
    }

    if ($value instanceof CljVar) {
        return (string) $value;
    }

    if ($value instanceof Atom) {
        return (string) $value;
    }

    if ($value instanceof Reduced) {
        return '#<Reduced: ' . printValue($value->deref(), $readable) . '>';
    }

    if (is_array($value)) {
        return printPhpArray($value, $readable);
    }

    if (is_callable($value) && !is_object($value)) {
        return '#<fn>';
    }

    if (is_object($value)) {
        return printObject($value, $readable);
    }

    return (string) $value;
}

function printReadableString(string $value): string
{
    $escaped = str_replace(
        ['\\', '"', "\n", "\r", "\t"],
        ['\\\\', '\\"', '\\n', '\\r', '\\t'],
        $value
    );
    return '"' . $escaped . '"';
}

function printKeyword(Kw $kw): string
{
    $ns = $kw->ns();
    if ($ns !== null && $ns !== '') {
        return ':' . $ns . '/' . $kw->name();
    }
    return ':' . $kw->name();
}

function printSymbol(Sym $sym): string
{
    $ns = $sym->ns();
    if ($ns !== null && $ns !== '') {
        return $ns . '/' . $sym->name();
    }
    return $sym->name();
}

function printVector(Vec $vec, bool $readable): string
{
    $items = [];
    foreach ($vec as $item) {
        $items[] = printValue($item, $readable);
    }
    return '[' . implode(' ', $items) . ']';
}

function printList(ISeq $list, bool $readable): string
{
    $items = [];
    $s = seq($list);
    while ($s !== null) {
        $items[] = printValue(first($s), $readable);
        $s = $s->next();
    }
    return '(' . implode(' ', $items) . ')';
}

function printSeq(ISeq $seq, bool $readable): string
{
    $items = [];
    $s = $seq->seq();
    while ($s !== null) {
        $items[] = printValue($s->first(), $readable);
        $s = $s->next();
    }
    return '(' . implode(' ', $items) . ')';
}

function printMap(Map $map, bool $readable): string
{
    $items = [];
    foreach ($map as $key => $value) {
        $items[] = printValue($key, $readable) . ' ' . printValue($value, $readable);
    }
    return '{' . implode(', ', $items) . '}';
}

function printSet(Set $set, bool $readable): string
{
    $items = [];
    foreach ($set as $item) {
        $items[] = printValue($item, $readable);
    }
    return '#{' . implode(' ', $items) . '}';
}

function printPhpArray(array $array, bool $readable): string
{
    if (array_is_list($array)) {
        $items = [];
        foreach ($array as $item) {
            $items[] = printValue($item, $readable);
        }
        return '#php/array [' . implode(' ', $items) . ']';
    }

    $items = [];
    foreach ($array as $key => $value) {
        $items[] = printValue($key, $readable) . ' ' . printValue($value, $readable);
    }
    return '#php/array {' . implode(', ', $items) . '}';
}

function printObject(object $object, bool $readable): string
{
    $class = get_class($object);
    if (method_exists($object, '__toString')) {
        return (string) $object;
    }
    return '#<' . $class . '@' . spl_object_id($object) . '>';
}
