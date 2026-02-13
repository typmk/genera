<?php

declare(strict_types=1);

/**
 * Metadata functions for Clojure objects.
 */

namespace Clojure\Php;

/**
 * Interface for objects that support metadata.
 */
interface IMeta
{
    public function meta(): ?Map;
    public function withMeta(?Map $meta): static;
}

// ============================================================
// Metadata functions
// ============================================================

/**
 * Get the metadata of an object.
 */
function meta(mixed $x): ?Map
{
    if ($x instanceof IMeta) {
        return $x->meta();
    }
    if ($x instanceof CljVar) {
        return $x->meta();
    }
    if ($x instanceof Atom) {
        return $x->meta();
    }
    if ($x instanceof Ns) {
        return $x->meta();
    }
    return null;
}

/**
 * Return an object of the same type with the given metadata.
 */
function withMeta(mixed $x, ?Map $meta): mixed
{
    if ($x instanceof IMeta) {
        return $x->withMeta($meta);
    }
    throw new \InvalidArgumentException(
        "withMeta not supported on " . (is_object($x) ? get_class($x) : gettype($x))
    );
}

/**
 * Return an object with metadata that is the result of (apply f meta args).
 */
function varyMeta(mixed $x, callable $f, mixed ...$args): mixed
{
    $m = meta($x) ?? hashMap();
    return withMeta($x, $f($m, ...$args));
}

/**
 * Reset the metadata of a mutable reference (Atom, Var, Ns).
 */
function resetMeta(mixed $ref, Map $meta): Map
{
    if ($ref instanceof Atom) {
        return $ref->resetMeta($meta);
    }
    if ($ref instanceof CljVar) {
        $ref->setMeta($meta);
        return $meta;
    }
    if ($ref instanceof Ns) {
        return $ref->resetMeta($meta);
    }
    throw new \InvalidArgumentException(
        "resetMeta not supported on " . (is_object($ref) ? get_class($ref) : gettype($ref))
    );
}

/**
 * Alter the metadata of a mutable reference.
 */
function alterMeta(mixed $ref, callable $f, mixed ...$args): Map
{
    if ($ref instanceof Atom) {
        return $ref->alterMeta($f, ...$args);
    }
    if ($ref instanceof CljVar) {
        return $ref->alterMeta($f, ...$args);
    }
    if ($ref instanceof Ns) {
        return $ref->alterMeta($f, ...$args);
    }
    throw new \InvalidArgumentException(
        "alterMeta not supported on " . (is_object($ref) ? get_class($ref) : gettype($ref))
    );
}
