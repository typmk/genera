<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Interface for invokable types.
 * Mirrors clojure.lang.IFn.
 *
 * In Clojure, IFn has invoke methods for 0-20+ args.
 * In PHP, we use __invoke with variadic args.
 */
interface IFn
{
    /**
     * Invoke this function with the given arguments.
     */
    public function __invoke(mixed ...$args): mixed;
}
