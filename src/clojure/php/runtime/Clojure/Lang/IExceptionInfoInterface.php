<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Clojure\Lang\Collections\Map\PersistentMapInterface;

/**
 * Interface for exceptions that carry data (a map) as additional payload.
 *
 * ClojurePHP programs that need richer semantics for exceptions should use
 * ExceptionInfo in lieu of defining project-specific exception classes.
 */
interface IExceptionInfoInterface
{
    /**
     * Returns the data map associated with this exception.
     */
    public function getData(): PersistentMapInterface;
}
