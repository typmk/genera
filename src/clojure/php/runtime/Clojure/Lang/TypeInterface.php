<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * @template TSelf
 */
interface TypeInterface extends MetaInterface, SourceLocationInterface, EqualsInterface, HashableInterface
{
}
