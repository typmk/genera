<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * @template-implements MetaInterface<static>
 */
abstract class AbstractFn implements FnInterface, MetaInterface
{
    use MetaTrait;
}
