<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Clojure\Lang\Collections\HashSet\PersistentHashSetInterface;
use Clojure\Lang\Collections\LinkedList\PersistentListInterface;
use Clojure\Lang\Collections\Map\PersistentMapInterface;
use Clojure\Lang\Collections\Vector\PersistentVectorInterface;

/**
 * Printer for ClojurePHP data structures.
 *
 * Provides EDN-compatible string representations of values.
 */
final class Printer
{
    private static ?self $readableInstance = null;
    private static ?self $nonReadableInstance = null;

    private function __construct(
        private readonly bool $isReadable,
    ) {}

    public static function readable(): self
    {
        return self::$readableInstance ??= new self(true);
    }

    public static function nonReadable(): self
    {
        return self::$nonReadableInstance ??= new self(false);
    }

    public function print(mixed $value): string
    {
        return $this->printValue($value);
    }

    private function printValue(mixed $value): string
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
            return (string) $value;
        }

        if (is_string($value)) {
            return $this->isReadable
                ? $this->printReadableString($value)
                : $value;
        }

        if ($value instanceof Keyword) {
            return $this->printKeyword($value);
        }

        if ($value instanceof Symbol) {
            return $this->printSymbol($value);
        }

        if ($value instanceof PersistentVectorInterface) {
            return $this->printVector($value);
        }

        if ($value instanceof PersistentListInterface) {
            return $this->printList($value);
        }

        if ($value instanceof PersistentMapInterface) {
            return $this->printMap($value);
        }

        if ($value instanceof PersistentHashSetInterface) {
            return $this->printSet($value);
        }

        if (is_array($value)) {
            return $this->printPhpArray($value);
        }

        if (is_object($value)) {
            return $this->printObject($value);
        }

        return (string) $value;
    }

    private function printReadableString(string $value): string
    {
        $escaped = str_replace(
            ['\\', '"', "\n", "\r", "\t"],
            ['\\\\', '\\"', '\\n', '\\r', '\\t'],
            $value
        );
        return '"' . $escaped . '"';
    }

    private function printKeyword(Keyword $keyword): string
    {
        $ns = $keyword->getNamespace();
        if ($ns !== null && $ns !== '') {
            return ':' . $ns . '/' . $keyword->getName();
        }
        return ':' . $keyword->getName();
    }

    private function printSymbol(Symbol $symbol): string
    {
        $ns = $symbol->getNamespace();
        if ($ns !== null && $ns !== '') {
            return $ns . '/' . $symbol->getName();
        }
        return $symbol->getName();
    }

    private function printVector(PersistentVectorInterface $vector): string
    {
        $items = [];
        foreach ($vector as $item) {
            $items[] = $this->printValue($item);
        }
        return '[' . implode(' ', $items) . ']';
    }

    private function printList(PersistentListInterface $list): string
    {
        $items = [];
        foreach ($list as $item) {
            $items[] = $this->printValue($item);
        }
        return '(' . implode(' ', $items) . ')';
    }

    private function printMap(PersistentMapInterface $map): string
    {
        $items = [];
        foreach ($map as $key => $value) {
            $items[] = $this->printValue($key) . ' ' . $this->printValue($value);
        }
        return '{' . implode(', ' , $items) . '}';
    }

    private function printSet(PersistentHashSetInterface $set): string
    {
        $items = [];
        foreach ($set->toPhpArray() as $item) {
            $items[] = $this->printValue($item);
        }
        return '#{' . implode(' ', $items) . '}';
    }

    private function printPhpArray(array $array): string
    {
        // Check if it's a sequential array (list)
        if (array_is_list($array)) {
            $items = [];
            foreach ($array as $item) {
                $items[] = $this->printValue($item);
            }
            return '(php-array [' . implode(' ', $items) . '])';
        }

        // Associative array
        $items = [];
        foreach ($array as $key => $value) {
            $items[] = $this->printValue($key) . ' ' . $this->printValue($value);
        }
        return '(php-array {' . implode(', ', $items) . '})';
    }

    private function printObject(object $object): string
    {
        $class = get_class($object);
        if (method_exists($object, '__toString')) {
            return '#<' . $class . ' ' . $object->__toString() . '>';
        }
        return '#<' . $class . '>';
    }
}
