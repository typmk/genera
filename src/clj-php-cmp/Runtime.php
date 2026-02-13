<?php

namespace Cljp;

use Clojure\Lang\Hasher;
use Clojure\Lang\Equalizer;
use Clojure\Lang\Printer;
use Clojure\Lang\Collections\Vector\PersistentVector;
use Clojure\Lang\Collections\Vector\PersistentVectorInterface;
use Clojure\Lang\Collections\Map\PersistentHashMap;
use Clojure\Lang\Collections\Map\PersistentMapInterface;
use Clojure\Lang\Collections\HashSet\PersistentHashSet;
use Clojure\Lang\Collections\HashSet\PersistentHashSetInterface;
use Clojure\Lang\Collections\HashSet\TransientHashSet;
use Clojure\Lang\Collections\LinkedList\PersistentList;
use Clojure\Lang\Collections\LinkedList\PersistentListInterface;
use Clojure\Lang\Keyword;
use Clojure\Lang\ExceptionInfo;
use Clojure\Lang\IExceptionInfoInterface;

class Runtime {
    private static ?Hasher $hasher = null;
    private static ?Equalizer $equalizer = null;

    // Protocol registry: protocol-name => [method-names]
    private static array $protocols = [];
    // Protocol implementations: protocol-name => type => method-name => callable
    private static array $protocolImpls = [];
    // Multimethod dispatch functions: multi-name => callable
    private static array $multis = [];
    // Multimethod implementations: multi-name => dispatch-value => callable
    private static array $multiMethods = [];

    public static function getHasher(): Hasher {
        if (self::$hasher === null) {
            self::$hasher = new Hasher();
        }
        return self::$hasher;
    }

    public static function getEqualizer(): Equalizer {
        if (self::$equalizer === null) {
            self::$equalizer = new Equalizer();
        }
        return self::$equalizer;
    }

    public static function vector(array $data): PersistentVectorInterface {
        return PersistentVector::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $data
        );
    }

    public static function map(array $data): PersistentMapInterface {
        return PersistentHashMap::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $data
        );
    }

    public static function set(array $data): PersistentHashSetInterface {
        $hasher = self::getHasher();
        $equalizer = self::getEqualizer();
        $map = PersistentHashMap::empty($hasher, $equalizer)->asTransient();
        $set = new TransientHashSet($hasher, $map);
        foreach ($data as $item) {
            $set->add($item);
        }
        return $set->persistent();
    }

    public static function list(array $data): PersistentListInterface {
        return PersistentList::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $data
        );
    }

    public static function keyword(string $name): Keyword {
        if ($name[0] === ':') {
            $name = substr($name, 1);
        }
        return Keyword::create($name);
    }

    public static function conj($coll, $x) {
        if ($coll instanceof PersistentVectorInterface) {
            return $coll->append($x);
        }
        if ($coll instanceof PersistentMapInterface) {
            // Assume $x is [k, v] or map?
            // Clojure conj on map expects map or entry.
            // PersistentMap put expects key, value.
            // For simplicitly, let's assume $x is a vector of two elements [k, v]
            if ($x instanceof PersistentVectorInterface && count($x) === 2) {
                return $coll->put($x->get(0), $x->get(1));
            }
        }
        if ($coll instanceof PersistentHashSetInterface) {
            return $coll->add($x);
        }
        if ($coll instanceof PersistentListInterface) {
            return $coll->cons($x);
        }
        return $coll;
    }

    public static function cons($x, $coll) {
        // Use the lazy Cons class to avoid forcing realization
        return new \Clojure\Lang\Cons($x, $coll);
    }

    public static function str($x): string {
        if ($x === null) {
            return '';
        }
        if (is_string($x)) {
            return $x;
        }
        if (is_bool($x)) {
            return $x ? 'true' : 'false';
        }
        if (is_numeric($x)) {
            return (string)$x;
        }
        if ($x instanceof Keyword) {
            return ':' . $x->getName();
        }
        if ($x instanceof \Stringable) {
            return (string)$x;
        }
        if (is_array($x) || $x instanceof \Traversable) {
            $items = [];
            foreach ($x as $item) {
                $items[] = self::str($item);
            }
            return '[' . implode(' ', $items) . ']';
        }
        return (string)$x;
    }

    public static function toArray($coll): array {
        if ($coll === null) {
            return [];
        }
        if (is_array($coll)) {
            return $coll;
        }
        if ($coll instanceof \Traversable) {
            return iterator_to_array($coll, false);
        }
        throw new \Exception("Cannot convert to array: " . gettype($coll));
    }

    public static function equals($a, $b): bool {
        return self::getEqualizer()->equals($a, $b);
    }

    public static function assoc($map, $key, $val) {
        if ($map instanceof PersistentMapInterface) {
            return $map->put($key, $val);
        }
        if ($map instanceof PersistentVectorInterface) {
            return $map->update($key, $val);
        }
        throw new \Exception("assoc requires a map or vector");
    }

    public static function get($coll, $key) {
        if ($coll === null) {
            return null;
        }
        if ($coll instanceof PersistentMapInterface) {
            return $coll->find($key);
        }
        if ($coll instanceof PersistentVectorInterface) {
            return $coll->get($key);
        }
        if (is_array($coll)) {
            return $coll[$key] ?? null;
        }
        return null;
    }

    public static function hashMap(array $kvs): PersistentMapInterface {
        return PersistentHashMap::fromArray(
            self::getHasher(),
            self::getEqualizer(),
            $kvs
        );
    }

    public static function keys($m): ?PersistentVectorInterface {
        if ($m === null) {
            return null;
        }
        if ($m instanceof PersistentMapInterface) {
            $result = [];
            foreach ($m as $k => $v) {
                $result[] = $k;
            }
            return self::vector($result);
        }
        if (is_array($m)) {
            return self::vector(array_keys($m));
        }
        throw new \Exception("keys requires a map");
    }

    public static function vals($m): ?PersistentVectorInterface {
        if ($m === null) {
            return null;
        }
        if ($m instanceof PersistentMapInterface) {
            $result = [];
            foreach ($m as $k => $v) {
                $result[] = $v;
            }
            return self::vector($result);
        }
        if (is_array($m)) {
            return self::vector(array_values($m));
        }
        throw new \Exception("vals requires a map");
    }

    public static function contains($coll, $key): bool {
        if ($coll === null) {
            return false;
        }
        if ($coll instanceof PersistentMapInterface) {
            return $coll->find($key) !== null;
        }
        if ($coll instanceof PersistentHashSetInterface) {
            return $coll->contains($key);
        }
        if ($coll instanceof PersistentVectorInterface) {
            return $key >= 0 && $key < count($coll);
        }
        if (is_array($coll)) {
            return array_key_exists($key, $coll);
        }
        return false;
    }

    public static function dissoc($m, $key) {
        if ($m === null) {
            return null;
        }
        if ($m instanceof PersistentMapInterface) {
            return $m->remove($key);
        }
        throw new \Exception("dissoc requires a map");
    }

    /**
     * Returns a readable string representation of a value (EDN format).
     */
    public static function prStr(mixed $x): string {
        return Printer::readable()->print($x);
    }

    /**
     * Returns a non-readable string representation (for display).
     */
    public static function printStr(mixed $x): string {
        return Printer::nonReadable()->print($x);
    }

    // ============================================================
    // Exception Functions (ex-info, ex-data, ex-message, ex-cause)
    // ============================================================

    /**
     * Create an ExceptionInfo with message, data map, and optional cause.
     * (ex-info msg map) or (ex-info msg map cause)
     */
    public static function exInfo(string $msg, ?PersistentMapInterface $data = null, ?\Throwable $cause = null): ExceptionInfo {
        return new ExceptionInfo($msg, $data, $cause);
    }

    /**
     * Returns the data map from an ExceptionInfo, or nil for other exceptions.
     * (ex-data ex)
     */
    public static function exData(?\Throwable $ex): ?PersistentMapInterface {
        if ($ex instanceof IExceptionInfoInterface) {
            return $ex->getData();
        }
        return null;
    }

    /**
     * Returns the message from an exception.
     * (ex-message ex)
     */
    public static function exMessage(?\Throwable $ex): ?string {
        if ($ex === null) {
            return null;
        }
        return $ex->getMessage();
    }

    /**
     * Returns the cause (previous exception) from an exception.
     * (ex-cause ex)
     */
    public static function exCause(?\Throwable $ex): ?\Throwable {
        if ($ex === null) {
            return null;
        }
        return $ex->getPrevious();
    }

    // ============================================================
    // Reduced - for early termination in reduce
    // ============================================================

    /**
     * Wraps a value to signal early termination from reduce.
     */
    public static function reduced(mixed $val): Reduced {
        return new Reduced($val);
    }

    /**
     * Returns true if x is a Reduced value.
     */
    public static function isReduced(mixed $x): bool {
        return $x instanceof Reduced;
    }

    /**
     * Unwraps a Reduced value, returning its inner value.
     */
    public static function unreduced(mixed $x): mixed {
        if ($x instanceof Reduced) {
            return $x->deref();
        }
        return $x;
    }

    // ============================================================
    // Protocols - type-based polymorphism
    // ============================================================

    /**
     * Register a protocol with its method names.
     */
    public static function defProtocol(string $name, array $methods): void {
        self::$protocols[$name] = $methods;
        if (!isset(self::$protocolImpls[$name])) {
            self::$protocolImpls[$name] = [];
        }
    }

    /**
     * Extend a protocol for a specific type.
     * $impls is a flat array [method-name, callable, method-name, callable, ...].
     */
    public static function extendType(string $protocol, string $type, array $impls): void {
        if (!isset(self::$protocolImpls[$protocol])) {
            self::$protocolImpls[$protocol] = [];
        }
        // Convert flat array to associative: [k1, v1, k2, v2] -> [k1 => v1, k2 => v2]
        $methods = [];
        for ($i = 0; $i < count($impls); $i += 2) {
            $methods[$impls[$i]] = $impls[$i + 1];
        }
        self::$protocolImpls[$protocol][$type] = $methods;
    }

    /**
     * Get the type name for protocol dispatch.
     */
    public static function getType(mixed $x): string {
        if ($x === null) {
            return 'nil';
        }
        if (is_object($x)) {
            return get_class($x);
        }
        return gettype($x);
    }

    /**
     * Dispatch a protocol method call.
     */
    public static function protocolDispatch(string $protocol, string $method, mixed $target, mixed $args = []): mixed {
        $type = self::getType($target);

        // Convert args to array if needed
        $argsArray = self::toArray($args);

        // Check if object has this method directly (for reify, deftype, etc.)
        // Also check __call for dynamic methods (reify uses __call)
        if (is_object($target)) {
            if (method_exists($target, $method)) {
                return call_user_func_array([$target, $method], $argsArray);
            }
            // Try calling via __call (for reify objects)
            if (method_exists($target, '__call')) {
                try {
                    return $target->$method(...$argsArray);
                } catch (\Exception $e) {
                    // Method not found in __call, continue to registry lookup
                }
            }
        }

        // Check for direct implementation in protocol registry
        if (isset(self::$protocolImpls[$protocol][$type][$method])) {
            return call_user_func_array(
                self::$protocolImpls[$protocol][$type][$method],
                array_merge([$target], $argsArray)
            );
        }

        // Check for default implementation
        if (isset(self::$protocolImpls[$protocol]['default'][$method])) {
            return call_user_func_array(
                self::$protocolImpls[$protocol]['default'][$method],
                array_merge([$target], $argsArray)
            );
        }

        throw new \Exception("No implementation of protocol '$protocol' method '$method' for type '$type'");
    }

    /**
     * Check if a type satisfies a protocol.
     */
    public static function satisfies(string $protocol, mixed $x): bool {
        $type = self::getType($x);
        return isset(self::$protocolImpls[$protocol][$type]) ||
               isset(self::$protocolImpls[$protocol]['default']);
    }

    // ============================================================
    // Multimethods - dispatch on arbitrary function of arguments
    // ============================================================

    /**
     * Define a multimethod with a dispatch function.
     */
    public static function defMulti(string $name, callable $dispatchFn): void {
        self::$multis[$name] = $dispatchFn;
        if (!isset(self::$multiMethods[$name])) {
            self::$multiMethods[$name] = [];
        }
    }

    /**
     * Add a method implementation for a dispatch value.
     */
    public static function addMethod(string $name, mixed $dispatchVal, callable $fn): void {
        // Convert dispatch value to string key
        $key = self::dispatchKey($dispatchVal);
        self::$multiMethods[$name][$key] = $fn;
    }

    /**
     * Convert a dispatch value to a string key.
     */
    private static function dispatchKey(mixed $val): string {
        if ($val === null) {
            return ':nil:';
        }
        if (is_string($val)) {
            return $val;
        }
        if ($val instanceof Keyword) {
            return ':' . $val->getName();
        }
        if (is_int($val) || is_float($val)) {
            return (string)$val;
        }
        if (is_bool($val)) {
            return $val ? ':true:' : ':false:';
        }
        // For complex values, use serialize
        return md5(serialize($val));
    }

    /**
     * Dispatch a multimethod call.
     */
    public static function multimethodDispatch(string $name, array $args): mixed {
        if (!isset(self::$multis[$name])) {
            throw new \Exception("No multimethod defined: $name");
        }

        // Get dispatch value
        $dispatchVal = call_user_func_array(self::$multis[$name], $args);
        $key = self::dispatchKey($dispatchVal);

        // Find implementation
        if (isset(self::$multiMethods[$name][$key])) {
            return call_user_func_array(self::$multiMethods[$name][$key], $args);
        }

        // Check for default (keyword :default becomes ':default')
        if (isset(self::$multiMethods[$name][':default'])) {
            return call_user_func_array(self::$multiMethods[$name][':default'], $args);
        }

        throw new \Exception("No method in multimethod '$name' for dispatch value: " . self::prStr($dispatchVal));
    }

    // ============================================================
    // Reify - anonymous objects implementing protocols
    // ============================================================

    /**
     * Create an anonymous object with method implementations.
     * $methods is a flat array [method-name, callable, method-name, callable, ...].
     */
    public static function reify(array $methods): object {
        // Convert flat array to associative: [k1, v1, k2, v2] -> [k1 => v1, k2 => v2]
        $methodMap = [];
        for ($i = 0; $i < count($methods); $i += 2) {
            $methodMap[$methods[$i]] = $methods[$i + 1];
        }

        return new class($methodMap) {
            private array $methods;

            public function __construct(array $methods) {
                $this->methods = $methods;
            }

            public function __call(string $name, array $args): mixed {
                if (isset($this->methods[$name])) {
                    // Pass $this as first argument
                    return call_user_func_array(
                        $this->methods[$name],
                        array_merge([$this], $args)
                    );
                }
                throw new \Exception("Method not found: $name");
            }
        };
    }

    // ============================================================
    // Types - deftype/defrecord
    // ============================================================

    // Type registry: type-name => {fields: [...], mutable: [...], methods: {...}}
    private static array $types = [];
    // Record registry: record-name => {fields: [...], methods: {...}}
    private static array $records = [];

    /**
     * Define a type (deftype).
     */
    public static function defType(string $name, array $fields, array $mutableFields, array $methods): void {
        // Convert flat methods array to associative
        $methodMap = [];
        for ($i = 0; $i < count($methods); $i += 2) {
            $methodMap[$methods[$i]] = $methods[$i + 1];
        }
        self::$types[$name] = [
            'fields' => $fields,
            'mutable' => $mutableFields,
            'methods' => $methodMap
        ];
    }

    /**
     * Create an instance of a type.
     */
    public static function createType(string $name, ...$args): object {
        if (!isset(self::$types[$name])) {
            throw new \Exception("Type not defined: $name");
        }
        $typeDef = self::$types[$name];
        $fields = $typeDef['fields'];
        $mutableFields = $typeDef['mutable'];
        $methods = $typeDef['methods'];

        // Create field values map
        $fieldValues = [];
        foreach ($fields as $i => $field) {
            $fieldValues[$field] = $args[$i] ?? null;
        }

        return new class($name, $fieldValues, $mutableFields, $methods) {
            private string $typeName;
            private array $fields;
            private array $mutableFields;
            private array $methods;

            public function __construct(string $typeName, array $fields, array $mutableFields, array $methods) {
                $this->typeName = $typeName;
                $this->fields = $fields;
                $this->mutableFields = $mutableFields;
                $this->methods = $methods;
            }

            public function __get(string $name): mixed {
                if (array_key_exists($name, $this->fields)) {
                    return $this->fields[$name];
                }
                throw new \Exception("Field not found: $name");
            }

            public function __set(string $name, mixed $value): void {
                if (in_array($name, $this->mutableFields)) {
                    $this->fields[$name] = $value;
                } else {
                    throw new \Exception("Field is not mutable: $name");
                }
            }

            public function __call(string $name, array $args): mixed {
                if (isset($this->methods[$name])) {
                    return call_user_func_array(
                        $this->methods[$name],
                        array_merge([$this], $args)
                    );
                }
                throw new \Exception("Method not found: $name");
            }

            public function getTypeName(): string {
                return $this->typeName;
            }
        };
    }

    /**
     * Define a record (defrecord).
     */
    public static function defRecord(string $name, array $fields, array $methods): void {
        // Convert flat methods array to associative
        $methodMap = [];
        for ($i = 0; $i < count($methods); $i += 2) {
            $methodMap[$methods[$i]] = $methods[$i + 1];
        }
        self::$records[$name] = [
            'fields' => $fields,
            'methods' => $methodMap
        ];
    }

    /**
     * Create an instance of a record from positional args.
     */
    public static function createRecord(string $name, ...$args): object {
        if (!isset(self::$records[$name])) {
            throw new \Exception("Record not defined: $name");
        }
        $recordDef = self::$records[$name];
        $fields = $recordDef['fields'];
        $methods = $recordDef['methods'];

        // Create field values map
        $fieldValues = [];
        foreach ($fields as $i => $field) {
            $fieldValues[$field] = $args[$i] ?? null;
        }

        return self::makeRecordInstance($name, $fieldValues, $fields, $methods);
    }

    /**
     * Create an instance of a record from a map.
     */
    public static function createRecordFromMap(string $name, $map): object {
        if (!isset(self::$records[$name])) {
            throw new \Exception("Record not defined: $name");
        }
        $recordDef = self::$records[$name];
        $fields = $recordDef['fields'];
        $methods = $recordDef['methods'];

        // Extract field values from map
        $fieldValues = [];
        foreach ($fields as $field) {
            $key = Keyword::create($field);
            $fieldValues[$field] = self::get($map, $key);
        }

        return self::makeRecordInstance($name, $fieldValues, $fields, $methods);
    }

    /**
     * Create a record instance with map-like behavior.
     */
    private static function makeRecordInstance(string $name, array $fieldValues, array $fields, array $methods): object {
        return new class($name, $fieldValues, $fields, $methods) implements \ArrayAccess {
            private string $recordName;
            private array $fieldValues;
            private array $fieldNames;
            private array $methods;

            public function __construct(string $recordName, array $fieldValues, array $fieldNames, array $methods) {
                $this->recordName = $recordName;
                $this->fieldValues = $fieldValues;
                $this->fieldNames = $fieldNames;
                $this->methods = $methods;
            }

            // Field access via ->field
            public function __get(string $name): mixed {
                if (array_key_exists($name, $this->fieldValues)) {
                    return $this->fieldValues[$name];
                }
                throw new \Exception("Field not found: $name");
            }

            // Method calls
            public function __call(string $name, array $args): mixed {
                if (isset($this->methods[$name])) {
                    return call_user_func_array(
                        $this->methods[$name],
                        array_merge([$this], $args)
                    );
                }
                throw new \Exception("Method not found: $name");
            }

            // ArrayAccess for map-like behavior (keyword access)
            public function offsetExists(mixed $offset): bool {
                $key = $offset instanceof Keyword ? $offset->getName() : (string)$offset;
                return array_key_exists($key, $this->fieldValues);
            }

            public function offsetGet(mixed $offset): mixed {
                $key = $offset instanceof Keyword ? $offset->getName() : (string)$offset;
                return $this->fieldValues[$key] ?? null;
            }

            public function offsetSet(mixed $offset, mixed $value): void {
                throw new \Exception("Records are immutable");
            }

            public function offsetUnset(mixed $offset): void {
                throw new \Exception("Records are immutable");
            }

            public function getRecordName(): string {
                return $this->recordName;
            }

            // Return as map for assoc/dissoc operations
            public function toMap(): PersistentMapInterface {
                $pairs = [];
                foreach ($this->fieldValues as $k => $v) {
                    $pairs[] = Keyword::create($k);
                    $pairs[] = $v;
                }
                return Runtime::hashMap($pairs);
            }
        };
    }
}

/**
 * Reduced - wrapper class for early termination in reduce.
 * Implements IDeref for consistency with Clojure.
 */
class Reduced {
    private mixed $val;

    public function __construct(mixed $val) {
        $this->val = $val;
    }

    public function deref(): mixed {
        return $this->val;
    }
}
