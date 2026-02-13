<?php

declare(strict_types=1);

/**
 * Protocol dispatch, reify, deftype, defrecord.
 *
 * Type-based polymorphism for ClojurePHP.
 */

namespace Clojure\Php;

// Protocol registry: protocol-name => [method-names]
$GLOBALS['__cljp_protocols'] = [];
// Protocol implementations: protocol-name => type => method-name => callable
$GLOBALS['__cljp_protocol_impls'] = [];

/**
 * Register a protocol with its method names.
 */
function defProtocol(string $name, array $methods): void
{
    $GLOBALS['__cljp_protocols'][$name] = $methods;
    if (!isset($GLOBALS['__cljp_protocol_impls'][$name])) {
        $GLOBALS['__cljp_protocol_impls'][$name] = [];
    }
}

/**
 * Extend a protocol for a specific type.
 *
 * $impls is a flat array [method-name, callable, method-name, callable, ...].
 */
function extendType(string $protocol, string $type, array $impls): void
{
    if (!isset($GLOBALS['__cljp_protocol_impls'][$protocol])) {
        $GLOBALS['__cljp_protocol_impls'][$protocol] = [];
    }
    $methods = [];
    for ($i = 0; $i < count($impls); $i += 2) {
        $methods[$impls[$i]] = $impls[$i + 1];
    }
    $GLOBALS['__cljp_protocol_impls'][$protocol][$type] = $methods;
}

/**
 * Get the type name for protocol dispatch.
 */
function getType(mixed $x): string
{
    if ($x === null) return 'nil';
    if (is_object($x)) return get_class($x);
    return gettype($x);
}

/**
 * Dispatch a protocol method call.
 */
function protocolDispatch(string $protocol, string $method, mixed $target, array $args = []): mixed
{
    $type = getType($target);

    // Check if object has this method directly (for reify, deftype, etc.)
    if (is_object($target)) {
        if (method_exists($target, $method)) {
            return $target->$method(...$args);
        }
        // Try calling via __call (for reify objects)
        if (method_exists($target, '__call')) {
            try {
                return $target->$method(...$args);
            } catch (\Exception $e) {
                // Method not found in __call, continue to registry lookup
            }
        }
    }

    // Check for direct implementation in protocol registry
    if (isset($GLOBALS['__cljp_protocol_impls'][$protocol][$type][$method])) {
        return ($GLOBALS['__cljp_protocol_impls'][$protocol][$type][$method])($target, ...$args);
    }

    // Check for default implementation
    if (isset($GLOBALS['__cljp_protocol_impls'][$protocol]['default'][$method])) {
        return ($GLOBALS['__cljp_protocol_impls'][$protocol]['default'][$method])($target, ...$args);
    }

    throw new \Exception("No implementation of protocol '$protocol' method '$method' for type '$type'");
}

/**
 * Check if a type satisfies a protocol.
 */
function satisfies(string $protocol, mixed $x): bool
{
    $type = getType($x);

    // Check if object implements methods directly
    if (is_object($x) && isset($GLOBALS['__cljp_protocols'][$protocol])) {
        $allMethodsFound = true;
        foreach ($GLOBALS['__cljp_protocols'][$protocol] as $method) {
            if (!method_exists($x, $method)) {
                $allMethodsFound = false;
                break;
            }
        }
        if ($allMethodsFound) return true;
    }

    return isset($GLOBALS['__cljp_protocol_impls'][$protocol][$type]) ||
           isset($GLOBALS['__cljp_protocol_impls'][$protocol]['default']);
}

/**
 * Create an anonymous object with method implementations (reify).
 *
 * $methods is a flat array [method-name, callable, method-name, callable, ...].
 */
function reify(array $methods): object
{
    $methodMap = [];
    for ($i = 0; $i < count($methods); $i += 2) {
        $methodMap[$methods[$i]] = $methods[$i + 1];
    }

    return new class($methodMap) {
        private array $methods;

        public function __construct(array $methods)
        {
            $this->methods = $methods;
        }

        public function __call(string $name, array $args): mixed
        {
            if (isset($this->methods[$name])) {
                return ($this->methods[$name])($this, ...$args);
            }
            throw new \Exception("Method not found: $name");
        }
    };
}

// ============================================================
// deftype / defrecord
// ============================================================

// Type registry: type-name => {fields: [...], mutable: [...], methods: {...}}
$GLOBALS['__cljp_types'] = [];
// Record registry: record-name => {fields: [...], methods: {...}}
$GLOBALS['__cljp_records'] = [];

/**
 * Define a type (deftype).
 *
 * @param string $name Type name
 * @param array $fields Field names
 * @param array $mutableFields Fields that can be set!
 * @param array $methods Flat array [method-name, callable, ...]
 */
function defType(string $name, array $fields, array $mutableFields, array $methods): void
{
    $methodMap = [];
    for ($i = 0; $i < count($methods); $i += 2) {
        $methodMap[$methods[$i]] = $methods[$i + 1];
    }
    $GLOBALS['__cljp_types'][$name] = [
        'fields' => $fields,
        'mutable' => $mutableFields,
        'methods' => $methodMap
    ];
}

/**
 * Create an instance of a type.
 */
function createType(string $name, mixed ...$args): object
{
    if (!isset($GLOBALS['__cljp_types'][$name])) {
        throw new \Exception("Type not defined: $name");
    }
    $typeDef = $GLOBALS['__cljp_types'][$name];
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

        public function __construct(string $typeName, array $fields, array $mutableFields, array $methods)
        {
            $this->typeName = $typeName;
            $this->fields = $fields;
            $this->mutableFields = $mutableFields;
            $this->methods = $methods;
        }

        public function __get(string $name): mixed
        {
            if (array_key_exists($name, $this->fields)) {
                return $this->fields[$name];
            }
            throw new \Exception("Field not found: $name");
        }

        public function __set(string $name, mixed $value): void
        {
            if (in_array($name, $this->mutableFields)) {
                $this->fields[$name] = $value;
            } else {
                throw new \Exception("Field is not mutable: $name");
            }
        }

        public function __call(string $name, array $args): mixed
        {
            if (isset($this->methods[$name])) {
                return ($this->methods[$name])($this, ...$args);
            }
            throw new \Exception("Method not found: $name");
        }

        public function getTypeName(): string
        {
            return $this->typeName;
        }
    };
}

/**
 * Define a record (defrecord).
 *
 * @param string $name Record name
 * @param array $fields Field names
 * @param array $methods Flat array [method-name, callable, ...]
 */
function defRecord(string $name, array $fields, array $methods): void
{
    $methodMap = [];
    for ($i = 0; $i < count($methods); $i += 2) {
        $methodMap[$methods[$i]] = $methods[$i + 1];
    }
    $GLOBALS['__cljp_records'][$name] = [
        'fields' => $fields,
        'methods' => $methodMap
    ];
}

/**
 * Create an instance of a record from positional args.
 */
function createRecord(string $name, mixed ...$args): object
{
    if (!isset($GLOBALS['__cljp_records'][$name])) {
        throw new \Exception("Record not defined: $name");
    }
    $recordDef = $GLOBALS['__cljp_records'][$name];
    $fields = $recordDef['fields'];
    $methods = $recordDef['methods'];

    $fieldValues = [];
    foreach ($fields as $i => $field) {
        $fieldValues[$field] = $args[$i] ?? null;
    }

    return makeRecordInstance($name, $fieldValues, $fields, $methods);
}

/**
 * Create an instance of a record from a map.
 */
function createRecordFromMap(string $name, mixed $map): object
{
    if (!isset($GLOBALS['__cljp_records'][$name])) {
        throw new \Exception("Record not defined: $name");
    }
    $recordDef = $GLOBALS['__cljp_records'][$name];
    $fields = $recordDef['fields'];
    $methods = $recordDef['methods'];

    $fieldValues = [];
    foreach ($fields as $field) {
        $key = kw($field);
        $fieldValues[$field] = get($map, $key);
    }

    return makeRecordInstance($name, $fieldValues, $fields, $methods);
}

/**
 * Create a record instance with map-like behavior.
 */
function makeRecordInstance(string $name, array $fieldValues, array $fields, array $methods): object
{
    return new class($name, $fieldValues, $fields, $methods) implements \ArrayAccess, \IteratorAggregate, \Countable {
        private string $recordName;
        private array $fieldValues;
        private array $fieldNames;
        private array $methods;

        public function __construct(string $recordName, array $fieldValues, array $fieldNames, array $methods)
        {
            $this->recordName = $recordName;
            $this->fieldValues = $fieldValues;
            $this->fieldNames = $fieldNames;
            $this->methods = $methods;
        }

        // Field access via ->field
        public function __get(string $name): mixed
        {
            if (array_key_exists($name, $this->fieldValues)) {
                return $this->fieldValues[$name];
            }
            throw new \Exception("Field not found: $name");
        }

        // Method calls
        public function __call(string $name, array $args): mixed
        {
            if (isset($this->methods[$name])) {
                return ($this->methods[$name])($this, ...$args);
            }
            throw new \Exception("Method not found: $name");
        }

        // ArrayAccess for map-like behavior (keyword access)
        public function offsetExists(mixed $offset): bool
        {
            $key = $offset instanceof Kw ? $offset->name() : (string)$offset;
            return array_key_exists($key, $this->fieldValues);
        }

        public function offsetGet(mixed $offset): mixed
        {
            $key = $offset instanceof Kw ? $offset->name() : (string)$offset;
            return $this->fieldValues[$key] ?? null;
        }

        public function offsetSet(mixed $offset, mixed $value): void
        {
            throw new \Exception("Records are immutable");
        }

        public function offsetUnset(mixed $offset): void
        {
            throw new \Exception("Records are immutable");
        }

        // IteratorAggregate for seq/iteration
        public function getIterator(): \Traversable
        {
            foreach ($this->fieldValues as $k => $v) {
                yield kw($k) => $v;
            }
        }

        // Countable
        public function count(): int
        {
            return count($this->fieldValues);
        }

        public function getRecordName(): string
        {
            return $this->recordName;
        }

        // Return as map for assoc/dissoc operations
        public function toMap(): Map
        {
            $pairs = [];
            foreach ($this->fieldValues as $k => $v) {
                $pairs[] = kw($k);
                $pairs[] = $v;
            }
            return hashMap(...$pairs);
        }
    };
}
