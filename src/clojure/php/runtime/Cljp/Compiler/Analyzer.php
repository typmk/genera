<?php

declare(strict_types=1);

namespace Cljp\Compiler;

use Clojure\Lang\Keyword;
use Clojure\Lang\Symbol;
use Clojure\Lang\Collections\Vector\PersistentVectorInterface;
use Clojure\Lang\Collections\Map\PersistentMapInterface;
use Clojure\Lang\Collections\HashSet\PersistentHashSetInterface;
use Clojure\Lang\Collections\LinkedList\PersistentListInterface;

/**
 * Analyzer: transforms Clojure forms into HIR nodes.
 *
 * Architecture:
 * - User-facing forms (let, fn, loop) are MACROS that expand to primitives
 * - Primitive forms (let*, fn*, loop*) are handled by the analyzer
 * - Host interop uses platform-agnostic :host-* nodes
 */
class Analyzer
{
    private array $macros = [];
    private int $loopIdCounter = 0;

    /** @var array<string, string> Clojure op -> PHP op */
    private const CLJ_TO_PHP_OP = [
        '+' => '+', '-' => '-', '*' => '*', '/' => '/',
        'mod' => '%', 'rem' => '%',
        '>' => '>', '<' => '<', '>=' => '>=', '<=' => '<=',
        'bit-and' => '&', 'bit-or' => '|', 'bit-xor' => '^',
        'bit-shift-left' => '<<', 'bit-shift-right' => '>>',
    ];

    /** @var array<string> Clojure operators */
    private const CLJ_OPERATORS = [
        '+', '-', '*', '/', 'mod', 'rem',
        '>', '<', '>=', '<=',
        'bit-and', 'bit-or', 'bit-xor', 'bit-shift-left', 'bit-shift-right',
    ];

    /** @var array<string> PHP infix operators */
    private const PHP_OPERATORS = [
        '+', '-', '*', '/', '%', '.',
        '==', '===', '!=', '!==', '<', '>', '<=', '>=',
        '&&', '||', 'and', 'or', 'xor',
        '&', '|', '^', '<<', '>>', 'instanceof',
    ];

    public function __construct()
    {
        $this->registerBuiltinMacros();
    }

    /**
     * Analyze a form into an HIR node.
     */
    public function analyze(array $env, mixed $form): array
    {
        $expanded = $this->expandMacros($form);
        return $this->analyzeForm($env, $expanded);
    }

    /**
     * Classify form for dispatch.
     */
    private function classify(mixed $form): string
    {
        // List with symbol operator
        if ($this->isList($form) && !$this->isEmpty($form)) {
            $first = $this->first($form);
            if ($first instanceof Symbol) {
                $name = $first->getName();
                $ns = $first->getNamespace();

                // Core special forms
                if ($name === 'if' && $ns === null) return 'if';
                if ($name === 'do' && $ns === null) return 'do';
                if ($name === 'def' && $ns === null) return 'def';
                if ($name === 'recur' && $ns === null) return 'recur';
                if ($name === 'throw' && $ns === null) return 'throw';
                if ($name === 'try' && $ns === null) return 'try';
                if ($name === 'quote' && $ns === null) return 'quote';
                if ($name === 'new' && $ns === null) return 'new';
                if ($ns === 'php' && $name === 'new') return 'new';
                if ($name === 'case*' && $ns === null) return 'case*';

                // Primitive forms
                if ($name === 'let*' && $ns === null) return 'let*';
                if ($name === 'fn*' && $ns === null) return 'fn*';
                if ($name === 'loop*' && $ns === null) return 'loop*';
                if ($name === 'letfn*' && $ns === null) return 'letfn*';

                // Clojure operators
                if ($ns === null && in_array($name, self::CLJ_OPERATORS, true)) {
                    return 'clj-operator';
                }

                // PHP operators
                if ($ns === 'php' && in_array($name, self::PHP_OPERATORS, true)) {
                    return 'host-operator';
                }

                // Host function call
                if (in_array($ns, ['php', 'js', 'rust'], true)) {
                    return 'host-function';
                }

                // Method call: (.method obj args)
                if ($ns === null && strlen($name) > 1 && $name[0] === '.') {
                    return 'host-method';
                }

                // Constructor: (Class. args)
                if ($ns === null && strlen($name) > 1 && str_ends_with($name, '.')) {
                    return 'host-constructor';
                }

                // Static call: (Class/method args)
                if ($ns !== null && !in_array($ns, ['php', 'js', 'rust'], true)) {
                    return 'host-static';
                }

                return 'invoke';
            }
            return 'invoke';
        }

        // Empty list
        if ($this->isList($form) && $this->isEmpty($form)) {
            return 'const';
        }

        // Collections
        if ($form instanceof PersistentVectorInterface) return 'vector';
        if ($form instanceof PersistentMapInterface) return 'map';
        if ($form instanceof PersistentHashSetInterface) return 'set';

        // Host constants: php/PHP_EOL, php/E_ALL, etc.
        if ($form instanceof Symbol && $form->getNamespace() === 'php') {
            $name = $form->getName();
            if (preg_match('/^[A-Z][A-Z0-9_]*$/', $name) ||
                preg_match('/^__[A-Z]+__$/', $name) ||
                in_array($name, ['true', 'false', 'null'], true)) {
                return 'host-const';
            }
        }

        // Symbols
        if ($form instanceof Symbol) return 'symbol';

        // Everything else is a constant
        return 'const';
    }

    /**
     * Dispatch to appropriate analyzer based on form type.
     */
    private function analyzeForm(array $env, mixed $form): array
    {
        $type = $this->classify($form);

        return match ($type) {
            'const' => $this->analyzeConst($env, $form),
            'symbol' => $this->analyzeSymbol($env, $form),
            'host-const' => $this->analyzeHostConst($env, $form),
            'if' => $this->analyzeIf($env, $form),
            'do' => $this->analyzeDo($env, $form),
            'let*' => $this->analyzeLet($env, $form),
            'fn*' => $this->analyzeFn($env, $form),
            'loop*' => $this->analyzeLoop($env, $form),
            'recur' => $this->analyzeRecur($env, $form),
            'letfn*' => $this->analyzeLetfn($env, $form),
            'throw' => $this->analyzeThrow($env, $form),
            'try' => $this->analyzeTry($env, $form),
            'quote' => $this->analyzeQuote($env, $form),
            'def' => $this->analyzeDef($env, $form),
            'new' => $this->analyzeNew($env, $form),
            'case*' => $this->analyzeCase($env, $form),
            'clj-operator' => $this->analyzeCljOperator($env, $form),
            'host-operator' => $this->analyzeHostOperator($env, $form),
            'host-function' => $this->analyzeHostFunction($env, $form),
            'host-method' => $this->analyzeHostMethod($env, $form),
            'host-constructor' => $this->analyzeHostConstructor($env, $form),
            'host-static' => $this->analyzeHostStatic($env, $form),
            'vector' => $this->analyzeVector($env, $form),
            'map' => $this->analyzeMap($env, $form),
            'set' => $this->analyzeSet($env, $form),
            'invoke' => $this->analyzeInvoke($env, $form),
            default => throw new \RuntimeException("Unknown form type: $type"),
        };
    }

    private function analyzeConst(array $env, mixed $form): array
    {
        return HIR::constNode($env, $form, $form);
    }

    private function analyzeSymbol(array $env, mixed $form): array
    {
        $name = (string)$form;
        if (HIR::hasLocal($env, $name)) {
            return HIR::localNode($env, $form, $form);
        }
        return HIR::varNode($env, $form, $form);
    }

    private function analyzeHostConst(array $env, mixed $form): array
    {
        return HIR::hostConstNode($env, $form, $form->getNamespace(), $form->getName());
    }

    private function analyzeIf(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $test = $this->analyze($env, $items[1]);
        $then = $this->analyze($env, $items[2]);
        $else = isset($items[3]) ? $this->analyze($env, $items[3]) : null;
        return HIR::ifNode($env, $form, $test, $then, $else);
    }

    private function analyzeDo(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        array_shift($items); // Remove 'do'

        if (empty($items)) {
            return HIR::constNode($env, $form, null);
        }

        $statements = [];
        for ($i = 0; $i < count($items) - 1; $i++) {
            $statements[] = $this->analyze($env, $items[$i]);
        }
        $ret = $this->analyze($env, $items[count($items) - 1]);

        return HIR::doNode($env, $form, $statements, $ret);
    }

    private function analyzeLet(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $bindings = $this->toArray($items[1]);
        $body = array_slice($items, 2);

        $analyzedBindings = [];
        $currentEnv = $env;

        for ($i = 0; $i < count($bindings); $i += 2) {
            $name = $bindings[$i];
            $init = $this->analyze($currentEnv, $bindings[$i + 1]);
            $analyzedBindings[] = ['name' => $name, 'init' => $init];
            $currentEnv = HIR::addLocal($currentEnv, $name, ['kind' => 'let', 'node' => $init]);
        }

        $bodyNode = $this->analyzeBody($currentEnv, $body);
        return HIR::letNode($env, $form, $analyzedBindings, $bodyNode);
    }

    private function analyzeFn(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        array_shift($items); // Remove 'fn*'

        $name = null;
        if (!empty($items) && $items[0] instanceof Symbol) {
            $name = (string)array_shift($items);
        }

        // Single arity or multi-arity?
        if (!empty($items) && $items[0] instanceof PersistentVectorInterface) {
            // Single arity: (fn* [params] body)
            $params = $this->toArray($items[0]);
            $body = array_slice($items, 1);
            $arity = $this->analyzeSingleArity($env, $params, $body);
            return HIR::fnNode($env, $form, $name, [$arity]);
        } else {
            // Multi-arity: (fn* ([x] body1) ([x y] body2))
            $arities = [];
            foreach ($items as $arityForm) {
                $arityItems = $this->toArray($arityForm);
                $params = $this->toArray($arityItems[0]);
                $body = array_slice($arityItems, 1);
                $arities[] = $this->analyzeSingleArity($env, $params, $body);
            }
            return HIR::fnNode($env, $form, $name, $arities);
        }
    }

    private function analyzeSingleArity(array $env, array $params, array $body): array
    {
        $variadic = false;
        $cleanParams = [];

        foreach ($params as $p) {
            if ($p instanceof Symbol && $p->getName() === '&') {
                $variadic = true;
            } else {
                $cleanParams[] = $p;
            }
        }

        $fnEnv = $env;
        $fnEnv['in-loop?'] = false;
        foreach ($cleanParams as $p) {
            $fnEnv = HIR::addLocal($fnEnv, $p, ['kind' => 'arg']);
        }

        $bodyNode = $this->analyzeBody($fnEnv, $body);

        return [
            'params' => $cleanParams,
            'variadic?' => $variadic,
            'body' => $bodyNode,
        ];
    }

    private function analyzeLoop(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $bindings = $this->toArray($items[1]);
        $body = array_slice($items, 2);

        $loopId = ++$this->loopIdCounter;
        $analyzedBindings = [];
        $currentEnv = $env;

        for ($i = 0; $i < count($bindings); $i += 2) {
            $name = $bindings[$i];
            $init = $this->analyze($currentEnv, $bindings[$i + 1]);
            $analyzedBindings[] = ['name' => $name, 'init' => $init];
            $currentEnv = HIR::addLocal($currentEnv, $name, ['kind' => 'loop']);
        }

        $loopEnv = $currentEnv;
        $loopEnv['in-loop?'] = true;
        $loopEnv['loop-id'] = $loopId;
        $loopEnv['loop-bindings'] = array_map(fn($b) => $b['name'], $analyzedBindings);

        $bodyNode = $this->analyzeBody($loopEnv, $body);
        return HIR::loopNode($env, $form, $analyzedBindings, $bodyNode, $loopId);
    }

    private function analyzeRecur(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        array_shift($items); // Remove 'recur'

        $args = array_map(fn($arg) => $this->analyze($env, $arg), $items);
        return HIR::recurNode($env, $form, $args, $env['loop-id'] ?? null);
    }

    private function analyzeLetfn(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $fnspecs = $this->toArray($items[1]);
        $body = array_slice($items, 2);

        // First pass: add all function names to environment
        $envWithNames = $env;
        foreach ($fnspecs as $fnspec) {
            $fnspecArr = $this->toArray($fnspec);
            $name = $fnspecArr[0];
            $envWithNames = HIR::addLocal($envWithNames, $name, ['kind' => 'letfn']);
        }

        // Second pass: analyze each function
        $analyzedFns = [];
        foreach ($fnspecs as $fnspec) {
            $fnspecArr = $this->toArray($fnspec);
            $fname = $fnspecArr[0];
            $params = $this->toArray($fnspecArr[1]);
            $fnBody = array_slice($fnspecArr, 2);
            $arity = $this->analyzeSingleArity($envWithNames, $params, $fnBody);
            $fnNode = HIR::fnNode($envWithNames, $fnspec, (string)$fname, [$arity]);
            $analyzedFns[] = ['name' => $fname, 'fn' => $fnNode];
        }

        $bodyNode = $this->analyzeBody($envWithNames, $body);
        return HIR::letfnNode($env, $form, $analyzedFns, $bodyNode);
    }

    private function analyzeThrow(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $exception = $this->analyze($env, $items[1]);
        return HIR::throwNode($env, $form, $exception);
    }

    private function analyzeTry(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        array_shift($items); // Remove 'try'

        $tryBody = [];
        $catches = [];
        $finally = null;

        foreach ($items as $item) {
            if ($this->isList($item) && !$this->isEmpty($item)) {
                $first = $this->first($item);
                if ($first instanceof Symbol) {
                    if ($first->getName() === 'catch') {
                        $catchItems = $this->toArray($item);
                        $class = $catchItems[1];
                        $name = $catchItems[2];
                        $handler = array_slice($catchItems, 3);
                        $catchEnv = HIR::addLocal($env, $name, ['kind' => 'catch']);
                        $catches[] = [
                            'class' => $class,
                            'name' => $name,
                            'body' => $this->analyzeBody($catchEnv, $handler),
                        ];
                        continue;
                    }
                    if ($first->getName() === 'finally') {
                        $finallyItems = $this->toArray($item);
                        $finally = $this->analyzeBody($env, array_slice($finallyItems, 1));
                        continue;
                    }
                }
            }
            $tryBody[] = $item;
        }

        $bodyNode = $this->analyzeBody($env, $tryBody);
        return HIR::tryNode($env, $form, $bodyNode, $catches, $finally);
    }

    private function analyzeQuote(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        return HIR::quoteNode($env, $form, $items[1]);
    }

    private function analyzeDef(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $name = $items[1];
        $init = isset($items[2]) ? $this->analyze($env, $items[2]) : null;
        return HIR::defNode($env, $form, $name, $init, null);
    }

    private function analyzeNew(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $class = (string)$items[1];
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 2));
        return HIR::hostNewNode($env, $form, $class, $args);
    }

    private function analyzeCase(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        array_shift($items); // Remove 'case*'
        $expr = $this->analyze($env, array_shift($items));

        $clauses = [];
        $default = null;
        $hasDefault = count($items) % 2 !== 0;

        if ($hasDefault) {
            $default = $this->analyze($env, array_pop($items));
        }

        for ($i = 0; $i < count($items); $i += 2) {
            $testExpr = $items[$i];
            $thenExpr = $items[$i + 1];

            $testVals = ($this->isList($testExpr) && !$this->isEmpty($testExpr) &&
                         !($this->first($testExpr) instanceof Symbol && $this->first($testExpr)->getName() === 'quote'))
                ? $this->toArray($testExpr)
                : [$testExpr];

            $clauses[] = [
                'test-vals' => $testVals,
                'then' => $this->analyze($env, $thenExpr),
            ];
        }

        return HIR::caseNode($env, $form, $expr, $clauses, $default);
    }

    private function analyzeCljOperator(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $op = $items[0];
        $opName = $op->getName();
        $hostOp = self::CLJ_TO_PHP_OP[$opName] ?? $opName;
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 1));

        return HIR::hostCallNode($env, $form, 'operator', null, null, null, $hostOp, $args);
    }

    private function analyzeHostOperator(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $op = $items[0];
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 1));

        return HIR::hostCallNode($env, $form, 'operator', $op->getNamespace(), null, null, $op->getName(), $args);
    }

    private function analyzeHostFunction(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $op = $items[0];
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 1));

        return HIR::hostCallNode($env, $form, 'function', $op->getNamespace(), null, null, $op->getName(), $args);
    }

    private function analyzeHostMethod(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $op = $items[0];
        $methodName = substr($op->getName(), 1); // Remove leading dot
        $target = $this->analyze($env, $items[1]);
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 2));

        return HIR::hostCallNode($env, $form, 'method', null, $target, null, $methodName, $args);
    }

    private function analyzeHostConstructor(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $op = $items[0];
        $className = substr($op->getName(), 0, -1); // Remove trailing dot
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 1));

        return HIR::hostNewNode($env, $form, $className, $args);
    }

    private function analyzeHostStatic(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $op = $items[0];
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 1));

        return HIR::hostCallNode($env, $form, 'static', null, null, $op->getNamespace(), $op->getName(), $args);
    }

    private function analyzeVector(array $env, mixed $form): array
    {
        $items = array_map(fn($item) => $this->analyze($env, $item), $this->toArray($form));
        return HIR::vectorNode($env, $form, $items);
    }

    private function analyzeMap(array $env, mixed $form): array
    {
        $keys = [];
        $vals = [];
        foreach ($form as $k => $v) {
            $keys[] = $this->analyze($env, $k);
            $vals[] = $this->analyze($env, $v);
        }
        return HIR::mapNode($env, $form, $keys, $vals);
    }

    private function analyzeSet(array $env, mixed $form): array
    {
        $items = array_map(fn($item) => $this->analyze($env, $item), $this->toArray($form));
        return HIR::setNode($env, $form, $items);
    }

    private function analyzeInvoke(array $env, mixed $form): array
    {
        $items = $this->toArray($form);
        $fn = $this->analyze($env, $items[0]);
        $args = array_map(fn($arg) => $this->analyze($env, $arg), array_slice($items, 1));
        return HIR::invokeNode($env, $form, $fn, $args);
    }

    private function analyzeBody(array $env, array $body): array
    {
        if (count($body) === 1) {
            return $this->analyze($env, $body[0]);
        }
        // Wrap in do
        $statements = [];
        for ($i = 0; $i < count($body) - 1; $i++) {
            $statements[] = $this->analyze($env, $body[$i]);
        }
        $ret = $this->analyze($env, $body[count($body) - 1]);
        return HIR::doNode($env, $body, $statements, $ret);
    }

    // Macro expansion

    private function registerBuiltinMacros(): void
    {
        // TODO: Register let, fn, loop, defn macros that expand to primitives
    }

    private function expandMacros(mixed $form): mixed
    {
        // TODO: Implement macro expansion
        return $form;
    }

    // Helpers

    private function isList(mixed $form): bool
    {
        return $form instanceof PersistentListInterface || (is_array($form) && array_is_list($form));
    }

    private function isEmpty(mixed $form): bool
    {
        if ($form instanceof \Countable) return count($form) === 0;
        if (is_array($form)) return empty($form);
        return false;
    }

    private function first(mixed $form): mixed
    {
        if ($form instanceof PersistentListInterface) return $form->first();
        if ($form instanceof PersistentVectorInterface) return $form->get(0);
        if (is_array($form)) return $form[0] ?? null;
        return null;
    }

    private function toArray(mixed $form): array
    {
        if (is_array($form)) return $form;
        if ($form instanceof \Traversable) return iterator_to_array($form, false);
        return [];
    }
}
