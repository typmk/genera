<?php

declare(strict_types=1);

namespace Cljp\Compiler;

/**
 * High-level Intermediate Representation (HIR) node types.
 *
 * Platform-agnostic semantic representation of Clojure programs.
 *
 * Node categories (21 total):
 * - Core (12):  const, local, var, if, do, let, fn, invoke, def, loop, recur, letfn
 * - Error (2):  throw, try
 * - Data (4):   quote, vector, map, set
 * - Host (3):   host-call, host-new, host-field
 */
class HIR
{
    // Node type constants
    public const CONST = 'const';
    public const LOCAL = 'local';
    public const VAR = 'var';
    public const IF = 'if';
    public const DO = 'do';
    public const LET = 'let';
    public const FN = 'fn';
    public const INVOKE = 'invoke';
    public const DEF = 'def';
    public const LOOP = 'loop';
    public const RECUR = 'recur';
    public const LETFN = 'letfn';
    public const THROW = 'throw';
    public const TRY = 'try';
    public const QUOTE = 'quote';
    public const VECTOR = 'vector';
    public const MAP = 'map';
    public const SET = 'set';
    public const HOST_CALL = 'host-call';
    public const HOST_NEW = 'host-new';
    public const HOST_FIELD = 'host-field';
    public const HOST_CONST = 'host-const';
    public const CASE = 'case';

    /**
     * Create a constant node.
     */
    public static function constNode(array $env, mixed $form, mixed $val): array
    {
        $type = match (true) {
            $val === null => 'nil',
            is_bool($val) => 'bool',
            is_int($val) => 'int',
            is_float($val) => 'float',
            is_string($val) => 'string',
            $val instanceof \Clojure\Lang\Keyword => 'keyword',
            $val instanceof \Clojure\Lang\Symbol => 'symbol',
            default => 'unknown',
        };

        return [
            'op' => self::CONST,
            'form' => $form,
            'env' => $env,
            'val' => $val,
            'type' => $type,
            'children' => [],
        ];
    }

    /**
     * Create a local variable reference node.
     */
    public static function localNode(array $env, mixed $form, mixed $name): array
    {
        return [
            'op' => self::LOCAL,
            'form' => $form,
            'env' => $env,
            'name' => $name,
            'children' => [],
        ];
    }

    /**
     * Create a var (global) reference node.
     */
    public static function varNode(array $env, mixed $form, mixed $sym): array
    {
        return [
            'op' => self::VAR,
            'form' => $form,
            'env' => $env,
            'sym' => $sym,
            'children' => [],
        ];
    }

    /**
     * Create an if node.
     */
    public static function ifNode(array $env, mixed $form, array $test, array $then, ?array $else): array
    {
        return [
            'op' => self::IF,
            'form' => $form,
            'env' => $env,
            'test' => $test,
            'then' => $then,
            'else' => $else,
            'children' => ['test', 'then', 'else'],
        ];
    }

    /**
     * Create a do node.
     */
    public static function doNode(array $env, mixed $form, array $statements, array $ret): array
    {
        return [
            'op' => self::DO,
            'form' => $form,
            'env' => $env,
            'statements' => $statements,
            'ret' => $ret,
            'children' => ['statements', 'ret'],
        ];
    }

    /**
     * Create a let node.
     */
    public static function letNode(array $env, mixed $form, array $bindings, array $body): array
    {
        return [
            'op' => self::LET,
            'form' => $form,
            'env' => $env,
            'bindings' => $bindings,
            'body' => $body,
            'children' => ['bindings', 'body'],
        ];
    }

    /**
     * Create a fn node with one or more arities.
     */
    public static function fnNode(array $env, mixed $form, ?string $name, array $arities): array
    {
        return [
            'op' => self::FN,
            'form' => $form,
            'env' => $env,
            'name' => $name,
            'arities' => $arities,
            'children' => ['arities'],
        ];
    }

    /**
     * Create an invoke node.
     */
    public static function invokeNode(array $env, mixed $form, array $fn, array $args): array
    {
        return [
            'op' => self::INVOKE,
            'form' => $form,
            'env' => $env,
            'fn' => $fn,
            'args' => $args,
            'children' => ['fn', 'args'],
        ];
    }

    /**
     * Create a def node.
     */
    public static function defNode(array $env, mixed $form, mixed $name, ?array $init, ?array $meta): array
    {
        return [
            'op' => self::DEF,
            'form' => $form,
            'env' => $env,
            'name' => $name,
            'init' => $init,
            'meta' => $meta,
            'children' => ['init'],
        ];
    }

    /**
     * Create a loop node.
     */
    public static function loopNode(array $env, mixed $form, array $bindings, array $body, int $loopId): array
    {
        return [
            'op' => self::LOOP,
            'form' => $form,
            'env' => $env,
            'bindings' => $bindings,
            'body' => $body,
            'loop-id' => $loopId,
            'children' => ['bindings', 'body'],
        ];
    }

    /**
     * Create a recur node.
     */
    public static function recurNode(array $env, mixed $form, array $args, ?int $loopId): array
    {
        return [
            'op' => self::RECUR,
            'form' => $form,
            'env' => $env,
            'args' => $args,
            'loop-id' => $loopId,
            'children' => ['args'],
        ];
    }

    /**
     * Create a letfn node.
     */
    public static function letfnNode(array $env, mixed $form, array $fns, array $body): array
    {
        return [
            'op' => self::LETFN,
            'form' => $form,
            'env' => $env,
            'fns' => $fns,
            'body' => $body,
            'children' => ['fns', 'body'],
        ];
    }

    /**
     * Create a throw node.
     */
    public static function throwNode(array $env, mixed $form, array $exception): array
    {
        return [
            'op' => self::THROW,
            'form' => $form,
            'env' => $env,
            'exception' => $exception,
            'children' => ['exception'],
        ];
    }

    /**
     * Create a try node.
     */
    public static function tryNode(array $env, mixed $form, array $body, array $catches, ?array $finally): array
    {
        return [
            'op' => self::TRY,
            'form' => $form,
            'env' => $env,
            'body' => $body,
            'catches' => $catches,
            'finally' => $finally,
            'children' => ['body', 'catches', 'finally'],
        ];
    }

    /**
     * Create a quote node.
     */
    public static function quoteNode(array $env, mixed $form, mixed $val): array
    {
        return [
            'op' => self::QUOTE,
            'form' => $form,
            'env' => $env,
            'val' => $val,
            'children' => [],
        ];
    }

    /**
     * Create a vector node.
     */
    public static function vectorNode(array $env, mixed $form, array $items): array
    {
        return [
            'op' => self::VECTOR,
            'form' => $form,
            'env' => $env,
            'items' => $items,
            'children' => ['items'],
        ];
    }

    /**
     * Create a map node.
     */
    public static function mapNode(array $env, mixed $form, array $keys, array $vals): array
    {
        return [
            'op' => self::MAP,
            'form' => $form,
            'env' => $env,
            'keys' => $keys,
            'vals' => $vals,
            'children' => ['keys', 'vals'],
        ];
    }

    /**
     * Create a set node.
     */
    public static function setNode(array $env, mixed $form, array $items): array
    {
        return [
            'op' => self::SET,
            'form' => $form,
            'env' => $env,
            'items' => $items,
            'children' => ['items'],
        ];
    }

    /**
     * Create a host-call node.
     */
    public static function hostCallNode(
        array $env,
        mixed $form,
        string $callType,
        ?string $hostNs,
        ?array $target,
        ?string $className,
        string $fnName,
        array $args
    ): array {
        return [
            'op' => self::HOST_CALL,
            'form' => $form,
            'env' => $env,
            'call-type' => $callType,
            'host-ns' => $hostNs,
            'target' => $target,
            'class' => $className,
            'fn-name' => $fnName,
            'args' => $args,
            'children' => ['target', 'args'],
        ];
    }

    /**
     * Create a host-new node.
     */
    public static function hostNewNode(array $env, mixed $form, string $className, array $args): array
    {
        return [
            'op' => self::HOST_NEW,
            'form' => $form,
            'env' => $env,
            'class' => $className,
            'args' => $args,
            'children' => ['args'],
        ];
    }

    /**
     * Create a host-const node.
     */
    public static function hostConstNode(array $env, mixed $form, string $hostNs, string $constName): array
    {
        return [
            'op' => self::HOST_CONST,
            'form' => $form,
            'env' => $env,
            'host-ns' => $hostNs,
            'const-name' => $constName,
            'children' => [],
        ];
    }

    /**
     * Create a case node.
     */
    public static function caseNode(array $env, mixed $form, array $expr, array $clauses, ?array $default): array
    {
        return [
            'op' => self::CASE,
            'form' => $form,
            'env' => $env,
            'expr' => $expr,
            'clauses' => $clauses,
            'default' => $default,
            'children' => ['expr', 'clauses', 'default'],
        ];
    }

    /**
     * Create a fresh environment.
     */
    public static function makeEnv(array $opts = []): array
    {
        return array_merge([
            'locals' => [],
            'in-loop?' => false,
            'loop-id' => null,
            'ns' => 'user',
        ], $opts);
    }

    /**
     * Add a local to the environment.
     */
    public static function addLocal(array $env, mixed $name, array $info): array
    {
        $env['locals'][(string)$name] = $info;
        return $env;
    }

    /**
     * Check if name is a local.
     */
    public static function hasLocal(array $env, mixed $name): bool
    {
        return isset($env['locals'][(string)$name]);
    }

    /**
     * Get local info.
     */
    public static function getLocal(array $env, mixed $name): ?array
    {
        return $env['locals'][(string)$name] ?? null;
    }
}
