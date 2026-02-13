<?php

declare(strict_types=1);

/**
 * Globals Bridge - Maps Clojure function names to $GLOBALS for compiled code.
 *
 * The ClojurePHP compiler emits code that calls functions via $GLOBALS['fn'].
 * This file bridges to the actual Clojure\Php namespace functions/classes.
 */

namespace {
    // ============================================================
    // Printer functions
    // ============================================================
    $GLOBALS['println'] = 'Clojure\Php\println_';
    $GLOBALS['print'] = 'Clojure\Php\print_';
    $GLOBALS['pr'] = 'Clojure\Php\pr_';
    $GLOBALS['prn'] = 'Clojure\Php\prn_';
    $GLOBALS['pr_str'] = 'Clojure\Php\prStr';
    $GLOBALS['pr-str'] = 'Clojure\Php\prStr';

    // ============================================================
    // Core sequence functions
    // ============================================================
    $GLOBALS['seq'] = 'Clojure\Php\seq';
    $GLOBALS['first'] = 'Clojure\Php\first';
    $GLOBALS['rest'] = 'Clojure\Php\rest';
    $GLOBALS['next'] = 'Clojure\Php\next_';
    $GLOBALS['cons'] = 'Clojure\Php\cons';
    $GLOBALS['conj'] = 'Clojure\Php\conj';
    $GLOBALS['count'] = 'Clojure\Php\count_';
    $GLOBALS['nth'] = 'Clojure\Php\nth';
    $GLOBALS['get'] = 'Clojure\Php\get';
    $GLOBALS['assoc'] = 'Clojure\Php\assoc';
    $GLOBALS['dissoc'] = 'Clojure\Php\dissoc';
    $GLOBALS['contains_QMARK_'] = 'Clojure\Php\contains';
    $GLOBALS['keys'] = 'Clojure\Php\keys_';
    $GLOBALS['vals'] = 'Clojure\Php\vals';

    // ============================================================
    // Predicates
    // ============================================================
    $GLOBALS['nil_QMARK_'] = 'Clojure\Php\isNil';
    $GLOBALS['nil?'] = 'Clojure\Php\isNil';
    $GLOBALS['some_QMARK_'] = 'Clojure\Php\some_';
    $GLOBALS['some?'] = 'Clojure\Php\some_';
    $GLOBALS['empty_QMARK_'] = 'Clojure\Php\isEmpty';
    $GLOBALS['string_QMARK_'] = 'Clojure\Php\isString';
    $GLOBALS['number_QMARK_'] = 'Clojure\Php\isNumber';
    $GLOBALS['keyword_QMARK_'] = 'Clojure\Php\isKeyword';
    $GLOBALS['symbol_QMARK_'] = 'Clojure\Php\isSymbol';
    $GLOBALS['map_QMARK_'] = 'Clojure\Php\isMap';
    $GLOBALS['vector_QMARK_'] = 'Clojure\Php\isVector';
    $GLOBALS['seq_QMARK_'] = 'Clojure\Php\isSeq';
    $GLOBALS['fn_QMARK_'] = 'Clojure\Php\isFn';
    $GLOBALS['coll_QMARK_'] = 'Clojure\Php\isColl';

    // ============================================================
    // Higher-order functions
    // ============================================================
    $GLOBALS['map'] = 'Clojure\Php\map_';
    $GLOBALS['filter'] = 'Clojure\Php\filter_';
    $GLOBALS['reduce'] = 'Clojure\Php\reduce_';
    $GLOBALS['apply'] = 'Clojure\Php\apply_';
    $GLOBALS['partial'] = 'Clojure\Php\partial';
    $GLOBALS['comp'] = 'Clojure\Php\comp';
    $GLOBALS['complement'] = 'Clojure\Php\complement';
    $GLOBALS['constantly'] = 'Clojure\Php\constantly';
    $GLOBALS['identity'] = 'Clojure\Php\identity_';
    $GLOBALS['juxt'] = 'Clojure\Php\juxt';
    $GLOBALS['take'] = 'Clojure\Php\take';
    $GLOBALS['drop'] = 'Clojure\Php\drop';
    $GLOBALS['take_while'] = 'Clojure\Php\takeWhile';
    $GLOBALS['drop_while'] = 'Clojure\Php\dropWhile';
    $GLOBALS['partition'] = 'Clojure\Php\partition';
    $GLOBALS['interleave'] = 'Clojure\Php\interleave';
    $GLOBALS['interpose'] = 'Clojure\Php\interpose';
    $GLOBALS['concat'] = 'Clojure\Php\concat_';
    $GLOBALS['mapcat'] = 'Clojure\Php\mapcat';
    $GLOBALS['flatten'] = 'Clojure\Php\flatten';
    $GLOBALS['distinct'] = 'Clojure\Php\distinct_';
    $GLOBALS['sort'] = 'Clojure\Php\sort_';
    $GLOBALS['reverse'] = 'Clojure\Php\reverse_';
    $GLOBALS['group_by'] = 'Clojure\Php\groupBy';
    $GLOBALS['frequencies'] = 'Clojure\Php\frequencies';

    // ============================================================
    // Comparison & Equality
    // ============================================================
    $GLOBALS['_EQ_'] = 'Clojure\Php\equals';
    $GLOBALS['='] = 'Clojure\Php\equals';
    $GLOBALS['not_EQ_'] = 'Clojure\Php\notEquals';
    $GLOBALS['not='] = 'Clojure\Php\notEquals';
    $GLOBALS['identical_QMARK_'] = 'Clojure\Php\identical_';
    $GLOBALS['identical?'] = 'Clojure\Php\identical_';
    $GLOBALS['compare'] = 'Clojure\Php\compare_';
    $GLOBALS['hash'] = 'Clojure\Php\hash_';

    // ============================================================
    // Arithmetic
    // ============================================================
    $GLOBALS['inc'] = 'Clojure\Php\inc';
    $GLOBALS['dec'] = 'Clojure\Php\dec';
    $GLOBALS['max'] = 'max';
    $GLOBALS['min'] = 'min';
    $GLOBALS['abs'] = 'abs';
    $GLOBALS['mod'] = 'Clojure\Php\mod_';
    $GLOBALS['rem'] = 'Clojure\Php\rem';
    $GLOBALS['quot'] = 'Clojure\Php\quot';

    // ============================================================
    // String operations
    // ============================================================
    $GLOBALS['str'] = 'Clojure\Php\str_';
    $GLOBALS['subs'] = 'Clojure\Php\subs';
    $GLOBALS['name'] = 'Clojure\Php\name_';
    $GLOBALS['namespace'] = 'Clojure\Php\namespace_';

    // ============================================================
    // Collection constructors
    // ============================================================
    $GLOBALS['list'] = 'Clojure\Php\lst';
    $GLOBALS['vec'] = 'Clojure\Php\vec';
    $GLOBALS['vector'] = 'Clojure\Php\vec';
    $GLOBALS['hash_map'] = 'Clojure\Php\hashMap';
    $GLOBALS['hash_set'] = 'Clojure\Php\hashSet';
    $GLOBALS['into'] = 'Clojure\Php\into';
    $GLOBALS['empty'] = 'Clojure\Php\emptyOf';
    $GLOBALS['range'] = 'Clojure\Php\range_';
    $GLOBALS['repeat'] = 'Clojure\Php\repeat_';
    $GLOBALS['iterate'] = 'Clojure\Php\iterate_';
    $GLOBALS['cycle'] = 'Clojure\Php\cycle_';
    $GLOBALS['lazy_seq'] = 'Clojure\Php\lazySeq';

    // ============================================================
    // Atom / State
    // ============================================================
    $GLOBALS['atom'] = 'Clojure\Php\atom';
    $GLOBALS['deref'] = 'Clojure\Php\deref';
    $GLOBALS['reset_BANG_'] = 'Clojure\Php\reset';
    $GLOBALS['swap_BANG_'] = 'Clojure\Php\swap';
    $GLOBALS['compare_and_set_BANG_'] = 'Clojure\Php\compareAndSet';
    $GLOBALS['add_watch'] = 'Clojure\Php\addWatch';
    $GLOBALS['remove_watch'] = 'Clojure\Php\removeWatch';

    // ============================================================
    // Namespace & Var
    // ============================================================
    $GLOBALS['ns'] = function($nsName, $docstring = null, ...$opts) {
        // Handle namespace declaration - noop for now
        return null;
    };

    $GLOBALS['def'] = function($name, $value) {
        $GLOBALS[(string)$name] = $value;
        return $value;
    };

    // ============================================================
    // Types & Protocols
    // ============================================================
    $GLOBALS['deftype'] = function(...$args) {
        // deftype is complex - placeholder
        return null;
    };

    $GLOBALS['satisfies_QMARK_'] = function($protocol, $x) {
        // Protocol check - placeholder
        return false;
    };

    $GLOBALS['instance_QMARK_'] = function($class, $x) {
        return $x instanceof $class;
    };

    // ============================================================
    // Exceptions
    // ============================================================
    $GLOBALS['ex_info'] = 'Clojure\Php\exInfo';
    $GLOBALS['ex_data'] = 'Clojure\Php\exData';
    $GLOBALS['ex_message'] = 'Clojure\Php\exMessage';

    // ============================================================
    // Operators as functions
    // ============================================================
    $GLOBALS['_PLUS_'] = function(...$args) {
        if (count($args) === 0) return 0;
        if (count($args) === 1) return $args[0];
        return array_reduce($args, fn($a, $b) => $a + $b, 0);
    };
    $GLOBALS['_'] = function(...$args) {
        if (count($args) === 0) return 0;
        if (count($args) === 1) return -$args[0];
        $first = array_shift($args);
        return array_reduce($args, fn($a, $b) => $a - $b, $first);
    };
    $GLOBALS['_STAR_'] = function(...$args) {
        if (count($args) === 0) return 1;
        if (count($args) === 1) return $args[0];
        return array_reduce($args, fn($a, $b) => $a * $b, 1);
    };
    $GLOBALS['_SLASH_'] = function(...$args) {
        if (count($args) === 0) throw new \InvalidArgumentException('/ requires at least 1 arg');
        if (count($args) === 1) return 1 / $args[0];
        $first = array_shift($args);
        return array_reduce($args, fn($a, $b) => $a / $b, $first);
    };

    // ============================================================
    // Misc
    // ============================================================
    $GLOBALS['not'] = 'Clojure\Php\not_';
    $GLOBALS['type'] = 'Clojure\Php\type_';
    $GLOBALS['class'] = 'Clojure\Php\getClass';
    $GLOBALS['meta'] = 'Clojure\Php\meta';
    $GLOBALS['with_meta'] = 'Clojure\Php\withMeta';
    $GLOBALS['vary_meta'] = 'Clojure\Php\varyMeta';
    $GLOBALS['gensym'] = 'Clojure\Php\gensym';
    $GLOBALS['rand'] = 'Clojure\Php\rand_';
    $GLOBALS['rand_int'] = 'Clojure\Php\randInt';
    $GLOBALS['rand_nth'] = 'Clojure\Php\randNth';
    $GLOBALS['shuffle'] = 'Clojure\Php\shuffle_';
    $GLOBALS['time'] = 'Clojure\Php\time_';
    $GLOBALS['slurp'] = 'Clojure\Php\slurp';
    $GLOBALS['spit'] = 'Clojure\Php\spit';
}
