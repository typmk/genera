<?php

declare(strict_types=1);

require_once __DIR__ . '/../vendor/autoload.php';

use Cljp\Compiler\Compiler;
use Cljp\Compiler\HIR;

echo "=== ClojurePHP Self-Hosting Compiler Test ===\n\n";

$compiler = new Compiler();
$passed = 0;
$failed = 0;

function test(string $name, callable $fn, &$passed, &$failed): void
{
    echo "Testing: $name... ";
    try {
        $fn();
        echo "\033[32mPASSED\033[0m\n";
        $passed++;
    } catch (\Throwable $e) {
        echo "\033[31mFAILED: " . $e->getMessage() . "\033[0m\n";
        $failed++;
    }
}

function assertEq($expected, $actual, string $msg = ''): void
{
    if ($expected !== $actual) {
        throw new \Exception("Expected " . var_export($expected, true) . " but got " . var_export($actual, true) . ($msg ? " ($msg)" : ''));
    }
}

/**
 * Compile and eval a Clojure expression, returning the result.
 */
function compileEval(Compiler $compiler, string $source): mixed
{
    $forms = $compiler->read($source);
    if (empty($forms)) {
        return null;
    }

    $result = null;
    foreach ($forms as $form) {
        $env = $compiler->makeEnv();
        $env['context'] = 'expr';  // Expression context for return value
        $hir = $compiler->getAnalyzer()->analyze($env, $form);
        $hir['env']['context'] = 'expr';  // Ensure expr context for ternary
        $php = $compiler->getEmitter()->emit($hir);
        $result = eval("return $php;");
    }
    return $result;
}

// Test constants
test('integer constant', function() use ($compiler) {
    assertEq(42, compileEval($compiler, '42'));
}, $passed, $failed);

test('string constant', function() use ($compiler) {
    assertEq('hello', compileEval($compiler, '"hello"'));
}, $passed, $failed);

test('nil', function() use ($compiler) {
    assertEq(null, compileEval($compiler, 'nil'));
}, $passed, $failed);

test('true/false', function() use ($compiler) {
    assertEq(true, compileEval($compiler, 'true'));
    assertEq(false, compileEval($compiler, 'false'));
}, $passed, $failed);

// Test arithmetic
test('addition', function() use ($compiler) {
    assertEq(3, compileEval($compiler, '(+ 1 2)'));
}, $passed, $failed);

test('nested arithmetic', function() use ($compiler) {
    assertEq(9, compileEval($compiler, '(* (+ 1 2) (- 5 2))'));
}, $passed, $failed);

// Test PHP interop
test('php function call', function() use ($compiler) {
    assertEq(5, compileEval($compiler, '(php/strlen "hello")'));
}, $passed, $failed);

test('php operator (.)', function() use ($compiler) {
    assertEq('Hello World', compileEval($compiler, '(php/. "Hello" " World")'));
}, $passed, $failed);

// Test conditionals
test('if true branch', function() use ($compiler) {
    assertEq(1, compileEval($compiler, '(if true 1 2)'));
}, $passed, $failed);

test('if false branch', function() use ($compiler) {
    assertEq(2, compileEval($compiler, '(if false 1 2)'));
}, $passed, $failed);

// Test do
test('do block', function() use ($compiler) {
    assertEq(3, compileEval($compiler, '(do 1 2 3)'));
}, $passed, $failed);

// Test let
test('let binding', function() use ($compiler) {
    assertEq(10, compileEval($compiler, '(let* [x 10] x)'));
}, $passed, $failed);

test('let multiple bindings', function() use ($compiler) {
    assertEq(30, compileEval($compiler, '(let* [x 10 y 20] (+ x y))'));
}, $passed, $failed);

// Test fn
test('anonymous function', function() use ($compiler) {
    assertEq(25, compileEval($compiler, '((fn* [x] (* x x)) 5)'));
}, $passed, $failed);

test('function with multiple args', function() use ($compiler) {
    assertEq(7, compileEval($compiler, '((fn* [x y] (+ x y)) 3 4)'));
}, $passed, $failed);

// Test comparisons
test('greater than', function() use ($compiler) {
    assertEq(true, compileEval($compiler, '(> 5 3)'));
}, $passed, $failed);

test('less than', function() use ($compiler) {
    assertEq(false, compileEval($compiler, '(< 5 3)'));
}, $passed, $failed);

// Test loop/recur
test('loop/recur sum', function() use ($compiler) {
    assertEq(3, compileEval($compiler, '
        (loop* [i 0 acc 0]
          (if (< i 3)
            (recur (+ i 1) (+ acc i))
            acc))
    '));  // 0 + 1 + 2 = 3
}, $passed, $failed);

// Test HIR directly
test('HIR const node', function() {
    $env = HIR::makeEnv();
    $node = HIR::constNode($env, 42, 42);
    assertEq('const', $node['op']);
    assertEq(42, $node['val']);
    assertEq('int', $node['type']);
}, $passed, $failed);

test('HIR local environment', function() {
    $env = HIR::makeEnv();
    $env = HIR::addLocal($env, 'x', ['kind' => 'let']);
    assertEq(true, HIR::hasLocal($env, 'x'));
    assertEq(false, HIR::hasLocal($env, 'y'));
}, $passed, $failed);

// Additional tests for self-hosting verification
test('factorial via loop', function() use ($compiler) {
    $result = compileEval($compiler, '
        (loop* [n 5 acc 1]
          (if (< n 2)
            acc
            (recur (- n 1) (* acc n))))
    ');
    assertEq(120, $result);  // 5! = 120
}, $passed, $failed);

test('nested let with closures', function() use ($compiler) {
    $result = compileEval($compiler, '
        (let* [x 10]
          (let* [f (fn* [y] (+ x y))]
            (f 5)))
    ');
    assertEq(15, $result);
}, $passed, $failed);

test('variadic function', function() use ($compiler) {
    $result = compileEval($compiler, '
        ((fn* [& args] (php/count args)) 1 2 3 4 5)
    ');
    assertEq(5, $result);
}, $passed, $failed);

test('php strlen on string', function() use ($compiler) {
    $result = compileEval($compiler, '
        (php/strlen "hello world")
    ');
    assertEq(11, $result);
}, $passed, $failed);

test('php max/min', function() use ($compiler) {
    // max and min work with multiple args
    assertEq(5, compileEval($compiler, '(php/max 1 5 3)'));
    assertEq(1, compileEval($compiler, '(php/min 1 5 3)'));
}, $passed, $failed);

test('php strtoupper', function() use ($compiler) {
    $result = compileEval($compiler, '
        (php/strtoupper "hello")
    ');
    assertEq('HELLO', $result);
}, $passed, $failed);

test('complex nested expression', function() use ($compiler) {
    $result = compileEval($compiler, '
        (let* [a 1 b 2 c 3]
          (if (> (+ a b) c)
            (* a b c)
            (+ a b c)))
    ');
    assertEq(6, $result);  // 1+2 > 3 is false, so 1+2+3 = 6
}, $passed, $failed);

// Summary
echo "\n=== Results ===\n";
echo "Passed: \033[32m$passed\033[0m\n";
echo "Failed: \033[31m$failed\033[0m\n";

exit($failed > 0 ? 1 : 0);
