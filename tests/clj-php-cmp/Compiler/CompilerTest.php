<?php

declare(strict_types=1);

namespace Tests\Cljp\Compiler;

use PHPUnit\Framework\TestCase;
use Cljp\Compiler\Compiler;
use Cljp\Compiler\HIR;
use Cljp\Compiler\Analyzer;
use Cljp\Compiler\Emitter;

class CompilerTest extends TestCase
{
    private Compiler $compiler;

    protected function setUp(): void
    {
        $this->compiler = new Compiler();
    }

    public function testCompileConstant(): void
    {
        $php = $this->compiler->compile('42');
        $this->assertStringContainsString('42', $php);

        $result = eval("return $php;");
        $this->assertEquals(42, $result);
    }

    public function testCompileString(): void
    {
        $php = $this->compiler->compile('"hello"');
        $result = eval("return $php;");
        $this->assertEquals('hello', $result);
    }

    public function testCompileArithmetic(): void
    {
        $php = $this->compiler->compile('(+ 1 2)');
        $result = eval("return $php;");
        $this->assertEquals(3, $result);
    }

    public function testCompileNestedArithmetic(): void
    {
        $php = $this->compiler->compile('(* (+ 1 2) (- 5 2))');
        $result = eval("return $php;");
        $this->assertEquals(9, $result);
    }

    public function testCompilePhpFunction(): void
    {
        $php = $this->compiler->compile('(php/strlen "hello")');
        $result = eval("return $php;");
        $this->assertEquals(5, $result);
    }

    public function testCompileIfTrue(): void
    {
        $php = $this->compiler->compile('(if true 1 2)');
        $result = eval("return $php;");
        $this->assertEquals(1, $result);
    }

    public function testCompileIfFalse(): void
    {
        $php = $this->compiler->compile('(if false 1 2)');
        $result = eval("return $php;");
        $this->assertEquals(2, $result);
    }

    public function testCompileDo(): void
    {
        $php = $this->compiler->compile('(do 1 2 3)');
        $result = eval("return $php;");
        $this->assertEquals(3, $result);
    }

    public function testCompileLet(): void
    {
        $php = $this->compiler->compile('(let* [x 10] x)');
        $result = eval("return $php;");
        $this->assertEquals(10, $result);
    }

    public function testCompileLetMultipleBindings(): void
    {
        $php = $this->compiler->compile('(let* [x 10 y 20] (+ x y))');
        $result = eval("return $php;");
        $this->assertEquals(30, $result);
    }

    public function testCompileFn(): void
    {
        $php = $this->compiler->compile('((fn* [x] (* x x)) 5)');
        $result = eval("return $php;");
        $this->assertEquals(25, $result);
    }

    public function testCompileFnMultipleArgs(): void
    {
        $php = $this->compiler->compile('((fn* [x y] (+ x y)) 3 4)');
        $result = eval("return $php;");
        $this->assertEquals(7, $result);
    }

    public function testCompileLoop(): void
    {
        // Simple loop that counts to 3
        $php = $this->compiler->compile('
            (loop* [i 0 acc 0]
              (if (< i 3)
                (recur (+ i 1) (+ acc i))
                acc))
        ');
        $result = eval("return $php;");
        $this->assertEquals(3, $result);  // 0 + 1 + 2 = 3
    }

    public function testAnalyzer(): void
    {
        $analyzer = new Analyzer();
        $env = HIR::makeEnv();

        // Read a simple form
        $forms = $this->compiler->read('42');
        $hir = $analyzer->analyze($env, $forms[0]);

        $this->assertEquals('const', $hir['op']);
        $this->assertEquals(42, $hir['val']);
        $this->assertEquals('int', $hir['type']);
    }

    public function testEmitter(): void
    {
        $emitter = new Emitter();
        $hir = HIR::constNode(['context' => 'expr'], 42, 42);
        $php = $emitter->emit($hir);
        $this->assertEquals('42', $php);
    }

    public function testHIRLocalNode(): void
    {
        $env = HIR::makeEnv();
        $env = HIR::addLocal($env, 'x', ['kind' => 'let']);

        $this->assertTrue(HIR::hasLocal($env, 'x'));
        $this->assertFalse(HIR::hasLocal($env, 'y'));

        $local = HIR::getLocal($env, 'x');
        $this->assertEquals('let', $local['kind']);
    }

    public function testHIRIfNode(): void
    {
        $env = HIR::makeEnv();
        $test = HIR::constNode($env, true, true);
        $then = HIR::constNode($env, 1, 1);
        $else = HIR::constNode($env, 2, 2);

        $node = HIR::ifNode($env, '(if true 1 2)', $test, $then, $else);

        $this->assertEquals('if', $node['op']);
        $this->assertEquals(['test', 'then', 'else'], $node['children']);
    }

    public function testCompileComparison(): void
    {
        $php = $this->compiler->compile('(> 5 3)');
        $result = eval("return $php;");
        $this->assertTrue($result);

        $php = $this->compiler->compile('(< 5 3)');
        $result = eval("return $php;");
        $this->assertFalse($result);
    }

    public function testCompilePhpOperator(): void
    {
        $php = $this->compiler->compile('(php/. "Hello" " World")');
        $result = eval("return $php;");
        $this->assertEquals('Hello World', $result);
    }

    public function testReadMultipleForms(): void
    {
        $forms = $this->compiler->read('1 2 3');
        $this->assertCount(3, $forms);
        $this->assertEquals(1, $forms[0]);
        $this->assertEquals(2, $forms[1]);
        $this->assertEquals(3, $forms[2]);
    }
}
