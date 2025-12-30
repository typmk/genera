<?php

declare(strict_types=1);

namespace Cljp\Compiler;

use Clojure\Lang\Keyword;
use Clojure\Lang\Symbol;

/**
 * PHP Emitter: transforms HIR nodes into PHP source code.
 */
class Emitter
{
    private string $output = '';
    private array $mungeCache = [];

    private const MUNGE_MAP = [
        '-' => '_',
        '.' => '_DOT_',
        ':' => '_COLON_',
        '+' => '_PLUS_',
        '>' => '_GT_',
        '<' => '_LT_',
        '=' => '_EQ_',
        '~' => '_TILDE_',
        '!' => '_BANG_',
        '@' => '_AT_',
        '#' => '_SHARP_',
        '?' => '_QMARK_',
        '*' => '_STAR_',
        '/' => '_SLASH_',
    ];

    /**
     * Emit PHP code for an HIR node.
     */
    public function emit(array $node): string
    {
        $this->output = '';
        $this->emitNode($node);
        return $this->output;
    }

    private function write(string ...$strings): void
    {
        foreach ($strings as $s) {
            $this->output .= $s;
        }
    }

    private function mungeName(string $s): string
    {
        if (isset($this->mungeCache[$s])) {
            return $this->mungeCache[$s];
        }

        $result = '';
        for ($i = 0; $i < strlen($s); $i++) {
            $ch = $s[$i];
            $result .= self::MUNGE_MAP[$ch] ?? $ch;
        }

        $this->mungeCache[$s] = $result;
        return $result;
    }

    private function mungeLocal(mixed $name): string
    {
        return '$__L_' . $this->mungeName((string)$name);
    }

    private function isStatementContext(array $node): bool
    {
        return ($node['env']['context'] ?? null) === 'statement';
    }

    private function isReturnContext(array $node): bool
    {
        return ($node['env']['context'] ?? null) === 'return';
    }

    private function isExprContext(array $node): bool
    {
        return ($node['env']['context'] ?? null) === 'expr';
    }

    private function isLoopResultContext(array $node): bool
    {
        return ($node['env']['context'] ?? null) === 'loop-result';
    }

    private function getLoopResultVar(array $node): ?string
    {
        return $node['env']['loop-result-var'] ?? null;
    }

    private function wrapStatement(array $node, callable $bodyFn): void
    {
        if ($this->isReturnContext($node)) {
            $this->write('return ');
        } elseif ($this->isLoopResultContext($node)) {
            $resultVar = $this->getLoopResultVar($node);
            if ($resultVar) {
                $this->write($resultVar, ' = ');
            }
        }
        $bodyFn();
        if ($this->isStatementContext($node) || $this->isReturnContext($node) || $this->isLoopResultContext($node)) {
            $this->write(';');
        }
        if ($this->isStatementContext($node)) {
            $this->write("\n");
        }
    }

    private function emitNode(array $node): void
    {
        $op = $node['op'];

        match ($op) {
            HIR::CONST => $this->emitConst($node),
            HIR::LOCAL => $this->emitLocal($node),
            HIR::VAR => $this->emitVar($node),
            HIR::IF => $this->emitIf($node),
            HIR::DO => $this->emitDo($node),
            HIR::LET => $this->emitLet($node),
            HIR::FN => $this->emitFn($node),
            HIR::INVOKE => $this->emitInvoke($node),
            HIR::DEF => $this->emitDef($node),
            HIR::LOOP => $this->emitLoop($node),
            HIR::RECUR => $this->emitRecur($node),
            HIR::LETFN => $this->emitLetfn($node),
            HIR::THROW => $this->emitThrow($node),
            HIR::TRY => $this->emitTry($node),
            HIR::QUOTE => $this->emitQuote($node),
            HIR::VECTOR => $this->emitVector($node),
            HIR::MAP => $this->emitMap($node),
            HIR::SET => $this->emitSet($node),
            HIR::HOST_CALL => $this->emitHostCall($node),
            HIR::HOST_NEW => $this->emitHostNew($node),
            HIR::HOST_CONST => $this->emitHostConst($node),
            HIR::CASE => $this->emitCase($node),
            default => throw new \RuntimeException("Unknown op: $op"),
        };
    }

    private function emitConst(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $val = $node['val'];
            match (true) {
                $val === null => $this->write('null'),
                $val === true => $this->write('true'),
                $val === false => $this->write('false'),
                is_string($val) => $this->write("'", str_replace("'", "\\'", $val), "'"),
                $val instanceof Keyword => $this->emitKeyword($val),
                is_int($val), is_float($val) => $this->write((string)$val),
                default => $this->write(var_export($val, true)),
            };
        });
    }

    private function emitKeyword(Keyword $kw): void
    {
        if ($kw->getNamespace() !== null) {
            $this->write("\\Clojure\\Lang\\Keyword::createForNamespace('",
                         $kw->getNamespace(), "', '", $kw->getName(), "')");
        } else {
            $this->write("\\Clojure\\Lang\\Keyword::create('", $kw->getName(), "')");
        }
    }

    private function emitLocal(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write($this->mungeLocal($node['name']));
        });
    }

    private function emitVar(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $name = $this->mungeName((string)$node['sym']);
            $this->write("\$GLOBALS['", $name, "']");
        });
    }

    private function emitIf(array $node): void
    {
        if ($this->isStatementContext($node) || $this->isReturnContext($node) || $this->isLoopResultContext($node)) {
            $this->write('if (');
            $this->emitNode($this->withContext($node['test'], 'expr'));
            $this->write(') { ');

            // Determine context for branches - preserve loop-result context
            $ctx = $this->isReturnContext($node) ? 'return' :
                   ($this->isLoopResultContext($node) ? 'loop-result' : 'statement');

            // Propagate loop-result-var through branches
            $thenNode = $node['then'];
            $elseNode = $node['else'];
            if ($this->isLoopResultContext($node) && $this->getLoopResultVar($node)) {
                $thenNode['env']['loop-result-var'] = $this->getLoopResultVar($node);
                if ($elseNode !== null) {
                    $elseNode['env']['loop-result-var'] = $this->getLoopResultVar($node);
                }
            }

            $this->emitNode($this->withContext($thenNode, $ctx));
            $this->write('}');
            if ($elseNode !== null) {
                $this->write(' else { ');
                $this->emitNode($this->withContext($elseNode, $ctx));
                $this->write('}');
            }
            if ($this->isStatementContext($node)) {
                $this->write("\n");
            }
        } else {
            // Ternary
            $this->write('(');
            $this->emitNode($this->withContext($node['test'], 'expr'));
            $this->write(' ? ');
            $this->emitNode($this->withContext($node['then'], 'expr'));
            $this->write(' : ');
            if ($node['else'] !== null) {
                $this->emitNode($this->withContext($node['else'], 'expr'));
            } else {
                $this->write('null');
            }
            $this->write(')');
        }
    }

    private function emitDo(array $node): void
    {
        if ($this->isExprContext($node)) {
            // In expression context, wrap in IIFE
            $this->write('(call_user_func(function() { ');
            foreach ($node['statements'] as $stmt) {
                $this->emitNode($this->withContext($stmt, 'statement'));
            }
            $this->emitNode($this->withContext($node['ret'], 'return'));
            $this->write(' }))');
        } else {
            foreach ($node['statements'] as $stmt) {
                $this->emitNode($this->withContext($stmt, 'statement'));
            }
            $this->emitNode($node['ret']);
        }
    }

    private function emitLet(array $node): void
    {
        if ($this->isExprContext($node)) {
            // IIFE
            $this->write('(call_user_func(function() { ');
            foreach ($node['bindings'] as $binding) {
                $this->write($this->mungeLocal($binding['name']), ' = ');
                $this->emitNode($this->withContext($binding['init'], 'expr'));
                $this->write('; ');
            }
            $this->emitNode($this->withContext($node['body'], 'return'));
            $this->write(' }))');
        } else {
            foreach ($node['bindings'] as $binding) {
                $this->write($this->mungeLocal($binding['name']), ' = ');
                $this->emitNode($this->withContext($binding['init'], 'expr'));
                $this->write(";\n");
            }
            $this->emitNode($this->withContext($node['body'], $node['env']['context'] ?? 'statement'));
        }
    }

    private function emitFn(array $node): void
    {
        $arities = $node['arities'];

        if (count($arities) === 1) {
            $this->emitSingleArityFn($node, $arities[0]);
        } else {
            $this->emitMultiArityFn($node);
        }
    }

    private function emitSingleArityFn(array $node, array $arity): void
    {
        $this->wrapStatement($node, function () use ($node, $arity) {
            $this->write('(function(');

            $params = $arity['params'];
            $variadic = $arity['variadic?'] ?? false;

            foreach ($params as $i => $p) {
                if ($i > 0) $this->write(', ');
                if ($variadic && $i === count($params) - 1) {
                    $this->write('...');
                }
                $this->write($this->mungeLocal($p));
            }
            $this->write(')');

            // Use clause for captured variables
            $freeVars = $this->collectFreeVars($node['env'], $params);
            if (!empty($freeVars)) {
                $this->write(' use (');
                $first = true;
                foreach ($freeVars as $v) {
                    if (!$first) $this->write(', ');
                    $this->write('&', $this->mungeLocal($v));
                    $first = false;
                }
                $this->write(')');
            }

            $this->write(' { ');
            $this->emitNode($this->withContext($arity['body'], 'return'));
            $this->write('})');
        });
    }

    private function emitMultiArityFn(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('(function(...$__args)');

            // Collect all params for use clause
            $allParams = [];
            foreach ($node['arities'] as $arity) {
                foreach ($arity['params'] as $p) {
                    $allParams[(string)$p] = $p;
                }
            }
            $freeVars = $this->collectFreeVars($node['env'], array_values($allParams));
            if (!empty($freeVars)) {
                $this->write(' use (');
                $first = true;
                foreach ($freeVars as $v) {
                    if (!$first) $this->write(', ');
                    $this->write('&', $this->mungeLocal($v));
                    $first = false;
                }
                $this->write(')');
            }

            $this->write(' { $__n = count($__args); ');

            // Sort: variadic last, then by param count desc
            $sorted = $node['arities'];
            usort($sorted, function ($a, $b) {
                $aVar = $a['variadic?'] ?? false;
                $bVar = $b['variadic?'] ?? false;
                if ($aVar !== $bVar) return $aVar ? 1 : -1;
                return count($b['params']) - count($a['params']);
            });

            $first = true;
            foreach ($sorted as $arity) {
                $params = $arity['params'];
                $variadic = $arity['variadic?'] ?? false;
                $minArgs = $variadic ? count($params) - 1 : count($params);

                if (!$first) $this->write(' else ');
                $first = false;

                if ($variadic) {
                    $this->write("if (\$__n >= $minArgs) { ");
                } else {
                    $this->write("if (\$__n == ", count($params), ") { ");
                }

                // Destructure args
                foreach ($params as $i => $p) {
                    if ($variadic && $i === count($params) - 1) {
                        $this->write($this->mungeLocal($p), " = array_slice(\$__args, $i); ");
                    } else {
                        $this->write($this->mungeLocal($p), " = \$__args[$i]; ");
                    }
                }

                $this->emitNode($this->withContext($arity['body'], 'return'));
                $this->write(' }');
            }

            $this->write(" else { throw new \\InvalidArgumentException('Wrong number of args: ' . \$__n); }");
            $this->write(' })');
        });
    }

    private function emitInvoke(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('call_user_func(');
            $this->emitNode($this->withContext($node['fn'], 'expr'));
            foreach ($node['args'] as $arg) {
                $this->write(', ');
                $this->emitNode($this->withContext($arg, 'expr'));
            }
            $this->write(')');
        });
    }

    private function emitDef(array $node): void
    {
        $varName = $this->mungeName((string)$node['name']);
        $this->write("\$GLOBALS['", $varName, "'] = ");
        if ($node['init'] !== null) {
            $this->emitNode($this->withContext($node['init'], 'expr'));
        } else {
            $this->write('null');
        }
        $this->write(";\n");
        $this->write('$', $varName, " = &\$GLOBALS['", $varName, "'];\n");
    }

    private function emitLoop(array $node): void
    {
        $bindingNames = array_map(fn($b) => $b['name'], $node['bindings']);

        if ($this->isExprContext($node)) {
            // IIFE with result variable
            $this->write('(call_user_func(function() { ');
            $this->write('$__RESULT = null; ');
            foreach ($node['bindings'] as $binding) {
                $this->write($this->mungeLocal($binding['name']), ' = ');
                $this->emitNode($this->withContext($binding['init'], 'expr'));
                $this->write('; ');
            }
            $this->write('while(true) { ');

            $bodyNode = $node['body'];
            $bodyNode['env']['loop-bindings'] = $bindingNames;
            $bodyNode['env']['loop-result-var'] = '$__RESULT';
            $this->emitNode($this->withContext($bodyNode, 'loop-result'));

            $this->write(' break; }');
            $this->write(' return $__RESULT;');
            $this->write(' }))');
        } else {
            foreach ($node['bindings'] as $binding) {
                $this->write($this->mungeLocal($binding['name']), ' = ');
                $this->emitNode($this->withContext($binding['init'], 'expr'));
                $this->write(";\n");
            }
            $this->write('while(true) { ');

            $ctx = $this->isReturnContext($node) ? 'return' : 'statement';
            $bodyNode = $node['body'];
            $bodyNode['env']['loop-bindings'] = $bindingNames;
            $this->emitNode($this->withContext($bodyNode, $ctx));

            $this->write(" break; }\n");
        }
    }

    private function emitRecur(array $node): void
    {
        $bindings = $node['env']['loop-bindings'] ?? [];

        // Evaluate all args to temps first (recur semantics: all args evaluated before assignment)
        $temps = [];
        foreach ($node['args'] as $i => $arg) {
            if (isset($bindings[$i])) {
                $temps[$i] = '$__T' . $i;
                $this->write($temps[$i], ' = ');
                $this->emitNode($this->withContext($arg, 'expr'));
                $this->write('; ');
            }
        }

        // Then assign temps to loop vars
        foreach ($temps as $i => $temp) {
            $this->write($this->mungeLocal($bindings[$i]), ' = ', $temp, '; ');
        }

        $this->write('continue;');
    }

    private function emitLetfn(array $node): void
    {
        if ($this->isExprContext($node)) {
            $this->write('(call_user_func(function() { ');
            foreach ($node['fns'] as $fn) {
                $this->write($this->mungeLocal($fn['name']), ' = ');
                $this->emitNode($this->withContext($fn['fn'], 'expr'));
                $this->write('; ');
            }
            $this->emitNode($this->withContext($node['body'], 'return'));
            $this->write(' }))');
        } else {
            foreach ($node['fns'] as $fn) {
                $this->write($this->mungeLocal($fn['name']), ' = ');
                $this->emitNode($this->withContext($fn['fn'], 'expr'));
                $this->write(";\n");
            }
            $this->emitNode($this->withContext($node['body'], $node['env']['context'] ?? 'statement'));
        }
    }

    private function emitThrow(array $node): void
    {
        $this->write('throw ');
        $this->emitNode($this->withContext($node['exception'], 'expr'));
        $this->write(";\n");
    }

    private function emitTry(array $node): void
    {
        $this->write('try { ');
        $this->emitNode($this->withContext($node['body'], 'return'));
        $this->write(' }');

        foreach ($node['catches'] as $catch) {
            $this->write(' catch (', (string)$catch['class'], ' ', $this->mungeLocal($catch['name']), ') { ');
            $this->emitNode($this->withContext($catch['body'], 'return'));
            $this->write(' }');
        }

        if ($node['finally'] !== null) {
            $this->write(' finally { ');
            $this->emitNode($this->withContext($node['finally'], 'statement'));
            $this->write(' }');
        }

        $this->write("\n");
    }

    private function emitQuote(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->emitQuoted($node['val']);
        });
    }

    private function emitQuoted(mixed $val): void
    {
        match (true) {
            $val === null => $this->write('null'),
            $val === true => $this->write('true'),
            $val === false => $this->write('false'),
            is_string($val) => $this->write("'", str_replace("'", "\\'", $val), "'"),
            $val instanceof Keyword => $this->emitKeyword($val),
            $val instanceof Symbol => $this->write("'", (string)$val, "'"),
            is_int($val), is_float($val) => $this->write((string)$val),
            is_array($val) => $this->emitQuotedArray($val),
            $val instanceof \Traversable => $this->emitQuotedTraversable($val),
            default => $this->write(var_export($val, true)),
        };
    }

    private function emitQuotedArray(array $arr): void
    {
        $this->write('[');
        $first = true;
        foreach ($arr as $item) {
            if (!$first) $this->write(', ');
            $this->emitQuoted($item);
            $first = false;
        }
        $this->write(']');
    }

    private function emitQuotedTraversable(\Traversable $coll): void
    {
        $this->write('\\Cljp\\Runtime::list([');
        $first = true;
        foreach ($coll as $item) {
            if (!$first) $this->write(', ');
            $this->emitQuoted($item);
            $first = false;
        }
        $this->write('])');
    }

    private function emitVector(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('\\Cljp\\Runtime::vector([');
            $first = true;
            foreach ($node['items'] as $item) {
                if (!$first) $this->write(', ');
                $this->emitNode($this->withContext($item, 'expr'));
                $first = false;
            }
            $this->write('])');
        });
    }

    private function emitMap(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('\\Cljp\\Runtime::hashMap([');
            $first = true;
            foreach ($node['keys'] as $i => $key) {
                if (!$first) $this->write(', ');
                $this->emitNode($this->withContext($key, 'expr'));
                $this->write(', ');
                $this->emitNode($this->withContext($node['vals'][$i], 'expr'));
                $first = false;
            }
            $this->write('])');
        });
    }

    private function emitSet(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('\\Cljp\\Runtime::set([');
            $first = true;
            foreach ($node['items'] as $item) {
                if (!$first) $this->write(', ');
                $this->emitNode($this->withContext($item, 'expr'));
                $first = false;
            }
            $this->write('])');
        });
    }

    private function emitHostCall(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $callType = $node['call-type'];
            $fnName = $node['fn-name'];
            $args = $node['args'];

            match ($callType) {
                'function' => $this->emitHostFunction($fnName, $args),
                'method' => $this->emitHostMethod($node['target'], $fnName, $args),
                'static' => $this->emitHostStatic($node['class'], $fnName, $args),
                'operator' => $this->emitHostOperator($fnName, $args),
                default => throw new \RuntimeException("Unknown call-type: $callType"),
            };
        });
    }

    private function emitHostFunction(string $fnName, array $args): void
    {
        $this->write($fnName, '(');
        $first = true;
        foreach ($args as $arg) {
            if (!$first) $this->write(', ');
            $this->emitNode($this->withContext($arg, 'expr'));
            $first = false;
        }
        $this->write(')');
    }

    private function emitHostMethod(array $target, string $method, array $args): void
    {
        $this->emitNode($this->withContext($target, 'expr'));
        $this->write('->', $method, '(');
        $first = true;
        foreach ($args as $arg) {
            if (!$first) $this->write(', ');
            $this->emitNode($this->withContext($arg, 'expr'));
            $first = false;
        }
        $this->write(')');
    }

    private function emitHostStatic(string $class, string $method, array $args): void
    {
        $this->write($class, '::', $method, '(');
        $first = true;
        foreach ($args as $arg) {
            if (!$first) $this->write(', ');
            $this->emitNode($this->withContext($arg, 'expr'));
            $first = false;
        }
        $this->write(')');
    }

    private function emitHostOperator(string $op, array $args): void
    {
        if (count($args) === 1 && $op === '-') {
            // Unary minus
            $this->write('(-');
            $this->emitNode($this->withContext($args[0], 'expr'));
            $this->write(')');
        } elseif (count($args) === 1) {
            // Single arg - just return the value
            $this->emitNode($this->withContext($args[0], 'expr'));
        } elseif ($op === 'instanceof') {
            $this->write('(');
            $this->emitNode($this->withContext($args[0], 'expr'));
            $this->write(' instanceof ');
            if ($args[1]['op'] === HIR::CONST) {
                $this->write((string)$args[1]['val']);
            } else {
                $this->emitNode($this->withContext($args[1], 'expr'));
            }
            $this->write(')');
        } else {
            // Binary/multi-arity
            $this->write('(');
            $first = true;
            foreach ($args as $arg) {
                if (!$first) $this->write(' ', $op, ' ');
                $this->emitNode($this->withContext($arg, 'expr'));
                $first = false;
            }
            $this->write(')');
        }
    }

    private function emitHostNew(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('(new ', $node['class'], '(');
            $first = true;
            foreach ($node['args'] as $arg) {
                if (!$first) $this->write(', ');
                $this->emitNode($this->withContext($arg, 'expr'));
                $first = false;
            }
            $this->write('))');
        });
    }

    private function emitHostConst(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write($node['const-name']);
        });
    }

    private function emitCase(array $node): void
    {
        $this->wrapStatement($node, function () use ($node) {
            $this->write('match(');
            $this->emitNode($this->withContext($node['expr'], 'expr'));
            $this->write(') { ');

            foreach ($node['clauses'] as $clause) {
                $first = true;
                foreach ($clause['test-vals'] as $val) {
                    if (!$first) $this->write(', ');
                    $this->emitQuoted($val);
                    $first = false;
                }
                $this->write(' => ');
                $this->emitNode($this->withContext($clause['then'], 'expr'));
                $this->write(', ');
            }

            if ($node['default'] !== null) {
                $this->write('default => ');
                $this->emitNode($this->withContext($node['default'], 'expr'));
            }

            $this->write(' }');
        });
    }

    private function withContext(array $node, string $context): array
    {
        $node['env']['context'] = $context;
        return $node;
    }

    private function collectFreeVars(array $env, array $params): array
    {
        $locals = array_keys($env['locals'] ?? []);
        $paramSet = array_map('strval', $params);
        return array_diff($locals, $paramSet);
    }
}
