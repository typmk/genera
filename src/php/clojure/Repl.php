<?php

declare(strict_types=1);

/**
 * ClojurePHP REPL - Read-Eval-Print Loop.
 */

namespace Clojure\Php;

/**
 * Interactive REPL for ClojurePHP.
 */
class Repl
{
    private bool $running = false;
    private string $prompt = 'user=> ';
    private array $history = [];
    private bool $debug = false;
    private ?object $compiler = null;

    /**
     * Set the compiler to use for evaluation.
     */
    public function setCompiler(object $compiler): void
    {
        $this->compiler = $compiler;
    }

    /**
     * Start the REPL.
     */
    public function run(): void
    {
        $this->running = true;
        $this->printBanner();

        // Initialize user namespace
        inNs('user');

        while ($this->running) {
            $input = $this->readInput();
            if ($input === null) {
                break;
            }

            $input = trim($input);
            if ($input === '') {
                continue;
            }

            // Handle REPL commands
            if (str_starts_with($input, ':')) {
                $this->handleCommand($input);
                continue;
            }

            $this->evalPrint($input);
        }

        echo "\nGoodbye!\n";
    }

    /**
     * Read input from stdin (handles multi-line).
     */
    private function readInput(): ?string
    {
        echo $this->prompt;
        $input = '';
        $depth = 0;

        while (true) {
            $line = fgets(STDIN);
            if ($line === false) {
                return $input ?: null;
            }

            $input .= $line;

            // Count brackets to handle multi-line input
            $depth += $this->bracketDepth($line);
            if ($depth <= 0) {
                break;
            }

            echo '      ';
        }

        return $input;
    }

    /**
     * Count net bracket depth in a line.
     */
    private function bracketDepth(string $line): int
    {
        $depth = 0;
        $inString = false;
        $escape = false;

        for ($i = 0; $i < strlen($line); $i++) {
            $ch = $line[$i];

            if ($escape) {
                $escape = false;
                continue;
            }

            if ($ch === '\\' && $inString) {
                $escape = true;
                continue;
            }

            if ($ch === '"') {
                $inString = !$inString;
                continue;
            }

            if ($inString) continue;

            if ($ch === ';') break; // Comment

            if ($ch === '(' || $ch === '[' || $ch === '{') {
                $depth++;
            } elseif ($ch === ')' || $ch === ']' || $ch === '}') {
                $depth--;
            }
        }

        return $depth;
    }

    /**
     * Evaluate and print a Clojure expression.
     */
    private function evalPrint(string $input): void
    {
        try {
            // Read
            $reader = new Reader($input, 'repl');
            $forms = $reader->readAll();

            if (empty($forms)) {
                return;
            }

            $this->history[] = $input;

            foreach ($forms as $form) {
                if ($this->compiler !== null) {
                    // Use compiler if available
                    $result = $this->compileAndEval($form);
                } else {
                    // Direct evaluation (limited)
                    $result = $this->directEval($form);
                }

                // Print result
                echo prStr($result) . "\n";
            }
        } catch (\Throwable $e) {
            $this->printError($e);
        }
    }

    /**
     * Compile and evaluate using the compiler.
     */
    private function compileAndEval(mixed $form): mixed
    {
        if ($this->compiler === null) {
            throw new \Exception("No compiler set");
        }

        // Analyze
        $hir = $this->compiler->analyze($form);

        if ($this->debug) {
            echo "HIR: ";
            print_r($hir);
        }

        // Emit
        $php = $this->compiler->emit($hir);

        if ($this->debug) {
            echo "PHP: $php\n";
        }

        // Eval
        return eval("return $php;");
    }

    /**
     * Direct evaluation (for simple forms without compiler).
     */
    private function directEval(mixed $form): mixed
    {
        // Handle literals
        if ($form === null || is_bool($form) || is_int($form) || is_float($form) || is_string($form)) {
            return $form;
        }

        if ($form instanceof Kw) {
            return $form;
        }

        if ($form instanceof Sym) {
            // Look up in current namespace
            $ns = currentNs();
            if ($ns !== null) {
                $var = $ns->resolve($form);
                if ($var !== null) {
                    return $var->deref();
                }
            }
            throw new \Exception("Unable to resolve symbol: " . $form->name());
        }

        if ($form instanceof Vec) {
            $results = [];
            foreach ($form as $item) {
                $results[] = $this->directEval($item);
            }
            return vec(...$results);
        }

        if ($form instanceof Map) {
            $results = [];
            foreach ($form as $k => $v) {
                $results[] = $this->directEval($k);
                $results[] = $this->directEval($v);
            }
            return hashMap(...$results);
        }

        if ($form instanceof Set) {
            $results = [];
            foreach ($form as $item) {
                $results[] = $this->directEval($item);
            }
            return hashSet(...$results);
        }

        if ($form instanceof ISeq) {
            return $this->evalList($form);
        }

        return $form;
    }

    /**
     * Evaluate a list form (function call).
     */
    private function evalList(ISeq $list): mixed
    {
        $s = seq($list);
        if ($s === null) {
            return plist();
        }

        $first = $s->first();
        $args = $s->next();

        // Handle special forms
        if ($first instanceof Sym) {
            $name = $first->name();
            switch ($name) {
                case 'quote':
                    return $args ? first($args) : null;

                case 'def':
                    $sym = first($args);
                    $val = $this->directEval(second($args));
                    $ns = currentNs() ?? Ns::findOrCreate('user');
                    $var = $ns->intern($sym->name(), $val);
                    return $var;

                case 'if':
                    $test = $this->directEval(first($args));
                    if ($test !== null && $test !== false) {
                        return $this->directEval(second($args));
                    }
                    return $this->directEval(third($args));

                case 'do':
                    $result = null;
                    while ($args !== null) {
                        $result = $this->directEval(first($args));
                        $args = $args->next();
                    }
                    return $result;
            }
        }

        // Regular function call
        $fn = $this->directEval($first);
        $argVals = [];
        while ($args !== null) {
            $argVals[] = $this->directEval(first($args));
            $args = $args->next();
        }

        return invoke($fn, ...$argVals);
    }

    /**
     * Print an error.
     */
    private function printError(\Throwable $e): void
    {
        echo "\033[31mError: " . $e->getMessage() . "\033[0m\n";
        if ($this->debug) {
            echo formatException($e, true) . "\n";
        }
    }

    /**
     * Handle REPL commands.
     */
    private function handleCommand(string $cmd): void
    {
        $parts = explode(' ', $cmd, 2);
        $command = $parts[0];
        $arg = $parts[1] ?? '';

        match ($command) {
            ':quit', ':q', ':exit' => $this->running = false,
            ':help', ':h', ':?' => $this->printHelp(),
            ':debug' => $this->toggleDebug(),
            ':history' => $this->printHistory(),
            ':clear' => $this->clearHistory(),
            ':ns' => $this->showNs($arg),
            ':vars' => $this->showVars($arg),
            ':php' => $this->showPhp($arg),
            ':hir' => $this->showHir($arg),
            default => print("Unknown command: $command. Type :help for help.\n"),
        };
    }

    /**
     * Toggle debug mode.
     */
    private function toggleDebug(): void
    {
        $this->debug = !$this->debug;
        echo "Debug mode: " . ($this->debug ? 'ON' : 'OFF') . "\n";
    }

    /**
     * Show current or switch namespace.
     */
    private function showNs(string $arg): void
    {
        if (trim($arg) === '') {
            $ns = currentNs();
            echo "Current namespace: " . ($ns ? $ns->name() : 'nil') . "\n";
            return;
        }

        $ns = inNs(trim($arg));
        $this->prompt = $ns->name() . '=> ';
        echo "Switched to namespace: " . $ns->name() . "\n";
    }

    /**
     * Show vars in namespace.
     */
    private function showVars(string $arg): void
    {
        $nsName = trim($arg) ?: (currentNs()?->name() ?? 'user');
        $ns = findNs($nsName);
        if ($ns === null) {
            echo "Namespace not found: $nsName\n";
            return;
        }

        echo "Vars in $nsName:\n";
        foreach ($ns->getMappings() as $name => $var) {
            echo "  $name\n";
        }
    }

    /**
     * Show generated PHP for an expression (without evaluating).
     */
    private function showPhp(string $expr): void
    {
        if (trim($expr) === '') {
            echo "Usage: :php (+ 1 2)\n";
            return;
        }

        if ($this->compiler === null) {
            echo "No compiler available.\n";
            return;
        }

        try {
            $reader = new Reader($expr, 'repl');
            $forms = $reader->readAll();
            foreach ($forms as $form) {
                $hir = $this->compiler->analyze($form);
                $php = $this->compiler->emit($hir);
                echo "PHP:\n$php\n";
            }
        } catch (\Throwable $e) {
            $this->printError($e);
        }
    }

    /**
     * Show HIR for an expression.
     */
    private function showHir(string $expr): void
    {
        if (trim($expr) === '') {
            echo "Usage: :hir (+ 1 2)\n";
            return;
        }

        if ($this->compiler === null) {
            echo "No compiler available.\n";
            return;
        }

        try {
            $reader = new Reader($expr, 'repl');
            $forms = $reader->readAll();
            foreach ($forms as $form) {
                $hir = $this->compiler->analyze($form);
                print_r($hir);
            }
        } catch (\Throwable $e) {
            $this->printError($e);
        }
    }

    /**
     * Print history.
     */
    private function printHistory(): void
    {
        foreach ($this->history as $i => $line) {
            echo ($i + 1) . ": " . trim($line) . "\n";
        }
    }

    /**
     * Clear history.
     */
    private function clearHistory(): void
    {
        $this->history = [];
        echo "History cleared.\n";
    }

    /**
     * Print help message.
     */
    private function printHelp(): void
    {
        echo <<<HELP
ClojurePHP REPL Commands:
  :quit, :q, :exit  - Exit the REPL
  :help, :h, :?     - Show this help
  :debug            - Toggle debug mode
  :history          - Show input history
  :clear            - Clear history
  :ns [name]        - Show/switch namespace
  :vars [ns]        - Show vars in namespace
  :php <expr>       - Show generated PHP for expression
  :hir <expr>       - Show HIR for expression

Examples:
  (+ 1 2)           - Arithmetic
  (def x 42)        - Define a var
  (fn [x] (* x x))  - Create a function
  (php/strlen "hi") - Call PHP function

HELP;
    }

    /**
     * Print banner.
     */
    private function printBanner(): void
    {
        echo <<<BANNER
ClojurePHP REPL
Type :help for help, :quit to exit.

BANNER;
    }

    /**
     * Set the prompt.
     */
    public function setPrompt(string $prompt): void
    {
        $this->prompt = $prompt;
    }

    /**
     * Enable/disable debug mode.
     */
    public function setDebug(bool $debug): void
    {
        $this->debug = $debug;
    }
}

// ============================================================
// REPL functions
// ============================================================

/**
 * Start an interactive REPL.
 */
function repl(?object $compiler = null): void
{
    $repl = new Repl();
    if ($compiler !== null) {
        $repl->setCompiler($compiler);
    }
    $repl->run();
}
