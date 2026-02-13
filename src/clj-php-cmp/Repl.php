<?php

declare(strict_types=1);

namespace Cljp;

use Cljp\Compiler\Compiler;
use Clojure\Lang\Printer;

/**
 * ClojurePHP REPL: Read-Eval-Print Loop.
 *
 * Provides an interactive environment for evaluating Clojure expressions.
 */
class Repl
{
    private Compiler $compiler;
    private bool $running = false;
    private string $prompt = 'user=> ';
    private array $history = [];
    private bool $debug = false;

    public function __construct()
    {
        $this->compiler = new Compiler();
    }

    /**
     * Start the REPL.
     */
    public function run(): void
    {
        $this->running = true;
        $this->printBanner();

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
     * Read input from stdin.
     */
    private function readInput(): ?string
    {
        echo $this->prompt;
        $input = fgets(STDIN);
        if ($input === false) {
            return null;
        }
        return $input;
    }

    /**
     * Evaluate and print a Clojure expression.
     */
    private function evalPrint(string $input): void
    {
        try {
            // Read
            $forms = $this->compiler->read($input);
            if (empty($forms)) {
                return;
            }

            $this->history[] = $input;

            foreach ($forms as $form) {
                // Analyze
                $hir = $this->compiler->analyze($form);

                if ($this->debug) {
                    echo "HIR:\n";
                    print_r($hir);
                }

                // Emit
                $php = $this->compiler->emit($hir);

                if ($this->debug) {
                    echo "PHP: $php\n";
                }

                // Eval
                $result = eval("return $php;");

                // Print
                $this->printResult($result);
            }
        } catch (\Throwable $e) {
            $this->printError($e);
        }
    }

    /**
     * Print a result value.
     */
    private function printResult(mixed $result): void
    {
        if (class_exists(Printer::class)) {
            echo Printer::prStr($result, true) . "\n";
        } else {
            $this->simplePrint($result);
        }
    }

    /**
     * Simple fallback printer.
     */
    private function simplePrint(mixed $result): void
    {
        match (true) {
            $result === null => print("nil\n"),
            $result === true => print("true\n"),
            $result === false => print("false\n"),
            is_string($result) => print("\"" . addslashes($result) . "\"\n"),
            is_int($result), is_float($result) => print($result . "\n"),
            is_array($result) => print("[" . implode(' ', array_map(fn($x) => $this->formatValue($x), $result)) . "]\n"),
            is_object($result) && method_exists($result, '__toString') => print((string)$result . "\n"),
            default => print(var_export($result, true) . "\n"),
        };
    }

    private function formatValue(mixed $val): string
    {
        return match (true) {
            $val === null => 'nil',
            $val === true => 'true',
            $val === false => 'false',
            is_string($val) => "\"" . addslashes($val) . "\"",
            is_int($val), is_float($val) => (string)$val,
            default => var_export($val, true),
        };
    }

    /**
     * Print an error.
     */
    private function printError(\Throwable $e): void
    {
        echo "\033[31mError: " . $e->getMessage() . "\033[0m\n";
        if ($this->debug) {
            echo $e->getTraceAsString() . "\n";
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
            ':php' => $this->showPhp($arg),
            ':hir' => $this->showHir($arg),
            default => echo "Unknown command: $command. Type :help for help.\n",
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
     * Show PHP output for an expression without evaluating.
     */
    private function showPhp(string $expr): void
    {
        if (trim($expr) === '') {
            echo "Usage: :php (+ 1 2)\n";
            return;
        }

        try {
            $php = $this->compiler->compile($expr);
            echo "PHP:\n$php\n";
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

        try {
            $forms = $this->compiler->read($expr);
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
            echo ($i + 1) . ": $line\n";
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
