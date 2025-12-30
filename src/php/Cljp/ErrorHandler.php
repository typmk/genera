<?php

namespace Cljp;

use Clojure\Lang\ExceptionInfo;
use Clojure\Lang\IExceptionInfoInterface;
use Clojure\Lang\Printer;
use Clojure\Lang\Keyword;

class ErrorHandler {
    private static $maps = [];

    public static function register() {
        set_exception_handler([self::class, 'handle']);
    }

    public static function handle(\Throwable $e) {
        $phpFile = $e->getFile();
        $phpLine = $e->getLine();

        self::loadMap($phpFile);
        $loc = self::mapLocation($phpFile, $phpLine);

        $msg = $e->getMessage();
        $file = $loc ? $loc['file'] : $phpFile;
        $line = $loc ? $loc['clj_line'] : $phpLine;
        $col = ($loc && isset($loc['clj_col'])) ? $loc['clj_col'] : null;
        $form = ($loc && isset($loc['form'])) ? $loc['form'] : null;

        // ANSI Colors
        $red = "\033[31m";
        $bold = "\033[1m";
        $reset = "\033[0m";
        $gray = "\033[90m";
        $cyan = "\033[36m";
        $yellow = "\033[33m";
        $blue = "\033[34m";
        $magenta = "\033[35m";

        // Determine error type and phase from ExceptionInfo data
        $isExInfo = $e instanceof IExceptionInfoInterface;
        $exData = $isExInfo ? $e->getData() : null;
        $phase = self::getErrorPhase($exData);
        $errorType = self::getErrorType($exData);

        // Header - show exception type for ExceptionInfo
        if ($isExInfo) {
            $typeLabel = $errorType ? self::formatErrorType($errorType) : 'ExceptionInfo';
            echo "\n{$red}{$bold}{$typeLabel}:{$reset} {$msg}\n";
        } else {
            $exceptionClass = (new \ReflectionClass($e))->getShortName();
            echo "\n{$red}{$bold}{$exceptionClass}:{$reset} {$msg}\n";
        }

        // Location with phase info
        $phaseInfo = $phase ? " ({$phase})" : "";
        echo "{$gray}in {$file}:{$line}" . ($col ? ":{$col}" : "") . "{$phaseInfo}{$reset}\n";

        // Source context (if we can read the source file)
        $context = self::getSourceContext($file, $line);
        if ($context) {
            echo "\n{$context}\n";
        }

        // Show the form if available
        if ($form) {
            echo "\n{$gray}Form: {$cyan}{$form}{$reset}\n";
        }

        // Show ex-data if this is an ExceptionInfo
        if ($exData !== null && count($exData) > 0) {
            echo "\n{$bold}Data:{$reset}\n";
            self::printExData($exData);
        }

        // Show hint if available
        $hint = self::getFromData($exData, 'cljp.error/hint');
        if ($hint) {
            echo "\n{$blue}Hint:{$reset} {$hint}\n";
        }

        // Stack trace
        echo "\n{$bold}Stack:{$reset}\n";

        $trace = $e->getTrace();
        foreach ($trace as $i => $frame) {
            $frameFile = $frame['file'] ?? null;
            $frameLine = $frame['line'] ?? null;
            $frameForm = null;

            if ($frameFile) {
                self::loadMap($frameFile);
                $frameLoc = self::mapLocation($frameFile, $frameLine);

                if ($frameLoc) {
                    $frameFile = $frameLoc['file'];
                    $frameLine = $frameLoc['clj_line'];
                    $frameForm = $frameLoc['form'] ?? null;
                }
            }

            $location = "{$frameFile}:{$frameLine}";
            $formInfo = $frameForm ? " {$gray}{$frameForm}{$reset}" : "";

            echo "  {$gray}{$i}.{$reset} {$location}{$formInfo}\n";
        }

        // Show cause chain if present
        $cause = $e->getPrevious();
        if ($cause) {
            echo "\n{$bold}Caused by:{$reset}\n";
            self::printCause($cause, 1);
        }

        exit(1);
    }

    private static function getErrorPhase($exData): ?string {
        $phase = self::getFromData($exData, 'cljp.error/phase');
        if ($phase instanceof Keyword) {
            return $phase->getName();
        }
        return $phase;
    }

    private static function getErrorType($exData): ?string {
        $type = self::getFromData($exData, 'cljp.error/type');
        if ($type instanceof Keyword) {
            return $type->getName();
        }
        return $type;
    }

    private static function formatErrorType(string $type): string {
        // Convert :arity -> ArityError, :bounds -> BoundsError, etc.
        return ucfirst($type) . 'Error';
    }

    private static function getFromData($exData, string $keyName) {
        if ($exData === null) {
            return null;
        }
        // Try namespaced keyword first
        $key = Keyword::create($keyName);
        $value = $exData->find($key);
        return $value;
    }

    private static function printExData($exData): void {
        $gray = "\033[90m";
        $cyan = "\033[36m";
        $reset = "\033[0m";

        foreach ($exData as $key => $value) {
            $keyStr = Printer::readable()->print($key);
            $valStr = Printer::readable()->print($value);

            // Truncate long values
            if (strlen($valStr) > 80) {
                $valStr = substr($valStr, 0, 77) . '...';
            }

            echo "  {$cyan}{$keyStr}{$reset} {$valStr}\n";
        }
    }

    private static function printCause(\Throwable $e, int $depth): void {
        $gray = "\033[90m";
        $red = "\033[31m";
        $reset = "\033[0m";
        $indent = str_repeat('  ', $depth);

        $className = (new \ReflectionClass($e))->getShortName();
        echo "{$indent}{$red}{$className}:{$reset} {$e->getMessage()}\n";
        echo "{$indent}{$gray}at {$e->getFile()}:{$e->getLine()}{$reset}\n";

        $prev = $e->getPrevious();
        if ($prev && $depth < 5) {  // Limit depth to prevent infinite loops
            self::printCause($prev, $depth + 1);
        }
    }

    private static function getSourceContext(string $file, int $line, int $context = 2): ?string {
        if (!file_exists($file)) {
            return null;
        }

        $lines = file($file);
        if (!$lines) {
            return null;
        }

        $start = max(0, $line - $context - 1);
        $end = min(count($lines), $line + $context);

        $result = [];
        $gray = "\033[90m";
        $reset = "\033[0m";
        $yellow = "\033[33m";
        $bold = "\033[1m";

        for ($i = $start; $i < $end; $i++) {
            $lineNum = $i + 1;
            $content = rtrim($lines[$i]);

            if ($lineNum == $line) {
                // Highlight the error line
                $result[] = sprintf("{$yellow}{$bold} >> %3d{$reset}{$gray}|{$reset} %s", $lineNum, $content);
            } else {
                $result[] = sprintf("{$gray}    %3d| %s{$reset}", $lineNum, $content);
            }
        }

        return implode("\n", $result);
    }

    private static function loadMap($phpFile) {
        if (isset(self::$maps[$phpFile])) return;

        $dir = dirname($phpFile);
        $base = basename($phpFile);
        $jsonPath = $dir . '/.xdebug/' . $base . '.json';

        if (file_exists($jsonPath)) {
            $json = json_decode(file_get_contents($jsonPath), true);
            if ($json) {
                // Index by php_line for fast lookup
                $indexed = [];
                foreach ($json as $entry) {
                    $indexed[$entry['php_line']] = $entry;
                }
                self::$maps[$phpFile] = $indexed;
            }
        } else {
            self::$maps[$phpFile] = false; // Mark as not found
        }
    }

    private static function mapLocation($phpFile, $phpLine) {
        if (empty(self::$maps[$phpFile])) return null;
        
        // Exact match preferred
        if (isset(self::$maps[$phpFile][$phpLine])) {
            return self::$maps[$phpFile][$phpLine];
        }
        
        // Fallback: Find closest line (handling multi-line PHP statements)?
        // For now, return null to be safe, or scan backwards?
        // Let's scan backwards up to 5 lines
        for ($i = 0; $i < 5; $i++) {
            if (isset(self::$maps[$phpFile][$phpLine - $i])) {
                return self::$maps[$phpFile][$phpLine - $i];
            }
        }
        
        return null;
    }
}
