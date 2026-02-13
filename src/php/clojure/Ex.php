<?php

declare(strict_types=1);

/**
 * Exception handling - ExceptionInfo, error formatting, source maps.
 *
 * Provides rich error formatting with:
 * - Source map integration (PHP line -> Clojure source)
 * - Source context display (show code around error)
 * - ExceptionInfo special handling (ex-data, phase, type, hints)
 * - ANSI colored output
 * - Cause chain printing
 */

namespace Clojure\Php;

/**
 * ExceptionInfo - exception with associated data map.
 */
class ExceptionInfo extends \Exception
{
    private Map $data;

    public function __construct(string $message, Map $data, ?\Throwable $cause = null)
    {
        parent::__construct($message, 0, $cause);
        $this->data = $data;
    }

    public function getData(): Map
    {
        return $this->data;
    }

    public function __toString(): string
    {
        return "ExceptionInfo: {$this->getMessage()} " . prStr($this->data);
    }
}

/**
 * ExceptionHandler - rich exception formatting with source map support.
 */
class ExceptionHandler
{
    private static array $maps = [];
    private static bool $registered = false;

    /**
     * Register as the global exception and error handler.
     */
    public static function register(): void
    {
        if (self::$registered) {
            return;
        }

        // Handle uncaught exceptions
        set_exception_handler([self::class, 'handleException']);

        // Convert PHP errors to ErrorException
        set_error_handler([self::class, 'handleError']);

        // Handle fatal errors on shutdown
        register_shutdown_function([self::class, 'handleShutdown']);

        self::$registered = true;
    }

    /**
     * Handle a PHP error by converting to exception.
     */
    public static function handleError(int $severity, string $message, string $file, int $line): bool
    {
        // Don't handle suppressed errors (@)
        if (!(error_reporting() & $severity)) {
            return false;
        }

        throw new \ErrorException($message, 0, $severity, $file, $line);
    }

    /**
     * Handle fatal errors on shutdown.
     */
    public static function handleShutdown(): void
    {
        $error = error_get_last();
        if ($error !== null && in_array($error['type'], [E_ERROR, E_PARSE, E_CORE_ERROR, E_COMPILE_ERROR])) {
            $e = new \ErrorException($error['message'], 0, $error['type'], $error['file'], $error['line']);
            self::handleException($e);
        }
    }

    /**
     * Handle an exception with rich formatting.
     */
    public static function handleException(\Throwable $e): void
    {
        // Output to stderr if available
        $output = defined('STDERR') ? STDERR : fopen('php://stderr', 'w');
        fwrite($output, self::format($e));
        exit(1);
    }

    /**
     * Format an exception for display (without exiting).
     */
    public static function format(\Throwable $e): string
    {
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
        $blue = "\033[34m";

        $out = '';

        // Determine error type from ExceptionInfo data
        $isExInfo = $e instanceof ExceptionInfo;
        $exData = $isExInfo ? $e->getData() : null;
        $phase = self::getErrorPhase($exData);
        $errorType = self::getErrorType($exData);

        // Header
        if ($isExInfo) {
            $typeLabel = $errorType ? self::formatErrorType($errorType) : 'ExceptionInfo';
            $out .= "\n{$red}{$bold}{$typeLabel}:{$reset} {$msg}\n";
        } else {
            $exceptionClass = (new \ReflectionClass($e))->getShortName();
            $out .= "\n{$red}{$bold}{$exceptionClass}:{$reset} {$msg}\n";
        }

        // Location with phase info
        $phaseInfo = $phase ? " ({$phase})" : "";
        $out .= "{$gray}in {$file}:{$line}" . ($col ? ":{$col}" : "") . "{$phaseInfo}{$reset}\n";

        // Source context (if we can read the source file)
        $context = self::getSourceContext($file, $line);
        if ($context) {
            $out .= "\n{$context}\n";
        }

        // Show the form if available
        if ($form) {
            $out .= "\n{$gray}Form: {$cyan}{$form}{$reset}\n";
        }

        // Show ex-data if this is an ExceptionInfo
        if ($exData !== null && count($exData) > 0) {
            $out .= "\n{$bold}Data:{$reset}\n";
            $out .= self::formatExData($exData);
        }

        // Show hint if available
        $hint = self::getFromData($exData, 'cljp.error/hint');
        if ($hint) {
            $out .= "\n{$blue}Hint:{$reset} {$hint}\n";
        }

        // Stack trace
        $out .= "\n{$bold}Stack:{$reset}\n";

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

            $out .= "  {$gray}{$i}.{$reset} {$location}{$formInfo}\n";
        }

        // Show cause chain if present
        $cause = $e->getPrevious();
        if ($cause) {
            $out .= "\n{$bold}Caused by:{$reset}\n";
            $out .= self::formatCause($cause, 1);
        }

        return $out;
    }

    /**
     * Get the error phase from ex-data.
     */
    private static function getErrorPhase(?Map $exData): ?string
    {
        $phase = self::getFromData($exData, 'cljp.error/phase');
        if ($phase instanceof Kw) {
            return $phase->name();
        }
        return $phase;
    }

    /**
     * Get the error type from ex-data.
     */
    private static function getErrorType(?Map $exData): ?string
    {
        $type = self::getFromData($exData, 'cljp.error/type');
        if ($type instanceof Kw) {
            return $type->name();
        }
        return $type;
    }

    /**
     * Format error type for display.
     * :arity -> ArityError, :bounds -> BoundsError
     */
    private static function formatErrorType(string $type): string
    {
        return ucfirst($type) . 'Error';
    }

    /**
     * Get a value from ex-data by namespaced keyword name.
     */
    private static function getFromData(?Map $exData, string $keyName): mixed
    {
        if ($exData === null) {
            return null;
        }
        $key = kw($keyName);
        return $exData->find($key);
    }

    /**
     * Format ex-data for display.
     */
    private static function formatExData(Map $exData): string
    {
        $gray = "\033[90m";
        $cyan = "\033[36m";
        $reset = "\033[0m";

        $out = '';
        foreach ($exData as $key => $value) {
            $keyStr = prStr($key);
            $valStr = prStr($value);

            // Truncate long values
            if (strlen($valStr) > 80) {
                $valStr = substr($valStr, 0, 77) . '...';
            }

            $out .= "  {$cyan}{$keyStr}{$reset} {$valStr}\n";
        }
        return $out;
    }

    /**
     * Format a cause in the chain.
     */
    private static function formatCause(\Throwable $e, int $depth): string
    {
        $gray = "\033[90m";
        $red = "\033[31m";
        $reset = "\033[0m";
        $indent = str_repeat('  ', $depth);

        $className = (new \ReflectionClass($e))->getShortName();
        $out = "{$indent}{$red}{$className}:{$reset} {$e->getMessage()}\n";
        $out .= "{$indent}{$gray}at {$e->getFile()}:{$e->getLine()}{$reset}\n";

        $prev = $e->getPrevious();
        if ($prev && $depth < 5) {  // Limit depth
            $out .= self::formatCause($prev, $depth + 1);
        }

        return $out;
    }

    /**
     * Get source context around a line.
     */
    private static function getSourceContext(string $file, int $line, int $context = 2): ?string
    {
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

    /**
     * Load a source map for a PHP file.
     */
    private static function loadMap(string $phpFile): void
    {
        if (isset(self::$maps[$phpFile])) {
            return;
        }

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

    /**
     * Map a PHP line to Clojure source location.
     */
    private static function mapLocation(string $phpFile, int $phpLine): ?array
    {
        if (empty(self::$maps[$phpFile])) {
            return null;
        }

        // Exact match preferred
        if (isset(self::$maps[$phpFile][$phpLine])) {
            return self::$maps[$phpFile][$phpLine];
        }

        // Fallback: scan backwards up to 5 lines
        for ($i = 0; $i < 5; $i++) {
            if (isset(self::$maps[$phpFile][$phpLine - $i])) {
                return self::$maps[$phpFile][$phpLine - $i];
            }
        }

        return null;
    }
}

// ============================================================
// Exception functions
// ============================================================

/**
 * Create an ExceptionInfo.
 */
function exInfo(string $msg, mixed $data, ?\Throwable $cause = null): ExceptionInfo
{
    $map = $data instanceof Map ? $data : hashMap();
    return new ExceptionInfo($msg, $map, $cause);
}

/**
 * Get the data map from an ExceptionInfo.
 */
function exData(mixed $ex): ?Map
{
    if ($ex instanceof ExceptionInfo) {
        return $ex->getData();
    }
    return null;
}

/**
 * Get the message from an exception.
 */
function exMessage(mixed $ex): ?string
{
    if ($ex instanceof \Throwable) {
        return $ex->getMessage();
    }
    return null;
}

/**
 * Get the cause from an exception.
 */
function exCause(mixed $ex): ?\Throwable
{
    if ($ex instanceof \Throwable) {
        return $ex->getPrevious();
    }
    return null;
}

/**
 * Throw an exception.
 */
function throw_(mixed $ex): never
{
    if ($ex instanceof \Throwable) {
        throw $ex;
    }
    throw new \Exception((string)$ex);
}

/**
 * Get the stack trace as an array.
 */
function stackTrace(\Throwable $ex): array
{
    return $ex->getTrace();
}

/**
 * Format an exception for display (rich formatting with source maps).
 */
function formatException(\Throwable $ex, bool $includeTrace = true): string
{
    return ExceptionHandler::format($ex);
}

/**
 * Register the global exception handler.
 */
function registerExceptionHandler(): void
{
    ExceptionHandler::register();
}
