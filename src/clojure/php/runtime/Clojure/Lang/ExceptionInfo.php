<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Clojure\Lang\Collections\Map\PersistentArrayMap;
use Clojure\Lang\Collections\Map\PersistentMapInterface;
use RuntimeException;
use Throwable;

/**
 * Exception that carries data (a map) as additional payload.
 *
 * ClojurePHP programs that need richer semantics for exceptions should use
 * this in lieu of defining project-specific exception classes.
 *
 * Created via (ex-info msg data) or (ex-info msg data cause).
 */
final class ExceptionInfo extends RuntimeException implements IExceptionInfoInterface
{
    private readonly PersistentMapInterface $data;

    public function __construct(
        string $message,
        ?PersistentMapInterface $data = null,
        ?Throwable $cause = null,
    ) {
        // Find the actual call site by walking up the stack
        // Skip frames from Runtime::exInfo and the constructor
        $trace = debug_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS, 10);
        $callSite = $this->findCallSite($trace);

        if ($callSite !== null) {
            // Override the default file/line with the actual call site
            $this->file = $callSite['file'];
            $this->line = $callSite['line'];
        }

        parent::__construct($message, 0, $cause);
        $this->data = $data ?? PersistentArrayMap::empty(new Hasher(), new Equalizer());
    }

    /**
     * Walk up the stack trace to find the actual call site.
     * Skip frames from ExceptionInfo constructor and Runtime::exInfo.
     */
    private function findCallSite(array $trace): ?array
    {
        foreach ($trace as $frame) {
            $file = $frame['file'] ?? '';
            $class = $frame['class'] ?? '';
            $function = $frame['function'] ?? '';

            // Skip internal frames (Runtime, ExceptionInfo constructor)
            if ($class === 'Cljp\\Runtime' && $function === 'exInfo') {
                continue;
            }
            if ($class === self::class && $function === '__construct') {
                continue;
            }
            // Skip call_user_func frames (they have no file)
            if (empty($file)) {
                continue;
            }
            // Skip vendor and internal PHP frames
            if (str_contains($file, 'vendor/')) {
                continue;
            }

            // Found the actual call site
            if (isset($frame['file']) && isset($frame['line'])) {
                return ['file' => $frame['file'], 'line' => $frame['line']];
            }
        }

        return null;
    }

    public function getData(): PersistentMapInterface
    {
        return $this->data;
    }

    public function __toString(): string
    {
        return 'clojure.lang.ExceptionInfo: ' . $this->getMessage() . ' ' . Printer::readable()->print($this->data);
    }
}
