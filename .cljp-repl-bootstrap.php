<?php
declare(strict_types=0);

require __DIR__ . '/vendor/autoload.php';
\Cljp\ErrorHandler::register();

// REPL state - result history
$GLOBALS['__cljp_1'] = null;  // *1 - last result
$GLOBALS['__cljp_2'] = null;  // *2 - second to last
$GLOBALS['__cljp_3'] = null;  // *3 - third to last
$GLOBALS['__cljp_e'] = null;  // *e - last exception

// Current namespace (for future use)
$GLOBALS['__cljp_ns'] = 'user';

// Helper to update result history
function __cljp_push_result($value) {
    $GLOBALS['__cljp_3'] = $GLOBALS['__cljp_2'];
    $GLOBALS['__cljp_2'] = $GLOBALS['__cljp_1'];
    $GLOBALS['__cljp_1'] = $value;
}

// Helper to set last exception
function __cljp_set_exception($e) {
    $GLOBALS['__cljp_e'] = $e;
}

// Signal ready
echo "__CLJP_READY__\n";
fflush(STDOUT);

// REPL loop - read eval requests from stdin
while (true) {
    $line = fgets(STDIN);
    if ($line === false) {
        break;
    }
    $line = trim($line);
    if ($line === '') {
        continue;
    }

    // Protocol: JSON-encoded request
    // {"type": "eval", "file": "/path/to/temp.php"}
    // {"type": "exit"}
    $request = json_decode($line, true);
    if ($request === null) {
        echo json_encode(['error' => 'Invalid JSON: ' . $line]) . "\n";
        fflush(STDOUT);
        continue;
    }

    $type = $request['type'] ?? 'unknown';

    if ($type === 'exit') {
        echo "__CLJP_EXIT__\n";
        fflush(STDOUT);
        break;
    }

    if ($type === 'eval') {
        $file = $request['file'] ?? null;
        if (!$file || !file_exists($file)) {
            echo json_encode(['error' => 'File not found: ' . $file]) . "\n";
            fflush(STDOUT);
            continue;
        }

        try {
            // Include the file and capture the last expression value
            $__cljp_result = require $file;

            // Update result history
            __cljp_push_result($__cljp_result);

            // Return result as EDN
            $resultStr = \Cljp\Runtime::prStr($__cljp_result);
            echo json_encode(['result' => $resultStr]) . "\n";
            fflush(STDOUT);

        } catch (\Throwable $e) {
            // Store exception
            __cljp_set_exception($e);

            // Format error using our error handler
            $msg = $e->getMessage();
            $file = $e->getFile();
            $line = $e->getLine();

            // Try to get source-mapped location
            $errorInfo = \Cljp\ErrorHandler::formatException($e);

            echo json_encode([
                'error' => $msg,
                'formatted' => $errorInfo,
                'class' => get_class($e)
            ]) . "\n";
            fflush(STDOUT);
        }
        continue;
    }

    echo json_encode(['error' => 'Unknown request type: ' . $type]) . "\n";
    fflush(STDOUT);
}
