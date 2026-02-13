<?php
require 'vendor/autoload.php';

$parser = (new PhpParser\ParserFactory())->createForNewestSupportedVersion();

$code = file_get_contents('c:/GitHub/phpstorm-stubs/Core/Core.php');

// Apply workaround
$code = preg_replace('/function (exit|die|echo|print|include|require|list|empty|isset|unset|eval)\s*\(/', 'function _$1(', $code);

// Debug: check if workaround applied
if (strpos($code, 'function _exit(') !== false) {
    echo "Workaround applied for exit\n";
} else {
    echo "WARNING: exit not found/replaced\n";
}

try {
    $ast = $parser->parse($code);
    echo "OK - parsed successfully\n";

    // Count functions
    $count = 0;
    foreach ($ast as $node) {
        if ($node instanceof PhpParser\Node\Stmt\Function_) {
            $count++;
        }
    }
    echo "Found $count top-level functions\n";
} catch (PhpParser\Error $e) {
    echo "Error: " . $e->getMessage() . "\n";
}
