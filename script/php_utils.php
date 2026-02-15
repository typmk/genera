<?php
/**
 * PHP utility scripts (consolidated from individual files)
 * Usage: php php_utils.php <command> [args...]
 *   parse-stubs <stubs-path>   - Parse PHP stubs and count functions
 *   copy-ns <source> <target>  - Copy files with Phel→Clojure namespace rename
 */

$cmd = $argv[1] ?? null;

match ($cmd) {
    'parse-stubs' => parseStubs($argv[2] ?? null),
    'copy-ns'     => copyNamespace($argv[2] ?? null, $argv[3] ?? null),
    default       => die("Usage: php php_utils.php <parse-stubs|copy-ns> [args...]\n"),
};

// =============================================================================
// 1. Parse PHP stubs (phpstorm-stubs) and count top-level functions
// =============================================================================

function parseStubs(?string $file): void
{
    if (!$file) die("Usage: php php_utils.php parse-stubs <file>\n");

    require 'vendor/autoload.php';

    $parser = (new PhpParser\ParserFactory())->createForNewestSupportedVersion();
    $code = file_get_contents($file);

    // Workaround: reserved words used as function names in stubs
    $code = preg_replace(
        '/function (exit|die|echo|print|include|require|list|empty|isset|unset|eval)\s*\(/',
        'function _$1(',
        $code
    );

    try {
        $ast = $parser->parse($code);
        $count = 0;
        foreach ($ast as $node) {
            if ($node instanceof PhpParser\Node\Stmt\Function_) {
                $count++;
            }
        }
        echo "OK - parsed successfully. Found $count top-level functions\n";
    } catch (PhpParser\Error $e) {
        echo "Error: " . $e->getMessage() . "\n";
    }
}

// =============================================================================
// 2. Copy directory with Phel\Lang → Clojure\Lang namespace replacement
// =============================================================================

function copyNamespace(?string $source, ?string $target): void
{
    if (!$source || !$target) die("Usage: php php_utils.php copy-ns <source-dir> <target-dir>\n");

    echo "Copying $source → $target with namespace replacement...\n\n";
    copyDirectory($source, $target, []);
    echo "\nDone!\n";
}

function copyWithNamespaceChange(string $source, string $target): void
{
    $content = file_get_contents($source);
    $content = preg_replace('/namespace Phel\\\\Lang\\\\/', 'namespace Clojure\\Lang\\', $content);
    $content = preg_replace('/namespace Phel\\\\Lang;/', 'namespace Clojure\\Lang;', $content);
    $content = preg_replace('/use Phel\\\\Lang\\\\/', 'use Clojure\\Lang\\', $content);
    $content = str_replace('\\Phel\\Lang\\', '\\Clojure\\Lang\\', $content);
    file_put_contents($target, $content);
    echo "Copied: " . basename($source) . "\n";
}

function copyDirectory(string $source, string $target, array $skip): void
{
    if (!is_dir($target)) mkdir($target, 0755, true);

    foreach (new DirectoryIterator($source) as $item) {
        if ($item->isDot()) continue;
        $name = $item->getFilename();
        if (in_array($name, $skip)) { echo "Skipping: $name\n"; continue; }

        if ($item->isDir()) {
            echo "\nEntering: $name/\n";
            copyDirectory($item->getPathname(), "$target/$name", $skip);
        } elseif ($item->getExtension() === 'php') {
            copyWithNamespaceChange($item->getPathname(), "$target/$name");
        }
    }
}
