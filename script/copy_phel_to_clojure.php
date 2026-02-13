<?php
/**
 * Copy Phel Lang files to Clojure\Lang with namespace replacement
 */

$sourceBase = 'C:/GitHub/ClojurePHP/vendor/phel-lang/phel-lang/src/php/Lang';
$targetBase = 'C:/GitHub/ClojurePHP/src/php/Clojure/Lang';

// Files/directories to skip (we have our own implementations or don't need)
$skip = [];

function copyWithNamespaceChange(string $source, string $target): void {
    $content = file_get_contents($source);

    // Replace namespace declarations like: namespace Phel\Lang\Collections\Vector;
    $content = preg_replace(
        '/namespace Phel\\\\Lang\\\\/',
        'namespace Clojure\\Lang\\',
        $content
    );
    $content = preg_replace(
        '/namespace Phel\\\\Lang;/',
        'namespace Clojure\\Lang;',
        $content
    );

    // Replace use statements like: use Phel\Lang\Something;
    $content = preg_replace(
        '/use Phel\\\\Lang\\\\/',
        'use Clojure\\Lang\\',
        $content
    );

    // Replace fully qualified references in code like: \Phel\Lang\Something
    $content = str_replace('\\Phel\\Lang\\', '\\Clojure\\Lang\\', $content);

    file_put_contents($target, $content);
    echo "Copied: " . basename($source) . "\n";
}

function copyDirectory(string $source, string $target, array $skip): void {
    if (!is_dir($target)) {
        mkdir($target, 0755, true);
    }

    $iterator = new DirectoryIterator($source);
    foreach ($iterator as $item) {
        if ($item->isDot()) continue;

        $name = $item->getFilename();
        if (in_array($name, $skip)) {
            echo "Skipping: $name\n";
            continue;
        }

        $sourcePath = $item->getPathname();
        $targetPath = $target . '/' . $name;

        if ($item->isDir()) {
            echo "\nEntering: $name/\n";
            copyDirectory($sourcePath, $targetPath, $skip);
        } else if ($item->getExtension() === 'php') {
            copyWithNamespaceChange($sourcePath, $targetPath);
        }
    }
}

echo "Copying Phel\\Lang to Clojure\\Lang...\n\n";
copyDirectory($sourceBase, $targetBase, $skip);
echo "\nDone!\n";
