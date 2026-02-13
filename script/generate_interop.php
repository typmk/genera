<?php

require 'vendor/autoload.php';

use PhpParser\Error;
use PhpParser\Node;
use PhpParser\NodeTraverser;
use PhpParser\NodeVisitorAbstract;
use PhpParser\ParserFactory;

$stubsPath = 'c:/GitHub/phpstorm-stubs';

// Load ALL stubs - complete coverage
// Exclude: tests, meta directories, non-stub folders
$excludeDirs = ['tests', 'meta', '.git', 'vendor', '.github'];

$targetDirs = array_filter(
    scandir($stubsPath),
    fn($d) => is_dir("$stubsPath/$d")
              && $d[0] !== '.'
              && !in_array($d, $excludeDirs)
);

$parser = (new ParserFactory())->createForNewestSupportedVersion();

$visitor = new class extends NodeVisitorAbstract {
    public array $specs = [
        'functions' => [],
        'classes' => []
    ];
    
    private ?string $currentClass = null;

    public function enterNode(Node $node) {
        if ($node instanceof Node\Stmt\ClassLike) {
            $this->currentClass = $node->name->toString();
            $this->specs['classes'][$this->currentClass] = ['methods' => []];
        }

        if ($node instanceof Node\Stmt\Function_) {
            $this->processFunction($node);
        } elseif ($node instanceof Node\Stmt\ClassMethod && $this->currentClass) {
            $this->processMethod($node);
        }
    }
    
    public function leaveNode(Node $node) {
        if ($node instanceof Node\Stmt\ClassLike) {
            $this->currentClass = null;
        }
    }

    private function processFunction(Node\Stmt\Function_ $node) {
        $name = $node->name->toString();
        $spec = [
            'params' => $this->mapParams($node->params),
            'return' => $this->typeToString($node->returnType)
        ];
        if ($this->hasPureAttribute($node)) {
            $spec['pure'] = true;
        }
        $this->specs['functions'][$name] = $spec;
    }

    private function hasPureAttribute(Node $node): bool {
        foreach ($node->attrGroups as $attrGroup) {
            foreach ($attrGroup->attrs as $attr) {
                $attrName = $attr->name->toString();
                if ($attrName === 'Pure' || $attrName === 'JetBrains\\PhpStorm\\Pure') {
                    return true;
                }
            }
        }
        return false;
    }

    private function processMethod(Node\Stmt\ClassMethod $node) {
        $name = $node->name->toString();
        $this->specs['classes'][$this->currentClass]['methods'][$name] = [
            'params' => $this->mapParams($node->params),
            'return' => $this->typeToString($node->returnType),
            'static' => $node->isStatic()
        ];
    }

    private function mapParams(array $params): array {
        $out = [];
        foreach ($params as $param) {
            $out[] = [
                'name' => $param->var->name,
                'type' => $this->typeToString($param->type),
                'variadic' => $param->variadic,
                'optional' => $param->default !== null
            ];
        }
        return $out;
    }

    private function typeToString($type) {
        if ($type === null) return 'mixed';
        if ($type instanceof Node\NullableType) {
            return '?' . $this->typeToString($type->type);
        }
        if ($type instanceof Node\UnionType) {
            return implode('|', array_map([$this, 'typeToString'], $type->types));
        }
        if ($type instanceof Node\Identifier) {
            return $type->toString();
        }
        if ($type instanceof Node\Name) {
            return $type->toString();
        }
        return (string)$type;
    }
};

$traverser = new NodeTraverser();
$traverser->addVisitor($visitor);

foreach ($targetDirs as $dir) {
    if (!is_dir("$stubsPath/$dir")) {
        echo "Warning: Directory $dir not found in $stubsPath\n";
        continue;
    }
    
    $iterator = new RecursiveIteratorIterator(
        new RecursiveDirectoryIterator("$stubsPath/$dir")
    );

    foreach ($iterator as $file) {
        if ($file->getExtension() !== 'php') continue;

        try {
            $code = file_get_contents($file->getPathname());

            // Workaround: PHP keywords used as function names in stubs
            // Replace 'function exit(' with 'function _exit(' etc. to allow parsing
            // These are language constructs that can't be function names in PHP 8
            $keywords = 'exit|die|echo|print|include|require|list|empty|isset|unset|eval|clone|new|array|class|interface|trait|extends|implements|static|abstract|final|public|private|protected|const|var|function|return|if|else|elseif|while|do|for|foreach|switch|case|default|break|continue|goto|throw|try|catch|finally|namespace|use|global|as|instanceof';
            $code = preg_replace('/function (' . $keywords . ')\s*\(/i', 'function _$1(', $code);

            $ast = $parser->parse($code);
            $traverser->traverse($ast);
        } catch (Error $error) {
            fprintf(STDERR, "Parse error in {$file->getFilename()}: {$error->getMessage()}\n");
        }
    }
}

echo json_encode($visitor->specs, JSON_PRETTY_PRINT);

