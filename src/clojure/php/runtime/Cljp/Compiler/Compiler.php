<?php

declare(strict_types=1);

namespace Cljp\Compiler;

use Clojure\Lang\LispReader;

/**
 * ClojurePHP Compiler: orchestrates Read → Analyze → Emit pipeline.
 *
 * This is the main entry point for compiling Clojure source code to PHP.
 */
class Compiler
{
    private Analyzer $analyzer;
    private Emitter $emitter;
    private array $defaultEnv;

    public function __construct()
    {
        $this->analyzer = new Analyzer();
        $this->emitter = new Emitter();
        $this->defaultEnv = HIR::makeEnv(['ns' => 'user']);
    }

    /**
     * Compile a string of Clojure source code to PHP.
     *
     * @param string $source Clojure source code
     * @param array $opts Options: 'ns' (namespace), 'wrap' (wrap in <?php)
     * @return string Generated PHP code
     */
    public function compile(string $source, array $opts = []): string
    {
        $forms = $this->read($source);
        return $this->compileFormsToPhp($forms, $opts);
    }

    /**
     * Read Clojure source code into forms.
     *
     * @param string $source Clojure source code
     * @return array Array of forms
     */
    public function read(string $source): array
    {
        $reader = new LispReader($source);
        return $reader->readAll();
    }

    /**
     * Analyze a single form into HIR.
     *
     * @param mixed $form The form to analyze
     * @param array|null $env Optional environment override
     * @return array HIR node
     */
    public function analyze(mixed $form, ?array $env = null): array
    {
        $env ??= $this->defaultEnv;
        return $this->analyzer->analyze($env, $form);
    }

    /**
     * Emit PHP code from an HIR node.
     *
     * @param array $hir HIR node
     * @return string PHP code
     */
    public function emit(array $hir): string
    {
        return $this->emitter->emit($hir);
    }

    /**
     * Compile forms to PHP code.
     *
     * @param array $forms Array of forms
     * @param array $opts Options
     * @return string PHP code
     */
    public function compileFormsToPhp(array $forms, array $opts = []): string
    {
        $ns = $opts['ns'] ?? 'user';
        $wrap = $opts['wrap'] ?? false;

        $env = HIR::makeEnv(['ns' => $ns]);
        $phpParts = [];

        foreach ($forms as $form) {
            $hir = $this->analyzer->analyze($env, $form);
            // Set statement context for top-level forms
            $hir['env']['context'] = 'statement';
            $php = $this->emitter->emit($hir);
            if (trim($php) !== '') {
                $phpParts[] = $php;
            }
        }

        $code = implode("\n", $phpParts);

        if ($wrap) {
            $code = "<?php\n\n" . $code;
        }

        return $code;
    }

    /**
     * Compile and evaluate Clojure source code.
     *
     * @param string $source Clojure source code
     * @return mixed Result of evaluation
     */
    public function eval(string $source): mixed
    {
        $php = $this->compile($source);
        return eval($php);
    }

    /**
     * Compile a file to PHP.
     *
     * @param string $inputPath Input .cljc file path
     * @param string|null $outputPath Output .php file path (null for stdout)
     * @param array $opts Options
     * @return string|null PHP code if no output path, null otherwise
     */
    public function compileFile(string $inputPath, ?string $outputPath = null, array $opts = []): ?string
    {
        $source = file_get_contents($inputPath);
        if ($source === false) {
            throw new \RuntimeException("Cannot read file: $inputPath");
        }

        $opts['wrap'] = $opts['wrap'] ?? true;
        $php = $this->compile($source, $opts);

        if ($outputPath !== null) {
            file_put_contents($outputPath, $php);
            return null;
        }

        return $php;
    }

    /**
     * Get a fresh compilation environment.
     */
    public function makeEnv(array $opts = []): array
    {
        return HIR::makeEnv($opts);
    }

    /**
     * Get the analyzer instance.
     */
    public function getAnalyzer(): Analyzer
    {
        return $this->analyzer;
    }

    /**
     * Get the emitter instance.
     */
    public function getEmitter(): Emitter
    {
        return $this->emitter;
    }
}
