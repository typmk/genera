<?php

declare(strict_types=1);

/**
 * Lisp Reader for ClojurePHP.
 *
 * Reads Clojure source code and returns data structures.
 */

namespace Clojure\Php;

/**
 * Exception thrown when reading fails.
 */
class ReaderException extends \Exception
{
    public readonly string $sourceFile;
    public readonly int $sourceLine;
    public readonly int $sourceColumn;

    public function __construct(
        string $message,
        string $file,
        int $line,
        int $column,
        ?\Throwable $previous = null
    ) {
        $this->sourceFile = $file;
        $this->sourceLine = $line;
        $this->sourceColumn = $column;
        parent::__construct("$file:$line:$column: $message", 0, $previous);
    }
}

/**
 * Lisp Reader - parses Clojure source into data structures.
 */
class Reader
{
    private string $text;
    private int $pos = 0;
    private int $line = 1;
    private int $col = 1;
    private string $file;
    private array $features = [];
    private mixed $eofValue = null;
    private bool $eofIsError = true;
    private array $gensyms = [];
    private array $argEnv = [];
    private int $nextId = 0;

    private const MACROS = [
        '"' => 'readString',
        ';' => 'readComment',
        "'" => 'readQuote',
        '@' => 'readDeref',
        '^' => 'readMeta',
        '`' => 'readSyntaxQuote',
        '~' => 'readUnquote',
        '(' => 'readList',
        ')' => 'readUnmatched',
        '[' => 'readVector',
        ']' => 'readUnmatched',
        '{' => 'readMap',
        '}' => 'readUnmatched',
        '\\' => 'readChar',
        '%' => 'readArg',
        '#' => 'readDispatch',
    ];

    private const DISPATCH_MACROS = [
        "'" => 'readVar',
        '"' => 'readRegex',
        '(' => 'readFn',
        '{' => 'readSet',
        '_' => 'readDiscard',
        '?' => 'readConditional',
        '!' => 'readComment',
        ':' => 'readNamespacedMap',
        '#' => 'readSymbolicValue',
        '^' => 'readMeta',
    ];

    public function __construct(string $text, string $file = 'unknown', array $opts = [])
    {
        $this->text = str_replace(["\r\n", "\r"], "\n", $text);
        $this->file = $file;
        $this->features = $opts['features'] ?? ['cljp', 'php'];
        if (array_key_exists('eof', $opts)) {
            $this->eofValue = $opts['eof'];
            $this->eofIsError = false;
        }
    }

    private function peek(int $ahead = 0): ?string
    {
        $p = $this->pos + $ahead;
        return $p < strlen($this->text) ? $this->text[$p] : null;
    }

    private function next(): ?string
    {
        if ($this->pos >= strlen($this->text)) return null;
        $ch = $this->text[$this->pos++];
        if ($ch === "\n") { $this->line++; $this->col = 1; }
        else { $this->col++; }
        return $ch;
    }

    private function eof(): bool
    {
        return $this->pos >= strlen($this->text);
    }

    private function skipWhitespace(): void
    {
        while (!$this->eof()) {
            $ch = $this->peek();
            if ($ch === ' ' || $ch === "\t" || $ch === "\n" || $ch === ',') {
                $this->next();
            } else {
                break;
            }
        }
    }

    private function error(string $msg): ReaderException
    {
        return new ReaderException($msg, $this->file, $this->line, $this->col);
    }

    public function read(): mixed
    {
        $this->skipWhitespace();
        if ($this->eof()) {
            if ($this->eofIsError) throw $this->error("EOF while reading");
            return $this->eofValue;
        }

        $ch = $this->peek();

        if (isset(self::MACROS[$ch])) {
            $this->next();
            $result = $this->{self::MACROS[$ch]}();
            if ($result === $this) return $this->read();
            return $result;
        }

        if ($this->isDigit($ch) || (($ch === '+' || $ch === '-') && $this->isDigit($this->peek(1)))) {
            return $this->readNumber();
        }

        $token = $this->readToken();
        return $this->interpretToken($token);
    }

    public function readAll(): array
    {
        $forms = [];
        while (true) {
            $this->skipWhitespace();
            if ($this->eof()) break;
            $forms[] = $this->read();
        }
        return $forms;
    }

    private function readToken(): string
    {
        $sb = '';
        while (!$this->eof()) {
            $ch = $this->peek();
            if ($this->isWhitespace($ch) || $this->isTerminatingMacro($ch)) break;
            $sb .= $this->next();
        }
        return $sb;
    }

    private function isWhitespace(string $ch): bool
    {
        return $ch === ' ' || $ch === "\t" || $ch === "\n" || $ch === ',';
    }

    private function isDigit(?string $ch): bool
    {
        return $ch !== null && $ch >= '0' && $ch <= '9';
    }

    private function isTerminatingMacro(?string $ch): bool
    {
        return $ch !== null && isset(self::MACROS[$ch]) && $ch !== '#' && $ch !== "'" && $ch !== '%';
    }

    private function interpretToken(string $token): mixed
    {
        return match ($token) {
            'nil' => null,
            'true' => true,
            'false' => false,
            default => $this->matchSymbolOrKeyword($token),
        };
    }

    private function matchSymbolOrKeyword(string $s): Sym|Kw
    {
        if ($s === '') throw $this->error("Invalid empty token");
        if ($s[0] === ':') return $this->parseKeyword(substr($s, 1));
        return $this->parseSymbol($s);
    }

    private function parseKeyword(string $s): Kw
    {
        if ($s === '') throw $this->error("Invalid keyword :");
        if ($s[0] === ':') {
            // Auto-resolved keyword ::foo
            $s = substr($s, 1);
            if (str_contains($s, '/')) {
                [$ns, $name] = explode('/', $s, 2);
                return Kw::createNs($ns, $name);
            }
            return Kw::create($s);
        }
        if (str_contains($s, '/')) {
            [$ns, $name] = explode('/', $s, 2);
            return Kw::createNs($ns, $name);
        }
        return Kw::create($s);
    }

    private function parseSymbol(string $s): Sym
    {
        if ($s === '/') return Sym::create('/');
        if (str_contains($s, '/')) {
            [$ns, $name] = explode('/', $s, 2);
            return Sym::createNs($ns, $name);
        }
        return Sym::create($s);
    }

    private function readNumber(): int|float|string
    {
        $token = $this->readToken();
        return $this->matchNumber($token);
    }

    private function matchNumber(string $s): int|float|string
    {
        // Integer
        if (preg_match('/^[-+]?(?:0|[1-9][0-9]*)$/', $s)) return intval($s);
        // Hex
        if (preg_match('/^[-+]?0[xX]([0-9A-Fa-f]+)$/', $s, $m)) {
            $val = hexdec($m[1]);
            return $s[0] === '-' ? -$val : $val;
        }
        // Octal
        if (preg_match('/^[-+]?0([0-7]+)$/', $s, $m)) {
            $val = octdec($m[1]);
            return $s[0] === '-' ? -$val : $val;
        }
        // Radix
        if (preg_match('/^[-+]?([1-9][0-9]?)[rR]([0-9A-Za-z]+)$/', $s, $m)) {
            $radix = (int)$m[1];
            if ($radix < 2 || $radix > 36) throw $this->error("Radix out of range: $radix");
            $val = intval($m[2], $radix);
            return $s[0] === '-' ? -$val : $val;
        }
        // BigInt (return as string)
        if (preg_match('/^[-+]?[0-9]+N$/', $s)) return substr($s, 0, -1);
        // Float
        if (preg_match('/^[-+]?[0-9]+(?:\.[0-9]*)?(?:[eE][-+]?[0-9]+)?$/', $s)) return (float)$s;
        // BigDecimal (return as string)
        if (preg_match('/^[-+]?[0-9]+(?:\.[0-9]*)?(?:[eE][-+]?[0-9]+)?M$/', $s)) return substr($s, 0, -1);
        // Ratio (return as string)
        if (preg_match('/^([-+]?[0-9]+)\/([0-9]+)$/', $s)) return $s;
        throw $this->error("Invalid number: $s");
    }

    private function readString(): string
    {
        $sb = '';
        while (true) {
            if ($this->eof()) throw $this->error("EOF while reading string");
            $ch = $this->next();
            if ($ch === '"') return $sb;
            if ($ch === '\\') $sb .= $this->readEscape();
            else $sb .= $ch;
        }
    }

    private function readEscape(): string
    {
        if ($this->eof()) throw $this->error("EOF while reading escape");
        $ch = $this->next();
        return match ($ch) {
            't' => "\t", 'r' => "\r", 'n' => "\n", '\\' => "\\", '"' => '"',
            'b' => "\x08", 'f' => "\x0C", 'u' => $this->readUnicodeEscape(),
            default => throw $this->error("Unsupported escape: \\$ch"),
        };
    }

    private function readUnicodeEscape(): string
    {
        $hex = '';
        for ($i = 0; $i < 4; $i++) {
            if ($this->eof()) throw $this->error("EOF while reading unicode escape");
            $ch = $this->next();
            if (!ctype_xdigit($ch)) throw $this->error("Invalid unicode digit: $ch");
            $hex .= $ch;
        }
        return mb_chr(hexdec($hex), 'UTF-8');
    }

    private function readChar(): string
    {
        if ($this->eof()) throw $this->error("EOF while reading character");
        $ch = $this->next();
        $token = $ch;
        while (!$this->eof()) {
            $next = $this->peek();
            if ($this->isWhitespace($next) || $this->isTerminatingMacro($next)) break;
            $token .= $this->next();
        }
        if (strlen($token) === 1) return $token;
        return match ($token) {
            'newline' => "\n", 'space' => ' ', 'tab' => "\t",
            'backspace' => "\x08", 'formfeed' => "\x0C", 'return' => "\r",
            default => $this->parseCharToken($token),
        };
    }

    private function parseCharToken(string $token): string
    {
        if ($token[0] === 'u' && strlen($token) === 5 && ctype_xdigit(substr($token, 1))) {
            return mb_chr(hexdec(substr($token, 1)), 'UTF-8');
        }
        if ($token[0] === 'o' && preg_match('/^o[0-7]{1,3}$/', $token)) {
            $val = octdec(substr($token, 1));
            if ($val > 255) throw $this->error("Octal out of range: $token");
            return chr($val);
        }
        throw $this->error("Unsupported character: \\$token");
    }

    private function readComment(): mixed
    {
        while (!$this->eof() && $this->peek() !== "\n") $this->next();
        return $this;
    }

    private function readList(): mixed
    {
        return plist(...$this->readDelimitedList(')'));
    }

    private function readVector(): mixed
    {
        return vec(...$this->readDelimitedList(']'));
    }

    private function readMap(): mixed
    {
        $items = $this->readDelimitedList('}');
        if (count($items) % 2 !== 0) throw $this->error("Map literal must contain even number of forms");
        return hashMap(...$items);
    }

    private function readSet(): mixed
    {
        return hashSet(...$this->readDelimitedList('}'));
    }

    private function readDelimitedList(string $delim): array
    {
        $items = [];
        $startLine = $this->line;
        while (true) {
            $this->skipWhitespace();
            if ($this->eof()) throw $this->error("EOF while reading, starting at line $startLine");
            if ($this->peek() === $delim) { $this->next(); return $items; }
            $items[] = $this->read();
        }
    }

    private function readUnmatched(): never
    {
        throw $this->error("Unmatched delimiter");
    }

    private function readQuote(): mixed
    {
        return plist(Sym::create('quote'), $this->read());
    }

    private function readDeref(): mixed
    {
        return plist(Sym::createNs('clojure.core', 'deref'), $this->read());
    }

    private function readVar(): mixed
    {
        return plist(Sym::create('var'), $this->read());
    }

    private function readUnquote(): mixed
    {
        if ($this->peek() === '@') {
            $this->next();
            return plist(Sym::createNs('clojure.core', 'unquote-splicing'), $this->read());
        }
        return plist(Sym::createNs('clojure.core', 'unquote'), $this->read());
    }

    private function readSyntaxQuote(): mixed
    {
        $savedGensyms = $this->gensyms;
        $this->gensyms = [];
        try {
            return $this->syntaxQuote($this->read());
        } finally {
            $this->gensyms = $savedGensyms;
        }
    }

    private function syntaxQuote(mixed $form): mixed
    {
        if ($form instanceof Sym) return $this->syntaxQuoteSymbol($form);
        if ($this->isUnquote($form)) return second($form);
        if ($this->isUnquoteSplicing($form)) throw $this->error("Splice not in list");
        if (is_iterable($form) && !is_string($form)) return $this->syntaxQuoteCollection($form);
        return $form;
    }

    private function syntaxQuoteSymbol(Sym $sym): mixed
    {
        $name = $sym->name();
        if ($sym->ns() === null && str_ends_with($name, '#')) {
            $base = substr($name, 0, -1);
            if (!isset($this->gensyms[$name])) {
                $this->gensyms[$name] = Sym::create($base . '__' . $this->nextId++ . '__auto__');
            }
            return plist(Sym::create('quote'), $this->gensyms[$name]);
        }
        return plist(Sym::create('quote'), $sym);
    }

    private function syntaxQuoteCollection(iterable $coll): mixed
    {
        $isVector = $coll instanceof Vec;
        $items = [];
        foreach ($coll as $item) {
            if ($this->isUnquote($item)) {
                $items[] = plist(Sym::createNs('clojure.core', 'list'), second($item));
            } elseif ($this->isUnquoteSplicing($item)) {
                $items[] = second($item);
            } else {
                $items[] = plist(Sym::createNs('clojure.core', 'list'), $this->syntaxQuote($item));
            }
        }
        $concat = plist(Sym::createNs('clojure.core', 'concat'), ...$items);
        $seq = plist(Sym::createNs('clojure.core', 'seq'), $concat);
        if ($isVector) {
            return plist(Sym::createNs('clojure.core', 'apply'),
                         Sym::createNs('clojure.core', 'vector'), $seq);
        }
        return $seq;
    }

    private function isUnquote(mixed $form): bool
    {
        if (!is_iterable($form) || is_string($form)) return false;
        $first = first($form);
        return $first instanceof Sym && $first->name() === 'unquote';
    }

    private function isUnquoteSplicing(mixed $form): bool
    {
        if (!is_iterable($form) || is_string($form)) return false;
        $first = first($form);
        return $first instanceof Sym && $first->name() === 'unquote-splicing';
    }

    private function readFn(): mixed
    {
        if (!empty($this->argEnv)) throw $this->error("Nested #()s are not allowed");
        $savedArgEnv = $this->argEnv;
        $this->argEnv = [];
        try {
            $form = $this->readList();
            return plist(Sym::create('fn*'), $this->buildArgVector(), $form);
        } finally {
            $this->argEnv = $savedArgEnv;
        }
    }

    private function readArg(): Sym
    {
        if (empty($this->argEnv)) return $this->parseSymbol('%' . $this->readToken());
        $ch = $this->peek();
        if ($ch === '&') { $this->next(); return $this->registerArg(-1); }
        if ($this->isDigit($ch)) {
            $num = '';
            while ($this->isDigit($this->peek())) $num .= $this->next();
            return $this->registerArg((int)$num);
        }
        return $this->registerArg(1);
    }

    private function registerArg(int $n): Sym
    {
        if (!isset($this->argEnv[$n])) {
            $name = $n === -1 ? 'rest' : 'p' . $n;
            $this->argEnv[$n] = Sym::create($name . '__' . $this->nextId++ . '#');
        }
        return $this->argEnv[$n];
    }

    private function buildArgVector(): Vec
    {
        $args = [];
        if (empty($this->argEnv)) return vec();
        $maxArg = 0;
        $hasRest = false;
        foreach ($this->argEnv as $n => $sym) {
            if ($n === -1) $hasRest = true;
            else $maxArg = max($maxArg, $n);
        }
        for ($i = 1; $i <= $maxArg; $i++) {
            $args[] = $this->argEnv[$i] ?? Sym::create('p' . $i . '__' . $this->nextId++ . '#');
        }
        if ($hasRest) {
            $args[] = Sym::create('&');
            $args[] = $this->argEnv[-1];
        }
        return vec(...$args);
    }

    private function readDispatch(): mixed
    {
        if ($this->eof()) throw $this->error("EOF while reading dispatch");
        $ch = $this->next();
        if (isset(self::DISPATCH_MACROS[$ch])) return $this->{self::DISPATCH_MACROS[$ch]}();
        if (ctype_alpha($ch)) return $this->readTagged($ch);
        throw $this->error("No dispatch macro for: $ch");
    }

    private function readRegex(): string
    {
        $sb = '';
        while (true) {
            if ($this->eof()) throw $this->error("EOF while reading regex");
            $ch = $this->next();
            if ($ch === '"') return $sb;
            if ($ch === '\\') { $sb .= $ch; if (!$this->eof()) $sb .= $this->next(); }
            else $sb .= $ch;
        }
    }

    private function readDiscard(): mixed
    {
        $this->read();
        return $this;
    }

    private function readSymbolicValue(): mixed
    {
        $sym = $this->read();
        if (!$sym instanceof Sym) throw $this->error("Invalid symbolic value: ##$sym");
        return match ($sym->name()) {
            'Inf' => INF, '-Inf' => -INF, 'NaN' => NAN,
            default => throw $this->error("Unknown symbolic value: ##" . $sym->name()),
        };
    }

    private function readTagged(string $firstChar): mixed
    {
        $tag = $firstChar . $this->readToken();
        $form = $this->read();
        if ($tag === 'inst') return new \DateTimeImmutable($form);
        if ($tag === 'uuid') return $form;
        // Return tagged literal as map
        return hashMap(Kw::create('tag'), Sym::create($tag), Kw::create('form'), $form);
    }

    private function readConditional(): mixed
    {
        $splicing = false;
        if ($this->peek() === '@') { $this->next(); $splicing = true; }
        $this->skipWhitespace();
        if ($this->peek() !== '(') throw $this->error("Reader conditional body must be a list");
        $this->next();
        $result = null;
        $found = false;
        while (true) {
            $this->skipWhitespace();
            if ($this->peek() === ')') { $this->next(); break; }
            $feature = $this->read();
            if (!$feature instanceof Kw) throw $this->error("Feature should be a keyword");
            $this->skipWhitespace();
            if ($this->peek() === ')') throw $this->error("Reader conditional requires even number of forms");
            $form = $this->read();
            if (!$found) {
                $featureName = $feature->name();
                if ($featureName === 'default' || in_array($featureName, $this->features, true)) {
                    $result = $form;
                    $found = true;
                }
            }
        }
        if (!$found) return $this;
        return $result;
    }

    private function readNamespacedMap(): mixed
    {
        $auto = false;
        if ($this->peek() === ':') { $this->next(); $auto = true; }
        $this->skipWhitespace();
        $ns = null;
        if ($this->peek() !== '{') {
            $sym = $this->read();
            if (!$sym instanceof Sym) throw $this->error("Namespaced map prefix must be a symbol");
            $ns = $sym->name();
            $this->skipWhitespace();
        }
        if ($this->peek() !== '{') throw $this->error("Namespaced map must specify a map");
        $this->next();
        if ($auto && $ns === null) $ns = 'user';
        $items = $this->readDelimitedList('}');
        if (count($items) % 2 !== 0) throw $this->error("Namespaced map must contain even number of forms");
        $result = [];
        for ($i = 0; $i < count($items); $i += 2) {
            $k = $items[$i];
            $v = $items[$i + 1];
            if ($k instanceof Kw && $k->ns() === null) {
                $k = Kw::createNs($ns, $k->name());
            } elseif ($k instanceof Sym && $k->ns() === null) {
                $k = Sym::createNs($ns, $k->name());
            }
            $result[] = $k;
            $result[] = $v;
        }
        return hashMap(...$result);
    }

    private function readMeta(): mixed
    {
        $meta = $this->read();
        if ($meta instanceof Sym || is_string($meta)) {
            $meta = hashMap(Kw::create('tag'), $meta);
        } elseif ($meta instanceof Kw) {
            $meta = hashMap($meta, true);
        } elseif (!$meta instanceof Map) {
            throw $this->error("Metadata must be Symbol, Keyword, String, or Map");
        }
        $target = $this->read();
        // Attach metadata if supported
        if ($target instanceof IMeta) {
            return $target->withMeta($meta);
        }
        return $target;
    }
}

// ============================================================
// Reader functions
// ============================================================

/**
 * Read a single form from a string.
 */
function readString(string $s, array $opts = []): mixed
{
    $reader = new Reader($s, $opts['file'] ?? 'string', $opts);
    return $reader->read();
}

/**
 * Read all forms from a string.
 */
function readAllForms(string $s, array $opts = []): array
{
    $reader = new Reader($s, $opts['file'] ?? 'string', $opts);
    return $reader->readAll();
}

/**
 * Read all forms from a file.
 */
function readFile(string $path, array $opts = []): array
{
    $content = file_get_contents($path);
    if ($content === false) {
        throw new \Exception("Cannot read file: $path");
    }
    $opts['file'] = $path;
    return readAllForms($content, $opts);
}

// Note: second, third, fourth now defined in Core.php
