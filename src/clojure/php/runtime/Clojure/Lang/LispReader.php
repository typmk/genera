<?php

declare(strict_types=1);

namespace Clojure\Lang;

use Cljp\Runtime;

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

class LispReader
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
        $this->features = $opts['features'] ?? ['cljp'];
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

    private function matchSymbolOrKeyword(string $s): Symbol|Keyword
    {
        if ($s === '') throw $this->error("Invalid empty token");
        if ($s[0] === ':') return $this->parseKeyword(substr($s, 1));
        return $this->parseSymbol($s);
    }

    private function parseKeyword(string $s): Keyword
    {
        if ($s === '') throw $this->error("Invalid keyword :");
        if ($s[0] === ':') {
            $s = substr($s, 1);
            if (str_contains($s, '/')) {
                [$ns, $name] = explode('/', $s, 2);
                return Keyword::createForNamespace($ns, $name);
            }
            return Keyword::create($s);
        }
        if (str_contains($s, '/')) {
            [$ns, $name] = explode('/', $s, 2);
            return Keyword::createForNamespace($ns, $name);
        }
        return Keyword::create($s);
    }

    private function parseSymbol(string $s): Symbol
    {
        if ($s === '/') return Symbol::create('/');
        if (str_contains($s, '/')) {
            [$ns, $name] = explode('/', $s, 2);
            return Symbol::createForNamespace($ns, $name);
        }
        return Symbol::create($s);
    }

    private function readNumber(): int|float|string
    {
        $token = $this->readToken();
        return $this->matchNumber($token);
    }

    private function matchNumber(string $s): int|float|string
    {
        if (preg_match('/^[-+]?(?:0|[1-9][0-9]*)$/', $s)) return intval($s);
        if (preg_match('/^[-+]?0[xX]([0-9A-Fa-f]+)$/', $s, $m)) {
            $val = hexdec($m[1]);
            return $s[0] === '-' ? -$val : $val;
        }
        if (preg_match('/^[-+]?0([0-7]+)$/', $s, $m)) {
            $val = octdec($m[1]);
            return $s[0] === '-' ? -$val : $val;
        }
        if (preg_match('/^[-+]?([1-9][0-9]?)[rR]([0-9A-Za-z]+)$/', $s, $m)) {
            $radix = (int)$m[1];
            if ($radix < 2 || $radix > 36) throw $this->error("Radix out of range: $radix");
            $val = intval($m[2], $radix);
            return $s[0] === '-' ? -$val : $val;
        }
        if (preg_match('/^[-+]?[0-9]+N$/', $s)) return substr($s, 0, -1);
        if (preg_match('/^[-+]?[0-9]+(?:\.[0-9]*)?(?:[eE][-+]?[0-9]+)?$/', $s)) return (float)$s;
        if (preg_match('/^[-+]?[0-9]+(?:\.[0-9]*)?(?:[eE][-+]?[0-9]+)?M$/', $s)) return substr($s, 0, -1);
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
        return Runtime::list($this->readDelimitedList(')'));
    }

    private function readVector(): mixed
    {
        return Runtime::vector($this->readDelimitedList(']'));
    }

    private function readMap(): mixed
    {
        $items = $this->readDelimitedList('}');
        if (count($items) % 2 !== 0) throw $this->error("Map literal must contain even number of forms");
        return Runtime::map($items);
    }

    private function readSet(): mixed
    {
        return Runtime::set($this->readDelimitedList('}'));
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
        return Runtime::list([Symbol::create('quote'), $this->read()]);
    }

    private function readDeref(): mixed
    {
        return Runtime::list([Symbol::createForNamespace('clojure.core', 'deref'), $this->read()]);
    }

    private function readVar(): mixed
    {
        return Runtime::list([Symbol::create('var'), $this->read()]);
    }

    private function readUnquote(): mixed
    {
        if ($this->peek() === '@') {
            $this->next();
            return Runtime::list([Symbol::createForNamespace('clojure.core', 'unquote-splicing'), $this->read()]);
        }
        return Runtime::list([Symbol::createForNamespace('clojure.core', 'unquote'), $this->read()]);
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
        if ($form instanceof Symbol) return $this->syntaxQuoteSymbol($form);
        if ($this->isUnquote($form)) return $form[1];
        if ($this->isUnquoteSplicing($form)) throw $this->error("Splice not in list");
        if (is_iterable($form) && !is_string($form)) return $this->syntaxQuoteCollection($form);
        return $form;
    }

    private function syntaxQuoteSymbol(Symbol $sym): mixed
    {
        $name = $sym->getName();
        if ($sym->getNamespace() === null && str_ends_with($name, '#')) {
            $base = substr($name, 0, -1);
            if (!isset($this->gensyms[$name])) {
                $this->gensyms[$name] = Symbol::create($base . '__' . $this->nextId++ . '__auto__');
            }
            return Runtime::list([Symbol::create('quote'), $this->gensyms[$name]]);
        }
        return Runtime::list([Symbol::create('quote'), $sym]);
    }

    private function syntaxQuoteCollection(iterable $coll): mixed
    {
        $isVector = $coll instanceof Collections\Vector\PersistentVectorInterface;
        $items = [];
        foreach ($coll as $item) {
            if ($this->isUnquote($item)) {
                $items[] = Runtime::list([Symbol::createForNamespace('clojure.core', 'list'), $item[1]]);
            } elseif ($this->isUnquoteSplicing($item)) {
                $items[] = $item[1];
            } else {
                $items[] = Runtime::list([Symbol::createForNamespace('clojure.core', 'list'), $this->syntaxQuote($item)]);
            }
        }
        $concat = Runtime::list(array_merge([Symbol::createForNamespace('clojure.core', 'concat')], $items));
        $seq = Runtime::list([Symbol::createForNamespace('clojure.core', 'seq'), $concat]);
        if ($isVector) {
            return Runtime::list([Symbol::createForNamespace('clojure.core', 'apply'),
                                  Symbol::createForNamespace('clojure.core', 'vector'), $seq]);
        }
        return $seq;
    }

    private function isUnquote(mixed $form): bool
    {
        if (!is_iterable($form) || is_string($form)) return false;
        $arr = is_array($form) ? $form : iterator_to_array($form);
        if (empty($arr)) return false;
        $first = reset($arr);
        return $first instanceof Symbol && $first->getName() === 'unquote';
    }

    private function isUnquoteSplicing(mixed $form): bool
    {
        if (!is_iterable($form) || is_string($form)) return false;
        $arr = is_array($form) ? $form : iterator_to_array($form);
        if (empty($arr)) return false;
        $first = reset($arr);
        return $first instanceof Symbol && $first->getName() === 'unquote-splicing';
    }

    private function readFn(): mixed
    {
        if (!empty($this->argEnv)) throw $this->error("Nested #()s are not allowed");
        $savedArgEnv = $this->argEnv;
        $this->argEnv = [];
        try {
            $form = $this->readList();
            return Runtime::list([Symbol::create('fn*'), $this->buildArgVector(), $form]);
        } finally {
            $this->argEnv = $savedArgEnv;
        }
    }

    private function readArg(): Symbol
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

    private function registerArg(int $n): Symbol
    {
        if (!isset($this->argEnv[$n])) {
            $name = $n === -1 ? 'rest' : 'p' . $n;
            $this->argEnv[$n] = Symbol::create($name . '__' . $this->nextId++ . '#');
        }
        return $this->argEnv[$n];
    }

    private function buildArgVector(): mixed
    {
        $args = [];
        if (empty($this->argEnv)) return Runtime::vector([]);
        $maxArg = 0;
        $hasRest = false;
        foreach ($this->argEnv as $n => $sym) {
            if ($n === -1) $hasRest = true;
            else $maxArg = max($maxArg, $n);
        }
        for ($i = 1; $i <= $maxArg; $i++) {
            $args[] = $this->argEnv[$i] ?? Symbol::create('p' . $i . '__' . $this->nextId++ . '#');
        }
        if ($hasRest) {
            $args[] = Symbol::create('&');
            $args[] = $this->argEnv[-1];
        }
        return Runtime::vector($args);
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
        if (!$sym instanceof Symbol) throw $this->error("Invalid symbolic value: ##$sym");
        return match ($sym->getName()) {
            'Inf' => INF, '-Inf' => -INF, 'NaN' => NAN,
            default => throw $this->error("Unknown symbolic value: ##" . $sym->getName()),
        };
    }

    private function readTagged(string $firstChar): mixed
    {
        $tag = $firstChar . $this->readToken();
        $form = $this->read();
        if ($tag === 'inst') return new \DateTimeImmutable($form);
        if ($tag === 'uuid') return $form;
        return Runtime::map([Keyword::create('tag'), Symbol::create($tag), Keyword::create('form'), $form]);
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
            if (!$feature instanceof Keyword) throw $this->error("Feature should be a keyword");
            $this->skipWhitespace();
            if ($this->peek() === ')') throw $this->error("Reader conditional requires even number of forms");
            $form = $this->read();
            if (!$found) {
                $featureName = $feature->getName();
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
            if (!$sym instanceof Symbol) throw $this->error("Namespaced map prefix must be a symbol");
            $ns = $sym->getName();
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
            if ($k instanceof Keyword && $k->getNamespace() === null) {
                $k = Keyword::createForNamespace($ns, $k->getName());
            } elseif ($k instanceof Symbol && $k->getNamespace() === null) {
                $k = Symbol::createForNamespace($ns, $k->getName());
            }
            $result[] = $k;
            $result[] = $v;
        }
        return Runtime::map($result);
    }

    private function readMeta(): mixed
    {
        $meta = $this->read();
        if ($meta instanceof Symbol || is_string($meta)) {
            $meta = Runtime::map([Keyword::create('tag'), $meta]);
        } elseif ($meta instanceof Keyword) {
            $meta = Runtime::map([$meta, true]);
        } elseif (!$meta instanceof Collections\Map\PersistentMapInterface) {
            throw $this->error("Metadata must be Symbol, Keyword, String, or Map");
        }
        return $this->read(); // TODO: attach metadata
    }
}
