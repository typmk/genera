<?php

declare(strict_types=1);

namespace Clojure\Lang;

/**
 * Converts Clojure identifiers to valid PHP identifiers.
 *
 * Clojure allows characters like -, ?, !, * in identifiers that are
 * not valid in PHP. This class provides encoding/decoding for these.
 */
final class Munge
{
    private const ENCODE_MAP = [
        '-' => '_',
        '.' => '_DOT_',
        ':' => '_COLON_',
        '+' => '_PLUS_',
        '>' => '_GT_',
        '<' => '_LT_',
        '=' => '_EQ_',
        '~' => '_TILDE_',
        '!' => '_BANG_',
        '@' => '_CIRCA_',
        '#' => '_SHARP_',
        '\'' => '_SINGLEQUOTE_',
        '"' => '_DOUBLEQUOTE_',
        '%' => '_PERCENT_',
        '^' => '_CARET_',
        '&' => '_AMPERSAND_',
        '*' => '_STAR_',
        '|' => '_BAR_',
        '{' => '_LBRACE_',
        '}' => '_RBRACE_',
        '[' => '_LBRACK_',
        ']' => '_RBRACK_',
        '/' => '_SLASH_',
        '\\' => '_BSLASH_',
        '?' => '_QMARK_',
    ];

    public function encode(string $name): string
    {
        return strtr($name, self::ENCODE_MAP);
    }

    public function decode(string $mungedName): string
    {
        return strtr($mungedName, array_flip(self::ENCODE_MAP));
    }
}
