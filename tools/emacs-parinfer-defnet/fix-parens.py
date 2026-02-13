#!/usr/bin/env python3
"""
Attempt to fix paren errors by intelligently adding closing parens.
Uses heuristics: add closing paren at end of line for likely candidates.
"""

import re

def fix_parens_heuristic(input_file, output_file):
    """
    Fix paren errors using simple heuristics.

    Strategy:
    1. Identify lines that likely need closing parens
    2. Look for common patterns (function definitions, reset! calls, etc.)
    3. Add closing parens at logical end points
    """
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    print("Analyzing file for fix opportunities...")
    fixes = []

    # Scan each line for patterns that suggest missing closing parens
    for i, line in enumerate(lines, start=1):
        stripped = line.rstrip()

        # Count parens on this line
        open_count = stripped.count('(')
        close_count = stripped.count(')')
        balance = open_count - close_count

        # Heuristic 1: Lines ending with values that should be closed
        # Example: (atom []) should be (atom []))
        # Example: (reset! x #{} should be (reset! x #{})
        if balance > 0:
            # Check if line ends with a likely complete expression
            if (re.search(r'\[\]$', stripped) or  # empty vector
                re.search(r'\{\}$', stripped) or  # empty map
                re.search(r'#\{\}$', stripped) or  # empty set
                re.search(r'"[^"]*"$', stripped) or  # string
                re.search(r'\d+$', stripped) or  # number
                re.search(r':\w+$', stripped) or # keyword
                re.search(r'false$', stripped) or  # boolean
                re.search(r'true$', stripped)):  # boolean

                # This line probably needs closing parens
                needed = balance
                print(f"Line {i}: Needs {needed} closing paren(s) - '{stripped[:60]}...'")
                fixes.append((i-1, needed))  # 0-indexed

    # Apply fixes
    for line_idx, num_closers in fixes:
        lines[line_idx] = lines[line_idx].rstrip() + ')' * num_closers + '\n'

    # Write fixed file
    with open(output_file, 'w', encoding='utf-8') as f:
        f.writelines(lines)

    print(f"\nApplied {len(fixes)} fixes")
    print(f"Fixed file written to: {output_file}")

    return len(fixes)

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 3:
        print("Usage: python fix-parens.py <input_file> <output_file>")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    num_fixes = fix_parens_heuristic(input_file, output_file)
    print(f"\n=== Summary ===")
    print(f"Total heuristic fixes applied: {num_fixes}")
