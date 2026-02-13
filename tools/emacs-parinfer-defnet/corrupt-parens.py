#!/usr/bin/env python3
"""
Randomly remove parentheses from a Clojure file for testing paren diagnostics.
Removes ~15% opening parens and ~85% closing parens by count.
"""

import random
import sys

def corrupt_parens(input_file, output_file, num_removals=10):
    """
    Randomly remove parentheses from a file.

    Args:
        input_file: Path to input file
        output_file: Path to output file
        num_removals: Number of parens to remove (default 10)
    """
    # Read file
    with open(input_file, 'r', encoding='utf-8') as f:
        content = f.read()

    # Find all paren positions
    open_parens = []
    close_parens = []

    for i, char in enumerate(content):
        if char == '(':
            open_parens.append(i)
        elif char == ')':
            close_parens.append(i)

    print(f"Found {len(open_parens)} opening parens and {len(close_parens)} closing parens")

    # Calculate how many to remove (15% open, 85% close)
    num_open_to_remove = max(1, int(num_removals * 0.15))
    num_close_to_remove = num_removals - num_open_to_remove

    print(f"Will remove {num_open_to_remove} opening parens and {num_close_to_remove} closing parens")

    # Randomly select which parens to remove
    random.seed(42)  # For reproducibility
    open_to_remove = set(random.sample(open_parens, min(num_open_to_remove, len(open_parens))))
    close_to_remove = set(random.sample(close_parens, min(num_close_to_remove, len(close_parens))))

    # Build corrupted content (mark removed parens)
    corrupted = []
    removals = []

    for i, char in enumerate(content):
        if i in open_to_remove:
            line_num = content[:i].count('\n') + 1
            col = i - content[:i].rfind('\n')
            removals.append(f"Line {line_num}, col {col}: Removed '('")
        elif i in close_to_remove:
            line_num = content[:i].count('\n') + 1
            col = i - content[:i].rfind('\n')
            removals.append(f"Line {line_num}, col {col}: Removed ')'")
        else:
            corrupted.append(char)

    # Write corrupted file
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(''.join(corrupted))

    print(f"\nCorruption complete! Removed {len(removals)} parens:")
    for removal in sorted(removals):
        print(f"  {removal}")

    print(f"\nCorrupted file written to: {output_file}")
    print(f"Total removals: {len(open_to_remove)} opening + {len(close_to_remove)} closing = {len(removals)} total")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: python corrupt-parens.py <input_file> <output_file> [num_removals]")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]
    num_removals = int(sys.argv[3]) if len(sys.argv) > 3 else 10

    corrupt_parens(input_file, output_file, num_removals)
