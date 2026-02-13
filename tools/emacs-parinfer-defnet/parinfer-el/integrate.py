#!/usr/bin/env python3
"""Integrate Week 3 changes into parinfer.el"""

def read_file(path):
    with open(path, 'r', encoding='utf-8') as f:
        return f.readlines()

def write_file(path, lines):
    with open(path, 'w', encoding='utf-8', newline='\n') as f:
        f.writelines(lines)

def main():
    # Read current parinfer.el
    lines = read_file('C:/Users/Apollo/em/parinfer-el/parinfer.el')

    # Section 1: Add helper functions after line 279 (index 278)
    # Find the line with "         (not is-closer))))"
    insert_idx = None
    for i, line in enumerate(lines):
        if '         (not is-closer))))' in line:
            insert_idx = i + 1
            # Skip the blank line
            if i + 1 < len(lines) and lines[i + 1].strip() == '':
                insert_idx = i + 2
            break

    if insert_idx is None:
        print("ERROR: Could not find insertion point for Section 1")
        return

    section1 = [
        '\n',
        '(defsubst parinfer--is-comment-char (ch comment-chars)\n',
        '  "Return t if CH is in COMMENT-CHARS vector."\n',
        '  (not (= (parinfer--index-of comment-chars (string-to-char ch)) -1)))\n',
        '\n',
        ';; ---------------------------------------------------------------------------\n',
        ';; Misc Utils\n',
        '\n',
        '(defsubst parinfer--clamp (val min-n max-n)\n',
        '  "Clamp VAL between MIN-N and MAX-N.\n',
        'If MIN-N or MAX-N is UINT-NULL, no clamping is done on that side."\n',
        '  (when (not (= min-n parinfer--UINT-NULL))\n',
        '    (setq val (max min-n val)))\n',
        '  (when (not (= max-n parinfer--UINT-NULL))\n',
        '    (setq val (min max-n val)))\n',
        '  val)\n',
        '\n',
    ]

    lines = lines[:insert_idx] + section1 + lines[insert_idx:]

    # Fix bugs in on-char function
    for i, line in enumerate(lines):
        # Fix open-paren-p call
        if '((parinfer--open-paren-p ch result)' in line:
            lines[i] = line.replace('((parinfer--open-paren-p ch result)',
                                   '((parinfer--open-paren-p ch (parinfer--result-open-paren-chars result))')
        # Fix close-paren-p call
        if '((parinfer--close-paren-p ch result)' in line:
            lines[i] = line.replace('((parinfer--close-paren-p ch result)',
                                   '((parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))')
        # Fix comment-char-p call
        if '((parinfer--comment-char-p ch result)' in line:
            lines[i] = line.replace('((parinfer--comment-char-p ch result)',
                                   '((parinfer--is-comment-char ch (parinfer--result-comment-chars result))')
        # Fix char-equal calls
        if '((char-equal ch' in line:
            lines[i] = line.replace('((char-equal ch', '((equal ch')
        # Fix tracking-arg-tab-stop symbol to string
        if "tracking-arg-tab-stop result) 'space" in line:
            lines[i] = line.replace("tracking-arg-tab-stop result) 'space",
                                   'tracking-arg-tab-stop result) "space"')

    # Write output
    write_file('C:/Users/Apollo/em/parinfer-el/parinfer-new.el', lines)
    print(f"Integration step 1 complete!")
    print(f"Original lines: 913")
    print(f"New lines: {len(lines)}")
    print(f"Added: {len(lines) - 913} lines")

if __name__ == '__main__':
    main()
