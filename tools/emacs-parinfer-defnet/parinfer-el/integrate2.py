#!/usr/bin/env python3
"""Integrate Week 3 changes - Step 2: Indentation functions and stub replacements"""

def read_file(path):
    with open(path, 'r', encoding='utf-8') as f:
        return f.readlines()

def write_file(path, lines):
    with open(path, 'w', encoding='utf-8', newline='\n') as f:
        f.writelines(lines)

def main():
    # Read the partially integrated file
    lines = read_file('C:/Users/Apollo/em/parinfer-el/parinfer-new.el')

    # Section 2: Add indentation functions before the stub section
    # Find ";; Character Processing (stubs for dependencies)"
    insert_idx2 = None
    for i, line in enumerate(lines):
        if ';; Character Processing (stubs for dependencies)' in line:
            insert_idx2 = i
            break

    if insert_idx2 is None:
        print("ERROR: Could not find insertion point for Section 2")
        return

    section2 = [
        ';; ---------------------------------------------------------------------------\n',
        ';; Cursor Helper Functions (for indentation)\n',
        '\n',
        '(defsubst parinfer--is-cursor-left-of (cursor-x cursor-line x line-no)\n',
        '  "Return t if cursor is to the left of position X on LINE-NO."\n',
        '  (and (= cursor-line line-no)\n',
        '       (not (= x parinfer--UINT-NULL))\n',
        '       (not (= cursor-x parinfer--UINT-NULL))\n',
        '       (<= cursor-x x)))  ; inclusive since (cursorX = x) implies (x-1 < cursor < x)\n',
        '\n',
        '(defsubst parinfer--is-cursor-right-of (cursor-x cursor-line x line-no)\n',
        '  "Return t if cursor is to the right of position X on LINE-NO."\n',
        '  (and (= cursor-line line-no)\n',
        '       (not (= x parinfer--UINT-NULL))\n',
        '       (not (= cursor-x parinfer--UINT-NULL))\n',
        '       (> cursor-x x)))\n',
        '\n',
        '(defsubst parinfer--is-cursor-in-comment (result cursor-x cursor-line)\n',
        '  "Return t if cursor is inside a comment in RESULT."\n',
        '  (parinfer--is-cursor-right-of cursor-x cursor-line\n',
        '                                 (parinfer--result-comment-x result)\n',
        '                                 (parinfer--result-line-no result)))\n',
        '\n',
        ';; ---------------------------------------------------------------------------\n',
        ';; Indentation Functions\n',
        '\n',
        '(defun parinfer--should-add-opener-indent (result opener)\n',
        '  "Return t if we should add OPENER\\'s indentDelta to current line in RESULT.\n',
        'Don\\'t add opener.indentDelta if the user already added it.\n',
        'This happens when multiple lines are indented together."\n',
        '  (not (= (plist-get opener :indent-delta)\n',
        '          (parinfer--result-indent-delta result))))\n',
        '\n',
        '(defun parinfer--add-indent (result delta)\n',
        '  "Add DELTA spaces of indentation to current line in RESULT."\n',
        '  (let* ((orig-indent (parinfer--result-x result))\n',
        '         (new-indent (+ orig-indent delta))\n',
        '         (indent-str (parinfer--repeat-string parinfer--BLANK-SPACE new-indent)))\n',
        '    (parinfer--replace-within-line result\n',
        '                                    (parinfer--result-line-no result)\n',
        '                                    0\n',
        '                                    orig-indent\n',
        '                                    indent-str)\n',
        '    (setf (parinfer--result-x result) new-indent)\n',
        '    (setf (parinfer--result-indent-x result) new-indent)\n',
        '    (setf (parinfer--result-indent-delta result)\n',
        '          (+ (parinfer--result-indent-delta result) delta))))\n',
        '\n',
        '(defun parinfer--correct-indent (result)\n',
        '  "Correct indentation for current line in RESULT (Paren Mode)."\n',
        '  (let* ((orig-indent (parinfer--result-x result))\n',
        '         (new-indent orig-indent)\n',
        '         (min-indent 0)\n',
        '         (max-indent (parinfer--result-max-indent result))\n',
        '         (opener (parinfer--peek (parinfer--result-paren-stack result) 0)))\n',
        '\n',
        '    (when opener\n',
        '      (setq min-indent (+ (plist-get opener :x) 1))\n',
        '      (setq max-indent (plist-get opener :max-child-indent))\n',
        '      (when (parinfer--should-add-opener-indent result opener)\n',
        '        (setq new-indent (+ new-indent (plist-get opener :indent-delta)))))\n',
        '\n',
        '    (setq new-indent (parinfer--clamp new-indent min-indent max-indent))\n',
        '\n',
        '    (when (not (= new-indent orig-indent))\n',
        '      (parinfer--add-indent result (- new-indent orig-indent)))))\n',
        '\n',
        '(defun parinfer--on-leading-close-paren (result)\n',
        '  "Handle leading close paren in RESULT."\n',
        '  (let ((mode (parinfer--result-mode result)))\n',
        '    (cond\n',
        '     ;; INDENT_MODE\n',
        '     ((eq mode parinfer--INDENT-MODE)\n',
        '      (when (not (parinfer--result-force-balance result))\n',
        '        (when (parinfer--result-smart result)\n',
        '          ;; Exit to paren mode\n',
        '          (signal \\'parinfer-paren-mode-exit \\'(leading-close-paren)))\n',
        '        (when (not (gethash parinfer--ERROR-LEADING-CLOSE-PAREN\n',
        '                           (parinfer--result-error-pos-cache result)))\n',
        '          (parinfer--cache-error-pos result parinfer--ERROR-LEADING-CLOSE-PAREN)))\n',
        '      (setf (parinfer--result-skip-char result) t))\n',
        '\n',
        '     ;; PAREN_MODE\n',
        '     ((eq mode parinfer--PAREN-MODE)\n',
        '      (if (not (parinfer--valid-close-paren-p (parinfer--result-paren-stack result)\n',
        '                                              (parinfer--result-ch result)))\n',
        '          (if (parinfer--result-smart result)\n',
        '              (setf (parinfer--result-skip-char result) t)\n',
        '            (signal \\'parinfer-error\n',
        '                    (list (parinfer--create-error result parinfer--ERROR-UNMATCHED-CLOSE-PAREN))))\n',
        '        (if (parinfer--is-cursor-left-of (parinfer--result-cursor-x result)\n',
        '                                         (parinfer--result-cursor-line result)\n',
        '                                         (parinfer--result-x result)\n',
        '                                         (parinfer--result-line-no result))\n',
        '            (progn\n',
        '              (parinfer--reset-paren-trail result\n',
        '                                           (parinfer--result-line-no result)\n',
        '                                           (parinfer--result-x result))\n',
        '              (parinfer--on-indent result))\n',
        '          (parinfer--append-paren-trail result)\n',
        '          (setf (parinfer--result-skip-char result) t)))))))\n',
        '\n',
        '(defun parinfer--on-comment-line (result)\n',
        '  "Handle comment line indentation in RESULT."\n',
        '  (let ((paren-trail-len (length (parinfer--paren-trail-openers\n',
        '                                  (parinfer--result-paren-trail result)))))\n',
        '\n',
        '    ;; restore the openers matching the previous paren trail\n',
        '    (when (eq (parinfer--result-mode result) parinfer--PAREN-MODE)\n',
        '      (let ((i 0))\n',
        '        (while (< i paren-trail-len)\n',
        '          (let ((opener (parinfer--peek (parinfer--paren-trail-openers\n',
        '                                         (parinfer--result-paren-trail result))\n',
        '                                       i)))\n',
        '            (setf (parinfer--result-paren-stack result)\n',
        '                  (vconcat (parinfer--result-paren-stack result) (vector opener))))\n',
        '          (cl-incf i))))\n',
        '\n',
        '    ;; Find parent opener and add its indent\n',
        '    (let* ((opener-idx (parinfer--get-parent-opener-index result (parinfer--result-x result)))\n',
        '           (opener (parinfer--peek (parinfer--result-paren-stack result) opener-idx)))\n',
        '      (when opener\n',
        '        ;; shift the comment line based on the parent open paren\n',
        '        (when (parinfer--should-add-opener-indent result opener)\n',
        '          (parinfer--add-indent result (plist-get opener :indent-delta)))))\n',
        '\n',
        '    ;; repop the openers matching the previous paren trail\n',
        '    (when (eq (parinfer--result-mode result) parinfer--PAREN-MODE)\n',
        '      (let ((i2 0))\n',
        '        (while (< i2 paren-trail-len)\n',
        '          (let ((stack-len (length (parinfer--result-paren-stack result))))\n',
        '            (setf (parinfer--result-paren-stack result)\n',
        '                  (substring (parinfer--result-paren-stack result) 0 (- stack-len 1))))\n',
        '          (cl-incf i2))))))\n',
        '\n',
        '(defun parinfer--on-indent (result)\n',
        '  "Handle indentation point in RESULT."\n',
        '  (setf (parinfer--result-indent-x result) (parinfer--result-x result))\n',
        '  (setf (parinfer--result-tracking-indent result) nil)\n',
        '\n',
        '  (when (parinfer--result-quote-danger result)\n',
        '    (signal \\'parinfer-error\n',
        '            (list (parinfer--create-error result parinfer--ERROR-QUOTE-DANGER))))\n',
        '\n',
        '  (let ((mode (parinfer--result-mode result)))\n',
        '    (cond\n',
        '     ((eq mode parinfer--INDENT-MODE)\n',
        '      (parinfer--correct-paren-trail result (parinfer--result-x result))\n',
        '      (let ((opener (parinfer--peek (parinfer--result-paren-stack result) 0)))\n',
        '        (when (and opener (parinfer--should-add-opener-indent result opener))\n',
        '          (parinfer--add-indent result (plist-get opener :indent-delta)))))\n',
        '\n',
        '     ((eq mode parinfer--PAREN-MODE)\n',
        '      (parinfer--correct-indent result)))))\n',
        '\n',
        ';; ---------------------------------------------------------------------------\n',
    ]

    lines = lines[:insert_idx2] + section2 + lines[insert_idx2:]

    #Now find and replace the stub functions
    # Find handle-change-delta stub
    start_stub = None
    end_stub = None
    for i, line in enumerate(lines):
        if '(defun parinfer--handle-change-delta (result)' in line:
            start_stub = i
        if start_stub is not None and '(defun parinfer--check-indent (result)' in line:
            end_stub = i
            break

    if start_stub and end_stub:
        # Replace all stub functions with real implementations
        section3 = [
            '(defun parinfer--handle-change-delta (result)\n',
            '  "Handle change delta tracking in RESULT."\n',
            '  (when (and (parinfer--result-changes result)\n',
            '             (or (parinfer--result-smart result)\n',
            '                 (eq (parinfer--result-mode result) parinfer--PAREN-MODE)))\n',
            '    (let ((line (gethash (parinfer--result-input-line-no result)\n',
            '                        (parinfer--result-changes result))))\n',
            '      (when line\n',
            '        (let ((change (gethash (parinfer--result-input-x result) line)))\n',
            '          (when change\n',
            '            (setf (parinfer--result-indent-delta result)\n',
            '                  (+ (parinfer--result-indent-delta result)\n',
            '                     (- (plist-get change :new-end-x)\n',
            '                        (plist-get change :old-end-x))))))))))\n',
            '\n',
            '(defun parinfer--check-indent (result)\n',
            '  "Check and handle indentation in RESULT (dispatcher)."\n',
            '  (let ((ch (parinfer--result-ch result)))\n',
            '    (cond\n',
            '     ((parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))\n',
            '      (parinfer--on-leading-close-paren result))\n',
            '\n',
            '     ((parinfer--is-comment-char ch (parinfer--result-comment-chars result))\n',
            '      ;; comments don\\'t count as indentation points\n',
            '      (parinfer--on-comment-line result)\n',
            '      (setf (parinfer--result-tracking-indent result) nil))\n',
            '\n',
            '     ((and (not (equal ch parinfer--NEWLINE))\n',
            '           (not (equal ch parinfer--BLANK-SPACE))\n',
            '           (not (equal ch parinfer--TAB)))\n',
            '      (parinfer--on-indent result)))))\n',
            '\n',
        ]
        lines = lines[:start_stub] + section3 + lines[end_stub:]

    # Now find is-closable and track-arg-tab-stop stubs to replace
    for i in range(len(lines)):
        if lines[i].strip().startswith('(defun parinfer--is-closable (result)'):
            # Find the end of this function
            j = i + 1
            while j < len(lines) and not (lines[j].strip().startswith('(defun ') or lines[j].strip().startswith(';;; ') or lines[j].strip().startswith('(provide')):
                j += 1
            # Replace with new implementation
            new_is_closable = [
                '(defun parinfer--is-closable (result)\n',
                '  "Check if we can close a paren trail in RESULT.\n',
                'Alias for parinfer--closable-p for compatibility."\n',
                '  (parinfer--closable-p result))\n',
                '\n',
            ]
            lines = lines[:i] + new_is_closable + lines[j:]
            break

    for i in range(len(lines)):
        if lines[i].strip().startswith('(defun parinfer--track-arg-tab-stop (result state)'):
            # Find the end of this function
            j = i + 1
            while j < len(lines) and not (lines[j].strip().startswith('(defun ') or lines[j].strip().startswith(';;; ') or lines[j].strip().startswith('(provide')):
                j += 1
            # Replace with new implementation
            new_track = [
                '(defun parinfer--track-arg-tab-stop (result state)\n',
                '  "Track argument tab stop in RESULT with STATE.\n',
                'STATE can be \\'space or \\'arg."\n',
                '  (cond\n',
                '   ((eq state \\'space)\n',
                '    (when (and (parinfer--result-is-in-code result)\n',
                '               (parinfer--whitespace-p result))\n',
                '      (setf (parinfer--result-tracking-arg-tab-stop result) \\'arg)))\n',
                '\n',
                '   ((eq state \\'arg)\n',
                '    (when (not (parinfer--whitespace-p result))\n',
                '      (let ((opener (parinfer--peek (parinfer--result-paren-stack result) 0)))\n',
                '        (when opener\n',
                '          (plist-put opener :arg-x (parinfer--result-x result)))\n',
                '        (setf (parinfer--result-tracking-arg-tab-stop result) nil))))))\n',
                '\n',
            ]
            lines = lines[:i] + new_track + lines[j:]
            break

    # Section 4: Add Week 4 stubs before the on-char function
    for i, line in enumerate(lines):
        if '(defun parinfer--on-char (result)' in line:
            section4 = [
                ';; ---------------------------------------------------------------------------\n',
                ';; Week 4 Stub Functions (dependencies for Week 3 code)\n',
                '\n',
                '(defun parinfer--get-parent-opener-index (result indent-x)\n',
                '  "Get index of parent opener for INDENT-X in RESULT.\n',
                'STUB: Returns 0 for now (will be fully implemented in Week 4)."\n',
                '  ;; TODO: Full implementation in Week 4\n',
                '  ;; For now, return 0 to pop all openers (basic behavior)\n',
                '  0)\n',
                '\n',
                '(defun parinfer--correct-paren-trail (result indent-x)\n',
                '  "Correct paren trail from INDENT-X in RESULT.\n',
                'STUB: Minimal implementation for now (will be completed in Week 4)."\n',
                '  ;; TODO: Full implementation in Week 4\n',
                '  nil)\n',
                '\n',
                '(defun parinfer--append-paren-trail (result)\n',
                '  "Append a valid close-paren to the end of the paren trail in RESULT.\n',
                'STUB: Not yet implemented (Week 4)."\n',
                '  ;; TODO: Implement in Week 4\n',
                '  nil)\n',
                '\n',
            ]
            lines = lines[:i] + section4 + lines[i:]
            break

    # Write final output
    write_file('C:/Users/Apollo/em/parinfer-el/parinfer-integrated.el', lines)
    print(f"Integration complete!")
    print(f"Input lines: {len(read_file('C:/Users/Apollo/em/parinfer-el/parinfer-new.el'))}")
    print(f"Output lines: {len(lines)}")
    print(f"Added in step 2: {len(lines) - len(read_file('C:/Users/Apollo/em/parinfer-el/parinfer-new.el'))} lines")

if __name__ == '__main__':
    main()
