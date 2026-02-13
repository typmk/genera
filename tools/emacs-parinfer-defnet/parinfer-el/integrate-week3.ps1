# PowerShell script to integrate Week 3 changes into parinfer.el
# This avoids concurrent modification issues by working with the file once

$parinferPath = "C:/Users/Apollo/em/parinfer-el/parinfer.el"
$week3Path = "C:/Users/Apollo/em/parinfer-el/week3-additions.el"
$outputPath = "C:/Users/Apollo/em/parinfer-el/parinfer-new.el"

# Read the files
$parinfer = Get-Content $parinferPath -Raw
$week3 = Get-Content $week3Path -Raw

# Extract sections from week3
$section1 = $week3 -match '(?s)SECTION 1.*?(?=SECTION 2)'
$section1Text = @"
(defsubst parinfer--is-comment-char (ch comment-chars)
  "Return t if CH is in COMMENT-CHARS vector."
  (not (= (parinfer--index-of comment-chars (string-to-char ch)) -1)))

;; ---------------------------------------------------------------------------
;; Misc Utils

(defsubst parinfer--clamp (val min-n max-n)
  "Clamp VAL between MIN-N and MAX-N.
If MIN-N or MAX-N is UINT-NULL, no clamping is done on that side."
  (when (not (= min-n parinfer--UINT-NULL))
    (setq val (max min-n val)))
  (when (not (= max-n parinfer--UINT-NULL))
    (setq val (min max-n val)))
  val)

"@

# Step 1: Insert Section 1 after line 279
$parinfer = $parinfer -replace '(?s)(         \(not is-closer\)\)\)\)\n\n)(;; -+\n;; Error Handling)', "`$1$section1Text`$2"

# Step 2: Bug fixes in on-char function
$parinfer = $parinfer -replace '\(\(parinfer--open-paren-p ch result\)', '((parinfer--open-paren-p ch (parinfer--result-open-paren-chars result))'
$parinfer = $parinfer -replace '\(\(parinfer--close-paren-p ch result\)', '((parinfer--close-paren-p ch (parinfer--result-close-paren-chars result))'
$parinfer = $parinfer -replace '\(\(parinfer--comment-char-p ch result\)', '((parinfer--is-comment-char ch (parinfer--result-comment-chars result))'
$parinfer = $parinfer -replace '\(\(char-equal ch', '((equal ch'
$parinfer = $parinfer -replace "tracking-arg-tab-stop result\) 'space", 'tracking-arg-tab-stop result) "space"'

# Save the output
$parinfer | Set-Content $outputPath -NoNewline

Write-Host "Integration step 1 complete. Check parinfer-new.el"
Write-Host "Lines in original: $(($parinfer -split '\n').Count)"
