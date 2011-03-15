;; -*- coding: utf-8 -*-
;; dyalog-mode.el -- Major mode for editing Dyalog APL source code
(require 'cl)

(defvar dyalog-mode-hook nil)

;; Set up mode specific keys below
(defvar dyalog-mode-map
  (let ((map(make-keymap)))
    (define-key map (kbd"M-RET") 'comment-indent-new-line)
    (define-key map (kbd"M-f") 'dyalog-ediff-forward-word)
    (define-key map [?\C-¯] "¯")
    (define-key map [?\C-≤] "≤")
    (define-key map [?\C-≥] "≥")
    (define-key map [?\C-≠] "≠")
    (define-key map [?\C-∨] "∨")
    (define-key map [?\C-∧] "∧")
    (define-key map [?\C-÷] "÷")
    (define-key map [?\C-⍵] "⍵")
    (define-key map [?\C-∊] "∊")
    (define-key map [?\C-⍴] "⍴")
    (define-key map [?\C-~] "~")
    (define-key map [?\C-↑] "↑")
    (define-key map [?\C-↓] "↓")
    (define-key map [?\C-⍳] "⍳")
    (define-key map [?\C-○] "○")
    (define-key map [?\C-*] "*")
    (define-key map [?\C-←] "←")
    (define-key map [?\C-→] "→")
    (define-key map [?\C-⍺] "⍺")
    (define-key map [?\C-⌈] "⌈")
    (define-key map [?\C-⌊] "⌊")
    (define-key map [?\C-∇] "∇")
    (define-key map [?\C-∆] "∆")
    (define-key map [?\C-∘] "∘")
    (define-key map [?\C-⎕] "⎕")
    (define-key map [?\C-⍎] "⍎")
    (define-key map [?\C-⍕] "⍕")
    (define-key map [?\C-⊂] "⊂")
    (define-key map [?\C-⊃] "⊃")
    (define-key map [?\C-∩] "∩")
    (define-key map [?\C-∪] "∪")
    (define-key map [?\C-⊥] "⊥")
    (define-key map [?\C-⊤] "⊤")
    (define-key map [?\C-⍝] "⍝")
    (define-key map [?\C-⍷] "⍷")
    (define-key map [?\C-⍨] "⍨")
    (define-key map [?\C-⍒] "⍒")
    (define-key map [?\C-⍋] "⍋")
    (define-key map [?\C-⌽] "⌽")
    (define-key map [?\C-⍉] "⍉")
    (define-key map [?\C-⊖] "⊖")
    (define-key map [?\C-⍟] "⍟")
    (define-key map [?\C-⍱] "⍱")
    (define-key map [?\C-⍲] "⍲") 
    (define-key map [?\C-⍬] "⍬")
    (define-key map [?\C-⌹] "⌹")
    (define-key map [?\C-≡] "≡")
    (define-key map [?\C-≢] "≢")
    (define-key map [?\C-⌶] "⌶")
    (define-key map [?\C-⍪] "⍪")
    (define-key map [?\C-⌿] "⌿")
    (define-key map [?\C-⍀] "⍀")
    map)
  "Keymap for Dyalog APL mode")

;; This should probably be split into several layers of highlighting
(defconst dyalog-font-lock-keywords1
   (list
    ;; See emacs help for `font-lock-keywords' for a description of how the
    ;; below values work
    '("⎕[A-Za-z]*" . font-lock-builtin-face)
    '("\\s-+\\(:[A-Za-z_∆]+\\)" (1 font-lock-keyword-face nil))
    '(":" . font-lock-keyword-face)
    '("[^A-Za-z_∆0-9]\\(¯?[0-9]+\\.?[0-9]*\\(E¯?[0-9]+\\.?[0-9]*\\)?\\)" (1 font-lock-constant-face nil))
    '("[][<>+---=/¨~\\\\?*(){}&|]" . font-lock-keyword-face)
    '("[*×≤≥>≠∨∧÷∊⍴↑↓⍳○←→⌈⌊∘⎕⍎⍕⊂⊃∩∪⊥⊤⍨⍒⍋⌽⍉⊖⍟⍱⍲⍬⌹≡≢⍪⌿⍀⍺⍵⎕⍞⋄⍷]" 
      . font-lock-keyword-face)
    ;; Below line is broken for dfuns and very broken for
    ;; nested dfuns
    ;;'("{\\([^}]*\\)}" (1 font-lock-constant-face t))
    ;; localizations
    '(";\\([A-Za-z0-9_∆]+\\)" (1 font-lock-constant-face nil))
    '("[∇$@]+" . font-lock-warning-face))
   "Minimal highlighting for Dyalog APL")

(defvar dyalog-font-lock-keywords dyalog-font-lock-keywords1
  "Default highlighting mode for Dyalog mode")

(defvar dyalog-ascii-chars "[]<>+---=/¨~\\?*(){}¨&|"
  "APL symbols also present in ASCII")

(defvar dyalog-keyword-chars
  "*×≤≥>≠∨∧÷∊⍴↑↓⍳○←→⌈⌊∘⍎⍕⊂⊃∩∪⊥⊤⍨⍒⍋⌽⍉⊖⍟⍱⍲⍬⌹≡≢⍪⌿⍀⍺⍵⎕⍞⋄⍷")

(defvar dyalog-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Make various APL chars punctuation
    (loop for char in
	  (string-to-list (concat dyalog-keyword-chars dyalog-ascii-chars))
	  do (modify-syntax-entry char "." st))
    ;; Make sure delta, quad and underscore are part of symbol names
    ;; This doesn't seem to work for delta and quad?
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?∆ "_" st)
    (modify-syntax-entry ?⎕ "_" st)
    ;; Comments
    (modify-syntax-entry ?⍝ "<" st)
    (modify-syntax-entry ?\n">" st)
    ;; Strings
    (modify-syntax-entry ?' "\"" st)
    ;; Delimiters
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
     st)
  "Syntax table for `dyalog-mode'.")

(defvar dyalog-name  "[A-Za-z∆_]+[A-Za-z∆_0-9]*")

(defvar dyalog-number "\\b¯?[0-9]+\\.?[0-9]*\\b")

(defun dyalog-ediff-forward-word ()
  "Move point forward one word."
  (interactive)
  (or 	(> (skip-chars-forward "A-Za-z_∆0-9") 0)  ; name
        (> (skip-chars-forward "⎕:A-Za-z") 0)     ; sys name/keyword
        (> (skip-chars-forward "0-9E¯.") 0)       ; numbers
        (> (skip-chars-forward "⍺⍵∇") 0)          ; meta chars
        (> (skip-chars-forward " ") 0)            ; white space
        (forward-char)))                          ; fallback

(defvar dyalog-indent-start
  "^\\s-*:\\(If\\|While\\|Repeat\\|Trap\\|Select\\|For\\|Class\\)")

(defvar dyalog-indent-pause
  "^\\s-*:\\(Else\\|AndIf\\|OrIf\\|Case\\)")

(defvar dyalog-indent-stop
  "^\\s-*:End[A-Za-z]+")

(defvar dyalog-leading-spaces 1)

(defun dyalog-dedent (line)
  (save-excursion
    (forward-line line)
    (- (current-indentation) tab-width)))

(defun dyalog-indent (line)
  (save-excursion
    (forward-line line)
    (+ (current-indentation) tab-width)))

(defun dyalog-get-indent ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (cond
     ((bobp) dyalog-leading-spaces)
     ((looking-at dyalog-indent-stop)
      (dyalog-dedent -1))
     ((looking-at dyalog-indent-pause)
      (dyalog-search-indent t))
     (t
      (dyalog-search-indent nil)))))

(defun dyalog-search-indent (at-pause)
  (interactive)
  (let ((indented nil))
    (progn
      (save-excursion
        (while (not indented)
          (forward-line -1)
          (cond ((looking-at dyalog-indent-stop)
                 (set 'indented (current-indentation)))
                ((looking-at  dyalog-indent-start)
                 (set 'indented (if at-pause
                                    (current-indentation)
                                  (dyalog-indent 0))))
                ((bobp)
                 (set 'indented dyalog-leading-spaces)))))
      indented)))

(defun dyalog-indent-line ()
  (interactive)
  (indent-line-to (max dyalog-leading-spaces (dyalog-get-indent))))

(defun dyalog-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map dyalog-mode-map)
  (set-syntax-table dyalog-mode-syntax-table)
  ;; Below lines make [un]comment region and fill paragraph work correctly, I'm
  ;; not sure why defining the syntax table isn't enough.
  (set (make-local-variable 'comment-start) "⍝ ")
  (set (make-local-variable 'comment-start-skip) "⍝\\s-+")
  ;; Make comment processing use the syntax table
  (set (make-local-variable 'comment-use-global-state) t)
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'font-lock-defaults) '(dyalog-font-lock-keywords))
  ;; below line doesn't seem to help, same results as with standard
  ;; ediff-forward-word. If same line is in .emacs it works, so setting
  ;; here is probably too late (or early?).
  (setq ediff-forward-word-function 'dyalog-ediff-forward-word)
  (set (make-local-variable 'indent-line-function ) 'dyalog-indent-line)
  (setq major-mode 'dyalog-mode)
  (setq mode-name "Dyalog")
  (run-hooks 'dyalog-mode-hook))

(provide 'dyalog-mode)