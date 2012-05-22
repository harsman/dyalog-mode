;;; dyalog-mode.el --- Major mode for editing Dyalog APL source code -*- coding: utf-8 -*-

;; Copyright (C) 2008, 2009, 2010, 2011 Joakim Hårsman

;; Author: Joakim Hårsman <joakim.harsman@gmail.com>
;; Version: 0.32
;; Keywords: languages
;; X-URL: http://bitbucket.org/harsman/dyalog-mode
;; URL: https://bitbucket.org/harsman/dyalog-mode/raw/tip/dyalog-mode.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Dyalog-mode is a major mode for editing Dyalog APL source code.
;;
;; It supports basic syntax highlighting and indentation. It relies on regex
;; hacks for both indentation and syntax highlighting, so it sometimes gets
;; confused.
;;
;; If you use ediff to diff Dyalog APL source code, you can set
;; ediff-forward-word-function to dyalog-ediff-forward-word to get better
;; diffs.
;;
;; Get the latest version at http://bitbucket.org/harsman/dyalog-mode

;;; Code:



(require 'cl)

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
  "Keymap for Dyalog APL mode.")

;; This should probably be split into several layers of highlighting
(defconst dyalog-font-lock-keywords1
  (list
   ;; See emacs help for `font-lock-keywords' for a description of how the
   ;; below values work
   ;; System functions
   '("⎕[A-Za-z]*" . font-lock-builtin-face)
   ;; Guards
   '(":" . font-lock-keyword-face)
   ;; Keywords
   '("^\\s-*\\(:[A-Za-z]+\\)" . (1 font-lock-keyword-face t))
   ;; Labels
   '("^\\s-*\\([A-Za-z_][A-Za-z0-9_]*:\\)" . (1 font-lock-keyword-face t))
   ;; Numerical constans
   '("[^A-Za-z_∆0-9]\\(¯?[0-9]+\\.?[0-9]*\\(E¯?[0-9]+\\.?[0-9]*\\)?\\)" (1 font-lock-constant-face nil))
   ;; APL chars
   '("[][<>+---=/¨~\\\\?*(){}&|]" . font-lock-keyword-face)
   '("[*×≤≥>≠∨∧÷∊⍴↑↓⍳○←→⌈⌊∘⎕⍎⍕⊂⊃∩∪⊥⊤⍨⍒⍋⌽⍉⊖⍟⍱⍲⍬⌹≡≢⍪⌿⍀⍺⍵⎕⍞⋄⍷]"
     . font-lock-keyword-face)
   ;; Below line is broken for dfuns and very broken for
   ;; nested dfuns
   ;;'("{\\([^}]*\\)}" (1 font-lock-constant-face t))
   ;; Localizations
   '(";\\([A-Za-z0-9_∆]+\\)" (1 font-lock-constant-face nil))
   ;; Illegal chars (and del/nabla)
   '("[∇$@]+" . font-lock-warning-face))
  "Minimal highlighting for Dyalog APL.")

(defvar dyalog-font-lock-keywords dyalog-font-lock-keywords1
  "Default highlighting mode for Dyalog mode.")

(defvar dyalog-ascii-chars "[]<>+---=/¨~\\?*(){}¨&|"
  "APL symbols also present in ASCII.")

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
  (concat
   "\\(.*{[^{}\r\n]*$\\)" "\\|"
   "\\(^\\s-*:\\(If\\|While\\|Repeat\\|Trap\\|Case"
   "\\|For\\|Class\\|Hold\\|With\\|Namespace\\)[^⋄\r\n]*$\\)"))

(defvar dyalog-block-start
  (concat
   "\\(.*{[^{}\r\n]*$\\)" "\\|"
   "\\(^\\s-*:\\(If\\|While\\|Repeat\\|Trap\\|"
   "For\\|Class\\|Hold\\|With\\|Namespace\\)[^⋄\r\n]*$\\)"))

(defvar dyalog-indent-pause
  "^\\s-*:\\(Else\\|AndIf\\|OrIf\\)[^⋄\r\n]*$")

(defvar dyalog-indent-case
  "^\\s-*:Case")

(defvar dyalog-indent-stop
  "\\([^{\n\r]*}[^{}\r\n]*$\\)\\|\\(^\\s-*:End[A-Za-z]+[^⋄\r\n]*$\\)")

(defgroup dyalog nil
  "Major mode `dyalog-mode' for editing Dyalog APL code."
  :group 'languages
  :prefix "dyalog-")

(defcustom dyalog-mode-hook nil
  "List of functions to be executed on entry to `dyalog-mode'."
  :type 'hook
  :group 'dyalog)

(defcustom dyalog-leading-spaces 1
  "The number of leading spaces to use in the left margin."
  :type 'integer
  :group 'dyalog)

(defcustom dyalog-indent-comments t
  "True if comments should be indented according to the surrounding scope."
  :type 'boolean
  :group 'dyalog)

(defcustom dyalog-fix-whitespace nil
  "True if buffers should be re-indented and have trailing whitespace removed before they are saved."
  :type 'boolean
  :group 'dyalog)

(defun dyalog-dedent (line)
  "Dedent current line one level relative to LINE lines before."
  (save-excursion
    (forward-line line)
    (- (current-indentation) tab-width)))

(defun dyalog-indent (line)
  "Indent current line one level relative to LINE lines before."
  (save-excursion
    (forward-line line)
    (+ (current-indentation) tab-width)))

(defun dyalog-get-indent ()
  (let ((indent 0))
    (save-excursion
      (move-beginning-of-line nil)
      (set 'indent
           (cond
            ((bobp)
             dyalog-leading-spaces)
            ((looking-at dyalog-indent-stop)
             (dyalog-search-indent t 'dyalog-indent-cond-generic 0))
            ((looking-at dyalog-indent-case)
             (dyalog-search-indent t 'dyalog-indent-cond-case 0))
            ((looking-at dyalog-indent-pause)
             (dyalog-search-indent t 'dyalog-indent-cond-generic 0))
            ((looking-at "^\\s-*⍝")
             (if dyalog-indent-comments
                 (dyalog-search-indent nil 'dyalog-indent-cond-generic 0)
               (skip-syntax-forward "-")))
            ((looking-at "^[A-Za-z_]+[A-Za-z0-9_]*:")
             0)
            (t
             (dyalog-search-indent nil 'dyalog-indent-cond-generic 0))))
      (if (and (eq indent 1) (looking-at "\\s-*$"))
          0
        indent))))

(defun dyalog-indent-cond-generic (at-pause indented count)
  (cond ((looking-at dyalog-indent-stop)
         (if (and (eq count 0) (not at-pause))
             (set 'indented (current-indentation))
           (set 'count (+ 1 count))))
        ((looking-at  dyalog-indent-start)
         (if (eq count 0)
             (set 'indented (if at-pause
                                (current-indentation)
                              (dyalog-indent 0)))
           (set 'count (- count (if (looking-at dyalog-block-start) 1 0)))))
        ((bobp)
         (set 'indented dyalog-leading-spaces)))
  (list indented count))

(defun dyalog-indent-cond-case (at-pause indented count)
  (let ((dyalog-indent-start "^\\s-*:\\(Select\\|Trap\\)")
        (dyalog-indent-stop "^\\s-*:End\\(Select\\|Trap\\)"))
    (dyalog-indent-cond-generic at-pause indented count)))

(defun dyalog-search-indent (at-pause cond-fun count)
  (let ((ret nil)(indented nil))
    (progn
      (save-excursion
        (while (not indented)
          (forward-line -1)
          (set 'ret (funcall cond-fun at-pause indented count))
          (set 'indented (car ret))
          (set 'count (cadr ret)))
        indented))))

(defun dyalog-indent-line ()
  (indent-line-to (max 0 (dyalog-get-indent))))

(defun dyalog-fix-whitespace ()
  (let ((dyalog-indent-comments nil))
    (if (and (eq major-mode 'dyalog-mode)
             dyalog-fix-whitespace)
        (progn
          (save-excursion
            (delete-trailing-whitespace)
            (dyalog-indent-buffer))))))

(defun dyalog-indent-buffer ()
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

(defconst dyalog-func-start "\\(\\`\\|∇[\r\n]*\\)\\s-*")

(defconst dyalog-func-retval "\\([A-Za-z]+←\\|{[a-zA-Z]}←\\)?")

(defconst dyalog-func-larg "\\([A-Za-z_]+ *\\|{[A-Za-z_]+} *\\)?")

(defconst dyalog-func-name "\\([A-Za-z_]+[A-Za-z_0-9]*\\)")

(defconst dyalog-func-rarg "\\( +[A-Za-z_]+\\)?")

(defconst dyalog-tradfn-header (concat dyalog-func-start dyalog-func-retval
                                       dyalog-func-larg dyalog-func-name
                                       dyalog-func-rarg))

(defvar dyalog-imenu-generic-expression
  `(("Functions"  ,dyalog-tradfn-header 4)
    ("Namespaces" "^\\s-*:Namespace *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)
    ("Classes"    "^\\s-*:Class *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)))

(defun dyalog-mode ()
  "Major mode for editing Dyalog APL code."
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
  ;; Dyalog always indents with spaces
  (set (make-local-variable 'indent-tabs-mode) nil)
  ;; below line doesn't seem to help, same results as with standard
  ;; ediff-forward-word. If same line is in .emacs it works, so setting
  ;; here is probably too late (or early?).
  (setq ediff-forward-word-function 'dyalog-ediff-forward-word)
  (set (make-local-variable 'indent-line-function ) 'dyalog-indent-line)
  (setq major-mode 'dyalog-mode)
  (setq mode-name "Dyalog")
  ;; Imenu and which-func-mode
  (setq imenu-generic-expression dyalog-imenu-generic-expression)
  (eval-after-load "which-func"
    '(add-to-list 'which-func-modes 'dyalog-mode))
  ;; Hooks
  (add-hook 'before-save-hook 'dyalog-fix-whitespace)
  (run-hooks 'dyalog-mode-hook))

(provide 'dyalog-mode)

;;; dyalog-mode.el ends here
