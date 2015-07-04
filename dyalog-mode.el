;;; dyalog-mode.el --- Major mode for editing Dyalog APL source code -*- coding: utf-8 lexical-binding: t -*-

;; Copyright (C) 2008, 2009, 2010, 2011 Joakim Hårsman

;; Author: Joakim Hårsman <joakim.harsman@gmail.com>
;; Version: 0.5
;; Package-Requires: ((cl-lib "0.2"))
;; Keywords: languages
;; URL: https://bitbucket.org/harsman/dyalog-mode/

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
;; It supports syntax highlighting, indentation and convenience function like
;; toggling localization of variables. It can communicate with Dyalog
;; processes over a socket, allowing Emacs to be used as the editor for a
;; Dyalog session.
;;
;; Get the latest version at http://bitbucket.org/harsman/dyalog-mode

;;; Code:



(require 'cl-lib)

;; Set up mode specific keys below
(defvar dyalog-mode-map
  (let ((map(make-keymap)))
    (define-key map (kbd"M-RET") 'comment-indent-new-line)
    (define-key map (kbd"M-f") 'dyalog-ediff-forward-word)
    (define-key map (kbd"C-c C-c") 'dyalog-editor-fix)
    (define-key map (kbd"C-c C-q") 'dyalog-editor-fix-and-quit)
    (define-key map (kbd"C-c C-e") 'dyalog-editor-edit-symbol-at-point)
    (define-key map (kbd"C-c C-l") 'dyalog-toggle-local)
    map)
  "Keymap for Dyalog APL mode.")

(defvar dyalog-array-mode-map
  (let ((map(make-sparse-keymap)))
    ;;(define-key map (kbd"C-c C-c") 'dyalog-array-fix)
    (define-key map (kbd"C-c C-e") 'dyalog-editor-edit-symbol-at-point)
    map)
  "Keymap for Dyalog Array edit mode.")

;;;###autoload
(defun dyalog-fix-altgr-chars (keymap aplchars regularchars)
  "Fix a key map so AltGr+char isn't confused with C-M-char.

KEYMAP is an Emacs keymap.

APLCHARS is a string of APL-characters produced by pressing AltGr together
with some character.

REGULARCHARS is a string of the characters that when pressed
together with AltGr produce the corresponding apl character in APLCHARS."
  (dolist (pair (cl-mapcar #'cons aplchars regularchars))
    (let* ((aplchar (car pair))
           (char    (cdr pair))
           (aplkey  (vector (list 'control 'meta aplchar)))
           (regkey  (vector (list 'control 'meta char)))
           (fun  (lookup-key (current-global-map) regkey)))
      (when fun
        (define-key keymap aplkey fun)))))

(defconst dyalog-keyword-regex
  "\\(^\\s-*:\\([A-Za-z]+\\)\\)\\|\\(⋄\\s-*:\\(?2:[A-Za-z]+\\)\\)")

(defconst dyalog-middle-keyword-regex
  "\\s-+\\(:\\(In\\|InEach\\)\\)\\s-+")

(defvar dyalog-ascii-chars "][<>+---=/¨~\\?*(){}&|.;"
  "APL symbols also present in ASCII.")

(defvar dyalog-keyword-chars
  "×≤≥≠∨∧÷∊⍴↑↓⍳○←→⌈⌊∘⍎⍕⊂⊃∩∪⊥⊤⍨⍒⍋⌽⍉⊖⍟⍱⍲⍬⌹≡≢⍪⌿⍀⍺⍵⎕⍞⋄⍷⌷⍣⊣⊢⌶")

(defvar dyalog-name  "[A-Za-z∆_]+[A-Za-z∆_0-9]*")

(defvar dyalog-number
  "[^A-Za-z_∆0-9]\\(¯?[0-9]+\\.?[0-9]*\\(E¯?[0-9]+\\.?[0-9]*\\)?\\)")

(defconst dyalog-access-type
  "^\\s-*:Access +\\(WebMethod\\|\\(?:\\(Public\\|Private\\)\\)?\\(?: +\\(Instance\\( +Override\\|Overridable\\)\\|Shared\\)\\)?\\)")

(defconst dyalog-field-def
  (concat "^\\s-*:Field"
          "\\(?: +\\(Public\\|Private\\)\\)?"
          "\\(?: +\\(Instance\\|Shared\\)\\)?"
          "\\(?: +\\(ReadOnly\\)\\)?"
          " +" "\\(" dyalog-name "\\)"))

(defconst dyalog-naked-nabla "^\\s-*∇\\s-*$")

(defconst dyalog-func-start "\\(?:\\`\\|∇[\r\n]*\\)\\s-*")

(defconst dyalog-func-retval "\\(?:\\(?2:[A-Za-z_]+\\) *← *\\|{\\(?2:[a-zA-Z_]+\\)} *← *\\)?")

(defconst dyalog-func-larg "\\(?:\\(?3:[A-Za-z_]+\\) +\\|{\\(?3:[A-Za-z_]+\\)} *\\)")

(defconst dyalog-func-name (concat "\\(?1:" dyalog-name "\\)"))

(defconst dyalog-func-rarg "\\(?: +\\(?4:[A-Za-z_]+\\)\\)")

(defconst dyalog-func-header-end "\\s-*\\(?5:;\\|$\\)")

(defconst dyalog-func-niladic (concat "\\(?:" dyalog-func-name
                                      dyalog-func-header-end "\\)"))

(defconst dyalog-func-monadic (concat "\\(?:" dyalog-func-name
                                      dyalog-func-rarg
                                      dyalog-func-header-end "\\)"))

(defconst dyalog-func-dyadic (concat "\\(?:" dyalog-func-larg
                                     dyalog-func-name
                                     dyalog-func-rarg
                                     dyalog-func-header-end "\\)"))

(defconst dyalog-tradfn-header (concat dyalog-func-start dyalog-func-retval
                                       "\\(?:" dyalog-func-niladic "\\|"
                                       dyalog-func-monadic "\\|"
                                       dyalog-func-dyadic "\\)"))

(defvar dyalog-font-lock-keywords
  (list
   ;; See emacs help for `font-lock-keywords' for a description of how the
   ;; below values work
   ;; System functions
   '("⎕[A-Za-z]*" . font-lock-builtin-face)
   ;; Keywords
   `(,dyalog-keyword-regex
     . (2 font-lock-keyword-face nil))
   `(,dyalog-middle-keyword-regex . (2 font-lock-keyword-face nil))
   ;; Guards
   '(":" . font-lock-keyword-face)
   ;; Labels
   '("^\\s-*\\([A-Za-z_][A-Za-z0-9_]*:\\)" . (1 font-lock-keyword-face t))
   ;; Numerical constans
   `(,dyalog-number (1 font-lock-constant-face nil))
   ;; APL chars
   (cons (concat "[" dyalog-ascii-chars "]") 'font-lock-keyword-face)
   (cons (concat "[" dyalog-keyword-chars "]") 'font-lock-keyword-face)
   ;; Localizations
   '(";\\([A-Za-z0-9_∆]+\\)" (1 font-lock-constant-face nil))
   ;; Illegal chars (and del/nabla)
   '("[∇$@\"%]+" . font-lock-warning-face)
   `(,dyalog-access-type (1 font-lock-keyword-face)
                         (2 font-lock-keyword-face)
                         (3 font-lock-keyword-face))
   `(,dyalog-field-def (1 font-lock-keyword-face t t)
                       (4 font-lock-variable-name-face)
                       (2 font-lock-keyword-face t t)
                       (3 font-lock-keyword-face t t)))
  "Default highlighting mode for Dyalog mode.")

(defvar dyalog-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Make various APL chars punctuation
    (dolist (char
             (string-to-list (concat dyalog-keyword-chars dyalog-ascii-chars)))
      (modify-syntax-entry char "." st))
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
    (modify-syntax-entry ?\" "." st)
    ;; Delimiters
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for `dyalog-mode'.")

(defvar dyalog-array-mode-syntax-table
  (let ((st (make-syntax-table)))
    (dolist (char
             (string-to-list (concat dyalog-keyword-chars dyalog-ascii-chars)))
      (modify-syntax-entry char "." st))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?∆ "_" st)
    (modify-syntax-entry ?⎕ "_" st)
    ;; Delimiters
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for `dyalog-array-mode'.")

(defconst dyalog-dfun-syntax-table
  (let ((st (copy-syntax-table dyalog-mode-syntax-table)))
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    (modify-syntax-entry ?\[ "." st)
    (modify-syntax-entry ?\] "." st)
    st)
  "Syntax table to only consider {} as parens.")

(defface dyalog-local-name
  '((t (:inherit font-lock-constant-face)))
  "Face used for localized names inside APL functions."
  :group 'dyalog)

(defface dyalog-local-system-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for localized system variables inside APL functions."
  :group 'dyalog)

;;;###autoload
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

(defcustom dyalog-fix-whitespace-before-save nil
  "If true, indent and delete redundant whitespace before saving."
  :type 'boolean
  :group 'dyalog)

;;; Indentation

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
  "Calculate the amount of indentation for the current line."
  (let ((indent 0))
    (save-excursion
      (move-beginning-of-line nil)
      (set 'indent
           (cond
            ((bobp)
             (dyalog-leading-indentation))
            ((looking-at dyalog-indent-stop)
             (dyalog-search-indent t 'dyalog-indent-cond-generic 0 0))
            ((looking-at dyalog-indent-case)
             (dyalog-search-indent t 'dyalog-indent-cond-case 0 0))
            ((looking-at dyalog-indent-pause)
             (dyalog-search-indent t 'dyalog-indent-cond-generic 0 0))
            ((looking-at "^\\s-*⍝")
             (if dyalog-indent-comments
                 (dyalog-search-indent nil 'dyalog-indent-cond-generic 0 0)
               (skip-syntax-forward "-")))
            ((looking-at "^[A-Za-z_]+[A-Za-z0-9_]*:")
             0)
            ((looking-at dyalog-naked-nabla)
             (dyalog-search-indent t 'dyalog-indent-cond-generic 0 0))
            ((looking-at (concat "\\s-*" dyalog-tradfn-header))
             (dyalog-search-indent nil 'dyalog-indent-cond-header 0 0))
            (t
             (dyalog-search-indent nil 'dyalog-indent-cond-generic 0 0))))
      (if (and (eq indent 1) (looking-at "\\s-*$"))
          0
        indent))))

(defun dyalog-indent-cond-generic (at-pause blockcount funcount)
  "Logic to use when searching for a point to calculate indent relative to.
AT-PAUSE is t if we are currently at a pausing indentation
keyword, i.e. a keyword that is indented at the same level as the
parent block, such as :Case.
BLOCKCOUNT is the number of currently open statement blocks.
FUNCOUNT is the number of currently open function blocks."
  (let ((indent nil))
    (cond ((looking-at dyalog-indent-stop)
           (if (and (eq blockcount 0) (eq funcount 0) (not at-pause))
               (set 'indent (current-indentation))
             (set 'blockcount (+ 1 blockcount))))

          ((looking-at dyalog-indent-start)
           (if (eq blockcount 0)
               (set 'indent (if at-pause
                                (current-indentation)
                              (dyalog-indent 0)))
             (set 'blockcount (- blockcount (if (looking-at
                                                 dyalog-block-start) 1 0)))))

          ((looking-at dyalog-naked-nabla)
           (set 'funcount (+ 1 funcount)))

          ((looking-at (concat "\\s-*" dyalog-tradfn-header))
           (if (eq funcount 0)
               (set 'indent (if at-pause
                                (current-indentation)
                              (skip-chars-forward "∇ ")))
             (set 'funcount (- funcount 1))))

          ((bobp)
           (set 'indent (dyalog-leading-indentation))))
    (list indent blockcount funcount)))

(defun dyalog-indent-cond-case (at-pause blockcount funcount)
  "Logic to use when indenting a :Case keyword.
When indenting a :Case, we should indent to any
matching :Trap or :Select.
AT-PAUSE is t if we are currently at a pausing indentation
keyword, i.e. a keyword that is indented at the same level as the
parent block, such as :Case.
BLOCKCOUNT is the number of currently open statement blocks.
FUNCOUNT is the number of currently open function blocks."
  (let ((dyalog-indent-start "^\\s-*:\\(Select\\|Trap\\)")
        (dyalog-indent-stop "^\\s-*:End\\(Select\\|Trap\\)"))
    (dyalog-indent-cond-generic at-pause blockcount funcount)))

(defun dyalog-indent-cond-header (at-pause blockcount funcount)
  "Logic to use when indenting a function header.
When indenting a header, we should indent to any
preceeding :Class or :Namespace.
AT-PAUSE is t if we are currently at a pausing indentation
keyword, i.e. a keyword that is indented at the same level as the
parent block, such as :Case.
BLOCKCOUNT is the number of currently open statement blocks.
FUNCOUNT is the number of currently open function blocks."
  (let ((dyalog-indent-start "^\\s-*:\\(Class\\|Namespace\\)")
        (dyalog-indent-stop  "^\\s-*:End\\(Class\\|Namespace\\)"))
    (dyalog-indent-cond-generic at-pause blockcount funcount)))

(defun dyalog-search-indent (at-pause cond-fun blockcount funcount)
  "Search backwards for a point to calculate indentation relative to.
AT-PAUSE is t if we are currently at a pausing indentation
keyword, i.e. a keyword that is indented at the same level as the
parent block, such as :Case.
COND-FUN is a function to call to do the actual search.
BLOCKCOUNT is the number of currently open statement blocks.
FUNCOUNT is the number of currently open function blocks."
  (let ((ret nil)(indented nil))
    (save-excursion
      (while (not indented)
        (beginning-of-line)
        (forward-line -1)
        (set 'ret (funcall cond-fun at-pause blockcount funcount))
        (set 'indented (car ret))
        (set 'blockcount (cadr ret))
        (set 'funcount (cl-caddr ret)))
      indented)))

(defun dyalog-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((restorepos (> (current-column) (current-indentation)))
        (indent (max 0 (dyalog-get-indent))))
    (if restorepos
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun dyalog-fix-whitespace-before-save ()
  "Clean up whitespace in the current buffer before saving."
  (when (and (eq major-mode 'dyalog-mode) dyalog-fix-whitespace-before-save)
    (dyalog-fix-whitespace)))

(defun dyalog-fix-whitespace ()
  "Clean up white space in the current buffer like Dyalog does."
  (interactive)
  (let ((dyalog-indent-comments nil)
        (punctuation-char "\\s.\\|\\s(\\|\\s)"))

    (save-excursion
      (delete-trailing-whitespace)
      ;; Reduce all runs of whitespace to a single space, except
      ;; when succeeded by a comment character, or if inside a comment
      ;; or string literal
      (goto-char (point-min))
      (while (re-search-forward "  +\\([^⍝ \r\n]\\)" (point-max) t)
        (let ((ws-start (match-beginning 1)))
          (unless (dyalog-in-comment-or-string ws-start)
            (replace-match " \\1"))))
      ;; Remove spaces before punctuation
      (goto-char (point-min))
      (while (re-search-forward (concat "\\([^ \r\n]\\)" "\\( +\\)"
                                        "\\(" punctuation-char "\\)")
                                (point-max)
                                t)
        (let ((start (match-beginning 0))
              (ws-start (match-beginning 2)))
          (unless (or (dyalog-in-comment-or-string ws-start)
                      (string-match "[∇⋄⍬]" (match-string 3))
                      (string-match "[∇⋄⍬]" (match-string 1))
                      (dyalog-in-keyword (match-beginning 3))
                      (dyalog-in-keyword (match-beginning 1)))
            (replace-match "\\1\\3")
            (goto-char start))))
      ;; Now remove spaces after punctuation unless they are followed by a
      ;; comment. We can't remove spaces both before and after punctuation in
      ;; one pass because matches might overlap.
      (goto-char (point-min))
      (while (re-search-forward (concat "\\(" punctuation-char "\\)"
                                        "\\( +\\)" "\\([^⍝ \r\n]\\)")
                                (point-max)
                                t)
        (let ((start (match-beginning 0))
              (ws-start (match-beginning 2)))
          (unless (or (dyalog-in-comment-or-string ws-start)
                      (string-match "[∇⋄⍬]" (match-string 1))
                      (string-match "[∇⋄⍬]" (match-string 3))
                      (dyalog-in-keyword (match-beginning 3)))
            (replace-match "\\1\\3")
            (goto-char start))))
      (dyalog-indent-buffer))))

(defun dyalog-indent-buffer ()
  "Indent the current buffer."
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

;;; Defun recognition and navigation

(defvar dyalog-imenu-generic-expression
  `(("Functions"  ,dyalog-tradfn-header 1)
    ("Namespaces" "^\\s-*:Namespace *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)
    ("Classes"    "^\\s-*:Class *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)))

(defun dyalog-beginning-of-dfun ()
  "Move backward to the beginning of a dynamic function definition.
Assumes that point is within a dynamic function definition."
  (dyalog-skip-comment-or-string)
  (with-syntax-table dyalog-dfun-syntax-table
    (condition-case error
        (goto-char (scan-lists (point) -1 1))
      (scan-error nil))))

(defun dyalog-previous-defun ()
  "Move backward to the start of a function definition."
  ;; Point can be anywhere when this function is called
  (let ((done nil))
    (if (dyalog-dfun-name)
        (dyalog-beginning-of-dfun)
      (while (not done)
        (skip-chars-backward "^∇{}")
        (if (or (bobp) (not (dyalog-in-comment-or-string)))
            (progn
              (setq done t)
              (if (dyalog-on-tradfn-header)
                  (progn
                    (skip-chars-backward "^∇")
                    (ignore-errors (backward-char)))
                (progn
                  (cond
                   ((looking-back "{")
                    (backward-char)) ;; already at start, done
                   ((looking-back "}")
                    (backward-sexp))
                   ((looking-back "∇")
                    (backward-char)
                    (setq done nil))
                   ((looking-at "∇") ;; at naked nabla we started at
                    (forward-line -1)
                    (end-of-line)
                    (setq done nil))))))
          (ignore-errors (backward-char)))))))

(defun dyalog-next-defun (&optional limit)
  "Move to the beginning of the next defun.
If supplied, LIMIT limits the search."
  (let ((lim (or limit (point-max)))
        (done nil))
    (when (looking-at "{")
      (ignore-errors (forward-char)))
    (while (not done)
      (skip-chars-forward "^∇{" lim)
      (if (or (= (point) lim) (not (dyalog-in-comment-or-string)))
            (progn
              (setq done t)
              (if (dyalog-on-tradfn-header)
                  (progn
                    (skip-chars-backward "^∇" (line-beginning-position))
                    (ignore-errors (backward-char)))
                (progn
                  (cond
                   ((looking-at "{")
                    (backward-char)) ;; already at start, done
                   ((looking-at "∇")
                    (forward-char)
                    (setq done nil)))))))
        (ignore-errors (forward-char)))))

(defun dyalog-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a function definition.
If supplied, ARG moves that many defuns back."
  (interactive "^p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (while (< arg 0)
        (dyalog-next-defun)
        (cl-incf arg))
    (while (> arg 0)
      (dyalog-previous-defun)
      (cl-decf arg))))

(defun dyalog-end-of-defun (&optional bound)
  "Move forward to the end of a function definition.
If it is supplied, BOUND limits the search."
  ;; We can assume point is at the start of a defun when
  ;; this function is called.
  (let ((end (or bound (point-max)))
        (done nil)
        (in-dfun-p nil)
        (dfun-mode (and (looking-at "{") (not (dyalog-on-tradfn-header)))))
    (if dfun-mode
        (forward-sexp)
      (ignore-errors (forward-char)) ; skip past nabla
      (while (not done)
        (if (not (re-search-forward "^ *∇" end t))
            (progn
              (goto-char end)
              (setq done t))
          (when (setq done (not in-dfun-p))
            (ignore-errors (backward-char 1))
            (if (looking-at dyalog-tradfn-header)
                (ignore-errors (backward-char 1))
              (ignore-errors (forward-char 1)))))))))

(defun dyalog-skip-comment-or-string (&optional context)
  "If point is in a comment or string, move backward out of it.
CONTEXT is the result of `syntax-ppss' at point, or nil."
  (let ((ctx (syntax-ppss-context (or context (syntax-ppss)))))
    (cond
     ((eq ctx 'string) (re-search-backward "\\s\""))
     ((eq ctx 'comment) (re-search-backward "\\s<")))))

(defun dyalog-dfun-name ()
  "If point is inside a dynamic function return the functions name.
If point is inside an anonymous function, return \"\", and if it
isn't inside a dynamic function, return nil"
  (interactive)
  (save-excursion
    (let ((syn-table dyalog-dfun-syntax-table)
          (openbrace nil)
          (context (syntax-ppss)))
      (dyalog-skip-comment-or-string context)
      (setq openbrace
            (with-syntax-table syn-table
              (condition-case err
                  (goto-char (scan-lists (point) -1 1))
                (scan-error nil))))
      (let* ((in-dfun-p (and openbrace
                             (not (dyalog-on-tradfn-header)))))
        (if in-dfun-p
            (progn
              (goto-char openbrace)
              (let ((dfun-name
                     (if (looking-back (concat "\\_<\\(" dyalog-name "\\) *← *")
                                       (line-beginning-position)
                                       t)
                         (match-string-no-properties 1)
                       "")))
                (condition-case err
                    (progn
                      (forward-sexp)
                      (if (looking-at " *[^\r\n ⋄]")
                          ""
                        dfun-name))
                  (scan-error dfun-name))))
          nil)))))

(defun dyalog-current-defun ()
  "Return the name of the defun point is in."
  (let ((dfun-name (dyalog-dfun-name)))
    (or dfun-name (car (dyalog-tradfn-info)))))

(defun dyalog-on-tradfn-header ()
  "Return t if point is on a tradfn header line, otherwise return nil."
  (save-excursion
    (let ((start (point))
          (min (save-excursion (forward-line -1)(line-beginning-position))))
      (forward-line)
      (end-of-line)
      (if (re-search-backward dyalog-tradfn-header min t)
          (progn
            (goto-char (match-end 0))
            (and (>= start (match-beginning 0)) (<= start (line-end-position))))
        nil))))

(defun dyalog-tradfn-info ()
  "Return a list of information on the tradfn defun point is in.
This name is only valid if point isn't inside a dfn.  The list
contains the name of the function a list containing the names of
the arguments, a list containing localized names, the character
position where the function header ends and the character
position where the defun ends."
  (save-excursion
    (let ((start-pos (point)))
      (dyalog-previous-defun)
      (when (not (looking-at "∇"))
        (forward-line -1))         ; Nabla is on its own line
      (if (re-search-forward dyalog-tradfn-header nil t)
          (let* ((tradfn-name (match-string-no-properties 1))
                 (retval (match-string-no-properties 2))
                 (larg (match-string-no-properties 3))
                 (rarg (match-string-no-properties 4))
                 (localstart (match-end 5))
                 (end-of-header (line-end-position))
                 (args (remq nil (list retval larg rarg)))
                 (locals nil)
                 (end-of-defun 0))
            (dyalog-end-of-defun)
            (setq end-of-defun (point))
            (if (< end-of-defun start-pos)
                (list "" nil nil 0 0)
              (progn
                (setq locals
                      (split-string
                       (buffer-substring-no-properties localstart end-of-header)
                       ";" 'omit-nulls "[ \t\r\n]+"))
                (list tradfn-name args locals end-of-header end-of-defun))))
        (list "" nil nil 0 0)))))

;;; Font Lock

(defun dyalog-defun-info ()
  "Return information on the defun at point."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "^∇\r\n")
    ;; TODO: In the tradfn case we already have a match, so no need to call
    ;; tradfn-info, just extract the match data directly.
    (if (looking-at dyalog-tradfn-header)
        (list 'tradfn (progn
                        (forward-char)
                        (dyalog-tradfn-info)))
      (list 'dfun   (dyalog-dfun-name)))))


;; TODO: We need a separate function for getting info on the defun at point,
;; which is something we can use to get an initial state, and moving forward
;; to the next defun and getting info on that. That way we avoid redundant
;; work.
;;
;; TODO: syntax-ppss consumes most of the cpu and does most of the
;; allocations. Try just skipping matches that have already been fontified
;; instead, that way we could cheaply skip matches inside comments, strings
;; and keywords.
(defun dyalog-fontify-locals (start end)
  "Fontify local names in tradfns.
START and END signify the region to fontify."
  (save-excursion
    (let* ((beg-line (progn (goto-char start)(line-beginning-position)))
           (done nil)
           (case-fold-search nil)
           (dfun-name nil)
           (info nil))
      ;; Remove old fontification here?
      (goto-char beg-line)
      (setq info (dyalog-tradfn-info))
      ;; FIXME: If start is inside a dfun, this won't work. Call
      ;; dyalog-defun-info instead?

      (while (not done)
        (if dfun-name
            (let ((dfunend end))
              ;; TODO add actually working code here. We should have a
              ;; dfun-info that returns extents as well
              (search-forward "}" dfunend t))
          (let ((fname (car info)))
            (when (and fname (not (string-equal fname "")))
              (let* ((args (nth 1 info))
                     (localizations (nth 2 info))
                     (locals (append args localizations))
                     (end-of-header (nth 3 info))
                     (end-of-defun (nth 4 info))
                     (limit (min end-of-defun end))
                     (rx (concat "\\_<\\("
                                 (mapconcat 'identity locals "\\|")
                                 "\\)\\_>")))
                (goto-char end-of-header)
                (while (re-search-forward rx limit t)
                  (let* ((symbol-start (match-beginning 0))
                         (symbol-end (match-end 0))
                         (state (syntax-ppss))
                         (context (syntax-ppss-context state))
                         (in-string (eq 'string context))
                         (in-comment (eq 'comment context))
                         (sysvar (eq ?⎕ (char-after symbol-start)))
                         (face (if sysvar
                                   'dyalog-local-system-name
                                 'dyalog-local-name)))
                    (unless (or in-string in-comment)
                      (put-text-property symbol-start symbol-end
                                         'face
                                         face)
                      (if (and (equal ?. (char-after symbol-end))
                               (looking-at (concat "\\." dyalog-name)))
                          (put-text-property (match-beginning 0)
                                             (match-end 0)
                                             'face
                                             face)))))
                (goto-char limit)))))
        (set 'done (>= (point) end))
        (when (not done)
          (dyalog-next-defun end)
          (when (not (setq done (>= (point) end)))
            (let ((defuninfo (dyalog-defun-info)))
              (pcase (car defuninfo)
                (`dfun (setq dfun-name (nth 1 defuninfo)
                             info (list "" nil nil)))
                (`tradfn (setq dfun-name nil
                               info (nth 1 defuninfo)))))))))))


;;; Syntax

(defun dyalog-syntax-propertize-function (start end)
  "Alter syntax table for escaped single quotes within strings.
START and END delimit the region to analyze."
  (save-excursion
    (goto-char start)
    (while (and
            (search-forward "''" end 'no-error)
            (< (point) end))
      (goto-char (match-beginning 0))
      (let* ((endpos (match-end 0))
             (state (syntax-ppss))
             (context (syntax-ppss-context state)))
        (when (eq 'string context)
          (put-text-property (point) (+ 2 (point))
                             'syntax-table
                             (string-to-syntax ".")))
        (goto-char endpos)))))

(defun dyalog-current-keyword (&optional pt)
  "Return the current keyword if PT is in a keyword (e.g. :If).
PT is optional and defaults to point.  If PT isn't in a keyword,
return nil."
  (save-excursion
    (when pt
      (goto-char pt))
    (skip-chars-backward "A-Za-z:")
    (skip-syntax-backward "-")
    (let ((keyword
           (if (or (looking-at dyalog-keyword-regex)
                   (looking-at dyalog-middle-keyword-regex))
               (string-trim (match-string-no-properties 0))
             nil)))
      (if (and keyword (dyalog-dfun-name))
          nil
        keyword))))

(defun dyalog-in-keyword (&optional pt)
  "Return t if PT (defaults to point) is inside a keyword (e.g. :If)."
  (not (not (dyalog-current-keyword))))

(defun dyalog-in-comment-or-string (&optional pt)
  "Return t if PT (defaults to point) is inside a string literal or a comment."
  (save-excursion
    (progn
      (when pt
        (goto-char pt))
      (let ((match (match-data))
            (res (not (not
                       (memq (syntax-ppss-context (syntax-ppss))
                             '(string comment))))))
        (set-match-data match)
        res))))


;;; Socket connection
(defvar dyalog-connection ()
  "The connection to a Dyalog process used for this buffer, if any.")

(defvar dyalog-connections ()
  "A list of all connections to Dyalog processes.")

;;;###autoload
(defun dyalog-session-connect (&optional host port)
  "Connect to a Dyalog session.
HOST (defaults to localhost) and PORT (defaults to 7979) give
adress to connect to."
  (interactive (list (read-string "Host (default localhost):"
                                  "127.0.0.1")
                     (read-number "Port (default 7979):" 7979)))
  (make-comint "dyalog" (cons host port))
  (switch-to-buffer "*dyalog*")
  (set-buffer-process-coding-system 'utf-8-dos 'utf-8-dos)
  (setq-default comint-scroll-show-maximum-output nil)
  (define-key (current-local-map)
    (kbd"C-c C-e") 'dyalog-editor-edit-symbol-at-point)
  (run-hooks 'dyalog-session-connect-hook))

;;;###autoload
(defun dyalog-editor-connect (&optional host port)
  "Connect to a Dyalog process as an editor.
HOST (defaults to localhost) and PORT (defaults to 8080) give
adress to connect to."
  (interactive (list (read-string "Host (default localhost):"
                                  "127.0.0.1")
                     (read-number "Port (default 8080):" 8080)))
  (let* ((bufname (generate-new-buffer-name " *dyalog-receive*"))
         (process (make-network-process :name "dyalog-edit"
                                        :buffer bufname
                                        :family 'ipv4 :host host :service port
                                        :sentinel 'dyalog-editor-sentinel
                                        :filter 'dyalog-editor-receive
                                        :coding 'utf-8-dos)))
    (push process dyalog-connections)
    (set-process-query-on-exit-flag process nil)
    process))

(defun dyalog-editor-sentinel (proc msg)
  "Callback for socket errors.
PROC is the socket/process and MSG is a string describing the event/error."
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))
    (setq dyalog-connections (delq proc dyalog-connections))))

(defun dyalog-editor-receive (process output)
  "Receive data from a Dyalog editor connection.
PROCESS is the socket receiving data and OUTPUT is the data received."
  (with-current-buffer (process-buffer process)
    (save-excursion
      ;; Insert the text, advancing the process marker.
      (goto-char (process-mark process))
      (insert output)
      (set-marker (process-mark process) (point))
      (goto-char (point-min))
      (while (search-forward "\e" nil t)
        (backward-char)
        (let ((m (point)))
          (goto-char (point-min))
          (dyalog-editor-munge-command process (point) m)
          (with-current-buffer (process-buffer process)
            (set-marker (process-mark process) 1)))
        (sit-for 0.01)))))

(defun dyalog-editor-munge-command (process start end)
  "Parse and delete a Dyalog editor command in the currently active region.
PROCESS is the socket receiving the command, START is the start
of the command and END is where it ends."
  (cond ((looking-at "edit \\([^ []+\\)\\(\\[\\([0-9]+\\)\\]\\)?\0\\([^\0]*\\)\0")
         (let ((name (match-string 1))
               (linetext (match-string 3))
               (lineno nil)
               (path (match-string 4))
               (src  (buffer-substring-no-properties (match-end 0) end)))
           (when linetext
             (set 'lineno (string-to-number linetext)))
           (delete-region start (1+ end))
           (dyalog-open-edit-buffer process name src lineno path)))
        ((looking-at "fxresult \\([^ ]+\\)\e")
         (let* ((result (match-string 1))
                (num    (string-to-number result)))
           (if (eq num 0)
               (message "Fixed as %s" result)
             (message "Can't fix, error in line %d" num))
           (delete-region start (1+ end))))
        ((looking-at "editarray \\([^ ]+\\) \\([^ ]+\\) ")
         (let* ((name (match-string 1))
                (kind (match-string 2))
                (src (buffer-substring-no-properties (match-end 0) end)))
           (delete-region start (1+ end))
           (dyalog-open-edit-array process name kind src)))
        ((looking-at "dyaloghello \n")
         (progn
           (goto-char (match-end 0))
           (while (looking-at "\\([a-z]+\\): \\([^\r\n]+\\)\n")
             (let* ((key (match-string-no-properties 1))
                    (val (match-string-no-properties 2))
                    (propname (concat "dyalog-" key)))
               (process-put process (intern propname) val)
               (goto-char (match-end 0))))
           (delete-region start (1+ end))))
        (t
         (error "Ivalid message received"))))

(defun dyalog-open-edit-buffer (process name src &optional lineno path)
  "Open a buffer to edit object from socket PROCESS named NAME with source SRC.
PROCESS is the socket connection associated with the buffer.
LINENO optionally moves point to the given line and PATH contains
a string with the path to the source file associated with the
edit buffer."
  (let* ((file-name (if (and path (not (string= path "")))
                        path
                      nil))
         (bufname (if file-name
                      (file-name-nondirectory file-name)
                    name)))
    (switch-to-buffer bufname)
    (setq buffer-undo-list t)
    (let ((pos (point)))
      (save-excursion
        (mark-whole-buffer)
        (delete-region (point) (mark))
        (insert src))
      (when file-name
        (set-visited-file-name file-name t)
        (set-buffer-modified-p nil))
      (dyalog-mode)
      (setq dyalog-connection process)
      (font-lock-fontify-buffer)
      (if lineno
          (forward-line (- lineno 1))
        (goto-char (min pos (point-max))))
      (setq buffer-undo-list nil)
      (select-frame-set-input-focus (window-frame (selected-window))))))

(defun dyalog-open-edit-array (process name kind src)
  "Open a buffer to edit array.
PROCESS is the socket connection associated with the buffer, NAME
is the name of the array, KIND is the type of array and is
\"charvec\", \"charmat\", \"stringvec\" or \"array\". SRC is the
formatted contents of the array"
  (switch-to-buffer name)
  (setq buffer-undo-list t)
  (let ((pos (point))
        (lineno nil))
    (save-excursion
      (when buffer-read-only
        (setq buffer-read-only nil))
      (mark-whole-buffer)
      (delete-region (point) (mark))
      (insert src))
    (dyalog-array-mode)
    (setq dyalog-connection process)
    (if (fboundp 'read-only-mode)    ; Only available in 24.4 and later
        (read-only-mode)
      (setq buffer-read-only t))
    (if lineno
        (forward-line (- lineno 1))
      (goto-char (min pos (point-max))))
    (setq buffer-undo-list nil)
    (select-frame-set-input-focus (window-frame (selected-window)))))

(defun dyalog-connection-desc (process)
  "Return a string describing PROCESS."
  (let ((version (process-get process 'dyalog-version))
        (wsid    (process-get process 'dyalog-wsid))
        (cwd     (process-get process 'dyalog-dir))
        (host (process-contact process :host))
        (port (process-contact process :service)))
    (if (and version wsid cwd)
        (let ((cwd-short (and (string-match "[^/\\]+\\'" cwd)
                              (match-string 0 cwd)))
              (wsid-short (and (string-match "[^/\\]+\\'" wsid)
                               (match-string 0 wsid))))
          (format "%s in %s v%s" wsid-short cwd-short version))
      (format "%s:%s" host port))))

(defun dyalog-connection-select (&optional prompt)
  "Select one of the active connections to Dyalog processes.
PROMPT is the prompt to show to the user."
  (let ((p (or prompt "Select a Dyalog process:"))
        (candidates (mapcar
                     'dyalog-connection-desc dyalog-connections)))
    (or (and (process-live-p dyalog-connection) dyalog-connection)
        (and (equal 1 (length dyalog-connections))
             (car dyalog-connections))
        (nth (cl-position (completing-read p candidates nil t)
                          candidates :test 'string-equal)
             dyalog-connections))))

(defun dyalog-editor-fix (&optional process)
  "Send the contents of the current buffer to the connected Dyalog process."
  (interactive)
  (let ((process (or process (dyalog-connection-select))))
    (setq dyalog-connection process)
    (process-send-string process "fx ")
    (process-send-region process (point-min) (point-max))
    (process-send-string process "\e")))

(defun dyalog-editor-fix-and-quit ()
  "Send the contents of the current buffer to the connected
  Dyalog process, kill the buffer and move focus to the Dyalog
  session."
  (interactive)
  (let ((process (dyalog-connection-select))
        (kill-buffer-query-functions ()))
    (dyalog-editor-fix process)
    ;; TODO: We really should verify that the fix is successful here...
    (when (kill-buffer)
      (process-send-string process "focus \e"))))

(defun dyalog-editor-edit (name)
  "Open source of symbol NAME in an edit buffer."
  (interactive "s")
  (let ((process (dyalog-connection-select)))
    (setq dyalog-connection process)
    (process-send-string process (concat "src " name "\e"))))

(defun dyalog-editor-edit-symbol-at-point ()
  "Edit the source for the symbol at point."
  (interactive)
  (let ((sym (symbol-at-point))
        (linespec ""))
    (when (looking-at "[A-Za-z∆_0-9]+\\(\\[[0-9]+\\]\\)")
      (setq linespec (match-string 1)))
    (dyalog-editor-edit (concat (symbol-name sym) linespec))))

(defun dyalog-toggle-local ()
  "Toggle localization for symbol at point."
  (interactive)
  (let* ((sym   (symbol-at-point))
         (symname  (symbol-name sym))
         (name (substring-no-properties symname))
         (str   (concat ";" name))
         (info (dyalog-tradfn-info))
         (fname (nth 0 info))
         (end-of-header (nth 3 info)))
    (unless (or (not sym)
                (equal (length fname) 0)
                (dyalog-in-comment-or-string)
                (dyalog-in-keyword)
                (dyalog-dfun-name))
      (save-excursion
        (beginning-of-defun)
        (skip-chars-forward "∇ \r\n")
        (when (looking-at dyalog-naked-nabla)
          (forward-line 1))
        (if (search-forward str end-of-header t)
            (progn
              (goto-char (match-beginning 0))
              (delete-char (length str))
              (message "Made %s non-local in function %s" name fname))
          (progn
            (move-end-of-line nil)
            (insert str)
            (message "Made %s local in function %s" name fname)))))))

(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'dyalog-mode))

;;;###autoload
(define-derived-mode dyalog-mode prog-mode "Dyalog"
  "Major mode for editing Dyalog APL code.

\\{dyalog-mode-map}"
  :group 'dyalog
  :syntax-table dyalog-mode-syntax-table
  (set (make-local-variable 'syntax-propertize-function)
       #'dyalog-syntax-propertize-function)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'beginning-of-defun-function)
       'dyalog-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'dyalog-end-of-defun)
  ;; Comments
  (set (make-local-variable 'comment-start) "⍝ ")
  (set (make-local-variable 'comment-start-skip) "⍝+\\s-*")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (set (make-local-variable 'font-lock-defaults) '(dyalog-font-lock-keywords))
  (jit-lock-register #'dyalog-fontify-locals)
  ;; Dyalog always indents with spaces
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'dyalog-indent-line)
  ;; Misc
  (set (make-local-variable 'require-final-newline) nil)
  ;; Socket connection
  (set (make-local-variable 'dyalog-connection) nil)
  ;; Imenu and which-func-mode
  (set (make-local-variable 'imenu-generic-expression)
       dyalog-imenu-generic-expression)
  (add-hook 'which-func-functions 'dyalog-current-defun nil 'make-it-local)
  ;; Hooks
  (add-hook 'before-save-hook
            'dyalog-fix-whitespace-before-save nil 'make-it-local))

(define-derived-mode dyalog-array-mode fundamental-mode "DyalogArr"
  "Major mode for editing Dyalog APL arrays.

\\{dyalog-array-mode-map\\}"
  :syntax-table dyalog-array-mode-syntax-table
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'dyalog-connection) nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dyalog$" . dyalog-mode))


(provide 'dyalog-mode)

;;; dyalog-mode.el ends here
