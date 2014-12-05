;;; dyalog-mode.el --- Major mode for editing Dyalog APL source code -*- coding: utf-8 -*-
;;; -*- lexical-binding: t -*-

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



(require 'cl-lib)

;; Set up mode specific keys below
(defvar dyalog-mode-map
  (let ((map(make-keymap)))
    (define-key map (kbd"M-RET") 'comment-indent-new-line)
    (define-key map (kbd"M-f") 'dyalog-ediff-forward-word)
    (define-key map (kbd"C-c C-c") 'dyalog-editor-fix)
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

(defun dyalog-fix-altgr-chars (keymap aplchars regularchars)
  "Fix up a key map so that if the Dyalog IME uses AltGr+char for an
APL character, Emacs doesn't confuse it for C-M-char.

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
  "*×≤≥>≠∨∧÷∊⍴↑↓⍳○←→⌈⌊∘⍎⍕⊂⊃∩∪⊥⊤⍨⍒⍋⌽⍉⊖⍟⍱⍲⍬⌹≡≢⍪⌿⍀⍺⍵⎕⍞⋄⍷⌷⍣⊣⊢⌶")

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
  "True if buffers should be re-indented and have trailing
whitespace removed before they are saved."
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
  (let ((indent 0))
    (save-excursion
      (move-beginning-of-line nil)
      (set 'indent
           (cond
            ((bobp)
             dyalog-leading-spaces)
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

(defun dyalog-indent-cond-generic (at-pause indented blockcount funcount)
  (cond ((looking-at dyalog-indent-stop)
         (if (and (eq blockcount 0) (eq funcount 0) (not at-pause))
             (set 'indented (current-indentation))
           (set 'blockcount (+ 1 blockcount))))

        ((looking-at dyalog-indent-start)
         (if (eq blockcount 0)
             (set 'indented (if at-pause
                                (current-indentation)
                              (dyalog-indent 0)))
           (set 'blockcount (- blockcount (if (looking-at
                                               dyalog-block-start) 1 0)))))

        ((looking-at dyalog-naked-nabla)
           (set 'funcount (+ 1 funcount)))

        ((looking-at (concat "\\s-*" dyalog-tradfn-header))
         (if (eq funcount 0)
             (set 'indented (if at-pause
                                (current-indentation)
                              (skip-chars-forward "∇ ")))
           (set 'funcount (- funcount 1))))

        ((bobp)
         (set 'indented dyalog-leading-spaces)))
  (list indented blockcount funcount))

(defun dyalog-indent-cond-case (at-pause indented blockcount funcount)
  (let ((dyalog-indent-start "^\\s-*:\\(Select\\|Trap\\)")
        (dyalog-indent-stop "^\\s-*:End\\(Select\\|Trap\\)"))
    (dyalog-indent-cond-generic at-pause indented blockcount funcount)))

(defun dyalog-indent-cond-header (at-pause indented blockcount funcount)
  (let ((dyalog-indent-start "^\\s-*:\\(Class\\|Namespace\\)")
        (dyalog-indent-stop  "^\\s-*:End\\(Class\\|Namespace\\)"))
    (dyalog-indent-cond-generic at-pause indented blockcount funcount)))

(defun dyalog-search-indent (at-pause cond-fun blockcount funcount)
  (let ((ret nil)(indented nil))
    (save-excursion
      (while (not indented)
        (beginning-of-line)
        (forward-line -1)
        (set 'ret (funcall cond-fun at-pause indented blockcount funcount))
        (set 'indented (car ret))
        (set 'blockcount (cadr ret))
        (set 'funcount (cl-caddr ret)))
      indented)))

(defun dyalog-indent-line ()
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
        (let ((start (match-beginning 0))
              (ws-start (match-beginning 1)))
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
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

;;; Defun recognition and navigation

(defconst dyalog-func-start "\\(?:\\`\\|∇[\r\n]*\\)\\s-*")

(defconst dyalog-func-retval "\\(?:\\(?2:[A-Za-z]+\\) *← *\\|{\\(?2:[a-zA-Z]+\\)} *← *\\)?")

(defconst dyalog-func-larg "\\(?:\\(?3:[A-Za-z_]+\\) +\\|{\\(?3:[A-Za-z_]+\\)} *\\)")

(defconst dyalog-func-name "\\(?1:[A-Za-z_]+[A-Za-z_0-9]*\\)")

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

(defconst dyalog-naked-nabla "^\\s-*∇\\s-*$")


(defvar dyalog-imenu-generic-expression
  `(("Functions"  ,dyalog-tradfn-header 1)
    ("Namespaces" "^\\s-*:Namespace *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)
    ("Classes"    "^\\s-*:Class *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)))

(defun dyalog-previous-defun ()
"Move backward to the start of a function definition."
;; Point can be anywhere when this function is called
  (let ((pos (point))
        (start nil))
    (beginning-of-line)
    (skip-chars-forward "^∇\r\n")
    (if (and (looking-at dyalog-tradfn-header)
             (> pos (match-beginning 0)))
        (setq start (match-beginning 0))
      (progn
        (forward-line -1)
        (if (and (looking-at dyalog-naked-nabla)
                 (skip-chars-forward "^∇\r\n")
                 (looking-at dyalog-tradfn-header))
            (setq start (match-beginning 0))
          (progn
            (goto-char pos)
            (if (re-search-backward dyalog-tradfn-header (point-min) t)
                (setq start (match-beginning 0))
              (setq start (point-min)))))))
    (goto-char start)))

(defun dyalog-next-defun ()
  (if (not (re-search-forward dyalog-tradfn-header (point-max) t))
      (goto-char (point-max))
    (if (looking-at dyalog-naked-nabla)
        (forward-line 1))))

(defun dyalog-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a function definition."
  (interactive "^p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (while (< arg 0)
        (dyalog-next-defun)
        (cl-incf arg))
    (while (> arg 0)
      (dyalog-previous-defun)
      (cl-decf arg))))

(defun dyalog-end-of-defun ()
  "Move forward to the end of a function definition."
  ;; We can assume point is at the start of a defun when
  ;; this function is called.
  (let ((defunstart (point)))
    (ignore-errors (forward-char))
    (if (not (re-search-forward dyalog-tradfn-header (point-max) t))
        (unless (re-search-forward dyalog-naked-nabla (point-max) t)
          (goto-char (point-max))
          (end-of-line))
      (goto-char (match-beginning 0))
      (skip-chars-backward " ")
      (if (re-search-backward dyalog-naked-nabla defunstart t)
          (goto-char (match-end 0))))))

(defun dyalog-dfun-name ()
  (interactive)
  "If point is inside a dynamic function return the functions name.
If point is inside an anonymous function, return \"\", and if it
isn't inside a dynamic function, return nil"
  (save-excursion
    (let ((syn-table (copy-syntax-table dyalog-mode-syntax-table))
          (openbrace nil)
          (context (syntax-ppss-context (syntax-ppss))))
      (modify-syntax-entry ?\( "." syn-table)
      (modify-syntax-entry ?\) "." syn-table)
      (modify-syntax-entry ?\[ "." syn-table)
      (modify-syntax-entry ?\] "." syn-table)
      (cond
       ((eq context 'string) (re-search-backward "\\s\""))
       ((eq context 'comment) (re-search-backward "\\s<")))
      (set 'openbrace
           (with-syntax-table syn-table
             (condition-case err
                 (goto-char (scan-lists (point) -1 1))
               (scan-error nil))))
      (let* ((ret-and-larg (concat "\\(?:[A-Za-z]+ *← *\\|\\(?1:{[a-zA-Z]+}\\) *← *\\)?"
                                   "\\(?:[A-Za-z_]+ +\\|\\(?2:{[A-Za-z_]+}\\) *\\)"))
             (in-dfun-p
              (and openbrace
                   (not
                    (progn
                      (forward-line)
                      (end-of-line)
                      (and
                       (re-search-backward dyalog-tradfn-header (point-min) t)
                       (re-search-forward ret-and-larg (match-end 0) t)
                       (memq openbrace (list (match-beginning 1)
                                             (match-beginning 2)))))))))
        (if in-dfun-p
            (progn
              (goto-char openbrace)
              (let ((dfun-name
                     (if (looking-back (concat "\\(" dyalog-name "\\) *← *")
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
  (let ((dfun-name (dyalog-dfun-name))
        (start-pos (point)))
    (or dfun-name (car (dyalog-tradfn-info)))))

(defun dyalog-tradfn-info ()
  "Return a list of information on the tradfn defun point is in.
This name is only valid if point isn't inside a dfn. The list
contains the name of the function a list containing the names of
the arguments and a list containing localized names."
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
                 (args (remq nil (list retval larg rarg)))
                 (locals nil))
            (dyalog-end-of-defun)
            (if (< (point) start-pos)
                (list "" nil nil)
              (progn
                (goto-char localstart)
                (if (looking-at "[A-Za-z]+\\(;[A-Za-z]+\\)*")
                    (setq locals 
                          (split-string
                           (match-string-no-properties 0)
                           ";" 'omit-nulls)))
                (list tradfn-name args locals))))
        (list "" nil nil)))))

;;; Font Lock
(defun dyalog-fontify-locals (begin end)
  "Fontify local names in tradfns."
  nil)


;;; Syntax

(defun dyalog-syntax-propertize-function (start end)
  "Alter syntax table for escaped single quotes within strings."
  (save-excursion
    (goto-char start)
    (let* ((syntax-state (syntax-ppss))
           (inside-string-p (not (not (nth 3 syntax-state)))))
      (while (< (point) end)
        (skip-chars-forward "^'" end)
        (when (looking-at "'")
          (if (and inside-string-p (looking-at "''"))
              (progn
                (put-text-property (point) (+ 2 (point))
                                   'syntax-table
                                   (string-to-syntax "."))
                (ignore-errors (forward-char)))
            (when (not (equal 'comment (syntax-ppss-context (syntax-ppss))))
              (setq inside-string-p (not inside-string-p))))
          (ignore-errors (forward-char)))))))

(defun dyalog-in-keyword (&optional pt)
  "Return t if PT (defaults to point) is inside a keyword (e.g. :If)."
  (save-excursion
    (when pt
      (goto-char pt))
    (skip-chars-backward "A-Za-z:")
    (skip-syntax-backward "-")
    (and (or (looking-at dyalog-keyword-regex)
             (looking-at dyalog-middle-keyword-regex))
         (not (dyalog-dfun-name)))))

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

;;;###autoload
(defun dyalog-session-connect (&optional host port)
  "Connect to a Dyalog session"
  (interactive (list (read-string "Host (default localhost):"
                                  "127.0.0.1")
                     (read-number "Port (default 7979):" 7979)))
  (make-comint "dyalog" (cons host port))
  (switch-to-buffer "*dyalog*")
  (setq comint-scroll-show-maximum-output nil)
  (define-key (current-local-map)
    (kbd"C-c C-e") 'dyalog-editor-edit-symbol-at-point)
  (run-hooks 'dyalog-session-connect-hook))

;;;###autoload
(defun dyalog-editor-connect (&optional host port)
  "Connect to a Dyalog process as an editor"
  (interactive (list (read-string "Host (default localhost):"
                                  "127.0.0.1")
                     (read-number "Port (default 8080):" 8080)))
  (make-network-process :name "dyalog-edit"
                        :buffer " *dyalog-receive-buffer*"
                        :family 'ipv4 :host host :service port
                        :sentinel 'dyalog-editor-sentinel
                        :filter 'dyalog-editor-receive))

(defun dyalog-editor-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))

(defun dyalog-editor-receive (process output)
  "Receive data from a Dyalog editor connection"
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
          (dyalog-editor-munge-command (point) m)
          (with-current-buffer (process-buffer process)
            (set-marker (process-mark process) 1)))
        (sit-for 0.01)))))

(defun dyalog-editor-munge-command (start end)
  "Parse and delete a Dyalog editor command in the currently active region.
START is the start of the command and END is where it ends."
  (cond ((looking-at "edit \\([^ []+\\)\\(\\[\\([0-9]+\\)\\]\\)?\0\\([^\0]*\\)\0")
         (let ((name (match-string 1))
               (linetext (match-string 3))
               (lineno nil)
               (path (match-string 4))
               (src  (buffer-substring-no-properties (match-end 0) m)))
           (when linetext
             (set 'lineno (string-to-number linetext)))
           (delete-region start (1+ end))
           (dyalog-open-edit-buffer name src lineno path)))
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
                (src (buffer-substring-no-properties (match-end 0) m)))
           (delete-region start (1+ end))
           (dyalog-open-edit-array name kind src)))
        (t
         (error "Ivalid message received"))))

(defun dyalog-open-edit-buffer (name src &optional lineno path)
  "Open a buffer to edit object NAME with source SRC"
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
      (font-lock-fontify-buffer)
      (if lineno
          (forward-line (- lineno 1))
        (goto-char (min pos (point-max))))
      (setq buffer-undo-list nil)
      (select-frame-set-input-focus (window-frame (selected-window))))))

(defun dyalog-open-edit-array (name kind src)
  "Open a buffer to edit array NAME of type KIND with contents SRC.
KIND is \"charvec\", \"charmat\", \"stringvec\" or \"array\"."
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
    (read-only-mode)
    (if lineno
        (forward-line (- lineno 1))
      (goto-char (min pos (point-max))))
    (setq buffer-undo-list nil)
    (select-frame-set-input-focus (window-frame (selected-window)))))

(defun dyalog-editor-fix (&optional arg)
  "Send the contents of the current buffer to the connected Dyalog process"
  (interactive)
  (progn
    (process-send-string "dyalog-edit" "fx ")
    (process-send-region "dyalog-edit" (point-min) (point-max))
    (process-send-string "dyalog-edit" "\e")))

(defun dyalog-editor-edit (name)
  "Ask the connected Dyalog process for the source of NAME and open it in an edit buffer"
  (interactive "s")
  (process-send-string "dyalog-edit" (concat "src " name "\e")))

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
         (name  (symbol-name sym))
         (str   (concat ";" name))
         (fname (dyalog-current-defun)))
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
        (if (search-forward str nil t)
            (progn
              (goto-char (match-beginning 0))
              (delete-char (length str))
              (message "Made %s non-local in function %s" name fname))
          (progn
            (move-end-of-line nil)
            (insert str)
            (message "Made %s local in function %s" name fname)))))))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

;;;###autoload
(define-derived-mode dyalog-mode prog-mode "Dyalog"
  "Major mode for editing Dyalog APL code.

\\{dyalog-mode-map}"
  :group 'dyalog
  :syntax-table dyalog-mode-syntax-table
  (setq-local syntax-propertize-function
              #'dyalog-syntax-propertize-function)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local beginning-of-defun-function 'dyalog-beginning-of-defun)
  (setq-local end-of-defun-function 'dyalog-end-of-defun)
  ;; Comments
  (setq-local comment-start "⍝ ")
  (setq-local comment-start-skip "⍝+\\s-*")
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local font-lock-defaults '(dyalog-font-lock-keywords))
  ;; Dyalog alays indents with spaces
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function 'dyalog-indent-line)
  ;; Misc
  (setq-local require-final-newline nil)
  ;; Imenu and which-func-mode
  (setq-local imenu-generic-expression
              dyalog-imenu-generic-expression)
  (eval-after-load "which-func"
    '(add-to-list 'which-func-modes 'dyalog-mode))
  (add-hook 'which-func-functions 'dyalog-current-defun nil 'make-it-local)
  ;; Hooks
  (add-hook 'before-save-hook
            'dyalog-fix-whitespace-before-save nil 'make-it-local))

(define-derived-mode dyalog-array-mode fundamental-mode "DyalogArr"
  "Major mode for editing Dyalog APL arrays.

\\{dyalog-array-mode-map\\}"
  :syntax-table dyalog-array-mode-syntax-table
  (setq-local require-final-newline nil))

(provide 'dyalog-mode)

;;; dyalog-mode.el ends here
