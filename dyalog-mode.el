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

(defconst dyalog-label-regex
  "^ *[A-Za-z_]+[A-Za-z0-9_]*:")

(defconst dyalog-keyword-regex
  (concat "\\(\\(?:^\\s-*\\|\\(?5:" dyalog-label-regex " *\\)\\)"
          ":\\(?2:[A-Za-z]+\\)\\)\\|\\(⋄\\s-*:\\(?2:[A-Za-z]+\\)\\)"))

(defconst dyalog-middle-keyword-regex
  "\\s-+\\(:\\(In\\|InEach\\)\\)\\s-+")

(defconst dyalog-comment-regex
  "^\\s-*⍝")

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

(defconst dyalog-func-retval
  "\\(?:\\(?2:[A-Za-z_]+\\) *← *\\|{\\(?2:[a-zA-Z_]+\\)} *← *\\)?")

(defconst dyalog-func-larg
  "\\(?:\\(?3:[A-Za-z_]+\\) +\\|{\\(?3:[A-Za-z_]+\\)} *\\)")

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
   `(,dyalog-access-type (1 font-lock-keyword-face))
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
  (or   (> (skip-chars-forward "A-Za-z_∆0-9") 0)  ; name
        (> (skip-chars-forward "⎕:A-Za-z") 0)     ; sys name/keyword
        (> (skip-chars-forward "0-9E¯.") 0)       ; numbers
        (> (skip-chars-forward "⍺⍵∇") 0)          ; meta chars
        (> (skip-chars-forward " ") 0)            ; white space
        (forward-char)))                          ; fallback

(defconst dyalog-delimiter-match
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e '((":If" . ":EndIf")("{"."}")
                 (":For" . ":EndFor")(":Repeat" ":Until")
                 (":While" . ":EndWhile")(":Trap" . ":EndTrap")
                 (":Hold" . ":EndHold")(":With" . ":EndWith")
                 (":Namespace" . ":EndNamespace")(":Class" . ":EndClass")
                 (":Select" . ":EndSelect")(":Interface" ":EndInterface")))
      (puthash (car e) (list (cdr e) 'block-start) h)
      (puthash (cdr e) (list (car e) 'block-end) h))
    (dolist (e '((":AndIf". ":If")(":OrIf".":If")(":ElseIf".":If")))
      (puthash (car e) (list (cdr e) 'block-pause) h))
    (dolist (e '((":Else" . ":\\(If\\|Select\\|Trap\\|Hold\\)")
                 (":Case" . ":\\(Select\\|Trap\\)")
                 (":CaseList" . ":\\(Select\\|Trap\\)")))
      (puthash (car e) (list (cdr e) 'block-pause) h))
    (dolist (e '((":Field" . ":\\(Class\\|Interface\\)")))
      (puthash (car e) (list (cdr e) nil) h))
    (dolist (e '(":Access"))
      (puthash e (list "" nil) h))
    h))

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

(defun dyalog-matching-delimiter (delimiter)
  "Return the match for the given DELIMITER.
For example, if ':EndIf' is provided, return ':If' and vice versa."
  (car (gethash delimiter dyalog-delimiter-match nil)))

(defun dyalog-keyword-indent-type (keyword)
  "Return a symbol indicating how a KEYWORD affects indentation.
If KEYWORD introduces a new block, (e.g :If), return
'block-start.  If it ends a block (e.g. :EndIf), return
'block-end.  If it ends a block and immediately starts a new
block (e.g. :Else or :Case), return 'block-pause.  If the keyword
should be indented the same way as everything else, return nil."
  (let ((d (gethash keyword dyalog-delimiter-match nil)))
    (and d (nth 1 d))))

(defun dyalog-specific-keyword-regex (keyword)
  "Return a regex mathcing KEYWORD when point is at bol."
  (concat "\\(\\(?:^\\s-*\\|\\(?:" dyalog-label-regex " *\\)\\)"
          keyword "\\)\\|\\(⋄\\s-*" keyword "\\)"))

(defun dyalog-relative-indent (n)
  "Return the no spaces to indent N tabstops relative to the current line."
  (max (+ (current-indentation) (* tab-width n))
       (dyalog-leading-indentation)))

(defun dyalog-previous-logical-line ()
  "Move backwards to the start of the previous logical line.
Assumes point is at the beginning of a logical line."
  (let ((bol (line-beginning-position))
        (done nil))
    (if (eq (point) bol)
        (progn
          (forward-line -1)
          (end-of-line))
      (when (eq (char-before) ?⋄)
        (backward-char)))
    (while (not done)
      (skip-chars-backward "^⋄\r\n")
      (if (eq (char-before) ?⋄)
          (progn
            (setq done (not (dyalog-in-comment-or-string)))
            (when (not done)
              (backward-char)))
        (setq done t)))))

(defun dyalog-next-logical-line ()
  "Move forward to the start of the next logical line.
Assumes point is at the start of a logical line."
  (let ((done nil))
    (when (eq (char-after) ?⋄)
      (forward-char))
    (while (not done)
      (skip-chars-forward "^⋄\r\n")
      (if (eq (char-after) ?⋄)
          (setq done (not (dyalog-in-comment-or-string)))
        (setq done t))
      (if (eobp)
          nil
        (forward-char)))))

(defun dyalog-indent-stop-block-end (match blockstack indent-status funcount)
  "Return whether we have found root for a block end, and amount of to indent.
MATCH is the keyword that matches the block end (e.g. :For
matches :EndFor), BLOCKSTACK is a stack of currently open blocks,
INDENT-STATUS is the indentation status of the current line (the
return value from `dyalog-indent-status', and FUNCOUNT is the
number of currently open tradfn definitions."
  (cond
   ((and (not blockstack)
         (looking-at (dyalog-specific-keyword-regex match)))
    (list t (dyalog-relative-indent 0)))
   ((and (memq (plist-get indent-status :indent-type)
               '(tradfn-start tradfn-end))
         (not (string-match ":\\(End\\)?\\(Namespace\\|Class\\)" match)))
    (list t (skip-chars-forward " ∇")))))

(defun dyalog-indent-stop-tradfn (blockstack indent-status funcount)
  "Return whether we have found root for a tradfn, and chars to indent.
BLOCKSTACK is a stack of currently open blocks, INDENT-STATUS is
the indentation status of the current line (the return value from
`dyalog-indent-status', and FUNCOUNT is the number of currently
open tradfn definitions."
  (cond ((and (not blockstack)
              (looking-at (dyalog-specific-keyword-regex
                           ":\\(Class\\|Namespace\\)")))
         (list t (dyalog-relative-indent 1)))
        ((and (not blockstack)
              (memq (plist-get indent-status :indent-type)
                    '(tradfn-start tradfn-end)))
         (list t (current-indentation)))))

(defun dyalog-indent-search-stop-function (keyword
                                           &optional match_ indent-type_)
  "Given a KEYWORD, return a function to check for indentation root.
Optional argument MATCH_ is the matching keyword (e.g. :If
for :EndIf) and only needs to be supplied if it differs from the
default.  INDENT-TYPE_ is also optional, and is the indentation
type for the given keyword (see `dyalog-keyword-indent-type') and
only needs to be supplied if it differs from the default."
  (let* ((match (or match_ (dyalog-matching-delimiter keyword)))
         (indent-type (or indent-type_ (dyalog-keyword-indent-type keyword))))
    (cond
     ((eq 'block-start indent-type)
        #'dyalog-indent-search-stop-generic)
     ((memq indent-type '(block-end block-pause))
      (apply-partially 'dyalog-indent-stop-block-end match))
     (t
      #'dyalog-indent-search-stop-generic))))

(defun dyalog-indent-search-stop-generic (blockstack indent-status funcount)
  "Return if we have found an indentation root, and no chars to indent.
BLOCKSTACK is a stack of currently open blocks, INDENT-TYPE is
the indentation type of the current keyword (if any), and
FUNCOUNT is the number of currently open tradfn definitions."
  (let ((indent-type  (plist-get indent-status :indent-type))
        (label-at-bol (plist-get indent-status :label-at-bol)))
    (cond
     ((and (eq indent-type 'block-start) (not blockstack) (eq funcount 0))
      (list t (+ (dyalog-relative-indent 1)
                 (if label-at-bol 1 0))))
     ((and (eq indent-type 'block-end) (not blockstack) (eq funcount 0))
      (list t (+ (current-indentation)
                 (if label-at-bol 1 0))))
     ((and (eq indent-type 'tradfn-start)
           (eq funcount 0))
      (list t (skip-chars-forward " ∇")))
     ((bobp)
      (list t (dyalog-leading-indentation)))
     (t
      (list nil 0)))))

(defun dyalog-indent-status ()
  "Return a list of information on the current indentation status.
This includes whether we are at the start of a block, or the
end (or at a pause inside a block), and the name of the delimiter
that triggers the starting or ending of a block (e.g. \":If\" or
\"∇\"."
  (pcase-let* ((`(,keyword ,label-at-bol) (dyalog-current-keyword))
               (indent-type (dyalog-keyword-indent-type keyword)))
    (cond
     ((dyalog-on-tradfn-header)
      (list :indent-type 'tradfn-start :delimiter "∇" :label-at-bol nil))
     ((looking-at dyalog-naked-nabla)
      (list :indent-type 'tradfn-end :delimiter "∇"   :label-at-bol nil))
     (t
      (list :indent-type indent-type :delimiter keyword
            :label-at-bol label-at-bol)))))

(defun dyalog-search-indent-root (at-root-function)
  "Given function AT-ROOT-FUNCTION, search backwards for the root indent.
AT-ROOT-FUNCTION assumes point is at the beginning of a logical
line and returns t when point is at the line containing the
indentation root.  For example if we are indenting a :EndFor,
AT-ROOT-FUNCTION returns t when we have reached the corresponding :For."
  (let* ((indentation nil)
         (blockstack ())
         (funcount 0))
    (save-excursion
      (while (not indentation)
        ;; TODO: We should probably skip past d-funs
        (dyalog-previous-logical-line)
        (let* ((status (dyalog-indent-status))
               (keyword (plist-get status :delimiter))
               (indent-type (plist-get status :indent-type))
               (root (apply at-root-function
                            (list blockstack status funcount)))
               (at-root (car root)))
          (setq indentation
                (cond
                 (at-root
                  (nth 1 root))
                 ((eq 'block-end indent-type)
                  (progn
                    (push (dyalog-matching-delimiter keyword)
                          blockstack)
                    nil))
                 ((eq 'block-start indent-type)
                  (progn
                    (when (string-equal keyword (car blockstack))
                      (pop blockstack))
                    nil))
                 ((eq 'tradfn-end indent-type)
                  (setq funcount (1+ funcount))
                  nil)
                 ((eq 'tradfn-start indent-type)
                  (setq funcount (1- funcount))
                  nil)))
          (when (and (not indentation) (bobp))
            (setq indentation (dyalog-leading-indentation)))))
      (list :indent indentation :has-label nil
            :funcount funcount :blockstack blockstack))))

(defun dyalog-calculate-dfun-indent ()
  "Calculate the indentation amount for a line in a dfun."
  (let* ((start (point))
         (line-start (+ start (skip-syntax-forward "-"))))
    (save-excursion
      (let ((containing-brace (scan-lists start -1 1)))
        (if (< containing-brace line-start)
            (progn
              (goto-char containing-brace)
              (dyalog-relative-indent
                      (if (equal (char-after line-start) ?})
                          0 1)))
          (dyalog-leading-indentation))))))

(defun dyalog-calculate-indent ()
  "Calculate the amount of indentation for the current line.
Return a plist with the indent in spaces, and whether the current
line has a label."
  (save-excursion
    (move-beginning-of-line nil)
    (let* ((dfun (dyalog-in-dfun))
           (keyword (if dfun nil (car (dyalog-current-keyword))))
           (indent-info nil))
      (setq indent-info
            (cond
             ((bobp)
              (list :indent (dyalog-leading-indentation)
                    :has-label nil
                    :is-comment nil
                    :funcount 0
                    :blockstack nil))
             (dfun
              (list :indent (dyalog-calculate-dfun-indent)
                    :has-label nil
                    :is-comment nil
                    :funcount 0
                    :blockstack (list "}")))
             ((looking-at dyalog-comment-regex)
              (if dyalog-indent-comments
                  (let ((l (dyalog-search-indent-root
                            #'dyalog-indent-search-stop-generic)))
                    (plist-put l :is-comment t))
                (list :indent (current-indentation)
                      :has-label nil
                      :is-comment t
                      :funcount 0
                      :blockstack nil)))
             ((and (looking-at-p dyalog-label-regex) (not dfun))
              (let* ((label-indent-info (dyalog-search-indent-root
                                         #'dyalog-indent-stop-tradfn))
                     (label-indent      (plist-get label-indent-info :indent))
                     (old-label         (dyalog-remove-label))
                     (rest-indent-info  (dyalog-calculate-indent)))
                ;; A label is always aligned 1 space to the left of the
                ;; surrounding tradfn, and since we search for tradfn
                ;; delimiters, we align to the nabla. So if we've reached the
                ;; beginning of the buffer, we subtract one and if we've
                ;; aligned to the nabla we add one.
                (setq label-indent (max 0 (+ label-indent
                                      (if (= label-indent
                                             (dyalog-leading-indentation))
                                          -1
                                        1))))
                (insert old-label)
                (plist-put rest-indent-info :has-label t)
                (plist-put rest-indent-info :label-indent label-indent)
                rest-indent-info))
             (keyword
              (dyalog-search-indent-root
               (dyalog-indent-search-stop-function keyword)))
             ((looking-at dyalog-naked-nabla)
              (dyalog-search-indent-root #'dyalog-indent-stop-tradfn))
             ((dyalog-on-tradfn-header)
              (dyalog-search-indent-root #'dyalog-indent-stop-tradfn))
             (t
              (dyalog-search-indent-root #'dyalog-indent-search-stop-generic))))
      indent-info)))

(defun dyalog-leading-indentation ()
  "Return the number of spaces to indent by in the current buffer.
This varies depending of the type of object being edited,
namespaces or classes have no extra leading indentation, but functions have
one extra space, to be consistent with separating multiple
functions with ∇."
  ;; TODO: Return different amounts depending on buffer type
  dyalog-leading-spaces)

(defun dyalog-indent-line-with (indent-info)
  "Indent the current line according to INDENT-INFO.
INDENT-INFO is the return value from `dyalog-calculate-indent'."
  (let* ((indent     (plist-get indent-info :indent))
         (has-label  (plist-get indent-info :has-label))
         (is-comment (plist-get indent-info :is-comment)))
      (if has-label
          (let* ((old-label    (dyalog-remove-label))
                 (label-length (length old-label))
                 (label-indent (plist-get indent-info :label-indent)))
            (if (and (not dyalog-indent-comments) is-comment)
                (setq indent (- indent label-indent))
              (if (> label-length indent)
                  ;; Label is longer than required indentation, so line
                  ;; should be flush with label
                  (setq indent 0)
                (setq indent (max 0
                                  (- indent (+ label-length label-indent))))))
            ;; Keywords are never flush with the label, since they start with
            ;; a colon, and the label ends with one
            (beginning-of-line)
            (when (looking-at-p "^ *:")
              (setq indent (max 1 indent)))
            (indent-line-to indent)
            (beginning-of-line)
            (insert (make-string label-indent ? ))
            (insert old-label)
            (back-to-indentation))
        (indent-line-to indent))))

(defun dyalog-indent-line ()
  "Indent the current line."
  (interactive)
  (let* ((restore-pos (> (current-column) (current-indentation)))
         (old-pos (point))
         (indent-info (dyalog-calculate-indent)))
    (dyalog-indent-line-with indent-info)
    (when restore-pos
      (goto-char (min old-pos (line-end-position))))))

(defun dyalog-current-tradfn-indentation ()
  "Return the column 0 indentation of the tradfn point is in, otherwise nil."
  (let* ((tradfn-info (dyalog-tradfn-info))
         (tradfn-name (car tradfn-info))
         (end-of-header (nth 3 tradfn-info)))
    (when (not (zerop (length tradfn-name)))
      (save-excursion
        (goto-char end-of-header)
        (beginning-of-line)
        (skip-chars-forward " ∇")
        (current-column)))))

(defun dyalog-indent-region (start end)
  "Indent every line in the current region.
START and END specify the region to indent."
  (save-excursion
    (goto-char start)
    (let ((indent-info nil))
      (goto-char (line-beginning-position))
      (setq indent-info    (dyalog-calculate-indent))
      (plist-put indent-info :tradfn-indent
                 (dyalog-current-tradfn-indentation))
      (dyalog-indent-line-with indent-info)
      (beginning-of-line)
      (while (< (point) end)
        (setq indent-info (dyalog-indent-update indent-info))
        (when (bolp)
          (save-excursion
            (dyalog-indent-line-with indent-info)))
        (dyalog-next-logical-line)))))

(defun dyalog-indent-update (indent-info)
  "Calculate an updated indentation after the current logical line.
INDENT-INFO is a plist of indentation information, on the same
form as the return value from `dyalog-calculate-indent'. Return
the updated amount of indentation, in characters."
  (let* ((indent-status (dyalog-indent-status))
         (indent-type   (plist-get indent-status :indent-type))
         (delimiter     (plist-get indent-status :delimiter))
         (blockstack    (plist-get indent-info :blockstack))
         (next-indent   (or (plist-get indent-info :next-indent) 0))
         (indent        (+ (plist-get indent-info :indent)
                           next-indent))
         (tradfn-indent (plist-get indent-info :tradfn-indent)))
    (plist-put indent-info :is-comment nil)
    (if (looking-at-p dyalog-label-regex)
        (let ((old-label (dyalog-remove-label)))
          (setq indent-info (dyalog-indent-update indent-info))
          (plist-put indent-info :has-label t)
          (insert old-label)
          (beginning-of-line))
      (cond
       ((looking-at-p dyalog-comment-regex)
        (when (not dyalog-indent-comments)
          (setq next-indent (- indent (current-indentation))
                indent      (current-indentation))
          (plist-put indent-info :is-comment t)))
       ((eq 'block-end indent-type)
        (progn
          ;; (unless (string-equal (car blockstack)
          ;;                       (dyalog-matching-delimiter delimiter))
          ;;   (error "Non matching delimiter"))
          ;; We assume delimiters match, since the region might cover
          ;; only part of matched delimiters
          (when blockstack
            (pop blockstack))
          (setq indent      (- indent tab-width)
                next-indent 0)))
       ((eq 'block-start indent-type)
        (progn
          (push delimiter blockstack)
          (setq next-indent tab-width)))
       ((eq 'block-pause indent-type)
        (setq indent      (- indent tab-width)
              next-indent tab-width))
       ((eq 'tradfn-end indent-type)
        (setq tradfn-indent nil
              indent (current-indentation)
              next-indent 0))
       ((eq 'tradfn-start indent-type)
        (setq tradfn-indent (save-excursion (skip-chars-forward " ∇"))
              indent        tradfn-indent
              next-indent   0))
       ((looking-at "^[ \t]*$")
        (setq next-indent indent
              indent 0))
       ((looking-at-p dyalog-comment-regex)
        (when (not dyalog-indent-comments)
          (setq next-indent (- indent (current-indentation))
                indent      (current-indentation))
          (plist-put indent-info :is-comment t)))
       ;; TODO: dfuns
       (t
        (setq next-indent 0)))
      (plist-put indent-info :blockstack blockstack)
      (plist-put indent-info :indent indent)
      (plist-put indent-info :next-indent next-indent)
      (plist-put indent-info :tradfn-indent tradfn-indent)
      (plist-put indent-info :has-label nil))
    indent-info))

(defun dyalog-remove-label ()
  "Remove the current label token at beginning of line, and return it."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let* ((start (point))
           (end (+ start 1 (skip-chars-forward "A-Za-z_0-9")))
           (label (buffer-substring-no-properties start end)))
    (delete-region start end)
    label)))

(defun dyalog-fix-whitespace-before-save ()
  "Clean up whitespace in the current buffer before saving."
  (when (and (eq major-mode 'dyalog-mode) dyalog-fix-whitespace-before-save)
    (dyalog-fix-whitespace)))

(defun dyalog-fix-whitespace ()
  "Clean up white space in the current buffer like Dyalog does."
  (interactive)
  (message "Cleaning up whitespace...")
  (let ((dyalog-indent-comments nil)
        (punctuation-char "\\s.\\|\\s(\\|\\s)"))

    (save-excursion
      (delete-trailing-whitespace)
      ;; Reduce all runs of whitespace to a single space, except when
      ;; preceeded by a newline, succeeded by a comment character, or if
      ;; inside a comment or string literal
      (goto-char (point-min))
      (while (re-search-forward "\\([^ \r\n]\\)\\(  +\\)\\([^⍝ \r\n]\\)" (point-max) t)
        (let ((ws-start (match-beginning 2)))
          (unless (dyalog-in-comment-or-string ws-start)
            (replace-match "\\1 \\3"))))
      ;; Remove spaces before punctuation
      (goto-char (point-min))
      (while (re-search-forward (concat "\\([^ \r\n]\\)" "\\( +\\)"
                                        "\\(" punctuation-char "\\)")
                                (point-max)
                                t)
        (let ((start (match-beginning 0))
              (ws-start (match-beginning 2)))
          (unless (or (string-equal "⍝" (match-string 3))
                      (dyalog-in-comment-or-string ws-start)
                      (string-match "[∇⋄⍬]" (match-string 3))
                      (string-match "[∇⋄⍬]" (match-string 1))
                      (and (string-equal ":" (substring (match-string 3) 0 1))
                           (dyalog-in-keyword (match-beginning 3)))
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
              (ws-start (match-beginning 2))
              (match-1  (match-string 1))
              (match-3  (match-string 3))
              (match-3-start (match-beginning 3)))
          (unless (or (string-equal "⍝" match-1)
                      (dyalog-in-comment-or-string ws-start)
                      (string-match "[∇⋄⍬]" match-1)
                      (string-match "[∇⋄⍬]" match-3)
                      (and (string-match "[⍺⍵]\\'" match-1)
                           (string-match "\\`⎕" match-3))
                      (and (string-equal ":" (substring match-3 0 1))
                           (dyalog-in-keyword match-3-start)))
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
    (condition-case nil
        (goto-char (scan-lists (point) -1 1))
      (scan-error nil))))

(defun dyalog-previous-defun (&optional tradfn-only)
  "Move backward to the start of a function definition.
If TRADFN-ONLY is t, only consider traditional function definitions."
  ;; Point can be anywhere when this function is called
  (let ((done nil)
        (dfun-info (dyalog-in-dfun)))
    (if dfun-info
        (goto-char (plist-get dfun-info :start))
      (while (not done)
        (skip-chars-backward (if tradfn-only "^∇" "^∇{}"))
        (if (or (bobp) (not (dyalog-in-comment-or-string)))
            (progn
              (setq done t)
              (if (dyalog-on-tradfn-header 'only-after-nabla)
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
              (if (dyalog-on-tradfn-header 'only-after-nabla)
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
        (dfun-mode
         (and (looking-at "{")
              (not (dyalog-on-tradfn-header 'only-after-nabla)))))
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
              (condition-case nil
                  (goto-char (scan-lists (point) -1 1))
                (scan-error nil))))
      (let* ((in-dfun-p
              (and openbrace
                   (not (dyalog-on-tradfn-header 'only-after-nabla)))))
        (if in-dfun-p
            (progn
              (goto-char openbrace)
              (let ((dfun-name
                     (if (looking-back (concat "\\_<\\(" dyalog-name "\\) *← *")
                                       (line-beginning-position)
                                       t)
                         (match-string-no-properties 1)
                       "")))
                (condition-case nil
                    (progn
                      (forward-sexp)
                      (if (looking-at " *[^\r\n ⋄]")
                          ""
                        dfun-name))
                  (scan-error dfun-name))))
          nil)))))

(defun dyalog-in-dfun ()
  "If point is inside a dfun, return a plist with it's start and end position.
If point isn't inside a dfun, return nil."
  (progn ;; with-syntax-table can't be at defun top-level apparently...
    (with-syntax-table dyalog-dfun-syntax-table
      (let* ((syntax-begin-function 'beginning-of-line)
             (ppss (syntax-ppss))
             (start-of-containing-parens (nth 1 ppss)))
        (when (and start-of-containing-parens
                   (not (eq (char-after start-of-containing-parens) ?{)))
          ;; When syntax-pps is called during jit-lock, it sometimes ignores
          ;; the syntax-table, and treats regular parens as syntactical
          ;; parens. Calling (syntax-ppss-flush-cache) doesn't seem to help,
          ;; so instead fall back on scan-lists, which seems to work. 
          (setq start-of-containing-parens
                (condition-case nil
                    (goto-char (scan-lists (point) -1 1))
                  (scan-error nil))))
        (if start-of-containing-parens
            (save-excursion
              (goto-char start-of-containing-parens)
              (if (not (dyalog-on-tradfn-header 'only-after-nabla))
                  (list :start start-of-containing-parens
                        :end   (condition-case nil
                                   (progn
                                     (forward-sexp)
                                     (point))
                                 (scan-error nil)))
                nil))
          nil)))))

(defun dyalog-current-defun ()
  "Return the name of the defun point is in."
  (let ((dfun-name (dyalog-dfun-name)))
    (or dfun-name (car (dyalog-tradfn-info)))))

(defun dyalog-on-tradfn-header (&optional only-after-nabla)
  "Return t if point is on a tradfn header line, otherwise return nil.
If ONLY-AFTER-NABLA is t, only return t when point is after
the nabla in the tradfn header."
  (save-excursion
    (let ((start (point))
          (min (line-beginning-position 0)))
      (forward-line)
      (end-of-line)
      (if (re-search-backward dyalog-tradfn-header min t)
          (progn
            (goto-char (match-end 0))
            (and (>= start (if only-after-nabla
                               (match-beginning 0)
                             (min (match-beginning 0)
                                  (line-beginning-position))))
                 (<= start (line-end-position))))
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
      (dyalog-previous-defun 'tradfn-only)
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
                       "[; ]" 'omit-nulls))
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
                (goto-char (max end-of-header start))
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
        (setq done (>= (point) end))
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

(defun dyalog-current-keyword (&optional pt in-dfun)
  "Return the current keyword and if the keyword is preceded by a label.
PT is optional and defaults to point and determines where to look
for the keyword. If PT isn't in a keyword, return nil. If
provided, IN-DFUN is t if PT is inside a dynamic function. If it
is not provided, it is computed, which takes some time, so
providing it is an optimization. Return a two element list with
the keyword (or nil) and t if it is preceded by a label."
  (save-excursion
    (when pt
      (goto-char pt))
    (skip-chars-backward "A-Za-z:")
    (skip-syntax-backward "-")
    (when (eq (char-before) ?⋄)
      (backward-char))
    (when (looking-back dyalog-label-regex)
      (beginning-of-line))
    (pcase-let ((`(,keyword ,label-at-bol)
           (if (or (looking-at dyalog-keyword-regex)
                   (looking-at dyalog-middle-keyword-regex))
               (list (concat ":" (match-string-no-properties 2))
                     (not (not (match-string 5))))
             nil)))
      (if (and keyword (or in-dfun (dyalog-in-dfun)))
          (list nil nil)
        (list keyword label-at-bol)))))

(defun dyalog-in-keyword (&optional pt)
  "Return t if PT (defaults to point) is inside a keyword (e.g. :If)."
  (not (not (car (dyalog-current-keyword (or pt (point)))))))

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
  (cond ((looking-at
          "edit \\([^ []+\\)\\(\\[\\([0-9]+\\)\\]\\)?\0\\([^\0]*\\)\0")
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
    (or (dyalog-editor-buffer-connected)
        (and (equal 1 (length dyalog-connections))
             (car dyalog-connections))
        (nth (cl-position (completing-read p candidates nil t)
                          candidates :test 'string-equal)
             dyalog-connections))))

(defun dyalog-editor-buffer-connected ()
  "When the current buffer is connected to Dyalog, return the connection.
Otherwise return nil."
  (and (process-live-p dyalog-connection) dyalog-connection))

(defun dyalog-editor-fix (&optional process)
  "Send the contents of the current buffer to the connected Dyalog PROCESS."
  (interactive)
  (let ((process (or process (dyalog-connection-select))))
    (setq dyalog-connection process)
    (process-send-string process "fx ")
    (process-send-region process (point-min) (point-max))
    (process-send-string process "\e")))

(defun dyalog-editor-fix-and-quit ()
  "Fix the current buffer, kill it, and move focus to Dyalog."
  (interactive)
  (let ((process (dyalog-connection-select))
        (kill-buffer-query-functions ()))
    (dyalog-editor-fix process)
    ;; TODO: We really should verify that the fix is successful here...
    (when (kill-buffer)
      (process-send-string process "focus \e"))))

(defun dyalog-editor-edit (name &optional line)
  "Open source of symbol NAME in an edit buffer.
Optional argument LINE specifies which line to move point to."
  (interactive "s")
  (let ((process (dyalog-connection-select))
        (linespec (if line (format "[%d]" line) nil )))
    (setq dyalog-connection process)
    (process-send-string process (concat "src " name linespec "\e"))))

(defun dyalog-editor-edit-symbol-at-point ()
  "Edit the source for the symbol at point."
  (interactive)
  (let ((sym (symbol-at-point))
        (lineno nil))
    (when (looking-at "[A-Za-z∆_0-9]+\\[\\([0-9]+\\)\\]")
      (setq lineno (string-to-number (match-string 1))))
    (dyalog-editor-edit (symbol-name sym) lineno)))

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
                (dyalog-in-dfun))
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
