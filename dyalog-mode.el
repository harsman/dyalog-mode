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
    (define-key map (kbd"C-c C-c") 'dyalog-editor-fix)
    (define-key map (kbd"C-c C-e") 'dyalog-editor-edit-symbol-at-point)
    map)
  "Keymap for Dyalog APL mode.")

(defun dyalog-fix-altgr-chars (keymap aplchars regularchars)
  "Fix up a key map so that if the Dyalog IME uses AltGr+char for an
APL character, Emacs doesn't confuse it for C-M-char.

KEYMAP is an Emacs keymap.

APLCHARS is a string of APL-characters produced by pressing AltGr together
with some character.

REGULARCHARS is a string of the characters that when pressed
together with AltGr produce the corresponding apl character in APLCHARS."
  (loop for pair in (mapcar* #'cons aplchars regularchars) do
        (let* ((aplchar (car pair))
               (char    (cdr pair))
               (aplkey  (vector (list 'control 'meta aplchar)))
               (regkey  (vector (list 'control 'meta char)))
               (fun  (lookup-key (current-global-map) regkey)))
          (if fun
              (progn
                (define-key keymap aplkey fun))))))
    
;; This should probably be split into several layers of highlighting
(defconst dyalog-font-lock-keywords1
  (list
   ;; See emacs help for `font-lock-keywords' for a description of how the
   ;; below values work
   ;; System functions
   '("⎕[A-Za-z]*" . font-lock-builtin-face)
   ;; Keywords
   '("\\(^\\s-*:\\([A-Za-z]+\\)\\)\\|\\(⋄\\s-*:\\(?2:[A-Za-z]+\\)\\)"
     . (2 font-lock-keyword-face nil))
   '("\\s-+\\(:\\(In\\|InEach\\)\\)\\s-+" . (2 font-lock-keyword-face nil))
   ;; Guards
   '(":" . font-lock-keyword-face)
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

(defun dyalog-syntax-propertize-function (start end)
  "Alter syntax table for escaped single quotes within strings."
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
              (forward-char))
          (setq inside-string-p (not inside-string-p))))
      (forward-char))))

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
        (set 'funcount (caddr ret)))
      indented)))

(defun dyalog-indent-line ()
  (let* ((startpos (point))
         (bol (progn (beginning-of-line) (point)))
         (relpos (- startpos bol))
         (oldindent (current-indentation))
         (newindent (max 0 (dyalog-get-indent))))
    (indent-line-to newindent)
    (if (> relpos newindent)
        (goto-char (min (+ startpos (- newindent oldindent))
                        (point-max))))))

(defun dyalog-fix-whitespace ()
  (let ((dyalog-indent-comments nil))
    (when (and (eq major-mode 'dyalog-mode)
               dyalog-fix-whitespace)
      (save-excursion
        (delete-trailing-whitespace)
        (dyalog-indent-buffer)))))

(defun dyalog-indent-buffer ()
  (save-excursion
    (mark-whole-buffer)
    (indent-region (region-beginning) (region-end))))

(defconst dyalog-func-start "\\(?:\\`\\|∇[\r\n]*\\)\\s-*")

(defconst dyalog-func-retval "\\(?:[A-Za-z]+ *← *\\|{[a-zA-Z]+} *← *\\)?")

(defconst dyalog-func-larg "\\(?:[A-Za-z_]+ +\\|{[A-Za-z_]+} *\\)")

(defconst dyalog-func-name "\\(?1:[A-Za-z_]+[A-Za-z_0-9]*\\)")

(defconst dyalog-func-rarg "\\(?: +[A-Za-z_]+\\)")

(defconst dyalog-func-header-end "\\s-*\\(?:;\\|$\\)")

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

(defconst dyalog-naked-nabla "\\s-*∇\\s-*$")


(defvar dyalog-imenu-generic-expression
  `(("Functions"  ,dyalog-tradfn-header 1)
    ("Namespaces" "^\\s-*:Namespace *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)
    ("Classes"    "^\\s-*:Class *\\([A-Za-z_]+[A-Za-z_0-9]*\\)" 1)))

(defun dyalog-previous-defun ()
  (let ((pos (point)))
    (beginning-of-line)
    (skip-chars-forward " \\t")
    (if (and (> pos (point)) (looking-at "∇"))
        (end-of-line)
      (goto-char pos)))
  (if (not (re-search-backward dyalog-tradfn-header (point-min) t))
      (goto-char (point-min))
    (if (looking-at dyalog-naked-nabla)
        (forward-line 1))))

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
        (incf arg))
    (while (> arg 0)
      (dyalog-previous-defun)
      (decf arg))))

(defun dyalog-next-defun-end ()
  (forward-line 1)
  (let ((defunstart (point)))
    (if (not (re-search-forward dyalog-tradfn-header (point-max) t))
          (unless (re-search-forward dyalog-naked-nabla (point-max) t)
            (goto-char (point-max))
            (end-of-line))
      (goto-char (match-beginning 0))
      (if (bobp)
          (progn
            (forward-line 1)
            (dyalog-next-defun-end)))
      (if (re-search-backward dyalog-naked-nabla defunstart t)
          (goto-char (match-beginning 0))
        (if (looking-at dyalog-naked-nabla)
            (forward-line -1)))
          (end-of-line))))

(defun dyalog-previous-defun-end ()
  (if (not (re-search-backward dyalog-tradfn-header (point-min) t))
      (goto-char (point-min))))

(defun dyalog-end-of-defun (&optional arg)
  "Move forward to the end of a function definition."
  (interactive "^p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      ;; This case doesn't seem to be required, it seems
      ;; like the top level end-of-defun handles negative
      ;; arguments by combining calls to end-of-defun-function
      ;; and beginning-of-defun-function.
      (while (< arg 0)
        (dyalog-previous-defun-end)
        (incf arg))
    (while (> arg 0)
      (dyalog-next-defun-end)
      (decf arg))))

(defun dyalog-dfun-name ()
  (interactive)
  "If point is inside a dynamic function return the functions name.
If point is inside an anonymous function, return \"\", and if it
isn't inside a dynamic function, return nil"
  (save-excursion
    (let ((syn-table (copy-syntax-table dyalog-mode-syntax-table))
          (openbrace nil))
      (modify-syntax-entry ?\( "." syn-table)
      (modify-syntax-entry ?\) "." syn-table)
      (modify-syntax-entry ?\[ "." syn-table)
      (modify-syntax-entry ?\] "." syn-table)
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
    (or dfun-name
        (save-excursion
          (dyalog-previous-defun)
          (when (not (looking-at "∇"))
            (forward-line -1))         ; Nabla is on its own line
          (if (re-search-forward dyalog-tradfn-header nil t)
              (let ((tradfn-name (match-string-no-properties 1)))
                (dyalog-end-of-defun)
                (if (< (point) start-pos)
                    ""
                  tradfn-name))
            "")))))

;; Socket connection


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

(defun dyalog-editor-connect (&optional host port)
  "Connect to a Dyalog process as an editor"
  (interactive (list (read-string "Host (default localhost):"
                                  "127.0.0.1")
                     (read-number "Port (default 8080):" 8080)))
  (setq dyalog-receive-state 'ready)
  (setq dyalog-receive-name "")
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
            (set-marker (process-mark
                         (get-process "dyalog-edit")) 1)))))))

(defun dyalog-editor-munge-command (p m)
  "Parse and delete a Dyalog editor command in the currently active region"
  ;;(interactive "r")
  (cond ((looking-at "edit \\([^ []+\\)\\(\\[\\([0-9]+\\)\\]\\)? ")
         (let ((name (match-string 1))
               (linetext (match-string 3))
               (lineno nil)
               (src  (buffer-substring-no-properties (match-end 0) m)))
           (when linetext
               (set 'lineno (string-to-int linetext)))
           (delete-region (point) (buffer-end 1))
           (dyalog-open-edit-buffer name src lineno)))
         ((looking-at "fxresult \\([^ ]+\\)\e")
          (let* ((result (match-string 1))
                 (num    (string-to-number result)))
            (progn
              (if (eq num 0)
                  (message "Fixed as %s" result)
                (message "Can't fix, error in line %d" num))
              (delete-region (point) (buffer-end 1)))))))

(defun dyalog-open-edit-buffer (name src &optional lineno)
  "Open a buffer to edit object NAME with source SRC"
  (switch-to-buffer name)
  (setq buffer-undo-list t)
  (let ((pos (point)))
    (save-excursion
      (mark-whole-buffer)
      (delete-region (point) (mark))
      (insert src))
    (dyalog-mode)
    (font-lock-fontify-buffer)
    (if lineno
        (forward-line lineno)
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


(defun dyalog-mode ()
  "Major mode for editing Dyalog APL code."
  (interactive)
  (kill-all-local-variables)
  (use-local-map dyalog-mode-map)
  (set-syntax-table dyalog-mode-syntax-table)
  (set (make-local-variable 'syntax-propertize-function)
                            #'dyalog-syntax-propertize-function)
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
  (set (make-local-variable 'indent-line-function) 'dyalog-indent-line)
  (set (make-local-variable 'beginning-of-defun-function) 'dyalog-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'dyalog-end-of-defun)

  (setq major-mode 'dyalog-mode)
  (setq mode-name "Dyalog")
  ;; Imenu and which-func-mode
  (setq imenu-generic-expression dyalog-imenu-generic-expression)
  (eval-after-load "which-func"
    '(add-to-list 'which-func-modes 'dyalog-mode))
  (add-hook 'which-func-functions 'dyalog-current-defun nil 'make-it-local)
  ;; Hooks
  (add-hook 'before-save-hook 'dyalog-fix-whitespace-before-save nil 'make-it-local)
  (run-hooks 'dyalog-mode-hook))

(provide 'dyalog-mode)

;;; dyalog-mode.el ends here
