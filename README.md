Dyalog mode
===========

Dyalog-mode is a GNU Emacs major mode for editing Dyalog APL source code. It
integrates with the Dyalog IDE so you can use Emacs instead of the built in
Dyalog editor.

Features
--------

Dyalog mode supports syntax highlighting (including correct highlighting of
localized variables), indentation and convenience functions like toggling
localization of variables.

Supported platforms
-------------------
Dyalog mode should work in Emacs 23, but for full functionality, you should
use Emacs 24.2 or newer.

Dyalog mode itself works on all platforms supported by Emacs, but the
integration with the Dyalog session requires functionality only present in the
GUI version of the Dyalog IDE, so that part only works on Windows. However,
you can still connect Emacs and Dyalog when running on Linux, you just have to
manually invoke editing in Emacs by calling `Emacs.editor.edit 'funcname'`.

Installation
------------

### From MELPA ###

Since version 24.1, Emacs includes a package management system. Using the
[MELPA](http://melpa.org) package repository is the easiest way to install and
update Dyalog mode.

If you have installed packages from Melpa before, all you have to do is:

`M-x package-install RET dyalog-mode RET`

If you haven't used Melpa before, then you have to configure `package.el`
first. Add the following to your
[init file](http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
```

Restart Emacs and update the package cache by running:

`M-x package-refresh-contents RET`

You can now install Dyalog mode by running `M-x package-install`, as above.

#### Installing in older versions of Emacs ####

If you have an older version of Emacs, please follow the
[instruction from MELPA](http://melpa.org/#/getting-started).

### Installing manually

To install manually, clone the
[mercurial repository](https://bitbucket.org/harsman/dyalog-mode) and add the
following to your init file:

```lisp
(autoload 'dyalog-mode "/path/to/dyalog-mode.el" "Edit Dyalog APL" t)
(autoload 'dyalog-editor-connect "/path/to/dyalog-mode.el" "Connect Emacs to Dyalog" t)
(add-to-list 'auto-mode-alist '("\\.apl\\'" . dyalog-mode))
(add-to-list 'auto-mode-alist '("\\.dyalog$" . dyalog-mode))
```

#### Dependencies ####

If you install with `package.el` then dependencies are automatically
installed. If you install manually, you have to install dependencies manually
as well.

* `cl-lib` is installed by default in Emacs 24.3 and newer. It provides
  various Common Lisp forms, but unlike the older `cl` library, it doesn't
  pollute the global namespace. If you have an older Emacs version, without
  `cl-lib`, you can install it from the [GNU ELPA](http://elpa.gnu.org/)
  repository by using `package.el` (by doing `M-x package-install RET cl-lib
  RET`), or you can get it manually
  [here](http://elpa.gnu.org/packages/cl-lib.html).

Getting started
---------------

To get started, just open any text file with APL source code and a .dyalog
extension in Emacs.

You can also use Emacs as the editor inside a Dyalog APL session. First you
need to load the code for this into Dyalog. Issue the following commands at
the Dyalog prompt:

```apl
      ]load /path/to/dyalog-mode/Emacs.apl
      Emacs.editor.setupmenu ⍬
      Emacs.editor.connect
```

This will first try to connect to a running Emacs instance, and if one isn't
available, start Emacs and connect to it from the Dyalog session. The call to
`setupmenu` will add a shortcut to the session's context menu, called "Edit in
Emacs" with the keyboard shortcut `Ctrl+Alt+Enter`.

To edit a function, class or namespace in Emacs, just place the cursor on a
name in the Dyalog session or editor and press `Ctrl+Alt+Enter`. Once you are
happy with your changes, press `C-c C-c` in Emacs to fix the changes back in
the Dyalog session. While you are editing in Emacs, you can press `C-c C-e` to
edit the name at point. You can also open arrays in Emacs, although currently
no arrays are editable, i.e. they are all read-only.

If you edit a namespace or class that has been loaded with Dyalog's SALT
toolkit (e.g. by using `]load` at the session prompt), the path to the source
file will be sent to Emacs, so you can also save directly from there. If Emacs
doesn't know the path to the source file, it will ask you to name a file when
you try to save.

### Entering APL characters in Emacs ###
By installing and enabling the Dyalog IME you can enter APL characters in
Emacs. Note that the classic Dyalog layout uses the Control key for entering
APL characters, which tends to conflict with Emacs' keyboard shortcuts. Your
best bet is to use a layout that uses AltGr or Ctrl+Alt instead (the Dyalog
keyboard for Linux uses a special compose key called the APL key).

If you use AltGr to enter APL characters in Emacs on Windows, Emacs may
interpret commands where you press Ctrl+Alt+key as Ctrl+Alt+aplchar. To avoid
this, call dyalog-fix-altgr-chars. For example, if you want to enable the
Dyalog IME globally in Emacs and want to fix the global keymap, so that ⊃,
which you produce by pressing AltGr+x, isn't confused with C-M⊃, you would
add the following to your init file:

```lisp
(dyalog-fix-altgr-chars (current-global-map) "⊃" "x")
```

Configuration
-------------

### Emacs ###

Run `M-x customize-group dyalog RET` to customize the various settings
available.

### Dyalog ###

If you want to use another keyboard shortcut for editing, just call
`setupmenu` with an argument of the `Accelerator` property for your desired
keyboard shortcut. For more information, see the built in Dyalog help for the
Accelerator property.

If you use something other than SALT to load source code into the session, you
can set `Emacs.editor.getPath` to the name of a function that given a name
(relative to root), will return the path to the corresponding source file.

The variable `Emacs.editor.onMissing` is the name of a function to call when
trying to edit a name that doesn't exist. It is called with the name as an
argument and should try to establish the corresponding function, namespace or
class. That way, you can press edit on names that haven't been established in
the session yet.

If `Emacs.editor.boxing` is true, arrays will be displayed with boxing inside
Emacs, using the DISPLAY function from the display workspace that comes with
Dyalog.

Known issues and limitations
----------------------------

Arrays are always read-only and cannot be edited.

In versions of Dyalog before 14.0, if you invoke Emacs to edit the current
function suspended in the debugger from inside the debugger and fix that
function, the debuggers display of the function's source code isn't updated.

Traditional operators are currently not supported, they can be edited, but can
cause errors in indentation and the defun navigation commands won't work.

If you invoke Emacs to edit the name under the cursor in the Dyalog editor,
any line number specified is ignored, e.g. even if you edit FUNC[3] Emacs
won't open with the cursor on line 3. This is a Dyalog limitation, the
required information isn't available from within the editor.
