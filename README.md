# org-autolinks-mode
Minor mode for auto-linking org files

## Purpose
Minor mode for auto-linking filenames, inspired by the linking behavior
of Tomboy Notes, notes.vim, and other similar notetaking tools.
This will get a list of all the .org files in the directory set by org-autolinks-dir,
and turn all those filenames into links in the current buffer.
If you have files called "foo.org" and "bar.org," for instance,
typing "foo" or "bar" should automatically highlight those words as links,
allowing you to click them or press <RET> to jump to those files.
This can be used to easily make a personal wiki, without having to
manually enter links to other files.

## Installation
Load the file =org-autolinks-mode.el= from this repo and
```lisp
(require 'org-autolinks-mode)
```

## Usage
To (re)create all autolinks on your current org file call
``` lisp
M-x org-autolinks-upsert-links
```
You should also be able to run the parser on load and on save with
```lisp
(add-hook 'org-mode-hook 'org-autolinks-mode)
(add-hook 'before-save-hook 'org-autolinks-before-save)
```


## Customize
By default =org-autolinks-dir= is set to =org-dir=. Feel free to change that
```lisp
(setq org-autolinks-dir "/some/other/directory")
```
