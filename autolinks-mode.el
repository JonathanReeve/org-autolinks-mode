;; Minor mode for auto-linking filenames, inspired by the linking behavior
;; of Tomboy Notes, notes.vim, and other similar notetaking tools.

;; This will get a list of all the .org files in the directory set by org-autolinks-dir,
;; and turn all those filenames into links in the current buffer.
;; If you have files called "foo.org" and "bar.org," for instance,
;; typing "foo" or "bar" should automatically highlight those words as links,
;; allowing you to click them or press <RET> to jump to those files.
;; This can be used to easily make a personal wiki, without having to
;; manually enter links to other files.

;; This code is adapted from:
;;; org-wikinodes.el --- Wiki-like CamelCase links to outline nodes
;; Copyright (C) 2010-2011 Free Software Foundation, Inc.

;; Wikinodes author: Carsten Dominik <carsten at orgmode dot org>
;; Autolinks author: Jonathan Reeve <jon dot reeve at gmail dot com>
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 's)
(eval-when-compile
  (require 'cl))

(defgroup org-autolinks nil
  "Wiki-like link words or phrases to files in Org mode."
  :tag "Org AutoLinks"
  :group 'org)

;; Set directory of files to search for *.org files 
(setq org-autolinks-dir "~/Dropbox/Org/")

;; Get all .org files in this directory. 
(setq org-autolinks-files (directory-files org-autolinks-dir t "[.]org\\'"))

;; Convert it to a list of just the filenames, without paths.
(setq org-autolinks-files-bare
      (mapcar 'file-name-nondirectory org-autolinks-files))

;; Strip extensions
(setq org-autolinks-topics
      (mapcar 'file-name-sans-extension org-autolinks-files-bare))

;; Make a regex of all the filenames
(setq org-autolinks-regex (s-join "\\|" org-autolinks-topics))

;; Debugging
;;(describe-variable 'org-autolinks-regex)

(defconst org-autolinks-camel-regexp org-autolinks-regex 
  "Regular expression matching CamelCase words.")

(defcustom org-autolinks-active t
  "Should CamelCase links be active in the current file?"
  :group 'org-autolinks
  :type 'boolean)
(put 'org-autolinks-active 'safe-local-variable 'booleanp)

(defcustom org-autolinks-scope 'directory
  "The scope of searches for wiki targets.
Allowed values are:

file       Search for targets in the current file only
directory  Search for targets in all org files in the current directory"
  :group 'org-autolinks
  :type '(choice
	  (const :tag "Find targets in current file" file)
	  (const :tag "Find targets in current directory" directory)))

(defcustom org-autolinks-create-targets 'query
  "Non-nil means create Wiki target when following a wiki link fails.
Allowed values are:

nil     never create node, just throw an error if the target does not exist
query   ask the user what to do
t       create the node in the current buffer
\"file.org\"  create the node in the file \"file.org\", in the same directory

If you are using wiki links across files, you need to set `org-autolinks-scope'
to `directory'."
  :group 'org-autolinks
  :type '(choice
	  (const :tag "Never automatically create node" nil)
	  (const :tag "In current file" t)
	  (file  :tag "In one special file\n")
	  (const :tag "Query the user" query)))

;;; Link activation

(defun org-autolinks-activate-links (limit)
  "Activate CamelCase words as links to Wiki targets."
  (when org-autolinks-active
    (let (case-fold-search)
      (if (re-search-forward org-autolinks-camel-regexp limit t)
	  (if (equal (char-after (point-at-bol)) ?*)
	      (progn
		;; in  heading - deactivate flyspell
		(org-remove-flyspell-overlays-in (match-beginning 0)
						 (match-end 0))
		(add-text-properties (match-beginning 0) (match-end 0)
				     '(org-no-flyspell t))
		t)
	    ;; this is a wiki link
	    (org-remove-flyspell-overlays-in (match-beginning 0)
					     (match-end 0))
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'mouse-face 'highlight
				       'face 'org-link
				       'keymap org-mouse-map
				       'help-echo "Wiki Link"))
	    t)))))

;;; Following links and creating non-existing target nodes

(defun org-autolinks-open-at-point ()
  "Check if the cursor is on a Wiki link and follow the link.

This function goes into `org-open-at-point-functions'."
  (and org-autolinks-active
       (not (org-on-heading-p))
       (let (case-fold-search) (org-in-regexp org-autolinks-camel-regexp))
       (progn (org-autolinks-follow-link (match-string 0)) t)))

(defun org-autolinks-follow-link (target)
  "Follow a wiki link to TARGET.

This need to be found as an exact headline match, either in the current
buffer, or in any .org file in the current directory, depending on the
variable `org-autolinks-scope'.

If a target headline is not found, it may be created according to the
setting of `org-autolinks-create-targets'."
  (if current-prefix-arg (org-autolinks-clear-direcory-targets-cache))
  (let ((create org-autolinks-create-targets)
	visiting buffer m pos file rpl)
    (setq pos
	  (or (org-find-exact-headline-in-buffer target (current-buffer))
	      (and (eq org-autolinks-scope 'directory)
		   (setq file (org-autolinks-which-file target))
		   (org-find-exact-headline-in-buffer
		    target (or (get-file-buffer file)
			       (find-file-noselect file))))))
    (if pos
	(progn
	  (org-mark-ring-push (point))
	  (org-goto-marker-or-bmk pos)
	  (move-marker pos nil))
      (when (eq create 'query)
	(if (eq org-autolinks-scope 'directory)
	    (progn
	      (message "Node \"%s\" does not exist.  Should it be created?
\[RET] in this buffer   [TAB] in another file  [q]uit" target)
	      (setq rpl (read-char-exclusive))
	      (cond
	       ((member rpl '(?\C-g ?q)) (error "Abort"))
	       ((equal rpl ?\C-m) (setq create t))
	       ((equal rpl ?\C-i)
		(setq create (file-name-nondirectory
			      (read-file-name "Create in file: "))))
	       (t (error "Invalid selection"))))
	  (if (y-or-n-p (format "Create new node \"%s\" in current buffer? "
				target))
	      (setq create t)
	    (error "Abort"))))

      (cond
       ((not create)
	;; We are not allowed to create the new node
	(error "No match for link to \"%s\"" target))
       ((stringp create)
	;; Make new node in another file
	(org-mark-ring-push (point))
	(org-pop-to-buffer-same-window (find-file-noselect create))
	(goto-char (point-max))
	(or (bolp) (newline))
	(insert "\n* " target "\n")
	(backward-char 1)
	(org-autolinks-add-target-to-cache target)
	(message "New Wiki target `%s' created in file \"%s\""
		 target create))
       (t
	;; Make new node in current buffer
	(org-mark-ring-push (point))
	(goto-char (point-max))
	(or (bolp) (newline))
	(insert "* " target "\n")
	(backward-char 1)
	(org-autolinks-add-target-to-cache target)
	(message "New Wiki target `%s' created in current buffer"
		 target))))))

;;; The target cache 

(defvar org-autolinks-directory-targets-cache nil)

(defun org-autolinks-clear-cache-when-on-target ()
  "When on a headline that is a Wiki target, clear the cache."
  (when (and (org-on-heading-p)
	     (org-in-regexp (format org-complex-heading-regexp-format
				    org-autolinks-camel-regexp))
	     (org-in-regexp org-autolinks-camel-regexp))
    (org-autolinks-clear-direcory-targets-cache)
    t))

(defun org-autolinks-clear-direcory-targets-cache ()
  "Clear the cache where to find wiki targets."
  (interactive)
  (setq org-autolinks-directory-targets-cache nil)
  (message "Wiki target cache cleared, so that it will update when used again"))

(defun org-autolinks-get-targets ()
  "Return a list of all wiki targets in the current buffer."
  (let ((re (format org-complex-heading-regexp-format
		    org-autolinks-camel-regexp))
	(case-fold-search nil)
	targets)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (push (org-match-string-no-properties 4) targets))))
    (nreverse targets)))
		    
(defun org-autolinks-get-links-for-directory (dir)
  "Return an alist that connects wiki links to files in directory DIR."
  (let ((files (directory-files dir nil "\\`[^.#].*\\.org\\'"))
	(org-inhibit-startup t)
	target-file-alist file visiting m buffer)
    (while (setq file (pop files))
      (setq visiting (org-find-base-buffer-visiting file))
      (setq buffer (or visiting (find-file-noselect file)))
      (with-current-buffer buffer
	(mapc
	 (lambda (target)
	   (setq target-file-alist (cons (cons target file) target-file-alist)))
	 (org-autolinks-get-targets)))
      (or visiting (kill-buffer buffer)))
    target-file-alist))

(defun org-autolinks-add-target-to-cache (target &optional file)
  (setq file (or file buffer-file-name (error "No file for new wiki target")))
  (set-text-properties 0 (length target) nil target)
  (let ((dir (file-name-directory (expand-file-name file)))
	a)
    (setq a (assoc dir org-autolinks-directory-targets-cache))
    (if a
	;; Push the new target onto the existing list
	(push (cons target (expand-file-name file)) (cdr a))
      ;; Call org-autolinks-which-file so that the cache will be filled
      (org-autolinks-which-file target dir))))

(defun org-autolinks-which-file (target &optional directory)
  "Return the file for wiki headline TARGET DIRECTORY.
If there is no such wiki target, return nil."
  (setq directory (expand-file-name (or directory default-directory)))
  (unless (assoc directory org-autolinks-directory-targets-cache)
    (push (cons directory (org-autolinks-get-links-for-directory directory))
	  org-autolinks-directory-targets-cache))
  (cdr (assoc target (cdr (assoc directory
				 org-autolinks-directory-targets-cache)))))

;;; Exporting Wiki links

(defvar target)
(defvar target-alist)
(defvar last-section-target)
(defvar org-export-target-aliases)
(defun org-autolinks-set-wiki-targets-during-export ()
  (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
	(case-fold-search nil)
	wtarget a)
    (when (string-match (format org-complex-heading-regexp-format
				org-autolinks-camel-regexp)
			line)
      (setq wtarget (match-string 4 line))
      (push (cons wtarget target) target-alist)
      (setq a (or (assoc last-section-target org-export-target-aliases)
		  (progn
		    (push (list last-section-target)
			  org-export-target-aliases)
		    (car org-export-target-aliases))))
      (push (caar target-alist) (cdr a)))))

(defvar org-current-export-file)
(defun org-autolinks-process-links-for-export ()
  "Process Wiki links in the export preprocess buffer.

Try to find target matches in the wiki scope and replace CamelCase words
with working links."
  (let ((re org-autolinks-camel-regexp)
	(case-fold-search nil)
	link file)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (org-if-unprotected-at (match-beginning 0)
	(unless (save-match-data
		  (or (org-on-heading-p)
		      (org-in-regexp org-bracket-link-regexp)
		      (org-in-regexp org-plain-link-re)
		      (org-in-regexp "<<[^<>]+>>")))
	  (setq link (match-string 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (save-match-data
	    (cond
	     ((org-find-exact-headline-in-buffer link (current-buffer))
	      ;; Found in current buffer
	      (insert (format "[[#%s][%s]]" link link)))
	     ((eq org-autolinks-scope 'file)
	      ;; No match in file, and other files are not allowed
	      (insert (format "%s" link)))
	     ((setq file
		    (and (org-string-nw-p org-current-export-file)
			 (org-autolinks-which-file
			  link (file-name-directory org-current-export-file))))
	      ;; Match in another file in the current directory
	      (insert (format "[[file:%s::%s][%s]]" file link link)))
	     (t ;; No match for this link
	      (insert (format "%s" link))))))))))

;;; Hook the WikiNode mechanism into Org

;; `C-c C-o' should follow wiki links
(add-hook 'org-open-at-point-functions 'org-autolinks-open-at-point)

;; `C-c C-c' should clear the cache
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-autolinks-clear-cache-when-on-target)

;; Make Wiki haeding create additional link names for headlines
(add-hook 'org-export-define-heading-targets-headline-hook
	  'org-autolinks-set-wiki-targets-during-export)

;; Turn Wiki links into links the exporter will treat correctly
(add-hook 'org-export-preprocess-after-radio-targets-hook
	  'org-autolinks-process-links-for-export)

;; Activate CamelCase words as part of Org mode font lock

(defun org-autolinks-add-to-font-lock-keywords ()
  "Add wikinode CamelCase highlighting to `org-font-lock-extra-keywords'."
  (let ((m (member '(org-activate-plain-links) org-font-lock-extra-keywords)))
    (if m
	(setcdr m (cons '(org-autolinks-activate-links) (cdr m)))
      (message
       "Failed to add wikinodes to `org-font-lock-extra-keywords'."))))
  
(add-hook 'org-font-lock-set-keywords-hook
	  'org-autolinks-add-to-font-lock-keywords)

;;(provide 'org-autolinks)

;;; org-wikinodes.el ends here

;;;###autoload
(define-minor-mode org-autolinks-mode
  "Automagically link to files in Org mode."
  :lighter " autolinks"
  )

;;;###autoload
(add-hook 'org-mode-hook 'org-autolinks-mode)

(provide 'org-autolinks-mode)
