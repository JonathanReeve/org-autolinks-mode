;;; org-autolinks-mode.el --- Interface for habitica.com

;; Version 0.1
;; Keywords: org, autolink, wiki
;; URL: https://github.com/JonathanReeve/org-autolinks-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((org "9.0") (emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Minor mode for auto-linking filenames, inspired by the linking behavior
;; of Tomboy Notes, notes.vim, and other similar notetaking tools.

;; This will get a list of all the .org files in the directory set by org-autolinks-dir,
;; and turn all those filenames into links in the current buffer.
;; If you have files called "foo.org" and "bar.org," for instance,
;; typing "foo" or "bar" should automatically highlight those words as links,
;; allowing you to click them or press <RET> to jump to those files.
;; This can be used to easily make a personal wiki, without having to
;; manually enter links to other files.

;; By default org-autolinks-dir will default to org-directory

;;; Recommended Usage:
;;
;; (add-hook 'org-mode-hook 'org-autolinks-mode)
;; (add-hook 'before-save-hook 'org-autolinks-before-save)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(require 'org)

(defvar org-autolinks-dir org-directory)

(defun org-autolink-store ()
  "Add an autolink store function."
  (when (and (eq major-mode 'org-mode)
             (org-at-heading-p))
    (org-store-link-props
     :type "autolink"
     :link (format "autolink:*%s.org" (word-at-point))
     :description "autolink")))

(defun org-autolinks-open (path)
  "Open the file the autolink is pointing to.

PATH is the file path."
  (find-file path))

(defun org-autolinks--clear-links (filenames)
  "Remove all autolinks.

FILENAMES is the list of org files paths."
  (let ((regex (org-autolinks--build-autolink-regex filenames)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
        (replace-match "\\1")))))

(defun org-autolinks--build-autolink-regex (filenames)
  "Create a regex to find all autolinks given the list of org file paths.

FILENAMES is the list of org files paths."
  (concat "\\[\\[autolink:[^ ]+\\]\\[\\("
          (mapconcat 'file-name-base filenames "\\|")
          "\\)\\]\\]"))

(defun org-autolinks--convert-to-links (filenames)
  "Find and convert to autolinks all expressions matching an org filename.

FILENAMES is the list of org files paths."
  (let ((regexp (org-autolinks--build-filenames-regex filenames)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp (point-max) t)
        (replace-match
         (concat "[[autolink:"
                 (org-autolinks--path-from-name filenames (match-string 1))
                 "][\\1]]")
         nil nil nil 1)))))

(defun org-autolinks--build-filenames-regex (filenames)
  "Create a regex to find all expression matching an org filename.

FILENAMES is the list of org files paths."
  (concat "\\("
          (mapconcat 'file-name-base filenames "\\|")
          "\\)"))

(defun org-autolinks--path-from-name (filenames basename)
  "Find org file path from name.

FILENAMES is the list of org files paths.
BASENAME is the expression we are trying to match."
  (mapconcat (lambda (x)
               (if (equal (file-name-base x) basename)
                   x
                 ""))
             filenames ""))

(defun org-autolinks-before-save ()
  "Run autolinks-upsert given that you are in org mode."
  (when (and (and (boundp 'org-autolinks-mode) org-autolinks-mode) (eq major-mode 'org-mode))
    (org-autolinks-upsert-links)))

;; on org-autolinks-mode, trigger the format
(add-hook 'org-autolinks-mode-hook 'org-autolinks-before-save)

;; adds the autolink as a type of external link to org-mode
(org-link-set-parameters
 "autolink" :follow 'org-autolinks-open :store 'org-autolink-store)

;;;###autoload
(defun org-autolinks-upsert-links ()
  "Remove all autolinks and recreate them."
  (interactive)
  (let ((filenames (directory-files-recursively org-autolinks-dir ".org$")))
    (org-autolinks--clear-links filenames)
    (org-autolinks--convert-to-links filenames)))

(define-minor-mode org-autolinks-mode
  "Mode to autolink org file based on filename")

(provide 'org-autolinks-mode)
;;; org-autolinks-mode.el ends here
