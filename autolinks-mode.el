;; Minor mode for auto-linking filenames.
(make-variable-buffer-local
 (defvar foo-count 0
   "Number of foos inserted into the current buffer."))

(defun insert-foo ()
  (interactive)
  (setq foo-count (1+ foo-count))
  (insert "foo"))

(setq autolinks-mode-dir "~/Dropbox/Org/")

(setq autolinks-mode-files (directory-files autolinks-mode-dir))

(insert autolinks-mode-files)

;;;###autoload
(define-minor-mode autolinks-mode
  "Automatically link to files in Org mode."
  :lighter " autolinks"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'insert-foo)
            map))

;;;###autoload
;; Change this to Org mode eventually
(add-hook 'text-mode-hook 'foo-mode)

(provide 'autolinks-mode)
