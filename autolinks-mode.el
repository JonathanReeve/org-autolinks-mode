;; Minor mode for auto-linking filenames.
;; This will scan the directory set in autolinks-mode-dir,
;; get a list of all the .org files there, and if this mode is enabled,
;; and you type the name of one of these .org files, it will automatically
;; treat that as a link, allowing you to quickly jump between files.
;; This can be used to easily make a personal wiki, without having to
;; manually enter links to other files. 

;; Set directory of files to search for *.org files 
(setq autolinks-mode-dir "~/Dropbox/Org/")

;;(setq autolinks-mode-files (directory-files autolinks-mode-dir))

;; Following code adapted from http://stackoverflow.com/a/26672609
(defun autolinks-mode-open-link ()
  "Open link, interpreting it as the name of a headline."
  (let* ((el (org-element-context))
         (type (first el))
         (link-type (plist-get (cadr el) :type))
         (path (let ((path-1 (plist-get (cadr el) :path)))
                 (when (stringp path-1)
                   (org-link-unescape path-1)))))
    (when (and (eql type 'link)
               path
               (string= link-type "fuzzy"))
      (let* ((path (regexp-quote path))
             (result
                 (delq nil
                       (org-map-entries
                        (lambda ()
                          (when (string-match
                                 path
                                 (org-get-heading))
                            (list (buffer-file-name) (point))))
                        nil
                        ;; Here we set the scope.
                        ;; 'agenda would search in all agenda files.
                        ;; We want a list of all org files in `autolinks-mode-dir'.
                        (directory-files
                         autolinks-mode-dir
                         t "[.]org\\'")))))
        (when result
          (when (> (length result) 1)
            (message "Warning: multiple search results for %s" path))
          (let ((file (caar result))
                (pos (cadar result)))
            (find-file file)
            (goto-char pos)))))))

(add-hook 
 'org-open-at-point-functions
 'my-open-link-function)

;;;###autoload
(define-minor-mode autolinks-mode
  "Automatically link to files in Org mode."
  :lighter " autolinks"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'autolinks-mode-open-link)
            map))

;;;###autoload
;; Change this to Org mode eventually
(add-hook 'text-mode-hook 'foo-mode)

(provide 'autolinks-mode)
