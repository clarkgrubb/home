(autoload 'vc-git-root "vc-git")
(autoload 'vc-hg-root "vc-hg")
(autoload 'vc-svn-root "vc-svn")

(defun make-tags/longest-string (&rest strings)
  "Return longest string in list.."
  (let ((longest-string nil))
    (dolist (string strings)
      (cond ((null longest-string)
             (setq longest-string string))
            ((stringp string)
             (when (< (length longest-string)
                      (length string))
               (setq longest-string string)))))
    longest-string))


(defun make-tags/project-root (file-path)
  "Find project root containing file-path."
  (or (make-tags/longest-string
       (vc-git-root file-path)
       (vc-hg-root file-path)
       (vc-svn-root file-path))
      file-path))

(defun make-tags ()
  "Find project root; prompt for suffixes; run etags; visit TAGS file."
  (interactive ())
  (let*
      ((root (make-tags/project-root
              (or load-file-name buffer-file-name default-directory)))
       (glob (read-from-minibuffer
              "Files to index: "
              "*.[ch]"))
       (tags (concat root "TAGS"))
       (cmd (concat "find " root " -name '" glob "' | etags -o " tags " -")))
    (shell-command cmd)
    (visit-tags-table root)))

(provide 'make-tags)
