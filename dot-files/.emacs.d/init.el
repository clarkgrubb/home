;; Turn off menu bar, tool bar, scroll bar, splash screen
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)

;; Set font on Mac
;;
(setq mac-font-size "12")

(when (eq system-type 'darwin)
  (create-fontset-from-fontset-spec
   (concat "-apple-consolas-medium-r-normal--"
           mac-font-size
           "-*-*-*-*-*-fontset-mac, "
           "ascii:-apple-consolas-medium-r-normal--"
           mac-font-size
           "-*-*-*-m-*-mac-roman, "
           "latin-iso8859-1:"
           "-apple-consolas-medium-r-normal--"
           mac-font-size
           "-*-*-*-m-*-mac-roman, "
           "mule-unicode-0100-24ff:"
           "-apple-consolas-medium-r-normal--"
           mac-font-size
           "-*-*-*-m-*-mac-roman"))
  (set-frame-font
   (concat
    "-apple-consolas-medium-r-normal--"
    mac-font-size
    "-*-*-*-*-*-fontset-mac")
   'keep))

;; Make names for ~ and ~/.emacs.d
;;
(setq home-dir (getenv "HOME"))
(setq emacs-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))

;; Add ~/.emacs.d/lib to library path
;;
(add-to-list 'load-path (concat emacs-dir "lib"))

;; Add Common Lisp extensions
;;
;;  http://www.gnu.org/software/emacs/manual/html_mono/cl.html
;;
(require 'cl)

;; Set point to previous position when visiting a file.
;; Stores them in ~/.emacs-places
;;
(require 'saveplace)
(setq-default save-place t)

;; Use part of full path instead of suffixes <1>, <2>, ...
;; to distinguish buffers when visiting files with the
;; same basename.
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Bind C-x b   to ido-switch-buffer
;;      C-x C-b to ibuffer (instead of list-buffers)
;;      C-x C-f to ido-find-file
;;      C-x C-i to ido-insert-file
;;      C-x C-w to ido-write-file
;;
;; When searching, will use prefix matches in
;; preference to flex-matching.  A flex-match
;; is any in which the typed characters are
;; found in the target in the same sequence.
;;
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(global-set-key "\C-x\C-b" 'ibuffer)

;; Make M-x ispell use aspell
;;
(dolist (path '("/usr/bin/aspell" "/usr/local/bin/aspell"))
  (if (file-exists-p path)
      (progn
        (setq ispell-program-name path)
        (return))))

;; For use with Chrome "Edit with Emacs" extension
;;
;; Use C-c C-c to send buffer back to Chrome.
;;
(require 'edit-server)
(edit-server-start)

;; For use with Firefox "It's All Text" extension
;; which has a hotkey which I've set to ⌘E.
;;
;; Use C-x # to send buffer back to Firefox.
;;
(server-start)

;; Put twiddle files in ~/.emacs-backups
;;
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat home-dir "/.emacs-backups")))))

;; Display line and column number in mode line as
;;
;;     (LINE,COL)
;;
(column-number-mode t)

;; Enable functions which are disabled by
;; default.  Keybindings for first two
;; are C-x C-u and C-x C-l
;;
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

;; Highlight these in prog mode:
;;
;;   tabs
;;   trailing whitespace on lines
;;   blank lines at the end of the file
;;   long lines
;;
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 79)
(add-hook 'prog-mode-hook (lambda () (whitespace-mode t)))

;; Replace tabs with spaces.  To insert a tab use
;;
;;   C-q TAB
;;
(setq-default indent-tabs-mode nil)

;; More text modes.
;;
(autoload 'markdown-mode "markdown-mode" "Markdown Mode." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; More programming language modes.
;;
(autoload 'erlang-mode "erlang" "Erlang Mode." t)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

(autoload 'go-mode "go-mode" "Go Mode." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(autoload 'lua-mode "lua-mode" "Lua Mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(autoload 'php-mode "php-mode" "PHP Mode." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(defalias 'perl-mode 'cperl-mode)

;; Display an image in a buffer.  If the buffer is saved
;; to a file, the image is not persisted in any way.
;;
(defun insert-file-image (file)
  (interactive "f")
  (insert-image (create-image file)))

;; Show visited file path in the minibuffer.
;;
(defun display-buffer-file-name ()
  (interactive)
  (message buffer-file-name))

;; Makes the latex input method available.  Use C-\
;; to turn it on.
;;
(require 'latex)

;; More personal key bindings
;;
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cc" 'clipboard-yank)
(global-set-key "\C-cd" 'ido-dired)
(global-set-key "\C-cf" 'display-buffer-file-name)
(global-set-key "\C-ci" 'insert-file-image)
(global-set-key "\C-cr" 'query-replace)
(global-set-key "\C-cv" 'clipboard-kill-ring-save)
(global-set-key "\C-cx" 'clipboard-kill-region)
