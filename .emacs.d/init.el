;; Turn off menu bar, tool bar, scroll bar, splash screen
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)

;; Set font on Mac
;;
(setq mac-font-size "12")

(if window-system
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
       'keep)))

;; Prevent scrolling from causing beeping.
;;
(defun my-bell-function ()
  (unless (memq this-command
    	'(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

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

;; Put a mode setting instruction in the *scratch* buffer, in case
;; we save it.
;;
(setq initial-scratch-message ";; -*- mode: lisp-interaction -*-\n\n")

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

;; Make shell-mode work with UTF-8.  This provides a way to use
;; Emacs input-methods to enter Unicode characters at the shell.
;;
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))


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

(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

(add-to-list 'load-path (concat emacs-dir "lib/scala-mode2/"))
(autoload 'scala-mode "scala-mode2" "Scala Mode." t)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(require 'scala-mode2)

;; Instead of objc-mode
;;
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(add-to-list 'load-path (concat emacs-dir "lib/tuareg-caml-mode"))
(add-to-list 'load-path (concat emacs-dir "lib/tuareg-2.0.4"))
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

(load (concat emacs-dir "lib/haskell-mode/haskell-site-file.el"))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(defalias 'perl-mode 'cperl-mode)

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
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(global-set-key "\C-cd" 'ido-dired)
(global-set-key "\C-cf" 'display-buffer-file-name)
(global-set-key "\C-cr" 'query-replace)
(global-set-key "\C-cv" 'clipboard-yank)
(global-set-key "\C-cx" 'clipboard-kill-region)

;; Mac key binding customizations.
;;
;; Make ⌘-= and ⌘-- alternatives for C-x C-= and
;; and C-x C--.
;;
;; Don't use the right option key as a meta key.
;; This way it can be used to enter Latin accent characters.
;;
(if window-system
    (when (eq system-type 'darwin)
      (global-set-key (kbd "s-=") 'text-scale-adjust)
      (global-set-key (kbd "s--") 'text-scale-adjust)
      (setq mac-right-option-modifier nil)))
