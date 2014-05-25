;; Turn off menu bar, tool bar, scroll bar, splash screen
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)

;; Make names for ~ and ~/.emacs.d;
;; add ~/.emacs.d/lib to library path:
;;
(setq home-dir (getenv "HOME"))
(setq emacs-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat emacs-dir "lib"))

;; Add Common Lisp extensions
;;
;;  http://www.gnu.org/software/emacs/manual/html_mono/cl.html
;;
(require 'cl)

;; Prevent scrolling from causing beeping.
;;
(defun my-bell-function ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll down up next-line
                                previous-line backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; Put a mode setting instruction in the *scratch* buffer, in case
;; we save it.
;;
(setq initial-scratch-message ";; -*- mode: lisp-interaction -*-\n\n")

;; Put twiddle files in ~/.emacs-backups
;;
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat home-dir "/.emacs-backups")))))

;; Display line and column number in mode line as
;;
;;     (LINE,COL)
;;
(column-number-mode t)

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

;; Add ido to M-x
;;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Make M-x ispell use aspell
;;
(dolist (path '("/usr/bin/aspell" "/usr/local/bin/aspell"))
  (if (file-exists-p path)
      (progn
        (setq ispell-program-name path)
        (return))))

;; Set defaults for M-x grep and M-x find-grep
;;
(setq grep-command "grep -nH ")
(setq grep-find-command "find . -name '*' | xargs grep -nH ")

;; Add these commands:
;;
;;    M-x ag
;;    M-x ag-files
;;    M-x ag-regexp
;;    M-x ag-project
;;    M-x ag-project-files
;;    M-x ag-project-regexp
;;
;; Uses an external executable called 'ag'.  To install
;;
;;    $ brew install the_silver_searcher
;;    $ sudo apt-get install silversearcher-ag
;;
(require 'ag)

;; Add this command:
;;
;;   M-x make-tags
;;
;; It finds project root using version control directories;
;; prompts for glob pattern of files to index; calls 'etags';
;; and visits the resulting table.
;;
(require 'make-tags)

;; Set the shell used by M-x shell; make shell-mode work w/ UTF-8.
;;
(setq explicit-shell-file-name "bash")
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

;; Add a replacement for M-x term:
;;
;;   M-x multi-term
;;
;; When using multi-term.el, M-x and C-x behave normally, so
;; it is easy to switch buffers.  Without it one must preface
;; each keystroke with C-c to send it to Emacs.
;;
(require 'multi-term)
 (setq multi-term-program "/bin/bash")

;; Add command to interact w/ git:
;;
;;  M-x magit-status
;;
(add-to-list 'load-path (concat emacs-dir "lib/git-modes/"))
(add-to-list 'load-path (concat emacs-dir "lib/magit/"))
(require 'magit)

;; Prevent ediff from opening a separate navigation window;
;; have ediff open windows side-by-side instead of stacked on
;; top of each other.
;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

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

;; Custom syntax highlight colors:
;;
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "gray51")))))


;; Add more text modes:
;;
(autoload 'markdown-mode "markdown-mode" "Markdown Mode." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))

;; Make latex input method available.  Use C-\
;; to turn it on.
;;
(require 'latex)

;; Add nore programming language modes:
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

(autoload 'groovy-mode "groovy-mode" "Groovy Mode." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(defalias 'perl-mode 'cperl-mode)

;; Turn off electric mode for shell here documents:
;;
(add-hook 'sh-mode-hook
          (lambda ()
            (sh-electric-here-document-mode -1)))

;; Enable functions which are disabled by
;; default.  Keybindings for first two
;; are C-x C-u and C-x C-l
;;
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

;; Add personal key bindings
;;
(global-set-key "\C-ca" 'ag-project)
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(global-set-key "\C-cd" 'ido-dired)
(defun display-buffer-file-name ()
  (interactive)
  (message buffer-file-name))
(global-set-key "\C-cf" 'display-buffer-file-name)
(global-set-key "\C-cg" 'find-grep)
(global-set-key "\C-cm" 'compile)
(global-set-key "\C-cr" 'query-replace)
(global-set-key "\C-cs" 'magit-status)
(global-set-key "\C-ct" 'make-tags)
(global-set-key "\C-cv" 'clipboard-yank)
(global-set-key "\C-cw" 'whitespace-mode)
(global-set-key "\C-cx" 'clipboard-kill-region)

;; Customize  Mac key binding:
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

;; Set font on Mac:
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
