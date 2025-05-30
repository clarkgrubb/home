
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq init-start (float-time))
(defun init-bench (msg)
  (message (concat (format "%.3f" (- (float-time) init-start)) ": " msg)))

(init-bench "START")

;; Turn off menu bar, tool bar, scroll bar, splash screen, beeping
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)
(setq ring-bell-function #'ignore)

;; Make names for ~ and ~/.emacs.d;
;; add ~/.emacs.d/lib to library path:
;;
(setq home-dir (getenv "HOME"))
(setq emacs-dir (file-name-directory
                 (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat emacs-dir "lib"))

;; Set PATH.  On Mac, when running as a GUI, it
;; will be set to /usr/bin:/bin:/usr/sbin:/bin
;; unless we do this
;;
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(init-bench "require 'exec-path-from-shell")

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
(init-bench "require 'saveplace")

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
(init-bench "require 'ido")

;; Disable compose-mail key bindging
;;
(global-unset-key (kbd "C-x m"))

;; Add ido to M-x
;;
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(init-bench "require 'smex")

;; Make M-x ispell use aspell
;;
;;   $ brew install aspell
;;   $ sudo apt-get install aspell
;;
;;(dolist (path '("/usr/bin/aspell" "/usr/local/bin/aspell"))
;;  (if (file-exists-p path)
;;      (progn
;;        (setq ispell-program-name path)
;;        (return))))
;;(init-bench "find spelling checker")

;; To install magit:
;;
;;    M-x package-refresh-contents RET
;;    M-x package-install RET magit RET
;;
;; M-x magit-status
;; C-c s
;;
(add-to-list 'load-path (concat emacs-dir "lib/git-modes"))
;;(add-to-list 'load-path (concat emacs-dir "lib/magit"))
;;(eval-after-load 'info
;;  '(progn (info-initialize)
;;          (add-to-list 'Info-directory-list (concat emacs-dir "lib/magit"))))
;;(require 'magit)
;;(defun disable-magit-highlight-in-buffer ()
;;  (face-remap-add-relative 'magit-item-highlight '()))
;;(add-hook 'magit-diff-mode-hook 'disable-magit-highlight-in-buffer)
;;(init-bench "require 'magit")

;; To install projectile:
;;
;;    M-x package-install RET projectile RET
;;
;; C-c p C-h   list projectile key bindings
;; C-c p b     switch to buffer in project
;; C-c p d     find directory in project by searching on full path
;; C-c p D     open project root in dired
;; C-c p f     find file in project by searching path relative to working dir
;; C-c p k     kill all project buffers
;; C-c p m     compile in root [customized in projectile.el]
;; C-c p o     search project buffers
;; C-c p p     switch between projects
;; C-c p r     query and replace in project
;; C-c p s     switch project
;; C-c p S     save all project buffers
;; C-c p !     run shell cmd at project root
;; C-c p ESC   switch to most recent project buffer

;; Set defaults for M-x grep and M-x find-grep
;;
(setq grep-command "grep -nH ")
(setq grep-find-command "find . -name '*' | xargs grep -nH ")

;; Define M-x ag
;;
(defun ag (command-args)
  (interactive
   (progn
     (let ((ag-cmd "ag --nocolor --literal --smart-case --nogroup --column -- "))
       (list (read-shell-command "Run: "
                                 ag-cmd
                                 'ag-history
                                 ag-cmd)))))
  (compilation-start command-args 'grep-mode))

;; Set the shell used by M-x shell; make shell-mode work w/ UTF-8.
;;
(setq explicit-shell-file-name "bash")
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))

;; Useful version control commands:
;;
;;  C-x v =   diff current buffer with what is in version control
;;  C-x v u   revert file to what is in version control
;;  C-x v g   git blame

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
(setq whitespace-line-column 99)
;; (add-hook 'prog-mode-hook (lambda () (whitespace-mode t)))
;; (add-hook 'yaml-mode-hook (lambda () (whitespace-mode t)))
(init-bench "require 'whitespace")

;; Turn off electric-indent-mode
;;
(add-hook 'prog-mode-hook (lambda () (electric-indent-mode 0)))

;; Replace tabs with spaces.  To insert a tab use
;;
;;   C-q TAB
;;
;;(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; set javscript-mode indent to 4
;;
(add-hook 'js-mode-hook
     (lambda()
       (setq js-indent-level 4)))

;; do not insert tabs into html
;;
(add-hook 'html-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))

;; do not insert tabs into groovy source
;;
(add-hook 'groovy-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)))


;; LaTeX input method.  To activate and deactivate:
;;
;;   C-x RET C-\ latex
;;   C-\
;;
;;(require 'latex)
;;(init-bench "require 'latex")

;; Set syntax highlight colors:
;;
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "gray51"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "gray51"))))
 '(font-lock-doc-face ((t (:foreground "gray51"))))
 '(font-lock-variable-name-face ((t (:foreground "chocolate"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "firebrick"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "firebrick"))))
 '(font-lock-string-face ((t (:foreground "firebrick"))))
 '(font-lock-keyword-face ((t (:foreground "purple"))))
 '(font-lock-preprocessor-face ((t (:foreground "light slate blue"))))
 '(font-lock-function-name-face ((t (:foreground "medium blue"))))
 '(font-lock-constant-face ((t (:foreground "olive drab"))))
 '(font-lock-builtin-face ((t (:foreground "olive drab"))))
 '(font-lock-type-face ((t (:foreground "dark green"))))
 '(font-lock-negation-char-face ((t (:foreground "black"))))
 '(sh-quoted-exec ((t (:foreground "black")))))


;; Add more text modes:
;;
(autoload 'markdown-mode "markdown-mode" "Markdown Mode." t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.xslt\\'" . xml-mode))
;; MSBuild project file:
(add-to-list 'auto-mode-alist '("\\.proj\\'" . xml-mode))

;; Add nore programming language modes:
;;
(autoload 'go-mode "go-mode" "Go Mode." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(autoload 'groovy-mode "groovy-mode" "Groovy Mode." t)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

(autoload 'lua-mode "lua-mode" "Lua Mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(autoload 'php-mode "php-mode" "PHP Mode." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

(autoload 'csv-mode "csv-mode" "CSV Mode." t)
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

(add-to-list 'load-path "/Users/clark/Source/home/.emacs.d/lib/rust-mode/")
(autoload 'rust-mode "rust-mode" "Rust Mode." t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . js-mode))

;;    M-x package-install RET ensime RET
;;    M-x package-install RET scala-mode RET
;;    M-x package-install RET lua-mode RET

(defalias 'perl-mode 'cperl-mode)

;; Use 2 spaces for indent;
;; don't align nested functions with containing paren.
;;
(custom-set-variables
 '(js-indent-level 2))
(custom-set-variables
 '(js-flat-functions t))

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
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(defun display-buffer-file-name ()
  (interactive)
  (message buffer-file-name))
(global-set-key "\C-cf" 'display-buffer-file-name)
(global-set-key "\C-cm" 'compile)
(global-set-key "\C-cr" 'query-replace)
(global-set-key "\C-cs" 'magit-status)
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

;;(if window-system
;;    (when (eq system-type 'darwin)
;;      (create-fontset-from-fontset-spec
;;       (concat "-apple-consolas-medium-r-normal--"
;;               mac-font-size
;;               "-*-*-*-*-*-fontset-mac, "
;;               "ascii:-apple-consolas-medium-r-normal--"
;;               mac-font-size
;;               "-*-*-*-m-*-mac-roman, "
;;               "latin-iso8859-1:"
;;               "-apple-consolas-medium-r-normal--"
;;               mac-font-size
;;               "-*-*-*-m-*-mac-roman, "
;;               "mule-unicode-0100-24ff:"
;;               "-apple-consolas-medium-r-normal--"
;;               mac-font-size
;;               "-*-*-*-m-*-mac-roman"))
;;      (set-frame-font
;;       (concat
;;        "-apple-consolas-medium-r-normal--"
;;        mac-font-size
;;        "-*-*-*-*-*-fontset-mac")
;;       'keep)))

(if window-system
    (when (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :height 115 :family "Consolas")))

(init-bench "END")
