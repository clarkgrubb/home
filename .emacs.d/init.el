;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit/jabber"))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
;; cgrubb disable (require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up ELPA, the package manager

(require 'package)
(package-initialize)
(require 'starter-kit-elpa)

;; cgrubb disable
;;(load "elpa-to-submit/nxhtml/autostart")

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
(require 'starter-kit-perl)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;; cgrubb
;;

;; hard-coded path: Mac only
(defun dired-emacs-lisp ()
  "Open the Emacs Lisp directory in dired."
  (interactive)
   (dired "/Applications/Emacs.app/Contents/Resources/lisp"))

(require 'column-marker)
(require 'edit-server)
;; generates warnings
;; (require 'inf-caml)
(require 'php-mode)
(require 'undo-tree)
(global-undo-tree-mode)
(edit-server-start)
(defun mark-column-80 () (interactive) (column-marker-1 80))
(global-set-key "\C-c8" 'mark-column-80)
(global-set-key "\C-cb" 'revert-buffer)
;; for windows
(global-set-key "\C-cc" 'clipboard-kill-ring-save)
(global-set-key "\C-cd" 'dired)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cl" 'dired-emacs-lisp)
(global-set-key "\C-cr" 'query-replace)
;; for windows
(global-set-key "\C-cv" 'clipboard-yank)

(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(setq ido-use-url-at-point nil)
(setq ido-use-filename-at-point nil)
(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))
(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(setq load-path (cons "~clark/.emacs.d/ocaml/" load-path))
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")
(set-cursor-color "red")
(blink-cursor-mode)

;; cgrubb: set default-directory
;;
(let ((os-user (if (getenv "USER")
                   (getenv "USER")
                 (getenv "USERNAME"))))
  (mapcar (lambda (x) (let ((dir (concat x os-user)))
                        (if (file-exists-p dir)
                            (setq default-directory dir)
                          nil)))
          '("/home/" "/Users/" "C:/Users/")))

;; used by starter-kit-ruby
(require 'tramp-cmds)

(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; SLIME
(add-to-list 'load-path "~/.emacs.d/slime/")
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
(require 'slime)
(slime-setup)
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl" "--sbcl-nolineedit"))
        (clisp ("/opt/local/bin/clisp"))
        (abcl ("/usr/local/bin/abcl"))))
(defmacro defslime-start (name mapping)
  `(defun ,name ()
     (interactive)
     (let ((slime-default-lisp ,mapping))
       (slime))))
(defslime-start abcl 'abcl)
(defslime-start clisp 'clisp)
(defslime-start sbcl 'sbcl)

;; MobileOrg
;; Set to the location of your Org files on your local system
(add-to-list 'load-path (concat dotfiles-dir "/org/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "/org/contrib/lisp"))


(setq org-directory "~/Org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")

(defun load-if-exists (library-file)
  (if (file-exists-p library-file)
      (load library-file)))

;;; init.el ends here