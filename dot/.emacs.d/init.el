;; Turn off menu bar, tool bar, and scroll bar
;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Causes Emacs to start in a lisp evaluation buffer
;; when no file name is provided on the command line
;; instead of the splash screen.
;;
(setq inhibit-splash-screen t)

;; ~/.emacs.d/lib is a place to put emacs libraries
;;
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "lib"))

;; Common Lisp extensions
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
;; to distinguish buffers visiting files with the
;; same basename.
;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Maintains a list of recently visited files.  They are
;; in the variable 'recentf-list.  Try these commands:
;;
;; M-: (length recentf-list)
;; M-: recent-list
;;
(require 'recentf)
(recentf-mode 1)

;;
;;
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

;;
;;
(require 'imenu)

;; Used with Chrome "Edit with Emacs" extension
;;
(require 'edit-server)
(edit-server-start)

;; For running SBCL inside Emacs
;;
(add-to-list 'load-path "~/.emacs.d/lib/slime/")
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
(require 'slime)
(slime-setup)
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl" "--sbcl-nolineedit"))))
(defmacro defslime-start (name mapping)
  `(defun ,name ()
     (interactive)
     (let ((slime-default-lisp ,mapping))
       (slime))))
(defslime-start sbcl 'sbcl)

;; Show trailing whitespace
;;
;; FIXME: source code only
;;
(setq-default show-trailing-whitespace t)

;; Show matching parens
;;
;; FIXME: source code only
;;
(show-paren-mode 1)

;; FIXME: 80 column visual indicator
;; FIXME: source code only
;;

;; Personal Key Bindings
;;
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cd" 'dired)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cr" 'query-replace)



