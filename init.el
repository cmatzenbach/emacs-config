;; sane defaults
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq fill-column 80)		; toggle wrapping text at the 80th character
(menu-bar-mode -1)                      ; disable menu bar
(setq initial-scratch-message "Welcome to Emacs") ; print a default message in the empty scratch buffer opened at startup
(setq user-full-name "Chris Matzenbach"
      user-mail-address "cmatzenbach@gmail.com")
(global-linum-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)     ; use spaces instead of tabs
;; avoid having to answer "yes" and "no" every time - change to "y" and "n"
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa". "http://melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; update package archives
  (package-install 'use-package)) ; install most recent version of use-package
(require 'use-package)
(setq use-package-always-ensure t)

(defconst user-config-dir "~/.emacs.d/config/")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-config-dir)))

;; load custom config modules
(load-user-file "appearance.el")
;; load manually installed local packages
(add-to-list 'load-path "~/.emacs.d/local-packages/")
(load "let-alist-1.0.5.el")
(load "highlight-escape-sequences.el")
(load "highlight-quoted.el")


;; ======== WINDOWS SPECIFIC ========
;; Set default font
;(set-face-attribute 'default nil
;                    :family "Source Code Pro"
;                    :height 108
;                    :weight 'normal
;                    :width 'normal)
;(load-theme 'challenger-deep t)


;; ======== COUNSEL/IVY/SWIPER ========
(use-package counsel
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
            ivy-count-format "%d/%d ")

  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  :bind
  ("C-s" . swiper)
  ; https://www.reddit.com/r/emacs/comments/6i8rmb/noob_change_ctrlnp_to_vimlike_binding_for_ivy_and/
  (:map ivy-minibuffer-map
	("C-h" . evil-delete-char) ; supposed to be (kbd "DEL")
	("C-k" . ivy-previous-line)
	("C-j" . ivy-next-line)
	("C-l" . ivy-alt-done)))


;; ======== EVIL MODE ========
(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))
    
;; evil-collection - WIP package to create evil keybindings for missing modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; load evil-evilified-state (from author of spacemacs)
(load-user-file "evil-evilified-state.el")
(require 'evil-evilified-state)

;; use esc to exit minibuffer
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit))) 

;; make esc get me out of different situations
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;(global-set-key [escape] 'evil-exit-emacs-state)

;; set c-u to have vim-like behavior (scroll up half page)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

;; map fd to escape normal mode
(require 'key-chord) 
(key-chord-mode 1) 
(key-chord-define-global "fd" 'evil-normal-state) 

(use-package evil-surround
  :config
  (global-evil-surround-mode))
;; or does this ^ need a t after it? global-evil-surround-mode t


;; ======== INDENTATION/BRACKET MANAGEMENT ========
;; evil-smartparens
(use-package smartparens)
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(define-key smartparens-mode-map (kbd ")") #'sp-paredit-like-close-round)
(use-package evil-smartparens)

;; evil-cleverparens
(use-package evil-cleverparens)

;; make emacs recognize .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; rainbow delimiters, a godsend for lisps and shitty es5 callback stacks from hell
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ======== SUBLIMITY ========
(use-package sublimity)
;; smooth scrolling
(require 'sublimity-scroll)
(sublimity-mode 1)

;; ======== PERSPECTIVE ========
(use-package persp-mode)
(persp-mode)

;; ======== EYEBROWSE ========
(use-package eyebrowse
  :init
  (eyebrowse-mode))

;; ======== AVY ========
(use-package avy
  :config
  (avy-setup-default))

;; ======== TARGETS.EL ========
(use-package targets
  :load-path "~/.emacs.d/local-packages/targets.el"
  :init
  (setq targets-user-text-objects '((pipe "|" nil separator)
                                    (paren "(" ")" pair :more-keys "b")
                                    (bracket "[" "]" pair :more-keys "r")
                                    (curly "{" "}" pair :more-keys "c")))
  :config
  (targets-setup t
                 :inside-key "i"
                 :around-key "a"
                 :remote-key nil))

;; ======== WHICH-KEY && GENERAL ========
(use-package which-key :config (which-key-mode 1))
(setq which-key-idle-delay 0.3) 
(use-package general)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "SPC" 'counsel-M-x
 "'" 'new-eshell
 ;"?" '(eshell- -goto-filedir-or-home :which-key "iterm - goto dir")
 "TAB" 'switch-to-previous-buffer
 "M-x" 'counsel-M-x ;; gives M-x command counsel features

 ;; applications
 "a" '(:ignore t :which-key "Applications")
 "ar" 'ranger
 "ad" 'dired

 ;; buffers
 "b" '(:ignore t :which-key "Buffers")
 "bb" 'ivy-switch-buffer
 "bd" 'kill-this-buffer
 "bD" 'kill-buffer-and-window
 "bn" 'next-buffer
 "bp" 'previous-buffer

 ;; elisp!
 "e" '(:ignore t :which-key "Elisp")
 "eb" 'eval-buffer
 "er" 'eval-region

 ;; files
 "f" '(:ignore t :which-key "Files")
 "ff" 'counsel-find-file
 "fl" 'counsel-find-library
 "fr" 'counsel-recentf
 "fed" 'open-config-file
 "feR" 'reload-config-file
 "fs" 'save-buffer
 "fS" 'save-all-buffers

 ;; git
 "g" '(:ignore t :which-key "Git")
 "gs" 'magit

 ;; help
 "h" '(:ignore t :which-key "Help")
 "hc" 'helpful-command
 "hf" 'helpful-callable
 "hk" 'helpful-describe-key
 "hm" 'helpful-macro
 "hp" 'helpful-at-point
 "hv" 'helpful-variable

 ;; insert
 "i" '(:ignore t :which-key "Insert")
 "iu" 'counsel-unicode-char

 ;; jump
 "j" '(:ignore t :which-key "Jump")
 "jc" 'avy-goto-char-timer
 "jl" 'avy-goto-line
 "jq" 'avy-goto-word-1
 "jw" 'avy-goto-word-0

 ;; perspective
 "l" '(:keymap persp-key-map :package persp-mode :which-key "Layout")
 "l0" 'eyebrowse-switch-to-window-config-0
 "l1" 'eyebrowse-switch-to-window-config-1
 "l2" 'eyebrowse-switch-to-window-config-2
 "l3" 'eyebrowse-switch-to-window-config-3
 "l4" 'eyebrowse-switch-to-window-config-4
 "l5" 'eyebrowse-switch-to-window-config-5
 "l6" 'eyebrowse-switch-to-window-config-6
 "l7" 'eyebrowse-switch-to-window-config-7
 "l8" 'eyebrowse-switch-to-window-config-8
 "l9" 'eyebrowse-switch-to-window-config-9
 
 ;; projectile
 ;; bind p to be the prefix for opening the map of projectile commands
 "p" '(:keymap projectile-command-map :package projectile :which-key "Project")
 
 ;; search
 "s" '(:ignore t :which-key "Search")
 "sg" 'counsel-git-grep

 ;; windows
 "w" '(:ignore t :which-key "Windows")
 "wd" 'evil-window-delete
 "wh" 'evil-window-left
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wl" 'evil-window-right
 "w/" 'evil-window-vsplit
 "w-" 'evil-window-split
 ) 


;; ======== COMPANY ========
(use-package company
  :config
  :init (add-hook 'prog-mode-hook 'global-company-mode)
  (setq company-tooltip-align-annotations t
	company-minimum-prefix-length 2
	company-idle-delay 0.1))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete))

;; Company Tern
(use-package company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (company-mode)))
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)


;; ======== YASNIPPET ========
(use-package yasnippet)
(yas-global-mode 1)
;; variable used in helper function to embed yas suggestions in company completion window
(defvar company-mode/enable-yas t)
;; snippet sources
(use-package yasnippet-snippets)
(use-package php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/config/Create-PHP-YASnippet.php")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)


;; ======== FLYCHECK ========
(use-package flycheck
  :init (global-flycheck-mode))


;; ======== GGTAGS ========
;(use-package ggtags)
;(add-hook 'c-mode-common-hook
;	  (lambda ()
;	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;	      (ggtags-mode 1))))


;; ======== COUNSEL GTAGS ========
(use-package counsel-gtags)
(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))


;; ======== SPACELINE =========
(use-package all-the-icons)
;; fix for lag on windows when using all-the-icons
(when (string-equal system-type "windows-nt") (setq inhibit-compacting-font-caches t))
;(use-package spaceline)
;(require 'spaceline-config)
;(use-package spaceline-all-the-icons
;  :after spaceline
;  :config (spaceline-all-the-icons-theme))
;(setq spaceline-all-the-icons-separator-type 'arrow) 
  (use-package powerline
    :ensure t
    :config

    (defun make-rect (color height width)
      "Create an XPM bitmap."
      (when window-system
        (propertize
         " " 'display
         (let ((data nil)
               (i 0))
           (setq data (make-list height (make-list width 1)))
           (pl/make-xpm "percent" color color (reverse data))))))


    (defun powerline-mode-icon ()
      (let ((icon (all-the-icons-icon-for-buffer)))
        (unless (symbolp icon) ;; This implies it's the major mode
          (format " %s"
                  (propertize icon
                              'help-echo (format "Major-mode: `%s`" major-mode)
                              'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))


    (setq-default mode-line-format 
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (modified (buffer-modified-p))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'powerline-active2 'powerline-inactive2))
                            (bar-color (cond ((and active modified) (face-foreground 'error))
                                             (active (face-background 'cursor))
                                             (t (face-background 'tooltip))))
                            (lhs (list
                                  (make-rect bar-color 30 3)
                                  (when modified
                                    (concat
                                     " "
                                     (all-the-icons-faicon "floppy-o"
                                                           :face (when active 'error)
                                                           :v-adjust -0.01)))
                                  " "
                                  (powerline-buffer-id)
                                  ))
                            (center (list
                                     " "
                                     (powerline-mode-icon)
                                     " "
                                     (powerline-major-mode)
                                     " "))
                            (rhs (list
                                  (format "%s" (eyebrowse--get 'current-slot))
                                  " | "
                                  (powerline-raw "%l:%c" 'mode-line 'r)
                                  " | "
                                  (powerline-raw "%6p" 'mode-line 'r)
                                  (powerline-hud 'highlight 'region 1)
                                  " "
                                  ))
                            )
                       (concat
                        (powerline-render lhs)
                        (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                        (powerline-render center)
                        (powerline-fill face2 (powerline-width rhs))
                        (powerline-render rhs))))))
    )

;; ======== PROJECTILE ========
(use-package projectile)
(use-package counsel-projectile)
(counsel-projectile-mode)
(setq projectile-enable-caching t)
;; overwrite default projectile functions with counsel-projectile alternatives
(define-key projectile-mode-map (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
(define-key projectile-mode-map (kbd "C-c p b") 'counsel-projectile-switch-to-buffer)
(define-key projectile-mode-map (kbd "C-c p d") 'counsel-projectile-find-dir)
;; currently overwriting projectile-find-file-in-directory (seems pointless)
(define-key projectile-mode-map (kbd "C-c p l") 'counsel-projectile)
(define-key projectile-mode-map (kbd "C-c p f") 'counsel-projectile-find-file)
(define-key projectile-mode-map (kbd "C-c p p") 'counsel-projectile-switch-project)
(define-key projectile-mode-map (kbd "C-c p s g") 'counsel-projectile-grep)
(define-key projectile-mode-map (kbd "C-c p s s") 'counsel-projectile-ag)


;; ======== C-MODE ========
(add-hook 'c-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))


;; ======== JAVASCRIPT ========
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 
;; better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode) 
(setq js2-highlight-level 3)

(use-package js2-refactor)
(use-package xref-js2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

;; setup smartparens and flycheck
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; indium
(use-package indium)


;; ======== TYPESCRIPT ==============
(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)) 

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; configure smartparens
(add-hook 'typescript-mode-hook #'smartparens-mode)
(add-hook 'typescript-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'typescript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
;; run required configuration function for tide
(add-hook 'typescript-mode-hook #'setup-tide-mode)


;; ======== PHP ========
(use-package php-mode)
(use-package company-php)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook
          '(lambda ()
             (require 'company-php)
             (company-mode t)
             (ac-php-core-eldoc-setup) ;; enable eldoc
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend)))

;; configure smartparens
(add-hook 'php-mode-hook #'smartparens-mode)
(add-hook 'php-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'php-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;; tool to test HTTP REST webservices
(use-package restclient)


;; ======== WEB MODE ========
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; ======== SQL IDE ========
;; windows only - fix to make emacs+mysql work
(setq sql-mysql-options '("-C" "-t" "-f" "-n"))
;; set up connection list
(setq sql-connection-alist
      '((casesdev (sql-product 'mysql)
                 (sql-port 3306)
                 (sql-server "dev.cluster-cpqhbit12kdd.us-east-1.rds.amazonaws.com")
                 (sql-user "rsnards")
                 (sql-password "RSNA1915")
                 (sql-database "case_repository"))
        (casesstage (sql-product 'mysql)
                    (sql-port 3306)
                    (sql-server "stage.cluster-cpqhbit12kdd.us-east-1.rds.amazonaws.com")
                    (sql-user "rsnards")
                    (sql-password "RSNA1915")
                    (sql-database "case_repository"))))

(defun mysql-cases-dev ()
  "Connect to casesdev database."
  (interactive)
  (my-sql-connect 'mysql 'casesdev))

(defun mysql-cases-stage ()
  "Connect to casesstage database."
  (interactive)
  (my-sql-connect 'mysql 'casesstage))

(defun my-sql-connect (product connection)
  "Connect to custom-defined databases."
  ;; remember to set sql product, otherwise it will fail for the first time you call the function
  (setq sql-product product)
  (sql-connect connection))


;; ======== ELISP ========
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
;; minor mode for highlighting lisp quotes and quoted symbols (locally installed package)
(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

;; testing - suggest.el https://github.com/Wilfred/suggest.el
(use-package suggest)


;; ======== RACKET ========
(use-package racket-mode)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)) 
(use-package quack)
(use-package scribble-mode)

;; setup smartparens and cleverparens
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'racket-mode-hook #'smartparens-strict-mode)
(add-hook 'racket-mode-hook #'evil-cleverparens-mode)
(add-hook 'racket-mode-hook #'highlight-quoted-mode)

;; ======== MAGIT ========
(use-package evil-magit)


;; ======== HELPFUL ========
(use-package helpful)


;; ======== THEMES/COLOR MODS ========
;; theme packages
;(use-package kaolin-themes)
;(use-package afternoon-theme)
;(use-package gruvbox-theme)
;(use-package nord-theme)
;(use-package oceanic-theme)
;(use-package liso-theme)
;(use-package challenger-deep-theme)
;(use-package creamsody-theme)
;(use-package material-theme)

;; additional syntax highlighting
;; highlights elisp symbols
(use-package highlight-defined)
;; highlights numbers
(use-package highlight-numbers)
;; activate highlight-escape-sequences mode (locally installed package)
(hes-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


;; ======== HELPER FUNCTIONS ======== 
(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-config-file()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun new-eshell ()
  "Create a new instance of eshell in a window split."
  (interactive)
  (let* ((lines (window-body-height))
         (new-window (split-window-vertically (floor (* 0.7 lines)))))
    (select-window new-window)
    (eshell "eshell"))) 

(defun kill-buffer-and-window ()
  "Kill buffer and window."
  (interactive)
  (kill-this-buffer)
  (evil-window-delete))

(defun enable-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1)) 
(sp-local-pair 'c++-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer. Repeated invocations toggle between two most recently opened buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun sp-paredit-like-close-round ()
      "If the next character is a closing character as according to smartparens skip it, otherwise insert `last-input-event'."
      (interactive)
      (let ((pt (point)))
        (if (and (< pt (point-max))
                 (sp--char-is-part-of-closing (buffer-substring-no-properties pt (1+ pt))))
            (forward-char 1)
          (call-interactively #'self-insert-command))))

(defun company-mode/backend-with-yas (backend)
  "Integrate yas suggestions into company completion window."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
;; call function to add to company backends
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(defun save-all-buffers ()
  "Save all open buffers."
  (interactive)
  (save-some-buffers t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("dcb9fd142d390bb289fee1d1bb49cb67ab7422cd46baddf11f5c9b7ff756f64c" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "7f6796a9b925f727bbe1781dc65f7f23c0aa4d4dc19613aa3cf96e41a96651e4" "50b66fad333100cc645a27ada899a7b1d44f1ceb32140ab8e88fedabfb7d0daf" "fec6c786b1d3088091715772839ac6051ed972b17991af04b50e9285a98c7463" "8ad35d6c2b35eacc328b732f0a4fe263abd96443a5075aa53b8535a9e8cb7eaf" "9a58c408a001318ce9b4eab64c620c8e8ebd55d4c52327e354f24d298fb6978f" "a9d2ed6e4266ea7f8c1f4a0d1af34a6282ad6ff91754bee5ec7c3b260ec721f4" "293b55c588c56fe062afe4b7a3a4b023712a26d26dc69ee89c347b30283a72eb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(linum-format " %5i ")
 '(package-selected-packages
   (quote
    (nord-theme eyebrowse evil-collection solarized-theme evil-magit ac-php company-php php-mode evil-cleverparens evil-smartparens smartparens tide indium js2-mode smart-mode-line sublime-themes counsel general evil)))
 '(sp-highlight-pair-overlay nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

