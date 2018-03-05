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
;; avoid having to answer "yes" and "no" every time - change to "y" and "n"
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa". "https://melpa.milkbox.net/packages/")
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
(load-user-file "evil-evilified-state.el")


;; ======== WINDOWS SPECIFIC ========
;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)


;; ======== COUNSELIVY/SWIPER ========
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
  (:map ivy-minibuffer-map
	("C-k" . ivy-previous-line)
	("C-j" . ivy-next-line)
	("C-l" . ivy-alt-done)))


;; ======== EVIL MODE ========
(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))
    
;; load evil-evilified-state (from author of spacemacs)
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

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;(global-set-key [escape] 'evil-exit-emacs-state)

;; map fd to escape normal mode
(require 'key-chord) 
(key-chord-mode 1) 
(key-chord-define-global "fd" 'evil-normal-state) 

(use-package evil-surround
  :config
  (global-evil-surround-mode))


;; ======== EVIL-SURROUND ========
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; ======== (EVIL) SMARTPARENS ========
(use-package smartparens)
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(define-key smartparens-mode-map (kbd ")") #'sp-paredit-like-close-round)
(use-package evil-smartparens)

;; ======== EVIL-CLEVERPARENS ========
(use-package evil-cleverparens)
(add-hook 'elisp-mode #'smartparens-mode)
(add-hook 'elisp-mode #'evil-cleverparens-mode)

;; ======== EDITORCONFIG ========
(use-package editorconfig
  :config
  (editorconfig-mode 1))


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

 ;; applications
 "a" '(:ignore t :which-key "Applications")
 "ar" 'ranger
 "ad" 'dired

 ;; buffers
 "b" '(:ignore t :which-key "Buffers")
 "bb" 'ivy-switch-buffer
 "bd" 'kill-this-buffer
 "bn" 'next-buffer
 "bp" 'previous-buffer

 ;; elisp!
 "e" '(:ignore t :which-key "Elisp")
 "eb" 'eval-buffer
 "er" 'eval-region

 ;; files
 "f" '(:ignore t :which-key "Files")
 "ff" 'counsel-find-file
 "fr" 'counsel-recentf
 "fed" 'open-config-file
 "feR" 'reload-config-file
 "fs" 'save-buffer

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
;(use-package all-the-icons)
;(use-package spaceline)
;(require 'spaceline-config)
;(use-package spaceline-all-the-icons
;  :after spaceline
;  :config (spaceline-all-the-icons-theme))
;(setq spaceline-all-the-icons-separator-type 'arrow) 


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
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; indium
;;(use-package indium)


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

;; enable paredit on mode and initialize
(add-hook 'typescript-mode-hook #'smartparens-mode)
(add-hook 'typescript-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'typescript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
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

(add-hook 'php-mode-hook #'smartparens-mode)
(add-hook 'php-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'php-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))


;; ======== WEB MODE ========
(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; ======== HELPER FUNCTIONS ======== 
(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun reload-config-file()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun new-eshell ()
  (interactive)
  (let* ((lines (window-body-height))
         (new-window (split-window-vertically (floor (* 0.7 lines)))))
    (select-window new-window)
    (eshell "eshell"))) 

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
  "Open a new brace or bracket expression, with relevant newlines and indent"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun sp-paredit-like-close-round ()
      "If the next character is a closing character as according to smartparens skip it, otherwise insert `last-input-event'"
      (interactive)
      (let ((pt (point)))
        (if (and (< pt (point-max))
                 (sp--char-is-part-of-closing (buffer-substring-no-properties pt (1+ pt))))
            (forward-char 1)
          (call-interactively #'self-insert-command))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(package-selected-packages
   (quote
    (ac-php company-php php-mode evil-cleverparens evil-smartparens smartparens tide indium js2-mode smart-mode-line sublime-themes counsel general evil)))
 '(sp-highlight-pair-overlay nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

