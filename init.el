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
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
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
(setq package-archives '(("melpa". "http://melpa.milkbox.net/packages/")
				 ("org" . "http://orgmode.org/elpa/")
				 ("marmalade" . "http://marmalade-repo.org/packages/")
				 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
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


;; ======== IVY/SWIPER ========
(use-package counsel
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
            ivy-count-format "%d/%d ")

  (setq ivy-re-builders-alist
	'((t . ivy--regex-ignore-order)))
  :bind
  ("s-f" . swiper)
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
 "TAB" '(switch-to-other-buffer :which-key "prev buffer")

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
  (add-hook 'prog-mode-hook 'global-company-mode)
  (setq company-tooltip-align-annotations t))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete))


;; ======== SPACELINE =========
(use-package all-the-icons)
(use-package spaceline)
(require 'spaceline-config)
(use-package spaceline-all-the-icons
  :after spaceline
  :config (spaceline-all-the-icons-theme))
(setq spaceline-all-the-icons-separator-type 'arrow) 


;; ======== JAVASCRIPT ========
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 
;; better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode) 

(use-package js2-refactor)
(use-package xref-js2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (js2-mode smart-mode-line sublime-themes counsel general evil))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

