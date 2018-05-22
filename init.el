;; ======== SANE DEFAULTS ========
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
(setq-default tab-width 2)
;; avoid having to answer "yes" and "no" every time - change to "y" and "n"
(defalias 'yes-or-no-p 'y-or-n-p)
;; keep fringes clean
(setq-default indicate-empty-lines nil)
;; no more ugly line splitting
;(setq-default truncate-lines t)


;; ======== HISTORY ========
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;; ======== PACKAGE SETUP/USE-PACKAGE ========
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


;; ======== LOCAL CONFIG FILES ========
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

;; ======== ACE-WINDOW ========
(use-package ace-window)

;; window hydra
 (defhydra hydra-window ()
   "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		    _q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	  _w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		      _e_ X↑
_l_ →        	_Z_ reset      	_s_wap		      _r_ X→
_F_ollow		_D_lt Other   	  _S_ave	        max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("q" hydra-move-splitter-left)
   ("w" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-move-splitter-right)
   ("b" ivy-switch-buffer)
   ("f" counsel-find-files)
   ("F" follow-mode)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" delete-other-windows)
   ("i" ace-maximize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("SPC" nil)
   )

;; ======== AVY ========
(use-package avy
  :config
  (avy-setup-default))

;; ======== TARGETS.EL ========
;; (use-package targets
;;   :load-path "~/.emacs.d/local-packages/targets.el"
;;   :init
;;   (setq targets-user-text-objects '((pipe "|" nil separator)
;;                                     (paren "(" ")" pair :more-keys "b")
;;                                     (bracket "[" "]" pair :more-keys "r")
;;                                     (curly "{" "}" pair :more-keys "c")))
;;   :config
;;   (targets-setup t
;;                  :inside-key "i"
;;                  :around-key "a"
;;                  :remote-key nil))

;; ======== HYDRA ========
(use-package hydra)

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
 ";" 'comment-dwim ;; comment out lines

 ;; applications
 "a" '(:ignore t :which-key "Applications")
 "ad" 'dired
 "am" 'hydra-macro/body
 "ar" 'ranger
 "ao" '(:ignore t :which-key "Org")
 "aoa" 'org-agenda
 "aob" 'org-switchb
 "aoc" 'org-capture
 "aol" 'org-store-link

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
 "fD" 'delete-current-buffer-file
 "ff" 'counsel-find-file
 "fl" 'counsel-find-library
 "fr" 'counsel-recentf
 "fed" 'open-config-file
 "feR" 'reload-config-file
 "fr" 'rename-current-buffer-file
 "fs" 'save-buffer
 "fS" 'save-all-buffers

 ;; git
 "g" '(:ignore t :which-key "Git")
 "gs" 'magit-status

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
 "jd" 'move-line-down
 "jfb" 'beginning-of-defun
 "jfe" 'end-of-defun
 "jl" 'avy-goto-line
 "jn" 'collapse-next-line
 "jq" 'avy-goto-word-0
 "ju" 'move-line-up
 "jw" 'avy-goto-word-1

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

 "m" 'hydra-by-major-mode

 ;; projectile
 ;; bind p to be the prefix for opening the map of projectile commands
 "p" '(:keymap projectile-command-map :package projectile :which-key "Project")
 
 ;; search
 "s" '(:ignore t :which-key "Search")
 "sg" 'counsel-git-grep

 ;; windows
 "w" '(:ignore t :which-key "Windows")
 "wa" 'hydra-window/body
 "wd" 'evil-window-delete
 "wh" 'evil-window-left
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wl" 'evil-window-right
 "wr" 'rotate-windows
 "w/" 'evil-window-vsplit
 "w-" 'evil-window-split
 )

;; (general-define-key
;;  :states '(normal emacs-lisp-mode-map)
;;  :major-modes '(emacs-lisp-mode t)
;;  :prefix "SPC m"

;;  "e" '(:ignore t :which-key "Eval")
;;  "eb" 'eval-buffer
;;  "er" 'eval-region)

;; (general-define-key
;;  :keymaps 'js2-mode-map
;;  :states 'normal
;;  :prefix "SPC m"

;;  "e" '(:ignore t :which-key "Errors")
;;  "en" 'js2-error-buffer-next
;;  "ep" 'js2-error-buffer-prev)


;; ======== HYDRA ========
(use-package hydra)

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

;; Company Quickhelp
;; adds documentation pop-ups to company-mode
(use-package company-quickhelp)
(company-quickhelp-mode)


;; ======== YASNIPPET ========
(use-package yasnippet
  :diminish yas-minor-mode)
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

(add-hook 'flycheck-mode-hook 'counsel-gtags-mode)

(defun lunaryorn-use-js-executables-from-node-modules ()
  "Set executables of JS checkers from local node modules."
  (-when-let* ((file-name (buffer-file-name))
               (root (locate-dominating-file file-name "node_modules"))
               (module-directory (expand-file-name "node_modules" root)))
    (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                           (javascript-eslint . "eslint")
                                           (javascript-jscs   . "jscs")))
      (let ((package-directory (expand-file-name module module-directory))
            (executable-var (flycheck-checker-executable-variable checker)))
        (when (file-directory-p package-directory)
          (set (make-local-variable executable-var)
               (expand-file-name (concat "bin/" module ".js")
package-directory)))))))

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
   :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
   :hint nil)
  "Errors"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

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
                                  (powerline-raw "%l:%c" 'face1 'r)
                                  " | "
                                  (powerline-raw "%6p" 'face1 'r)
                                  (powerline-hud 'highlight 'region 1)
                                  " "
                                  ))
                            )
                       (concat
                        (powerline-render lhs)
                        ;; changed bg color - variables changed from face1/face2 to bar-color 
                        (powerline-fill-center bar-color (/ (powerline-width center) 1.0)) ;;changed from 2.0 - seems to center better on yoga3
                        (powerline-render center)
                        (powerline-fill bar-color (powerline-width rhs))
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
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'c-mode-hook 'flycheck-mode)

;; auto-insert include guards in header files
;; autoinsert C/C++ header
(define-auto-insert
    (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "My C/C++ header")
    '(nil
      (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
                 (nopath (file-name-nondirectory noext))
                 (ident (concat (upcase nopath) "_H_")))
        (concat "#ifndef " ident "\n"
                        "#define " ident "\n\n\n"
                        "\n\n#endif // " ident "\n"))
      ))

(add-hook 'find-file-hook 'auto-insert)

(add-hook 'c-mode-hook (lambda()
                         (setq company-backends '(company-clang company-dabbrev-code company-keywords company-yasnippet company-files company-dabbrev))
                         (company-mode 1)
(global-set-key [C-return] 'company-complete-common)))

; Don't ask to reload TAGS if newer, just do it
(setq tags-revert-without-query 1)

(defhydra hydra-c (:color red
                   :hint nil)
"
_f_ flycheck
"
("f" hydra-flycheck/body :exit t)
)

;; ======== JAVASCRIPT ========
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode)) 
;; better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode) 
(setq js2-highlight-level 3)

(use-package js2-refactor
  :defer t)
(js2r-add-keybindings-with-prefix "C-c C-m")
(define-key js2-mode-map (kbd "C-c r") #'js2-refactor-hydra/body)
;; (use-package js2-refactor
;;   :defer t
;;   :commands (js2r-add-keybindings-with-prefix)
;;   :init (after :js2-mode
;;           (js2r-add-keybindings-with-prefix "SPC m r")
;;           (add-hook 'js2-mode-hook 'js2-refactor-mode)))
(use-package xref-js2)
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; add-node-modules-path retrives binaries from node_modules for things like eslint
(use-package add-node-modules-path)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

;; setup mode hooks
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook 'add-node-modules-path)

;; indium
(use-package indium
  :defer t)

;; skewer
(use-package skewer-mode
  :defer t)

;; spacemacs skewer functions

(defun spacemacs/skewer-start-repl ()
  "Attach a browser to Emacs and start a skewer REPL."
  (interactive)
  (run-skewer)
  (skewer-repl))

(defun spacemacs/skewer-load-buffer-and-focus ()
  "Execute whole buffer in browser and switch to REPL in insert state."
  (interactive)
  (skewer-load-buffer)
  (skewer-repl)
  (evil-insert-state))

(defun spacemacs/skewer-eval-defun-and-focus ()
  "Execute function at point in browser and switch to REPL in insert state."
  (interactive)
  (skewer-eval-defun)
  (skewer-repl)
  (evil-insert-state))

(defun spacemacs/skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browser."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

(defun spacemacs/skewer-eval-region-and-focus (beg end)
  "Execute the region in browser and swith to REPL in insert state."
  (interactive "r")
  (spacemacs/skewer-eval-region beg end)
  (skewer-repl)
  (evil-insert-state))

;; js2-refactor hydra
(defhydra js2-refactor-hydra (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_z_] return                   [_q_]  quit"
    ("ee" js2r-expand-node-at-point)
("cc" js2r-contract-node-at-point)
("ef" js2r-extract-function)
("em" js2r-extract-method)
("tf" js2r-toggle-function-expression-and-declaration)
("ta" js2r-toggle-arrow-function-and-expression)
("ip" js2r-introduce-parameter)
("lp" js2r-localize-parameter)
("wi" js2r-wrap-buffer-in-iife)
("ig" js2r-inject-global-in-iife)
("ag" js2r-add-to-globals-annotation)
("ev" js2r-extract-var)
("iv" js2r-inline-var)
("rv" js2r-rename-var)
("vt" js2r-var-to-this)
("ao" js2r-arguments-to-object)
("ti" js2r-ternary-to-if)
("sv" js2r-split-var-declaration)
("ss" js2r-split-string)
("uw" js2r-unwrap)
("lt" js2r-log-this)
("dt" js2r-debug-this)
("sl" js2r-forward-slurp)
("ba" js2r-forward-barf)
("k" js2r-kill)
("q" nil)
("z" hydra-javascript/body)
)

(defhydra hydra-javascript (:color red
                            :hint nil)
"
_'_ → REPL     _f_ → flycheck     _in_ → indium node      _r_ → refactor
_j_ jump (imenu)                  _ic_ → indium chrome
"
("'" spacemacs/skewer-start-repl :exit t)
("f" hydra-flycheck/body :exit t)
("in" indium-run-node :exit t)
("ic" indium-connect-to-chrome :exit t)
("j" counsel-imenu :exit t)
("r" js2-refactor-hydra/body :exit t)
)

;; ======== TYPESCRIPT ========
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

;; add completion details
(setq tide-completion-detailed t)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; configure smartparens
(add-hook 'typescript-mode-hook #'smartparens-mode)
(add-hook 'typescript-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'typescript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
;; run required configuration function for tide
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook 'add-node-modules-path)

(defhydra hydra-typescript (:color red
                                   :hint nil)
"
_d_ → doc at point      _f_ → format       _r_ → refactor
_s_ → list references   _a_ → apply fix    _j_ → jsdoc comment
_i_ → organize imports
"
  ("d" tide-documentation-at-point :exit t)
  ("s" tide-references :exit t)
  ("f" tide-format)
  ("a" tide-fix)
  ("r" tide-refactor)
  ("j" tide-jsdoc-template)
  ("i" tide-organize-imports) 
 )

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

(add-hook 'web-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-web-html))
                          (company-mode t)))
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'evil-smartparens-mode)


;; ======== SQL IDE ========
;; windows only - fix to make emacs+mysql work
;; (setq sql-mysql-options '("-C" "-t" "-f" "-n"))
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
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map "\C-x\C-e" 'pp-eval-last-sexp)))

;; testing - suggest.el https://github.com/Wilfred/suggest.el
(use-package suggest)

;; eldoc - provides minibuffer hints when working in elisp
(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(defhydra hydra-elisp (:color red
                       :hint nil)
"
_eb_ eval buffer
_er_ eval region
_es_ eval sexp
_f_ flycheck
"
("eb" eval-buffer)
("er" eval-region)
("es" pp-eval-last-sexp)
("f" hydra-flycheck/body :exit t)
)

;; ======== COMMON LISP ========
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'auto-mode-alist '("\\.lisp\\'\\|\\.cl\\'" . lisp-mode)) 

(add-hook 'lisp-mode-hook (lambda ()
                            (slime-mode t)
                            (use-package slime-company)
                            (company-mode t)))

;(setq slime-contribs '(slime-fancy slime-company))
(slime-setup '(slime-fancy slime-company))

(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'lisp-mode-hook #'highlight-quoted-mode)


;; ======== RACKET ========
(use-package racket-mode)
(use-package quack)
(use-package scribble-mode)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)) 

;; setup smartparens and cleverparens
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'racket-mode-hook #'smartparens-strict-mode)
(add-hook 'racket-mode-hook #'evil-cleverparens-mode)
(add-hook 'racket-mode-hook #'highlight-quoted-mode)


;; ======== COMMON LISP ========
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")


;; ======== MAGIT ========
(use-package evil-magit)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)


;; ======== HELPFUL ========
(use-package helpful)


;; ======== UNDO TREE ========
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))


;; ======== MARKDOWN ========
(when (use-package markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\.html\\'" . markdown-mode))
  (after-load 'whitespace-cleanup-mode
(push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

(defhydra hydra-markdown (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item   

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
 
"


  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim) 
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)  

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue) 
)


;; ======== ORG-MODE ========
(use-package org)
(setq org-M-RET-may-split-line nil)

;; use uuid's for org links
;; (require org-id)
;; (setq org-id-link-to-org-use-id 'create-if-interactive)

;; beautify org-mode
;(use-package'org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))
(setq org-ellipsis "⤵")

(setq org-default-notes-file "~/org/tasks.org")
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %u\n  %a")
        ("n" "Note/Data" entry (file+headline "inbox.org" "Notes/Data")
         "* %?   \n  %i\n  %u\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("J" "Work-Journal" entry (file+datetree "~/org/wjournal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ))
(setq org-irc-link-to-logs t)

;; note time when something is marked as complete
(setq org-log-done 'time)

;; configure which files to use in org-agenda
 (setq org-agenda-files (list "~/org/inbox.org"
                               "~/org/email.org"
                               "~/org/tasks.org"
                               "~/org/wtasks.org"
                               "~/org/journal.org"
                               "~/org/wjournal.org"
                               "~/org/kb.org"
                               "~/org/wkb.org"
  ))
  (setq org-agenda-text-search-extra-files
        (list "~/org/someday.org"
              "~/org/config.org"
  ))

  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)
                             ("~/org/someday.org" :maxlevel . 2)
                             ("~/org/templates.org" :maxlevel . 2)
                             )
        )
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)

;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))


;; ======== EDITOR MACROS ========
(defhydra hydra-macro (:hint nil :color pink :pre 
                             (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register     
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))

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

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  "C-d on empty line in shell kills process. C-d again kills shell."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

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

(defun hydra-by-major-mode ()
  "Selects specific hydra to be used based on major mode."
  (interactive)
  (cl-case major-mode
    ;; major modes with their associated hydras
    (emacs-lisp-mode
     (hydra-elisp/body))
    (c-mode
     (hydra-c/body))
    (js2-mode
     (hydra-javascript/body))
    (typescript-mode
     (hydra-typescript/body))
    (markdown-mode
     (hydra-markdown/body))
    (org-agenda-mode
     (hydra-org-agenda/body))
    (t
     (error "%S not supported" major-mode))))

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

(defun collapse-next-line ()
  "Pulls next line up to current."
  (interactive)
  (join-line -1))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(defun rotate-windows ()
  "Switch buffers in two windows, preserving layout."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun my/describe-random-interactive-function ()
  (interactive)
  "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
  (let (result)
    (mapatoms
     (lambda (s)
       (when (and (commandp s) 
                  (documentation s t)
                  (null (get s 'byte-obsolete-info)))
         (setq result (cons s result)))))
    (describe-function (elt result (random (length result))))))


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
    (skewer-mode skewer markdown-mode hydra org-bullets slime magit nord-theme eyebrowse evil-collection solarized-theme evil-magit ac-php company-php php-mode evil-cleverparens evil-smartparens smartparens tide indium js2-mode smart-mode-line sublime-themes counsel general evil)))
 '(sp-highlight-pair-overlay nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

