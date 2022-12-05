;; ======== TEMP BUGFIX FOR EMACS 28 MACRO CHANGE ========
(defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.
This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:
  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")
This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.
If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).
For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(defmacro define-obsolete-face-alias (obsolete-face current-face &optional when)
  "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
WHEN should be a string indicating when the face was first made
obsolete, for example a date or a release number."
  `(progn (put ,obsolete-face 'face-alias ,current-face)
          (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

(defmacro define-obsolete-function-alias (obsolete-name current-name &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.
\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")
is equivalent to the following two lines of code:
\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")
WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.
See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4))
  `(progn (defalias ,obsolete-name ,current-name ,docstring)
          (make-obsolete ,obsolete-name ,current-name ,when)))


;; ======== SANE DEFAULTS ========
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))) ;; transform backups file name
(setq inhibit-startup-screen t)	;; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)	;; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)	;; use utf-8 by default(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)  ;; sentence SHOULD end with only a point.
(setq fill-column 80)  ;; toggle wrapping text at the 80th character
(menu-bar-mode -1) ;; disable menu bar
(setq initial-scratch-message "Welcome to Emacs") ;; print a default message in the empty scratch buffer opened at startup
(setq user-full-name "Chris Matzenbach"
      user-mail-address "matzy@proton.me")
(global-linum-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)  ;; use spaces instead of tabs
(setq-default tab-width 2)
;; avoid having to answer "yes" and "no" every time - change to "y" and "n"
(defalias 'yes-or-no-p 'y-or-n-p)
;; keep fringes clean
(setq-default indicate-empty-lines nil)
;; no more ugly line splitting
;;(setq-default truncate-lines t)
;; For MacOS - Add these to the PATH so that proper executables are found
(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/texbin")))
(setq exec-path (append exec-path '("/usr/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

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
                         ;; melpa recommends not to use melpa-stable unless you have to
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))
(package-initialize)

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; update package archives
  (package-install 'use-package)) ; install most recent version of use-package
(require 'use-package)
;; always download packages if not already installed
(setq use-package-always-ensure t)
;; TESTING enable imenu support for use-package
;; (setq use-package-enable-imenu-support t)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)


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
;; (add-to-list 'load-path "~/.emacs.d/config/")
(load "let-alist-1.0.5.el")
(load "highlight-escape-sequences.el")
(load "highlight-quoted.el")
;; (load "evil-evilified-state.el")

;; additional syntax highlighting
;; highlights elisp symbols
(use-package highlight-defined)
;; highlights numbers
(use-package highlight-numbers)
;; activate highlight-escape-sequences mode (locally installed package)
(hes-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


;; ======== RANDOM/GENERAL PACKAGES ========
(use-package sudo-edit
  :ensure t)
(use-package page-break-lines)
;; customized starting page dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 8)
                        ;; (agenda . 5)
                        ;; (registers . 5)
                        ))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))


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
  ;; https://www.reddit.com/r/emacs/comments/6i8rmb/noob_change_ctrlnp_to_vimlike_binding_for_ivy_and/
  (:map ivy-minibuffer-map
        ("C-h" . evil-delete-char) ; supposed to be (kbd "DEL")
        ("C-k" . ivy-previous-line)
        ("C-j" . ivy-next-line)
        ("C-l" . ivy-alt-done)))

(use-package ivy-hydra
  :after ivy-mode)


;; ======== EVIL MODE ========
(use-package evil
  :ensure t
  :diminish evil-mode
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

;; evil-collection - WIP package to create evil keybindings for missing modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; load evil-evilified-state (from author of spacemacs)
;; (load-user-file "evil-evilified-state.el")
;; (require 'evil-evilified-state)

;; ;; use esc to exit minibuffer
;; (defun minibuffer-keyboard-quit ()
;;   "Abort recursive edit.
;;   In Delete Selection mode, if the mark is active, just deactivate it;
;;   then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;   (interactive)
;;   (if (and delete-selection-mode transient-mark-mode mark-active)
;;       (setq deactivate-mark  t)
;;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;     (abort-recursive-edit))) 

;; ;; make esc get me out of different situations
;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)
;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
                                        ;(global-set-key [escape] 'evil-exit-emacs-state)

(require 'evil-collection-minibuffer)

;; set c-u to have vim-like behavior (scroll up half page)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-insert-state-map (kbd "C-u")
;;   (lambda ()
;;     (interactive)
;;     (evil-delete (point-at-bol) (point))))

;; map fd to escape normal mode
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "fd" 'evil-normal-state)) 
 
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
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ======== SUBLIMITY ========
(use-package sublimity)
;; smooth scrolling
(require 'sublimity-scroll)
(sublimity-mode 1)

;; ======== PERSPECTIVE ========
;; (use-package persp-mode)
;; (persp-mode)

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


;; ======== WHICH-KEY && GENERAL ========
(use-package which-key
  :ensure t
  :config (which-key-mode 1))
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
 "ar" 'start-restclient
 "au" 'counsel-unicode-char

 ;; buffers
 "b" '(:ignore t :which-key "Buffers")
 "bb" 'ivy-switch-buffer
 "bd" 'kill-this-buffer
 "bD" 'kill-buffer-and-window
 "bm" 'hydra-buffer-menu/body
 "bn" 'next-buffer
 "bp" 'previous-buffer

 ;; elisp!
 "e" '(:ignore t :which-key "Elisp")
 "eb" 'eval-buffer
 "eeh" 'hydra-edebug/body
 "er" 'eval-region
 "ef" 'hydra-flycheck/body
 "es" 'symex-mode-interface

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
 "ha" 'hydra-apropos/body
 "hc" 'helpful-command
 "hf" 'helpful-callable
 "hk" 'helpful-describe-key
 "hm" 'helpful-macro
 "hp" 'helpful-at-point
 "hv" 'helpful-variable

 ;; ivy/counsel
 "i" '(hydra-ivy/body :which-key "Ivy")

 ;; jump
 "j" '(:ignore t :which-key "Jump")
 "jc" 'avy-goto-char-timer
 "jd" 'move-line-down
 "jfb" 'beginning-of-defun
 "jfe" 'end-of-defun
 "ji" 'counsel-imenu
 "jl" 'avy-goto-line
 "jn" 'collapse-next-line
 "jq" 'avy-goto-word-0
 "ju" 'move-line-up
 "jw" 'avy-goto-word-1

 ;; perspective
 ;; "l" '(:keymap persp-key-map :package persp-mode :which-key "Layout")
 "l" '(:ignore t :which-key "Layout")
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

 ;; org
 "o" '(:ignore t :which-key "Org")
 "oa" 'org-agenda
 "ob" 'org-switchb
 "oc" 'org-capture
 "ol" 'org-store-link

 ;; projectile
 ;; bind p to be the prefix for opening the map of projectile commands
 "p" '(:keymap projectile-command-map :package projectile :which-key "Project")
 
 ;; search
 "s" '(:ignore t :which-key "Search")
 "sa" 'counsel-ag
 "sg" 'counsel-git-grep

 ;; windows
 "w" '(:ignore t :which-key "Windows")
 "wd" 'evil-window-delete
 "wh" 'evil-window-left
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wl" 'evil-window-right
 "wm" 'hydra-window/body
 "wr" 'rotate-windows
 "w/" 'evil-window-vsplit
 "w-" 'evil-window-split
 )


;; ======== HYDRA ========
(use-package hydra
  :ensure t
  :defer t)


;; ======== GENERAL HYDRAS ========
(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only^^    *  /\\---/\\
_~_: modified      ^ ^                ^ ^                ^^                                 ~~   ~~
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(defhydra hydra-apropos (:color blue
                         :hint nil)
  "
_a_propos        _c_ommand
_d_ocumentation  _l_ibrary
_v_ariable       _u_ser-option
^ ^          valu_e_"
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))


;; ======== COMPANY ========
(use-package company
  :init
  (add-hook 'prog-mode-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay 0.0))

;; trigger company anytime in insert mode with C-l
(define-key evil-insert-state-map (kbd "C-l") #'company-complete)
;; recenter-top-bottom was originally mapped to C-l, change to C-;
(global-set-key (kbd "C-;") 'recenter-top-bottom)
;; use C-j and C-k to navigate through completion menu, C-l to complete
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  (define-key company-active-map (kbd "C-l") #'company-complete))

;; Company Box (pretty front-end with icons)
(use-package company-box
  :hook (company-mode . company-box-mode))
;; (add-to-list 'load-path "~/.local/share/icons-in-terminal/icons-in-terminal.el")
;; (setq company-box-icons-unknown 'fa_question_circle)

;; (setq company-box-icons-elisp
;;    '((fa_tag :face font-lock-function-name-face) ;; Function
;;      (fa_cog :face font-lock-variable-name-face) ;; Variable
;;      (fa_cube :face font-lock-constant-face) ;; Feature
;;      (md_color_lens :face font-lock-doc-face))) ;; Face

;; (setq company-box-icons-yasnippet 'fa_bookmark)

;; (setq company-box-icons-lsp
;;       '((1 . fa_text_height) ;; Text
;;         (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
;;         (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
;;         (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
;;         (5 . (fa_cog :foreground "#FF9800")) ;; Field
;;         (6 . (fa_cog :foreground "#FF9800")) ;; Variable
;;         (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
;;         (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
;;         (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
;;         (10 . (fa_cog :foreground "#FF9800")) ;; Property
;;         (11 . md_settings_system_daydream) ;; Unit
;;         (12 . (fa_cog :foreground "#FF9800")) ;; Value
;;         (13 . (md_storage :face font-lock-type-face)) ;; Enum
;;         (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
;;         (15 . md_closed_caption) ;; Snippet
;;         (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
;;         (17 . fa_file_text_o) ;; File
;;         (18 . md_refresh) ;; Reference
;;         (19 . fa_folder_open) ;; Folder
;;         (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
;;         (21 . (fa_square :face font-lock-constant-face)) ;; Constant
;;         (22 . (fa_cube :face font-lock-type-face)) ;; Struct
;;         (23 . fa_calendar) ;; Event
;;         (24 . fa_square_o) ;; Operator
;;         (25 . fa_arrows)) ;; TypeParameter
;;       )

;; Company Quickhelp
;; adds documentation pop-ups to company-mode
(use-package company-quickhelp)
(company-quickhelp-mode)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))


;; ======== YASNIPPET ========
(use-package yasnippet
  :diminish yas-minor-mode)
(yas-global-mode 1)
;; variable used in helper function to embed yas suggestions in company completion window
(defvar company-mode/enable-yas t)
;; snippet sources
(use-package yasnippet-snippets)
;; BUG: no longer exists in package managers
;; (use-package php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/config/Create-PHP-YASnippet.php")


;; ======== FLYCHECK ========
(use-package flycheck
  :defer t
  :init (global-flycheck-mode))


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


;; ======== ALL-THE-ICONS =========
(use-package all-the-icons)
;; fix for lag on windows when using all-the-icons
(when (string-equal system-type "windows-nt") (setq inhibit-compacting-font-caches t))
                                        ;(use-package spaceline)
                                        ;(require 'spaceline-config)
                                        ;(use-package spaceline-all-the-icons
                                        ;  :after spaceline
                                        ;  :config (spaceline-all-the-icons-theme))
                                        ;(setq spaceline-all-the-icons-separator-type 'arrow) 


;; ======== DOOM MODELINE ========
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))


;; ======== PROJECTILE ========
(use-package projectile)
(use-package counsel-projectile
  :config
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
  (define-key projectile-mode-map (kbd "C-c p s s") 'counsel-projectile-ag))


;; ======== LSP ========
(use-package lsp-mode
  :ensure t
  :defer t
  ;; :config
  ;; (require 'lsp-ui-imenu)
  ;; (add-hook 'lsp-after-open-hook 'lsp-ui-enable-imenu)
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
:init (setq lsp-ui-doc-enable t
         lsp-ui-doc-use-webkit nil
         lsp-ui-doc-header nil
         lsp-ui-doc-delay 0.0
         lsp-ui-doc-include-signature t
         lsp-ui-doc-alignment 'frame
         ;; lsp-ui-doc-use-childframe nil
         ;; lsp-ui-doc-border (face-foreground 'default)
         lsp-ui-peek-enable t
         lsp-ui-peek-show-directory t
         lsp-ui-sideline-update-mode 'line
         lsp-ui-sideline-enable t
         lsp-ui-sideline-show-code-actions t
         lsp-ui-sideline-show-hover nil
         lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; (lsp-ui-peek-jump-backward)
  ;; (lsp-ui-peek-jump-forward)
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
       (lambda ()
         (setq lsp-ui-doc-border (face-foreground 'default))
         (set-face-background 'lsp-ui-doc-background
                              (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))


;; ======== TREE-SITTER ========
;; (use-package tree-sitter
;;   :hook (typescript-mode . tree-sitter-hl-mode)
;;   :config
;;   (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))
(use-package tree-sitter
  :ensure t
  :hook (typescript-mode . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)
(with-eval-after-load 'tree-sitter-langs
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx))
  (tree-sitter-require 'json)
  (add-to-list 'tree-sitter-major-mode-language-alist '(bbjson-mode . json)))


;; ======== C-MODE ========
(add-hook 'c-mode-hook #'smartparens-mode)
(add-hook 'c-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;;;;;;;; cquery setup
;; options include irony, cquery, rtags, ggtags, and ycmd

(use-package cquery
  :commands (lsp-cquery-enable)
  :hook (c-mode-common . lsp-cquery-enable)
  :custom
  (cquery-executable "/Users/matzy/Development/cquery/BUILD/cquery")
  (cquery-project-roots '("/Users/matzy/Projects/IupEmscripten/src" "/Users/matzy/Projects/IupEmscripten/include" "/Users/matzy/Projects/IupEmscripten/src/emscripten"))
  ;; (cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
  (cquery-extra-init-params '(:completion (:detailedLabel t) :extraClangArguments ("-I/Users/matzy/Projects/IupEmscripten/src" "-I/Users/matzy/Projects/IupEmscripten/include"))))

;; BUG: lsp-mode dropped company-lsp support
;; https://github.com/emacs-lsp/lsp-mode/pull/1983
;; (use-package company-lsp
;;   :after (lsp-mode company-mode)
;;   :custom (company-lsp-enable-recompletion t)
;;   :config (add-to-list 'company-backends 'company-lsp))

(use-package ivy-xref
  :after ivy
  :custom (xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; irony setup
;; (use-package irony
;;   :init
;;   (setq irony--compile-options
;;         '(
;;           "-I/Users/matzy/Projects/IupEmscripten/include"
;;           "-I/Users/matzy/Projects/IupEmscripten/src")
;;         )
;;   :config
;;   (use-package company-irony)
;;   (use-package company-irony-c-headers)
;;   (company-mode)
;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
;;   ;; (setq company-backends '(company-irony-c-headers company-irony company-dabbrev-code company-keywords company-yasnippet company-files company-dabbrev))
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   )
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (add-hook 'irony-mode-hook 'irony-eldoc)
;; ;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

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

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Don't ask to reload TAGS if newer, just do it
;; (setq tags-revert-without-query 1)

(defhydra hydra-c (:color red
                          :hint nil)
  "
_f_ flycheck
"
  ("f" hydra-flycheck/body :exit t)
  )

;; (use-package cmake-ide
;;   :config
;;   (cmake-ide-setup)
;;   )
;; (require 'cmake-ide)
;; (cmake-ide-setup)
;; ;; Set cmake-ide-flags-c++ to use C++11
;; (setq cmake-ide-cmake-opts "-DCMAKE_TOOLCHAIN_FILE=/Users/matzy/Development/emsdk_portable/emscripten/1.37.9/cmake/Modules/Platform/Emscripten.cmake -DCMAKE_BUILD_TYPE=Debug")
;; ;; (setq cmake-ide-flags-c (append '("-g4")))
;; (setq cmake-ide-build-dir "~/Projects/IupEmscripten/BUILD/emscripten")
;; ;; We want to be able to compile with a keyboard shortcut
;; (global-set-key (kbd "C-c m") 'cmake-ide-compile)


;; ======== JAVASCRIPT ========
(use-package js2-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 
  (setq js2-include-node-externs t)
  (setq js2-include-browser-externs t)
  (setq js2-highlight-level 3)
  :config
  ;; better imenu
  (js2-imenu-extras-mode)
  ;; tern autocompletion
  ;; (use-package company-tern)
  ;; (add-to-list 'company-backends 'company-tern)
  (add-hook 'js2-mode-hook (lambda ()
                             ;; (tern-mode)
                             (company-mode)
                             (smartparens-mode)
                             (evil-smartparens-mode)
                             (flycheck-mode)))
  ;; (define-key tern-mode-keymap (kbd "M-.") nil)
  ;; (define-key tern-mode-keymap (kbd "M-,") nil)
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
  )

;; javascript/typescript lsp - not bad but doesn't provide much that tide doesnt
;; (use-package lsp-javascript-typescript
;;   :init
;;   (add-hook 'js2-mode-hook 'lsp-mode)
;;   (add-hook 'js2-mode-hook 'company-mode)
;;   (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
;;   :config
;;   (defun my-company-transformer (candidates)
;;     (let ((completion-ignore-case t))
;;       (all-completions (company-grab-symbol) candidates)))
;;   (defun my-js-hook nil
;;     (make-local-variable 'company-transformers)
;;     (push 'my-company-transformer company-transformers))
;;   (add-hook 'js2-mode-hook 'my-js-hook)
;;   )


(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config 
  (js2r-add-keybindings-with-prefix "C-c C-m"))

(use-package xref-js2
  :defer t
  :config
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  )

;; add-node-modules-path retrives binaries from node_modules for things like eslint
(use-package add-node-modules-path
  :hook (js2-mode . add-node-modules-path))


;; setup mode hooks
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; indium
(use-package indium
  :defer t
  :config
  (add-to-list 'evil-emacs-state-modes 'indium-repl-mode))

;; skewer
;; (use-package skewer-mode
;;   :defer t)

;; spacemacs skewer functions

;; (defun spacemacs/skewer-start-repl ()
;;   "Attach a browser to Emacs and start a skewer REPL."
;;   (interactive)
;;   (run-skewer)
;;   (skewer-repl))

;; (defun spacemacs/skewer-load-buffer-and-focus ()
;;   "Execute whole buffer in browser and switch to REPL in insert state."
;;   (interactive)
;;   (skewer-load-buffer)
;;   (skewer-repl)
;;   (evil-insert-state))

;; (defun spacemacs/skewer-eval-defun-and-focus ()
;;   "Execute function at point in browser and switch to REPL in insert state."
;;   (interactive)
;;   (skewer-eval-defun)
;;   (skewer-repl)
;;   (evil-insert-state))

;; (defun spacemacs/skewer-eval-region (beg end)
;;   "Execute the region as JavaScript code in the attached browser."
;;   (interactive "r")
;;   (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

;; (defun spacemacs/skewer-eval-region-and-focus (beg end)
;;   "Execute the region in browser and swith to REPL in insert state."
;;   (interactive "r")
;;   (spacemacs/skewer-eval-region beg end)
;;   (skewer-repl)
;;   (evil-insert-state))

;; js2-refactor hydra
(defhydra hydra-js2-refactor (:color blue :hint nil)
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
  ^Buffer^                    ^Errors/Format^             ^Refactor^                   ^Indium^                 ^Tide^
---------------------------------------------------------------------------------------------------------------------------------------
[_d_]   Documentation         [_e_] Flycheck            [_rs_]  Rename symbol         [_in_]  Indium node       [_*_]  Restart server
[_fd_]  Find definition       [_a_] Apply error fix     [_rf_]  Refactor              [_ic_]  Indium chrome     [_v_]  Verify setup
[_fr_]  Find references       [_t_]  Tide format        [_rj_]  js2-refactor          [_is_]  Indium scratch    [_oi_]  Organize imports 
[_fj_]  Jump to func def      [_c_]  JSDoc comment
[_fw_]  Show func def window
[_fx_]  xref find refs
"
  ("d" tide-documentation-at-point :exit t)
  ("fd" tide-jump-to-definition :exit t)
  ("fr" tide-references :exit t)
  ("fj" xref-find-definitions)
  ("fw" xref-find-definitions-other-window)
  ("fx" xref-find-references)
  ("e" hydra-flycheck/body :exit t)
  ("a" tide-fix :exit t)
  ("t" tide-format :exit t)
  ("c" tide-jsdoc-template :exit t)
  ("rs" tide-rename-symbol :exit t)
  ("rf" tide-refactor :exit t)
  ("rj" hydra-js2-refactor/body :exit t)
  ("in" indium-connect-to-nodejs :exit t)
  ("ic" indium-connect-to-chrome :exit t)
  ("is" indium-scratch :exit t)
  ("*" tide-restart-server :exit t)
  ("v" tide-verify-setup :exit t)
  ("oi" tide-organize-imports :exit t)
  )


;; ======== TYPESCRIPT ========
;; (use-package typescript-mode
;;   :mode (rx ".ts" string-end)
;;   :init
;;   (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
;;   (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode))
;;   :config
;;   (setq typescript-indent-level 2))
(use-package typescript-mode
  :defer t
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))
;; (require 'tsi-typescript)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)) 


(use-package tide
  :config
  (setq company-tooltip-align-annotations t)
  (setq tide-completion-detailed t)
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'rjsx-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  (add-to-list 'company-backends 'company-tide)
  )

;; (add-hook
;;  'typescript-mode-hook
;;  (lambda ()
;;    (setup-tide-mode)
;;    (tree-sitter-mode)
;;    (tree-sitter-hl-mode)
;;    (tsi-typescript-mode)))
(push '("\\.js[x]?\\'" . typescript-mode) auto-mode-alist)
(push '("\\.ts[x]?\\'" . typescript-mode) auto-mode-alist)

;; typescript lsp (slow and not as many features as javascript-typescript-langserver)
;; (require 'lsp-typescript)
;;   (add-hook 'typescript-mode-hook 'lsp-mode)
;;   (add-hook 'typescript-mode-hook 'company-mode)
;;   (add-hook 'typescript-mode-hook #'lsp-typescript-enable)

;; ;; configure smartparens
;; (add-hook 'typescript-mode-hook #'smartparens-mode)
;; (add-hook 'typescript-mode-hook #'evil-smartparens-mode)
;; (sp-local-pair 'typescript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
;; (add-hook 'typescript-mode-hook #'add-node-modules-path)

(use-package coverlay
  :ensure t
  :defer t)
(use-package origami
  :ensure t
  :defer t)
(use-package graphql-mode
  :ensure t
  :defer t)
(use-package tsi
  :after tree-sitter
  :ensure t
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))
  )
(use-package tsx-mode
  :ensure t
  :defer t
  :quelpa (tsx-mode :fetcher github :repo "orzechowskid/tsx-mode.el")
  :mode
  ("\\.tsx\\'" . tsx-mode)
  ("\\.jsx\\'" . tsx-mode))

(defhydra hydra-lsp-typescript (:color red
                                   :hint nil)
  "
  ^Buffer^                 ^Errors^                   ^Refactor^                   ^Format^                 ^LSP^
------------------------------------------------------------------------------------------------------------------------------------
[_d_]   Documentation      [_e_] Errors              [_rs_]  Rename symbol         [_p_]  Prettier format    [_*_]  Restart server
[_fd_]  Find definition                              [_rf_]  Refactor              
[_fr_]  Find references                                                                                [_i_]  Organize imports 
"
  ("d" lsp-ui-doc-show :exit t)
  ("fd" lsp-ui-peek-find-definitions :exit t)
  ("fr" lsp-ui-peek-find-references :exit t)
  ;; ("c" tide-jsdoc-template :exit t)
  ("e" flycheck-list-errors :exit t)
  ;; ("a" tide-fix :exit t)
  ;; ("rj" hydra-js2-refactor/body :exit t)
  ("rs" lsp-rename :exit t)
  ("rf" lsp-modeline-code-actions-mode :exit t)
  ("p" prettier-prettify :exit t)
  ("*" lsp-workspace-restart :exit t)
  ;; ("v" tide-verify-setup :exit t)
  ("i" lsp-organize-imports :exit t)
  )


;; orzechovski's typescript/tsx setup
(use-package coverlay
  :ensure t
  :defer t)
(use-package origami
  :ensure t
  :defer t)
(use-package graphql-mode
  :ensure t
  :defer t)
(use-package tsi
  ;; :after tree-sitter
  :ensure t
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  ;; :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  ;; :init
  ;; (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  ;; (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  ;; (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  ;; (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1)))
  )
(use-package tsx-mode
  :ensure t
  :defer t
  :quelpa (tsx-mode :fetcher github :repo "orzechowskid/tsx-mode.el")
  :mode ("\\.tsx\\'" . tsx-mode))


(defhydra hydra-typescript (:color red
                                   :hint nil)
  "
  ^Buffer^                 ^Errors^                   ^Refactor^                   ^Format^                 ^Tide^
------------------------------------------------------------------------------------------------------------------------------------
[_d_]   Documentation      [_e_] Errors              [_rs_]  Rename symbol         [_t_]  Tide format       [_*_]  Restart server
[_fd_]  Find definition    [_a_] Apply error fix     [_rf_]  Refactor              [_c_]  JSDoc comment     [_v_]  Verify setup
[_fr_]  Find references                            [_rj_]  js2-refactor                                 [_i_]  Organize imports 
"
  ("d" tide-documentation-at-point :exit t)
  ("fd" tide-jump-to-definition :exit t)
  ("fr" tide-references :exit t)
  ("c" tide-jsdoc-template :exit t)
  ("e" tide-project-errors :exit t)
  ("a" tide-fix :exit t)
  ("rj" hydra-js2-refactor/body :exit t)
  ("rs" tide-rename-symbol :exit t)
  ("rf" tide-refactor :exit t)
  ("t" tide-format :exit t)
  ("*" tide-restart-server :exit t)
  ("v" tide-verify-setup :exit t)
  ("i" tide-organize-imports :exit t)
  )


;; ======== REACT/JSX ========
(use-package rjsx-mode
  ;; currently have a hook in tide which uses tide for rjsx checking and completion - need jsconfig.json in root of project
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  ;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  (setq js2-strict-missing-semi-warning nil)
  (setq indent-tabs-mode t)
  (setq flycheck-disabled-checkers 'jsx-tide)
  )

;; add completion details
(setq tide-completion-detailed t)
;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; configure smartparens
(add-hook 'rjsx-mode-hook #'smartparens-mode)
(add-hook 'rjxs-mode-hook #'evil-smartparens-mode)
(sp-local-pair 'rjsx-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(add-hook 'rjsx-mode-hook 'add-node-modules-path)

(defhydra hydra-react (:color red
                                   :hint nil)
  "
  ^Buffer^                 ^Errors^                   ^Refactor^                   ^Format^                 ^Tide^
------------------------------------------------------------------------------------------------------------------------------------
[_d_]   Documentation      [_e_] Flycheck            [_rs_]  Rename symbol         [_t_]  Tide format       [_*_]  Restart server
[_fd_]  Find definition    [_a_] Apply error fix     [_rf_]  Refactor              [_c_]  JSDoc comment     [_v_]  Verify setup
[_fr_]  Find references                              [_rj_]  js2-refactor                                   [_i_]  Organize imports 
"
  ("d" tide-documentation-at-point :exit t)
  ("fd" tide-jump-to-definition :exit t)
  ("fr" tide-references :exit t)
  ("c" tide-jsdoc-template :exit t)
  ("e" hydra-flycheck/body :exit t)
  ("a" tide-fix :exit t)
  ("rs" tide-rename-symbol :exit t)
  ("rf" tide-refactor :exit t)
  ("rj" hydra-js2-refactor/body :exit t)
  ("t" tide-format :exit t)
  ("*" tide-restart-server :exit t)
  ("v" tide-verify-setup :exit t)
  ("i" tide-organize-imports :exit t)
  )


;; ======== PRETTIER ========
(use-package prettier
  :ensure t
  :defer t
  :hook
  (js-mode . prettier-mode)
  (typescript-mode . prettier-mode)
  (tsi-typescript-mode . prettier-mode)
  (tsi-json-mode . prettier-mode)
  (tsi-css-mode . prettier-mode)
  (tsi-scss-mode . prettier-mode)
  (tsx-mode . prettier-mode))


;; ======== JSON ========
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
	   ("\\manifest.webapp\\'" . json-mode )
	   ("\\.tern-project\\'" . json-mode)))


;; ======== PHP ========
(use-package php-mode
  :defer t
  :config (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet))
(use-package company-php
  :after php-mode)
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

;; yasnippet additions


;; tool to test HTTP REST webservices
(use-package restclient
  :defer t)
(defun start-restclient ()
  (interactive)
  (let ((buf (generate-new-buffer "restclient")))
    (switch-to-buffer buf)
    (restclient-mode)
    buf))


;; ======== WEB MODE ========
(use-package web-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (add-hook 'web-mode-hook (lambda ()
;;                            (set (make-local-variable 'company-backends) '(company-web-html))
;;                            (company-mode t)))
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
(use-package suggest
  :defer t)

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

(use-package elisp-format
  :defer t
  :ensure t)

(use-package symex
  :ensure t
  :config
  (symex-initialize)
  ;; (global-set-key (kbd "s-;") 'symex-mode-interface)
  :custom
  (symex-modal-backend 'hydra))

(defhydra hydra-elisp (:color red
                              :hint nil)
  "
_eb_ eval buffer
_er_ eval region
_es_ eval sexp
_f_ flycheck
_s_ symex-mode
"
  ("eb" eval-buffer)
  ("er" eval-region)
  ("es" pp-eval-last-sexp)
  ("eeh" hydra-edebug/body)
  ("f" hydra-flycheck/body :exit t)
  ("s" symex-mode-interface)
  )

(defhydra hydra-edebug (:hint t :foreign-keys run)

  ("q" nil "quit")
  ("b" #'edebug-backtrace "backtrace" :column "common")
  ("-" #'negative-argument "neg argument" :column "common")

  ;; breaking
  ("I" #'edebug-instrument-callee "instrument callee" :column "break")
  ("x" #'edebug-set-breakpoint "set breakpoint" :column "break")
  ("X" #'edebug-unset-breakpoint "unset breakpoint" :column "break")
  ("N" #'edebug-next-breakpoint "next breakpoint" :column "break")
  ("c" #'edebug-set-conditional-breakpoint "conditional bp" :column "break")
  ("C" #'edebug-set-global-break-condition "global conditional bp"
   :column "break")

  ;; navigation
  ("w" #'edebug-where "where" :column "common")
  ("z" #'edebug-bounce-point "bounce point" :column "common")

  ;; stepping
  ("h" #'edebug-goto-here "continue until point" :column "step")
  ("s" #'edebug-stop "stop" :column "step")
  ("o" #'edebug-step-out "step out" :column "step")
  ("i" #'edebug-step-in "step in" :column "step")
  ("f" #'edebug-forward "forward" :column "step")

  ;; sexp oriented
  ("l" #'edeug-forward-sexp "forward sexp" :column "sexp")
  ("e" #'edebug-eval-expression "eval expression" :column "sexp")
  ("E" #'edebug-eval-last-sexp "eval expression" :column "sexp")
  ("r" #'edebug-previous-result "previous result" :column "sexp")
  (";" #'edebug-visit-eval-list "visit eval list" :column "sexp")

  ;; exiting
  ("Q" #'edebug-top-level-nonstop "toplevel non stop" :column "common")
  ("S" #'edebug-stop "edebug stop" :column "common")

  ;; modes
  ("1" #'edebug-Go-nonstop-mode "go nonstop" :column "modes")
  ("2" #'edebug-go-mode "go until break" :column "modes")
  ("3" #'edebug-step-mode "step mode" :column "modes")
  ("4" #'edebug-next-mode "next mode" :column "modes")
  ("5" #'edebug-continue-mode "continue" :column "modes")
  ("6" #'edebug-Continue-fast-mode "continue fast" :column "modes")
  ("7" #'edebug-trace-mode "trace" :column "modes")
  ("8" #'edebug-Trace-fast-mode "trace fast" :column "modes"))


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

;;; Set of hydra menus to work with Slime.
;;; vindarel 2017
;;; copyleft
(defhydra hydra-slime (:color blue :columns 3)
  "Slime"
  ("." slime-edit-definition "edit definition")
  ("," slime-pop-find-definition-stack "return from definition")
  ("c" hydra-slime-compile/body "compile")
  ("e" hydra-slime-eval/body "Eval")
  ("g" hydra-slime-navigate/body "Navigate")
  ("h" hydra-slime-help/body "Help")
  ("x" slime-scratch "Scratch")
  ("ma" slime-macroexpand-all "Macroexpand All")
  ("mo" slime-macroexpand-1 "Macroexpand One")
  ("se" slime-eval-last-expression-in-repl "Eval Last Expression in Repl")
  ("si" slime "Slime")
  ("sq" slime-quit-lisp "Quit Lisp")
  ("tf" slime-toggle-fancy-trace "Toggle Fancy Trace"))

(defun slime-selector-call-by-key (key)
  "Call a slime-selector function associated with the given KEY."
  ;; Strangely, this code is obscured in slime.el. Functions are not
  ;; defined by name.
  (funcall (cl-third (cl-find key slime-selector-methods :key #'car))))

(defhydra slime-selector-hydra (:color red
                                :columns 4)
  " Slime selector "
  ("4" (slime-selector-call-by-key ?4) "other window")
  ("c" (slime-selector-call-by-key ?c) "connections buffer")
  ("d" (slime-selector-call-by-key ?d) "*sldb* buffer for the current connection")
  ("e" (slime-selector-call-by-key ?e) "most recently emacs-lisp-mode buffer")
  ("i" (slime-selector-call-by-key ?i) "*inferior-lisp* buffer")
  ("l" (slime-selector-call-by-key ?l) "most recently visited lisp-mode buffer")
  ("n" (slime-selector-call-by-key ?n) "next Lisp connection")
  ("p" (slime-selector-call-by-key ?p) "previous Lisp connection")
  ("r" (slime-selector-call-by-key ?r) "REPL")
  ("s" (slime-selector-call-by-key ?s) "*slime-scratch* buffer")
  ("t" (slime-selector-call-by-key ?t) "threads buffer")
  ("v" (slime-selector-call-by-key ?v) "*slime-events* buffer")
  ("q" nil "quit")
  ("S" slime-hydra/body "Slime hydra" :color blue))


;; TODO: Add Debug Hydra
(defhydra hydra-slime-compile (:color blue :columns 3)
  "Compile"
  ("c" slime-compile-file "Compile")
  ("C" slime-compile-and-load-file "Compile and Load")
  ("l" slime-load-file "Load File")
  ("f" slime-compile-defun "Compile Defun")
  ("r" slime-compile-region "Compile Region")
  ("n" slime-remove-notes "Remove Notes"))

(defhydra hydra-slime-eval (:color blue :columns 3)
  "Eval"
  ("b" slime-eval-buffer "Buffer")
  ("f" slime-eval-defun "Defun")
  ("F" slime-undefine-function "Undefine Function")
  ("e" slime-eval-last-expression "Last Sexp")
  ("r" slime-eval-region "Region"))

(defhydra hydra-slime-help (:color blue :columns 3)
  "Help"
  ("a" slime-apropos "Apropros")
  ("A" slime-apropos-all "Apropros All")
  ("d" slime-disassemble-symbol "Disassemble")
  ("h" slime-describe-symbol "Describe Symbol")
  ("H" slime-hyperspec-lookup "Hyperspec Lookup")
  ("p" slime-apropos-package "Apropos Package")
  ("t" slime-toggle-trace-fdefinition "Toggle Trace Fdefinition")
  ("T" slime-untrace-all "Untrace All")
  ("<" slime-who-calls "Who Calls")
  (">" slime-calls-who "Calls Who")
  ;; TODO: Add key bindings for who binds/sets globals?
  ("r" slime-who-references "Who References")
  ("m" slime-who-macroexpands "Who Macroexpands")
  ("s" slime-who-specializes "Who Specializes"))

(defhydra hydra-slime-navigate (:color blue :columns 3)
  "Navigate"
  ("g" slime-edit-definition "Find Definition")
  ("b" slime-pop-find-definition-stack "Find Definition Pop")
  ("n" slime-next-note "Next Note")
  ("p" slime-previous-note "Previous Note")
  ("r" slime-who-references "Who References")
  ("m" slime-who-macroexpands "Who Macroexpands")
  ("s" slime-who-specializes "Who Specializes"))


;; ======== RACKET ========
(use-package racket-mode
  :defer t)
;; TODO wouldn't quack interfere with racket mode? i had them both active without problem seemingly...
;; (use-package quack
;;   :after racket-mode)
(use-package scribble-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)) 

;; setup smartparens and cleverparens
(add-hook 'racket-mode-hook #'smartparens-mode)
(add-hook 'racket-mode-hook #'smartparens-strict-mode)
(add-hook 'racket-mode-hook #'evil-cleverparens-mode)
(add-hook 'racket-mode-hook #'highlight-quoted-mode)


;; ======== ESS ========
(use-package ess
  :defer t)
;; (add-to-list 'auto-mode-alist '("\\.r\\'" . php-mode))


;; ======== MAGIT ========
;; full screen magit-status
(use-package magit)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; ======== HELPFUL ========
(use-package helpful)


;; ======== UNDO TREE ========
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    (setq undo-tree-auto-save-history nil)))


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
;; get code block syntax highlighting in HTML export
(setq org-src-fontify-natively t)

;; auto-org-md - automatically export a markdown file when saving an org file
(use-package auto-org-md
  :defer t)

;; use uuid's for org links
(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive)

;; beautify org-mode
(use-package org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))
(setq org-ellipsis "⤵")

(setq org-default-notes-file "~/Dropbox/org/tasks.org")
(setq org-capture-templates
      '(
        ("t" "Todo" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %u\n  %a")
        ("n" "Note/Data" entry (file+headline "~/Dropbox/org/inbox.org" "Notes/Data")
         "* %?   \n  %i\n  %u\n  %a")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("J" "Work-Journal" entry (file+datetree "~/Dropbox/org/wjournal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ))
(setq org-irc-link-to-logs t)

;; note time when something is marked as complete
(setq org-log-done 'time)

;; configure which files to use in org-agenda
(setq org-agenda-files (list "~/Dropox/org/inbox.org"
                             "~/Dropox/org/email.org"
                             "~/Dropox/org/tasks.org"
                             "~/Dropox/org/wtasks.org"
                             "~/Dropox/org/journal.org"
                             "~/Dropox/org/wjournal.org"
                             "~/Dropox/org/kb.org"
                             "~/Dropox/org/wkb.org"
                             ))
(setq org-agenda-text-search-extra-files
      (list "~/Dropox/org/someday.org"
            "~/Dropbox/org/config.org"
            ))

(setq org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2)
                           ("~/Dropbox/org/someday.org" :maxlevel . 2)
                           ("~/Dropbox/org/templates.org" :maxlevel . 2)
                           )
      )
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)

(use-package org-jira
  :defer t
  :config
  (setq jiralib-url "https://jira.rsna.org/"))

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
    (elisp-mode
     (hydra-elisp/body))
    (c-mode
     (hydra-c/body))
    (js2-mode
     (hydra-javascript/body))
    (typescript-mode
     (hydra-lsp-typescript/body))
    (tsx-mode
     (hydra-lsp-typescript/body))
    (rjsx-mode
     (hydra-react/body))
    (markdown-mode
     (hydra-markdown/body))
    (org-agenda-mode
     (hydra-org-agenda/body))
    (lisp-mode
     (hydra-slime/body))
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
   '("e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "6a23db7bccf6288fd7c80475dc35804c73f9c9769ad527306d2e0eada1f8b466" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "3be1f5387122b935a26e02795196bc90860c57a62940f768f138b02383d9a257" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "dcb9fd142d390bb289fee1d1bb49cb67ab7422cd46baddf11f5c9b7ff756f64c" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "7f6796a9b925f727bbe1781dc65f7f23c0aa4d4dc19613aa3cf96e41a96651e4" "50b66fad333100cc645a27ada899a7b1d44f1ceb32140ab8e88fedabfb7d0daf" "fec6c786b1d3088091715772839ac6051ed972b17991af04b50e9285a98c7463" "8ad35d6c2b35eacc328b732f0a4fe263abd96443a5075aa53b8535a9e8cb7eaf" "9a58c408a001318ce9b4eab64c620c8e8ebd55d4c52327e354f24d298fb6978f" "a9d2ed6e4266ea7f8c1f4a0d1af34a6282ad6ff91754bee5ec7c3b260ec721f4" "293b55c588c56fe062afe4b7a3a4b023712a26d26dc69ee89c347b30283a72eb" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(linum-format " %5i ")
 '(package-selected-packages
   '(symex elisp-format doom-modeline quelpa doom-themes dashboard page-break-lines tree-sitter-langs tree-sitter ivy-xref lsp-ui company-lsp flycheck-irony irony-eldoc cmake-ide atom-one-dark-theme atom-dark-theme base16-theme oceanic-theme org-jira web-mode ivy-hydra auto-org-md org-id company-box skewer-mode skewer markdown-mode hydra org-bullets slime magit nord-theme eyebrowse evil-collection solarized-theme evil-magit ac-php company-php php-mode evil-cleverparens evil-smartparens smartparens tide indium js2-mode smart-mode-line sublime-themes counsel general evil))
 '(sp-highlight-pair-overlay nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

