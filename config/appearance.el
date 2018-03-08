;; don't display startup message
(setq inhibit-startup-message t)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbars
(when (display-graphic-p) (set-scroll-bar-mode nil))

;; change cursor color based on mode
(when (display-graphic-p)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
)

;; set solarized-dark colorscheme
;(when (display-graphic-p) (load-theme 'brin t)) 

(cond
 ((string-equal system-type "windows-nt")
  (progn
    (load-theme 'challenger-deep t)
    (set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 108
                    :weight 'normal
                    :width 'normal)))
 ((string-equal system-type "gnu/linux")
  (progn
    (load-theme 'brin t))))
