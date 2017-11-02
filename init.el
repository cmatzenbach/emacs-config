(require 'package)
(add-to-list 'package-archives '(("melpa". "http://melpa.org/packages/")
				 ("org" . "http://orgmode.org/elpa/")
				 ;("marmalade" . "http://marmalade-repo.org/packages/")
				 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)

(defconst user-config-dir "~/.emacs.d/config/")

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-config-dir)))

;; load custom config modules
(load-user-file "evil.el")
(load-user-file "appearance.el")

;; enable c-u and c-d scrolling
(setq evil-want-C-u-scroll t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (evil))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

