(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      delete-old-versions -1
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      inhibit-startup-screen t
      initial-scratch-message ""
      make-backup-files nil
      ring-bell-function 'ignore
      sentence-end-double-space nil)

(setq-default mode-line-format nil)

(defun display-startup-echo-area-message ()
  (message ""))

(global-auto-revert-mode t)
(electric-pair-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)

;; Bootstrap the straight.el package manager.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Pull in the use-package macro. We can still use it with straight.el, as long as we
;; use :straight t instead of :ensure t.
(straight-use-package 'use-package)
(straight-use-package 'org)

;; Make sure emacs has the same $PATH as the shell, even if launched from the GUI.
(use-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; macOS settings.
(when (string= system-type "darwin")
  (setq dired-use-ls-dired t
        insert-directory-program "/opt/homebrew/bin/gls"
        dired-listing-switches "-aBhl --group-directories-first"))

;; Fundamental packages.
(use-package smex :straight t)
(use-package ivy :straight t)
(use-package counsel :straight t)

(provide 'core-prelude)
