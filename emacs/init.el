;; Add lisp subdirectory to load path.
(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; Don't store custom-set-* here.
(setq custom-file (concat user-emacs-directory "/custom.el"))

(require 'core-prelude)
(require 'core-functions)
(require 'core-keybindings)
(require 'core-code)
(require 'core-ui)
(require 'core-projects)
(require 'core-term)
(require 'mode-clojure)
(require 'mode-elisp)
(require 'mode-erlang)
(require 'mode-git)
(require 'mode-org)
(require 'mode-sql)
(require 'mode-web)
