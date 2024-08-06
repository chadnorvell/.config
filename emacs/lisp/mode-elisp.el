(use-package elisp-mode
  :config
  :general
  (cxn/major-def (emacs-lisp-mode-map lisp-interaction-mode-map)
    "r"  '("repl "    . ielm)
    "e"  (cons "eval" (make-sparse-keymap))
    "eb"   '("buffer" . eval-buffer)
    "er"   '("region" . eval-region)
    "ef"   '("defun"  . eval-defun)
    "es"   '("sexp"   . eval-last-sexp)))

(provide 'mode-elisp)
