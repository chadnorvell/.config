;; Settings for elisp.

(use-package elisp-mode
  :config
  :general
  (cxn/major-def (emacs-lisp-mode-map lisp-interaction-mode-map)
    "r"  '("repl"          . ielm)
    "f"  '("format buffer" . apheleia-format-buffer)
    "b"  '("eval buffer"   . eval-buffer)
    "r"  '("eval region"   . eval-region)
    "f"  '("eval defun"    . eval-defun)
    "s"  '("eval sexp"     . eval-last-sexp)
    "l"  '("load file"     . load-file)))

(provide 'mode-elisp)
