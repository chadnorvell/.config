;; Use eglot for code intelligence.
(use-package eglot
  :general
  (cxn/ctrl-c-def
    "c" 'completion-at-point
    "d" 'eglot-find-declaration
    "h" 'eldoc-box-help-at-point
    "H" 'eldoc
    "i" 'eglot-find-implementations
    "E" 'eglot
    "k" 'xref-find-definitions
    "n" 'eglot-rename
    "r" 'xref-find-references
    "t" 'eglot-find-typeDefinition)

  (cxn/leader-def
    "c"  (cons "code" (make-sparse-keymap))
    "cc" '("completion"      . completion-at-point)
    "cd" '("declaration"     . eglot-find-declaration)
    "ch" '("docs"            . eldoc-box-help-at-point)
    "cH" '("docs buffer"     . eldoc)
    "ci" '("implementations" . eglot-find-implementations)
    "cE" '("start eglot"     . eglot)
    "ck" '("definition"      . xref-find-definitions)
    "cn" '("rename"          . eglot-rename)
    "cr" '("references"      . xref-find-references)
    "ct" '("type definition" . eglot-find-typeDefinition)))

;; Use apheleia for async formatting.
;; This call pulls the dependency and configures keybindings, but other files
;; can use-package apheleia without :straight t to add additional, mode-specific
;; configuration
(use-package apheleia
  :straight t
  :general
  (cxn/ctrl-c-def
    "f" 'apheleia-format-buffer
    "F" 'apheleia-mode)

  (cxn/leader-def
    "cf" '("format buffer"          . apheleia-format-buffer)
    "cF" '("toggle format on save"  . apheleia-mode)))

(provide 'core-code)
