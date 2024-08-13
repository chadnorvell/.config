;; Settings for code-related functionality.

(use-package magit
  :straight t
  :general
  (cxn/leader-def
    "g"   (cons "git" (make-sparse-keymap))
    "gs"    '("status"       . magit-status)
    "gb"    '("blame"        . magit-blame)
    "gd"    '("diff"         . magit-diff)
    "g+"    '("stage file"   . magit-stage-file)
    "g-"    '("unstage file" . magit-unstage-file)))

(use-package treesit
  :demand t
  :config
  ;; This defines the tree-sitter grammars we use, but you still need to call
  ;; tree-sit-install-language grammar for each entry to actually get the grammar.
  (setq treesit-language-source-alist
        '((css        "https://github.com/tree-sitter/tree-sitter-css"        "v0.21.0")
          (html       "https://github.com/tree-sitter/tree-sitter-html"       "v0.20.1")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
	  (json       "https://github.com/tree-sitter/tree-sitter-json"       "v0.21.0"))))

;; Use eglot for code intelligence.
(use-package eglot
  :general
  (general-define-key
    "s-?" 'eldoc
    "s-/" 'eldoc-box-help-at-point
    "s-:" 'eglot-format-buffer
    "s-c" 'completion-at-point
    "s-f" 'xref-find-definitions
    "s-i" 'eglot-find-declaration
    "s-m" 'eglot-find-implementations
    "s-n" 'eglot-rename
    "s-r" 'xref-find-references
    "s-y" 'eglot-find-typeDefinition)

  (cxn/leader-def
    "c"  (cons "code" (make-sparse-keymap))
    "c:" '("format buffer"   . eglot-format-buffer)
    "cc" '("completion"      . completion-at-point)
    "cE" '("start eglot"     . eglot)
    "cf" '("definition"      . xref-find-definitions)
    "ci" '("declaration"     . eglot-find-declaration)
    "ch" '("docs"            . eldoc-box-help-at-point)
    "cH" '("docs buffer"     . eldoc)
    "cm" '("implementations" . eglot-find-implementations)
    "cn" '("rename"          . eglot-rename)
    "cr" '("references"      . xref-find-references)
    "ct" '("type definition" . eglot-find-typeDefinition)))

;; Use apheleia for async formatting.
;; eglot also provides formatting via eglot-format-buffer, but that
;; relies on the language's LSP to do the formatting. That's fine for
;; some languages, but in other cases we might want or need to use an
;; external formatting program, which apheleia enables for us.
(use-package apheleia
  :straight t
  :general
  (general-define-key
    "s-;" 'apheleia-format-buffer)

  (cxn/leader-def
    "c;" '("format buffer with apheleia" . apheleia-format-buffer)
    "cA" 'apheleia-mode))

(use-package smartparens
  :straight t
  :demand t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'core-functions)
  (require 'smartparens-config)
  :general
  (general-define-key
   :keymaps 'smartparens-mode-map
   "s-a" 'sp-beginning-of-sexp
   "s-e" 'sp-end-of-sexp
   "s-j" 'sp-down-sexp
   "s-J" 'sp-backward-down-sexp
   "s-k" 'sp-up-sexp
   "s-K" 'sp-backward-up-sexp
   "s-l" 'sp-forward-sexp
   "s-L" 'sp-next-sexp
   "s-h" 'sp-backward-sexp
   "s-H" 'sp-previous-sexp
   "s-." 'sp-forward-symbol
   "s-," 'sp-backward-symbol

   "s-("  'wrap-with-paren
   "s-["  'wrap-with-bracket
   "s-{"  'wrap-with-brace
   "s-<"  'wrap-with-angle
   "s-'"  'wrap-with-single-quote
   "s-\"" 'wrap-with-double-quote
   "s-u"  'sp-unwrap-sexp
   "s-U"  'sp-backward-unwrap-sexp

   "s-d"   'sp-kill-sexp
   "s-D"   'sp-backward-kill-sexp
   "C-s-d" 'sp-kill-hybrid-sexp

   "C-s-l" 'sp-foward-slurp-sexp
   "C-s-h" 'sp-backward-slurp-sexp
   "M-s-l" 'sp-forward-barf-sexp
   "M-s-h" 'sp-backward-barf-sexp
   "C-s-t" 'sp-transpose-sexp))

(provide 'core-code)
