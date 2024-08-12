;; Settings for code-related functionality.

(use-package treesit
  :demand t
  :config
  ;; This defines the tree-sitter grammars we use, but you still need to call
  ;; tree-sit-install-language grammar for each entry to actually get the grammar.
  (setq treesit-language-source-alist
        '((css        "https://github.com/tree-sitter/tree-sitter-css"        "v0.21.0")
          (elixir     "https://github.com/elixir-lang/tree-sitter-elixir"     "v0.2.0")
          (heex       "http://github.com/phoenixframework/tree-sitter-heex"   "v0.6.0")
          (html       "https://github.com/tree-sitter/tree-sitter-html"       "v0.20.1")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
	  (json       "https://github.com/tree-sitter/tree-sitter-json"       "v0.21.0"))))

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
    "f" 'eglot-format-buffer
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
    "cf" '("format buffer"   . eglot-format-buffer)
    "ck" '("definition"      . xref-find-definitions)
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
  (cxn/ctrl-c-def
    "F" 'apheleia-format-buffer
    "A" 'apheleia-mode)

  (cxn/leader-def
    "cF" '("format buffer with apheleia" . apheleia-format-buffer)
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
