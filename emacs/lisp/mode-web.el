;; Settings for JavaScript, TypeScript, and other web-centric modes.

;; Bring in tree-sitter modes that aren't already bundled in emacs.
(use-package html-ts-mode :straight (:type git :host github :repo "mickeynp/html-ts-mode"))

;; Enable tree-sitter modes instead of the default non-tree-sitter modes.
(add-to-list 'auto-mode-alist '("\\.[cm]?[jt]s\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\\'"     . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json[c]?\\'"   . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'"       . html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"        . css-ts-mode))

;; Bring in tree-sitter modes that aren't already bundled in emacs, part 2.
;; Note that deno-ts-mode already adds "\\.ts[x]?\\'" to auto-mode-alist and then
;; determines whether to activate deno-ts-mode or typescript/tsx-ts-mode.
;; We do this after the auto-mode-alist associations above to ensure that the
;; deno associations take precedence.
(use-package deno-ts-mode :straight t)

;; Ensure binaries in a project's node_modules are available in the path.
(use-package add-node-modules-path
  :straight t
  :hook ((typescript-ts-mode tsx-ts-mode clojurescript-mode) . add-node-modules-path))

;; Async format-on-save.
(use-package apheleia
  :config
  ;; Link the appropriate formatter configs to their corresponding tree-sitter modes.
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier-typescript))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode        . prettier-typescript))
  (add-to-list 'apheleia-mode-alist '(deno-ts-mode       . denofmt-ts))
  (add-to-list 'apheleia-mode-alist '(json-ts-mode       . prettier-json))
  (add-to-list 'apheleia-mode-alist '(html-ts-mode       . prettier-html))
  (add-to-list 'apheleia-mode-alist '(css-ts-mode        . prettier-css))

  ;; The default prettier-* formatters use npx to get prettier, but we would rather use
  ;; the version included with the project. If there isn't a project or the project doesn't
  ;; have a dependency on prettier, this can fall back to a global version in $PATH.
  ;; Note that this isn't done with the denofmt-* formatters, since those already look for
  ;; deno on the $PATH.
  (add-to-list 'apheleia-formatters '(prettier            . ("prettier" "--stdin-filepath" filepath)))
  (add-to-list 'apheleia-formatters '(prettier-css        . ("prettier" "--stdin-filepath" filepath "--parser=css")))
  (add-to-list 'apheleia-formatters '(prettier-html       . ("prettier" "--stdin-filepath" filepath "--parser=html")))
  (add-to-list 'apheleia-formatters '(prettier-json       . ("prettier" "--stdin-filepath" filepath "--parser=json")))
  (add-to-list 'apheleia-formatters '(prettier-javascript . ("prettier" "--stdin-filepath" filepath "--parser=babel-flow")))
  (add-to-list 'apheleia-formatters '(prettier-typescript . ("prettier" "--stdin-filepath" filepath "--parser=typescript"))))

(provide 'mode-web)
