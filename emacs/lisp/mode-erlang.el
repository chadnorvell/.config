;; Settings for Erlang and Elixir.

(add-to-list 'auto-mode-alist '("\\.ex[s]?\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'"   . heex-ts-mode))

(provide 'mode-erlang)
