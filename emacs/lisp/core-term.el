(use-package vterm
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)))

(provide 'core-term)
