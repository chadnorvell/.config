(use-package projectile
  :straight t
  :init
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :after projectile
  :init
  (counsel-projectile-mode))

(provide 'core-projects)
