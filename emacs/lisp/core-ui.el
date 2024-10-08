(menu-bar-mode   -1)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(window-divider-mode t)

(setq window-divider-default-places t
      frame-resize-pixelwise t)

(setq fixed-family "Iosevka")
(set-face-font 'default (concat fixed-family "-14"))
(copy-face 'default 'fixed-pitch)

(setq variable-family "Linux Biolinum")
(set-face-font 'variable-pitch (concat variable-family  "-16"))

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t))

(use-package mixed-pitch
  :straight t
  :hook (org-mode . mixed-pitch-mode))

;; Ensures eldoc can render markdown.
(use-package markdown-mode :straight t)

;; Show eldoc content in a childframe.
(use-package eldoc-box :straight t)
(add-hook 'elgot-managed-mode-hook #'eldoc-box-hover-mode t)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(use-package treemacs
  :straight t
  :defer t)

(use-package treemacs-evil
  :straight t
  :after (treemacs evil))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package posframe
  :straight t)

(use-package hydra-posframe
  :straight (:type git :host github :repo "Ladicle/hydra-posframe")
  :after (hydra posframe)
  :hook (after-init . hydra-posframe-mode)
  :init
  (setq hydra-posframe-border-width 1
        hydra-posframe-poshandler 'posframe-poshandler-window-bottom-center))

(use-package corfu
  :straight t
  :init (global-corfu-mode))

(provide 'core-ui)
