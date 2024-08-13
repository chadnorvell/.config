(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-separator " "
        which-key-prefix-prefix "+"
        which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-allow-evil-operators t))

(use-package general
  :straight t
  ;; We want this to load after evil, but evil also needs to be loaded eagerly,
  ;; otherwise these macros won't exist when subsequent scripts are loaded, and
  ;; using definers outside of this file won't work.
  :after evil
  :config
  (require 'core-functions)

  (general-define-key
   "M-x"   'counsel-M-x
   "s-`"   'other-frame
   "s-q"   'save-buffers-kill-emacs
   "s-s"   'save-buffer
   "M-s-h" 'evil-window-left
   "M-s-j" 'evil-window-down
   "M-s-k" 'evil-window-up
   "M-s-l" 'evil-window-right
   "C-s-h" 'split-window-right
   "C-s-j" 'split-window-below-and-focus
   "C-s-k" 'split-window-below
   "C-s-l" 'split-window-right-and-focus)

  (general-create-definer cxn/ctrl-x-def :prefix "C-x")
  (cxn/ctrl-x-def
   "b" 'counsel-switch-buffer
   "/" 'counsel-rg
   ">" 'vterm
   "x" 'hydra-ui/body
   "." 'counsel-fzf)

  (general-define-key
   :states '(normal insert motion visual emacs)
   :keymaps 'override
   :prefix-map 'cxn/leader-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer cxn/leader-def :keymaps 'cxn/leader-map)
  (cxn/leader-def "" nil)

  (general-create-definer cxn/major-def
   :states '(normal insert motion visual emacs)
   :keymaps 'override
   :major-modes t
   :prefix "SPC m"
   :non-normal-prefix "M-SPC m")

  (cxn/major-def "" nil)

  (cxn/leader-def
    "SPC"   '("M-x"         . counsel-M-x)
    "TAB"   '("last buffer" . alternate-buffer)
    "DEL"   '("last window" . alternate-window)
    "RET"   '("next frame"  . other-frame)
    ";"     '("eval"        . eval-expression)
    "!"     '("shell"       . shell-command)
    ">"     '("term"        . vterm)
    "."     '("find"        . counsel-fzf)
    "/"     '("search"      . counsel-rg)
    "?"     '("replace"     . query-replace)

    "m"     (cons "major" (make-sparse-keymap))
    "x"     '("ui"          . hydra-ui/body)

    "b"   (cons "buffers" (make-sparse-keymap))
    "bb"    '("switch"            . counsel-switch-buffer)
    "bp"    '("switch in project" . counsel-projectile-switch-to-buffer)
    "br"    '("reload"            . revert-buffer-no-confirm)
    "bd"    '("kill"              . kill-current-buffer)
    "bx"    '("kill with window"  . kill-buffer-and-window)

    "e"   (cons "frames" (make-sparse-keymap))
    "en"    '("next"        . other-frame)
    "eN"    '("new"         . make-frame)
    "ed"    '("kill"        . delete-frame)
    "eD"    '("kill others" . delete-other-frames)

    "f"   (cons "files" (make-sparse-keymap))
    "fs"    '("save"            . save-buffer)
    "ff"    '("find"            . counsel-find-file)
    "fp"    '("find in project" . counsel-projectile-find-file)
    "fd"    '("dired"           . counsel-dired)
    "fj"    '("dired jump"      . counsel-dired-jump)
    "ft"    'treemacs

    "p"   (cons "project" (make-sparse-keymap))
    "pp"    '("switch"  . counsel-projectile-switch-project)
    "pb"    '("buffers" . counsel-projectile-switch-to-buffer)
    "pf"    '("files"   . counsel-projectile-find-file)
    "pg"    '("search"  . counsel-projectile-rg)
    "pd"    '("dired"   . projectile-dired)

    "q"   (cons "quit" (make-sparse-keymap))
    "qq"    '("quit"    . save-buffers-kill-emacs)
    "qQ"    '("kill"    . kill-emacs)
    "qr"    '("restart" . restart-emacs)

    "w"   (cons "window" (make-sparse-keymap))
    "wd"    '("kill"        . delete-window)
    "wD"    '("kill others" . delete-other-windows)
    "w<"    '("shrink fit"  . shrink-window-if-larger-than-buffer)
    "w="    '("balance"     . balance-windows)
    "wm"  (cons "switch" (make-sparse-keymap))
    "wmh"   '("←"          . evil-window-left)
    "wmj"   '("↓"          . evil-window-down)
    "wmk"   '("↑"          . evil-window-up)
    "wml"   '("→"          . evil-window-right)
    "ws"  (cons "split" (make-sparse-keymap))
    "wsh"   '("←"          . split-window-right)
    "wsj"   '("↓"          . split-window-below-and-focus)
    "wsk"   '("↑"          . split-window-below)
    "wsl"   '("→"          . split-window-right-and-focus)
    )
  )

(use-package pretty-hydra
  :straight t
  :demand t
  :config
  (pretty-hydra-define
    hydra-ui
    (:color red :quit-key "q")
    ("UI"
     (("n"   display-line-numbers-mode       "line numbers"     :toggle t)
      ("w"   whitespace-mode                 "whitespace"       :toggle t)
      ("v"   visual-line-mode                "visual line mode" :toggle t)
      ("r"   toggle-truncate-lines           "truncate lines")
      ("="   text-scale-increase             "text scale +")
      ("-"   text-scale-decrease             "text scale -")
      ("0"   (text-scale-adjust 0)           "text reset"))
     "Window Switch"
     (("h"   evil-window-left                "←")
      ("j"   evil-window-down                "↓")
      ("k"   evil-window-up                  "↑")
      ("l"   evil-window-right               "→")
      ("C-h" split-window-right              "⇐")
      ("C-j" split-window-below-and-focus    "⇓")
      ("C-k" split-window-below              "⇑")
      ("C-l" split-window-right-and-focus    "⇒"))
     "Window Resize"
     (("H"   window-move-splitter-left       "←")
      ("J"   window-move-splitter-down       "↓")
      ("K"   window-move-splitter-up         "↑")
      ("L"   window-move-splitter-right      "→")
      ("C-H" (window-move-splitter-left 16)  "⇐")
      ("C-J" (window-move-splitter-down 16)  "⇓")
      ("C-K" (window-move-splitter-up 16)    "⇑")
      ("C-L" (window-move-splitter-right 16) "⇒")))))

(use-package evil
  :straight t
  ;; Load evil eagerly so that general is loaded eagerly.
  :demand t
  :hook (after-init . evil-mode)
  :config
  ;; Redefine C-w w to open the window hydra. We already have another
  ;; binding to move to the last window.
  (define-key evil-window-map "w" 'hydra-window/body)
  (evil-set-undo-system 'undo-redo))

(use-package evil-surround
  :straight t
  :hook ((text-mode prog-mode conf-mode) . evil-surround-mode))

(use-package dired
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)))

(provide 'core-keybindings)
