(require 'core-functions)

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
  (general-define-key "M-x" 'counsel-M-x)

  (general-create-definer cxn/ctrl-x-def :prefix "C-x")

  (cxn/ctrl-x-def
   "b"   'counsel-switch-buffer
   "d"   'dired
   "f"   'counsel-find-file
   "i"   'hydra-parens/body
   "g"   'magit-status
   "r"   'counsel-rg
   "t"   'vterm
   "w"   'hydra-window/body
   "x"   'hydra-ui/body
   "z"   'counsel-fzf)

  (general-create-definer cxn/ctrl-c-def :prefix "C-c")
  (cxn/ctrl-c-def "" nil)

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

    "i"     '("parens"      . hydra-parens/body)
    "m"     (cons "major" (make-sparse-keymap))
    "W"     '("window+"     . hydra-window/body)
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
    "wmh"   '("←" . evil-window-left)
    "wmj"   '("↓" . evil-window-down)
    "wmk"   '("↑" . evil-window-up)
    "wml"   '("→" . evil-window-right)
    "ws"  (cons "split" (make-sparse-keymap))
    "wsh"   '("←" . split-window-right)
    "wsj"   '("↓" . split-window-below-and-focus)
    "wsk"   '("↑" . split-window-below)
    "wsl"   '("→" . split-window-right-and-focus)
    )
  )

(use-package pretty-hydra
  :straight t
  :demand t
  :config
  (pretty-hydra-define
    hydra-window
    (:color pink :quit-key "q")
    ("Window"
     (("d"  delete-window                       "kill")
      ("D"  delete-other-windows                "kill others")
      ("<"  shrink-window-if-larger-than-buffer "shrink fit")
      ("="  balance-windows                     "balance"))

     "Switch"
     (("h" evil-window-left  "←")
      ("j" evil-window-down  "↓")
      ("k" evil-window-up    "↑")
      ("l" evil-window-right "→"))

     "Split"
     (("C-h" split-window-right           "←")
      ("C-j" split-window-below-and-focus "↓")
      ("C-k" split-window-below           "↑")
      ("C-l" split-window-right-and-focus "→"))

     "Resize"
     (("H" window-move-splitter-left  "←")
      ("J" window-move-splitter-down  "↓")
      ("K" window-move-splitter-up    "↑")
      ("L" window-move-splitter-right "→"))))

  (pretty-hydra-define
    hydra-ui
    (:color red :quit-key "q")
    ("Display"
     (("l" display-line-numbers-mode "line numbers"     :toggle t)
      ("w" whitespace-mode           "whitespace"       :toggle t)
      ("v" visual-line-mode          "visual line mode" :toggle t)
      ("r" toggle-truncate-lines     "truncate lines"))
     "Text"
     (("=" text-scale-increase       "scale +")
      ("-" text-scale-decrease       "scale -")
      ("0" (text-scale-adjust 0)     "reset"))))

  (pretty-hydra-define
    hydra-parens
    (:color pink :quit-key "q")
    ("Parens"
     (("d"   sp-kill-sexp            "kill")
      ("D"   sp-kill-hybrid-sexp     "kill to end")
      ("b"   sp-backward-kill-sexp   "kill behind")
      ("C-l" sp-forward-slurp-sexp   "slurp fwd")
      ("C-h" sp-backward-slurp-sexp  "slurp bwd")
      ("M-l" sp-forward-barf-sexp    "barf fwd")
      ("M-h" sp-backward-barf-sexp   "barf bwd")
      ("t"   sp-transpose-sexp       "transpose"))
     "Wrap"
     (("("   wrap-with-paren         "()")
      ("["   wrap-with-bracket       "[]")
      ("{"   wrap-with-brace         "{}")
      ("<"   wrap-with-angle         "<>")
      ("'"   wrap-with-single-quote  "'")
      ("\""  wrap-with-double-quote  "\"")
      ("`"   wrap-with-back-quote    "`")
      ("u"   sp-unwrap-sexp          "unwrap fwd")
      ("U"   sp-backward-unwrap-sexp "unwrap bwd"))
     "Move"
     (("a"   sp-beginning-of-sexp   "⇤ start")
      ("e"   sp-end-of-sexp         "⇥ end")
      ("j"   sp-down-sexp           "↘ down")
      ("J"   sp-backward-down-sexp  "↙ down")
      ("k"   sp-up-sexp             "↗ up")
      ("K"   sp-backward-up-sexp    "↖ up")
      ("l"   sp-forward-sexp        "→ fwd")
      ("h"   sp-backward-sexp       "← bwd")
      (","   sp-previous-sexp       "⇐ prev")
      ("."   sp-next-sexp           "⇒ next")
      ("="   sp-forward-symbol      "▶ symbol fwd")
      ("-"   sp-backward-symbol     "◀ symbol bwd")))))

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
