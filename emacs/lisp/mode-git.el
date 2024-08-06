(use-package magit
  :straight t
  :general
  (cxn/leader-def
    "g"   (cons "git" (make-sparse-keymap))
    "gg"    '("status"       . magit-status)
    "gb"    '("blame"        . magit-blame)
    "gd"    '("diff"         . magit-diff)
    "gs"    '("stage file"   . magit-stage-file)
    "gu"    '("unstage file" . magit-unstage-file)))

(provide 'mode-git)
