;; Settings for Clojure.

(use-package clojure-mode
  :straight t
  :general
  (cxn/major-def clojure-mode-map
    "a"     '("add arity"       . clojure-add-arity)
    "p"     '("promote fn lit"  . clojure-promot-fn-literal)
    "k"     '("toggle kw/str"   . clojure-toggle-keyword-string)
    "l"     '("move to let"     . clojure-move-to-let)
    "v"     '("align"           . clojure-align)
    "c"  (cons "convert" (make-sparse-keymap))
    "cl"   '("to list"          . clojure-convert-collection-to-list)
    "cq"   '("to 'list"         . clojure-convert-collection-to-quoted-list)
    "cv"   '("to vector"        . clojure-convert-collection-to-vector)
    "cs"   '("to set"           . clojure-convert-collection-to-set)
    "cm"   '("to map"           . clojure-convert-collection-to-map)
    "t"  (cons "thread" (make-sparse-keymap))
    "tt"   '("thread"           . clojure-thread)
    "tf"   '("thread first all" . clojure-thread-first-all)
    "tl"   '("thread last all"  . clojure-thread-last-all)
    "y"  (cons "cycle" (make-sparse-keymap))
    "yn"   '("cycle not"        . clojure-cycle-not)
    "yi"   '("cycle if not"     . clojure-cycle-if)
    "yw"   '("cycle when not"   . clojure-cycle-if)
    "r"  (cons "repl" (make-sparse-keymap))
    "rr"   '("select"           . cider-selector)
    "rl"   '("log"              . cider-log)
    "rn"   '("run"              . cider-run)
    "rs"   '("scratch"          . cider-scratch)
    "rd"   '("doc"              . cider-doc)
    "rD"   '("clj doc"          . cider-clojuredocs)
    "rc" (cons "connect" (make-sparse-keymap))
    "rcc"  '("connect"          . cider-connect)
    "rcj"  '("connect clj"      . cider-connect-clj)
    "rcs"  '("connect cljs"     . cider-connect-cljs)
    "rcS"  '("connect clj&cljs" . cider-connect-clj&cljs)
    "rca"  '("connect sib cljs" . cider-connect-sibling-cljs)
    "rcA"  '("connect sib clj"  . cider-connect-sibling-clj)
    "rj" (cons "jack in" (make-sparse-keymap))
    "rji"  '("jack in"          . cider-jack-in)
    "rjj"  '("jack in clj"      . cider-jack-in-clj)
    "rjs"  '("jack in cljs"     . cider-jack-in-cljs)
    "rjS"  '("jack in clj&cljs" . cider-jack-in-cljs&cljs)))

(use-package apheleia
  :general
  (cxn/major-def clojure-mode-map
    "f" '("format buffer" . apheleia-format-buffer)))

(use-package cider
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'cider-repl-mode 'emacs)))

(provide 'mode-clojure)
