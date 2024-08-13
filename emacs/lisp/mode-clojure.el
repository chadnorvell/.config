;; Settings for Clojure.

(use-package clojure-mode
  :straight t
  :general
  (cxn/major-def clojure-mode-map
    "v"    '("align"            . clojure-align)
    "r"  (cons "refactor" (make-sparse-keymap))
    "ra"   '("add arity"        . clojure-add-arity)
    "rp"   '("promote fn lit"   . clojure-promot-fn-literal)
    "rk"   '("toggle kw/str"    . clojure-toggle-keyword-string)
    "rl"   '("move to let"      . clojure-move-to-let)
    "c"  (cons "convert" (make-sparse-keymap))
    "cl"   '("to list"          . clojure-convert-collection-to-list)
    "cq"   '("to 'list"         . clojure-convert-collection-to-quoted-list)
    "cv"   '("to vector"        . clojure-convert-collection-to-vector)
    "cs"   '("to set"           . clojure-convert-collection-to-set)
    "cm"   '("to map"           . clojure-convert-collection-to-map)
    "h"  (cons "thread" (make-sparse-keymap))
    "ht"   '("thread"           . clojure-thread)
    "hf"   '("thread first all" . clojure-thread-first-all)
    "hl"   '("thread last all"  . clojure-thread-last-all)
    "y"  (cons "cycle" (make-sparse-keymap))
    "yn"   '("cycle not"        . clojure-cycle-not)
    "yi"   '("cycle if not"     . clojure-cycle-if)
    "yw"   '("cycle when not"   . clojure-cycle-if)))

(use-package cider
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'cider-repl-mode 'emacs))

  :general
  (general-define-key
   "s-v"   'cider-eval-buffer
   "s-t"   'cider-test-run-test)

  (cxn/major-def clojure-mode-map
    "R"    '("repl buffer"      . cider-switch-to-repl-buffer)
    "S"    '("repl scratch"     . cider-scratch)
    "e"  (cons "eval" (make-sparse-keymap))
    "eb"   '("buffer"           . cider-eval-buffer)
    "ef"   '("file"             . cider-eval-file)
    "ea"   '("all files"        . cider-eval-all-files)
    "en"   '("ns form"          . cider-eval-ns-form)
    "ed"   '("defun@"           . cider-eval-defun-at-point)
    "ee"   '("sexp"             . cider-eval-last-sexp)
    "em"   '("macro1"           . cider-macroexpand-1)
    "eM"   '("macro*"           . cider-macroexpand-all)
    "es"   '("sexp@"            . cider-eval-sexp-at-point)
    "eR"   '("s/sexp"           . cider-eval-last-sexp-and-replace)
    "ev"   '("region"           . cider-eval-region)
    "d"  (cons "docs" (make-sparse-keymap))
    "da"   '("apropos"          . cider-apropos)
    "dA"   '("apropos doc"      . cider-apropos-documentation)
    "dd"   '("doc"              . cider-doc)
    "dc"   '("clj doc"          . cider-clojuredocs)
    "dj"   '("java doc"         . cider-javadoc)
    "C"  (cons "repl connect" (make-sparse-keymap))
    "Cc"   '("connect"          . cider-connect)
    "Cj"   '("connect clj"      . cider-connect-clj)
    "Cs"   '("connect cljs"     . cider-connect-cljs)
    "Ci"   '("connect clj&cljs" . cider-connect-clj&cljs)
    "Ca"   '("connect sib cljs" . cider-connect-sibling-cljs)
    "CA"   '("connect sib clj"  . cider-connect-sibling-clj)
    "j"  (cons "repl jack in" (make-sparse-keymap))
    "jI"   '("jack in"          . cider-jack-in)
    "jj"   '("jack in clj"      . cider-jack-in-clj)
    "js"   '("jack in cljs"     . cider-jack-in-cljs)
    "ji"   '("jack in clj&cljs" . cider-jack-in-clj&cljs)
    "t"  (cons "test" (make-sparse-keymap))
    "tt"   '("run test"         . cider-test-run-test)
    "tn"   '("run ns tests"     . cider-test-run-ns-tests)
    "tp"   '("run proj tests"   . cider-test-run-project-tests)))

(use-package apheleia
  :general
  (cxn/major-def clojure-mode-map
    "f"    '("format buffer"    . apheleia-format-buffer)))

(provide 'mode-clojure)
