(setq cxn/org-directory      "~/org/"
      cxn/org-roam-directory (concat cxn/org-directory "notes/")
      cxn/org-file-inbox     (concat cxn/org-directory "inbox.org")
      cxn/org-file-ctrl      (concat cxn/org-directory "ctrl.org"))

(progn
  (defmacro +org-emphasize (fname char)
    "Make function for setting the emphasis in org-mode."
    `(defun ,fname () (interactive)
	    (org-emphasize ,char)))
  
  (+org-emphasize org-bold ?*)
  (+org-emphasize org-code ?~)
  (+org-emphasize org-italic ?/)
  (+org-emphasize org-clear ? )
  (+org-emphasize org-strike-through ?+)
  (+org-emphasize org-underline ?_)
  (+org-emphasize org-verbatim ?=))

(defun set-org-styles ()
  "Set the preferred buffer styles for org-mode."
  (setq left-margin-width 4
	right-margin-width 4)

  (dolist (face '((org-document-title . 1.4)
                  (org-level-1        . 1.3)
                  (org-level-2        . 1.2)
                  (org-level-3        . 1.1)
                  (org-level-4        . 1.1)
                  (org-level-5        . 1.0)
                  (org-level-6        . 1.0)
                  (org-level-7        . 1.0)
                  (org-level-8        . 1.0)))
    (set-face-attribute (car face) nil
		        :weight 'bold
		        :height (cdr face)))

  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(use-package org
  :init
  (setq
   org-directory              cxn/org-directory
   org-default-notes-file     cxn/org-file-inbox
   org-auto-align-tags        nil
   org-catch-invisible-edits 'show-and-error
   org-hide-emphasis-markers  t
   org-pretty-entities        t
   org-special-ctrl-a/e       t
   org-tags-column            0
   org-insert-heading-respect-content t
   org-todo-keywords '((sequence "TODO(t)"
                                 "IDEA(i)"
                                 "PROJ(p)"
                                 "HOLD(h@/!)"
                                 "|"
                                 "DONE(d!)"
                                 "DROP(D@/!)")))

  :hook
  ;; Ensure text styles are displayed as configured.
  (org-mode . buffer-face-mode)
  ;; Wrap lines and allow operations on virtual lines.
  (org-mode . visual-line-mode)
  ;; Set preferred buffer styles.
  (org-mode . set-org-styles)

  :general
  (general-define-key
   :keymaps 'org-mode-map
   "s-g" 'counsel-org-goto
   "s-G" 'counsel-org-link
   "s-a" 'org-attach
   "s-p" 'org-set-property
   "s-3" 'org-roam-tag-add
   "s-#" 'org-roam-tad-delete

   "s-<space>" 'org-clear
   "s-h" 'org-toggle-heading
   "s-b" 'org-bold
   "s-i" 'org-italic
   "s-u" 'org-underline
   "s--" 'org-strikethrough
   "s-'" 'org-code
   "s-=" 'org-verbatim

   "s-d" 'org-deadline
   "s-D" 'org-schedule
   "s-t" 'org-time-stamp
   "s-T" 'org-time-stamp-inactive

   "s-j" 'outline-next-visible-heading
   "s-k" 'outline-previous-visible-heading
   "s-l" 'org-forward-heading-same-level
   "s-h" 'org-backward-heading-same-level
   "s-<backspace>" 'outline-up-heading

   "s-n" 'org-narrow-to-subtree
   "s-N" 'widen

   "s-H" 'org-promote-subtree
   "s-L" 'org-demote-subtree
   "s-K" 'org-move-subtree-up
   "s-J" 'org-move-subtree-down

   "s-x" 'org-cut-subtree
   "s-r" 'org-refile)

  (cxn/ctrl-x-def
    "C-a" 'org-agenda
    "C-c" 'counsel-org-capture
    "C-C" 'org-roam-capture)

  (cxn/leader-def
    "o"  (cons "org" (make-sparse-keymap))
    "oa" '("agenda"              . org-agenda)
    "oc" '("capture item"        . counsel-org-capture)
    "oC" '("capture note"        . org-roam-capture)
    "oR" '("search notes"        . deft))

  (cxn/major-def org-mode-map
    "m" '("edit"                 . hydra-org-edit/body)
    "g" '("go to heading"        . counsel-org-goto)
    "k" '("link to heading"      . counsel-org-link)
    "p" '("set property"         . org-set-property)
    "a" '("attach"               . org-attach)
    "t" '("add tag"              . org-roam-tag-add)
    "T" '("remove tag"           . org-roam-tag-delete)
    "i" '("insert node"          . org-roam-node-insert)
    "f" '("find node"            . org-roam-node-find)
    "d" '("deft"                 . deft)
    "M" '("toggle inline images" . org-toggle-inline-images)
    "K" '("toggle link display"  . org-toggle-link-display)
    "R" '("roam buffer"          . org-roam-buffer-toggle))

  :config
  (use-package org-capture
    :defer t
    :config
    (setq org-capture-templates
	  '(("t"  "Task" entry (file+headline org-inbox-file "Tasks") "* TODO %?\n  %i\n  %a")
	    ("c"  "ctrl.inc")
	    ("ci" "Idea" entry (file+datetree org-file-ctrl "Log") "* IDEA  %?\n%T\n")
	    ("cl" "Log" entry (file+datetree org-file-ctrl "Log") "* %?\n%T\n")
	    ("ct" "Task" entry (file+datetree org-file-ctrl "Log") "* TODO  %?\n%T\n"))))

  (use-package evil-org
    :straight t
    :after evil
    :hook (org-mode . evil-org-mode)
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package org-modern
    :straight t
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-star 'replace
	  org-modern-replace-stars (concat "◉" "○" "›" "»" "⁖" "⁘" "⁙" "►" "▻" "•")))

  (use-package org-roam
    :straight t
    :after org
    :init
    (setq org-roam-database-connector 'sqlite-builtin
	  org-roam-directory cxn/org-roam-directory
	  org-roam-db-location (concat cxn/org-roam-directory "org-roam.db"))
    (with-eval-after-load 'org (org-roam-db-autosync-mode)))

  (use-package deft
    :straight t
    :init
    (setq deft-extensions '("org")
	  deft-directory cxn/org-roam-directory
	  deft-recursive t
	  ;; Change summary parsing to look for the #+TITLE.
	  deft-strip-summary-regexp (concat "\\("
                                            "^:.+:.*\n"     ; any line with a :SOMETHING:
	                                    "\\|^#\\+.*\n"  ; any line starting with a #+
	                                    "\\|^\\*.+.*\n" ; any line where an asterisk starts the line
	                                    "\\)")))

  (advice-add 'deft-parse-title :override 
	      (lambda (file contents)
		(if deft-use-filename-as-title
		    (deft-base-filename file)
		  (let* ((case-fold-search 't)
			 (begin (string-match "title: " contents))
			 (end-of-begin (match-end 0))
			 (end (string-match "\n" contents begin)))
		    (if begin 
			(substring contents end-of-begin end)
		      (format "%s" file)))))))

(provide 'mode-org)
