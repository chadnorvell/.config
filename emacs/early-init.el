;; Defer garbage collection further back in the startup process.
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (defun reset-gc-cons-threshold ()
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1)))

;; Native compilation settings.
(when (featurep 'native-compile)
  ;; Silence compiler warnings, as they can be pretty disruptive.
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set the right directory to store the native compilation cache.
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Inhibit package initialize.
(setq package-enable-at-startup nil)

;; Inhibit resizing frame.
(setq frame-inhibit-implied-resize t)

;; Inhibit byte-compiler warnings.
(setq byte-compile-warnings nil)

;; Remove some unneeded UI elements.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
