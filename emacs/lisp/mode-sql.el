(use-package apheleia
  :straight t
  :config
  ;; Override this because the default contains some style args.
  ;; These should just be pulled from the pg_format config file.
  (add-to-list 'apheleia-formatters '(pgformatter . ("pg_format")))
  (add-to-list 'apheleia-mode-alist '(sql-mode . pgformatter)))

(provide 'mode-sql)
