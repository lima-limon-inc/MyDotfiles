(setq org-directory (my-emacs-dir "org/work/"))
(setq org-default-notes-file (concat org-directory "notes.org"))

;; (setq org-agenda-files (list org-default-notes-file))
(setq org-agenda-files (list org-directory))

(defun prompt-file-name ()
  (concat org-directory (read-string "File name: ") ".org"))

(setq org-capture-templates
      `(
        ("t" "New todo" plain
                (file ,(function prompt-file-name)) "* TODO %?"))
        )

(defhydra org-functions ()
  "Org related functions"
    ("a" org-agenda "Agenda")
    ("c" org-capture "Capture")
    )
(evil-leader/set-key "o" 'org-functions/body)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))


(provide 'fabri-org)
