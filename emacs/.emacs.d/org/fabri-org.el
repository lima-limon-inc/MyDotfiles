(setq org-default-dir (my-emacs-dir "org/work/"))
(setq org-default-notes-file (concat org-default-dir "notes.org"))

(setq org-agenda-files (list org-default-notes-file))

(defun prompt-file-name ()
  (concat org-default-dir (read-string "File name: ") ".org"))

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


(provide 'fabri-org)
