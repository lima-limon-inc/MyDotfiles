; Create "work" directory if missing
(let (
      (my-org-dir (my-emacs-dir "org/work/")))
      (unless (file-directory-p my-org-dir)
        (make-directory my-org-dir))
      (setq org-directory (my-emacs-dir "org/work/")))
(setq org-default-notes-file (concat org-directory "notes.org"))

;; (setq org-agenda-files (list org-default-notes-file))
(setq org-agenda-files (list org-directory))

;; Only show top level todo items
(setq org-agenda-todo-list-sublevels nil)


(defun prompt-file-name ()
  (concat org-directory (read-string "File name: ") ".org"))

(setq org-capture-templates
      `(
        ("t" "New todo" plain (file ,(function prompt-file-name))
         "* TODO %? [%] \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"))
        )

(defhydra org-functions ()
  "Org related functions"
    ("a" org-agenda "Agenda")
    ("c" org-capture "Capture")
    ("u" org-todo "TODO States")
    )
(evil-leader/set-key "o" 'org-functions/body)

; Log done tasks
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(w!)" "BLOCKED(b@/!)" "REVIEW(r!)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-todo-keyword-faces
      '(("BLOCKED" . modus-themes-fg-blue)
        ("WIP" . modus-themes-fg-yellow)
        ("REVIEW" . "purple")
        ("CANCELED" . (:foreground "#44bc44" :underline (:color "red")))
        ))


(provide 'fabri-org)
