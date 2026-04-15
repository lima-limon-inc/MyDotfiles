; Create org directories if missing
(let (
      (my-org-dir (my-emacs-dir "org/work/"))
      (my-non-work-dir (my-emacs-dir "org/non-work/")))
      (unless (file-directory-p my-org-dir)
        (make-directory my-org-dir))
      (unless (file-directory-p my-non-work-dir)
        (make-directory my-non-work-dir))
      (setq org-directory (my-emacs-dir "org/work/")))
(setq org-default-notes-file (concat org-directory "notes.org"))

;; (setq org-agenda-files (list org-default-notes-file))
(setq org-agenda-files (list org-directory))


; Sort by todo state
(setq org-agenda-sorting-strategy
      '((agenda todo-state-up priority-down time-up)
        (todo todo-state-up priority-down category-keep)
        (tags todo-state-up priority-down category-keep)
        (search category-keep)))

;; Only show top level todo items
(setq org-agenda-todo-list-sublevels nil)


(defun prompt-file-name (type)
  (let ((dir (cond
              ((eq type 'work) (my-emacs-dir "org/work/"))
              ((eq type 'non-work) (my-emacs-dir "org/non-work/")))))
    (concat dir (read-string "File name: ") ".org")))

(setq org-capture-templates
      `(
        ("t" "New todo" plain (file ,(lambda () (prompt-file-name 'work)))
         "* TODO %? [%]
:PROPERTIES:
:CREATED: %U
:ISSUE:
:PR:
:END:
")
        ("n" "Non-work todo" plain (file ,(lambda () (prompt-file-name 'non-work)))
         "* TODO %? [%]
:PROPERTIES:
:CREATED: %U
:END:
")
        ))

(defhydra org-functions ()
  "Org related functions"
    ("a" org-agenda "Agenda" :exit t)
    ("c" org-capture "Capture" :exit t)
    ("u" org-todo "TODO States" :exit t)
    ("t" org-set-tags-command "Tag" :exit t)
    ("d" org-deadline "Deadline" :exit t)
    )
(evil-leader/set-key "o" 'org-functions/body)

; Log done tasks
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(w!)" "BLOCKED(b@/!)" "REVIEW(r!)" "UNREPLIED(u!)" "|" "DONE(d)" "CANCELED(c@)")))

(setq org-todo-keyword-faces
      '(("BLOCKED" . modus-themes-fg-blue)
        ("WIP" . modus-themes-fg-yellow)
        ("UNREPLIED" . "orange")
        ("REVIEW" . "purple")
        ("CANCELED" . (:foreground "#44bc44" :underline (:color "red")))
        ))

; Used tags
(if (equal fabri-profile 'work)
    (setq org-tag-alist '(
                        ; Repos de Miden
                        (:startgroup . nil)
                        ("vm" . ?v)
                        ("compiler" . ?C)
                        ("client" . ?c)
                        ("midenup" . ?m)
                        ("faucet" . ?f)
                        (:endgroup . nil)
                        ; Tipo de tarea
                        ("review" . ?r)
                        ("pr" . ?p)
                        ("discussion" . ?d)
                        ))
    (setq org-tag-alist '(
                        ; Materias de la facu
                        (:startgroup . nil)
                        ("is1" . ?i)
                        ("numerico" . ?n)
                        ("fisica" . ?f)
                        (:endgroup . nil)
                        ("tp" . ?t)
                        ("examen" . ?e)
                        ("tarea" . ?h)
                        )))



; Keybind
;================================= Org agenda ==================================
(use-package org-agenda
  :ensure nil
  :bind
  (:map org-agenda-mode-map
        ("j" . org-agenda-next-line)
        ("k" . org-agenda-previous-line)
        ("l" . right-char)
        ("h" . left-char)))

(provide 'fabri-org)
