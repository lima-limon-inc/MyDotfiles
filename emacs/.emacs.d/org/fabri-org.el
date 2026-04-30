;; -*- lexical-binding: t; -*-

; Create org directories if missing
(setq fabri-org/note-types (list
                           'work      ; Work related tasks
                           'personal  ; Personal stuff
                           'school    ; College
                           'later     ; Notes that aren't TODO's, simply captured to not forget things.
                           ))

; TODO: Don't do these one by one
(defun fabri-org/note-dir (type)
  (pcase type
    ('work     (my-emacs-dir "org/work/"))
    ('personal (my-emacs-dir "org/personal/"))
    ('school   (my-emacs-dir "org/school/"))
    ('later    (my-emacs-dir "org/later/"))
    ))

(defun fabri-org/default-type ()
  ; High level stuff
  (pcase fabri-profile
    ('work     'work)
    ('personal 'school)
    ))

(let (
      (dirs (mapcar
             (lambda (type) (fabri-org/note-dir type))
             fabri-org/note-types)))
  (mapc
   (lambda (dir) (unless (file-directory-p dir) (make-directory dir)))
   dirs)
  (setq org-directory (list
                       (fabri-org/note-dir (fabri-org/default-type))
                       (fabri-org/note-dir 'personal)
                       )))
(setq org-default-notes-file (concat (fabri-org/note-dir 'personal) "notes.org"))

;; (setq org-agenda-files (list org-default-notes-file))
(setq org-agenda-files org-directory)


; Sort by todo state
(setq org-agenda-sorting-strategy
      '((agenda todo-state-up priority-down deadline-up)
        (todo todo-state-up priority-down deadline-up category-keep)
        (tags todo-state-up priority-down deadline-up category-keep)
        (search category-keep)))

;; Only show top level todo items
(setq org-agenda-todo-list-sublevels nil)


(defun fabri-org/prompt-file-name (type)
  (let ((dir (fabri-org/note-dir type)))
    (concat dir (read-string "File name: ") ".org")))

(defun fabri--org/file-header (type)
  (let ((tag-name (symbol-name type))
        (header   (pcase type
                    ('work     "
:PROPERTIES:
:CREATED: %U
:ISSUE: 
:PR: 
:END:
")
                    (_         "
:PROPERTIES:
:CREATED: %U
:END:
"))))
  (format "#+FILETAGS: :%s:
* TODO %%? [%%]
%s
" tag-name header)))

(defun fabri-org/create-template (type)
  (let ((shortcut       (substring (symbol-name type) 0 1))
        (title          (format "New %s task" (symbol-name type))))
  `(,shortcut  ,title plain (file ,(lambda () (fabri-org/prompt-file-name type))) ,(fabri--org/file-header type))))


(setq org-capture-templates
      (mapcar 'fabri-org/create-template fabri-org/note-types))

(defhydra org-functions ()
  "Org related functions"
    ("a" org-agenda "Agenda" :exit t)
    ("c" org-capture "Capture" :exit t)
    ("u" org-todo "TODO States" :exit t)
    ("t" org-set-tags-command "Tag" :exit t)
    ("d" org-deadline "Deadline" :exit t)
    ("s" org-schedule "Schedule" :exit t)
    ("l" org-store-link "Store link" :exit t)
    ("m" org-timestamp "Insert timestamp" :exit t)
    ("i" org-insert-structure-template "Template" :exit t)
    )
(evil-leader/set-key "o" 'org-functions/body)

; Log done tasks
(setq org-log-done 'time)

(setq org-todo-keywords
      (if (equal fabri-profile 'work)
          '((sequence "TODO(t!)" "WIP(w!)" "BLOCKED(b@/!)" "REVIEW(r!)" "UNREPLIED(u!)" "|" "DONE(d)" "CANCELED(c@)"))
          '((sequence "TODO(t!)" "WIP(w!)" "BLOCKED(b@/!)" "|" "CALIFICANDO(c!)" "DONE(d)" "CANCELED(C@)"))
        ))

(setq org-todo-keyword-faces
      '(("BLOCKED"        . modus-themes-fg-blue)
        ("WIP"            . modus-themes-fg-yellow)
        ("UNREPLIED"      . "orange")
        ("REVIEW"         . "purple")
        ("CALIFICANDO"    . "purple")
        ("CANCELED"       . (:foreground "#44bc44" :underline (:color "red")))
        ))

; Used tags
(if (equal fabri-profile 'work)
    (setq org-tag-alist '(
                        ; Repos de Miden
                        ("vm" . ?v)
                        ("compiler" . ?C)
                        ("client" . ?c)
                        ("midenup" . ?m)
                        ("faucet" . ?f)
                        (:newline . nil)
                        ; Tipo de tarea
                        ("review" . ?r)
                        ("pr" . ?p)
                        ("discussion" . ?d)
                        ("research" . ?R)
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
                        (:startgroup . nil)
                        ("personal" . ?p)
                        (:endgroup . nil)
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

;================================= Org clock ==================================

(provide 'fabri-org)
