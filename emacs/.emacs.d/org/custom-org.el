; Funcion que usa org-map-entries para devolver la informacion
; Aca creo el objeto tarea que me interesa para despues.
(defun org-mode-task-deserializer ()
  (let (
        (heading (org-get-heading))
        )
    (list
     :heading (org-get-heading t nil t t)
     )))

;; (org-map-entries
;;  'org-mode-task-deserializer
;;  t
;;  (org-agenda-files))
