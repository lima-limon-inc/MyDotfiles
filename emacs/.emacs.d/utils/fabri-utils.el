; Get current-day format
(defun current-day ()
  (format-time-string "%Y-%m-%d" (current-time))
  )

; Toggle from relative to global numbers
(defun relative-number-toggle ()
  (interactive)
  (if display-line-numbers
      (if (eq display-line-numbers 'relative)
          (setq display-line-numbers t)
        (setq display-line-numbers 'relative))
    (setq display-line-numbers 'relative)))

;; Get string for subdirectory in emacs directory
(defun my-emacs-dir (directory)
  (concat user-emacs-directory directory))


;;;Cancel backspace in order to force me to not move my hands
(defun nothing-delete ()
  "Functions that does nothing"
  (interactive)
  (message "Do not delete with backspace. It hurts your fingers!. Try using 'diw' or 'x' to delete stuff")
  )

(defun nothing-move ()
  "Functions that does nothing"
  (interactive)
  (message "Use hjkl to move")
  )

; Remove all advice
(defun remove-all-advice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun isCharUpper (char)
  "Returns true if char is uppercase"
  (equal (upcase char) char)
  )

(defun separateCaptial (word)
  "Separates word based on their capital letters.
For example:
thisIsAWord -> this Is A Word
"
  (let (
        (transformedWord "")
        (strLength (length word))
        (pos 0)
        )
    (while (> strLength pos)
      (and (isCharUpper (aref word pos)) ;; If the character is uppercase
	 (> pos 0)                     ;;AND it is not the first one
	 (setq transformedWord (concat transformedWord " "))
	 )
      (setq transformedWord (concat transformedWord (make-string 1 (aref word pos))))
      (setq pos (+ pos 1))
      )
    transformedWord
    )
  )

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (let (
        (current-file (buffer-file-name))
        )
    (message current-file)
    (kill-new current-file)
    )
  )
(defalias 'current-file 'show-file-name)

;; Create temporary dir
(defun tmp-dir ()
  (interactive)
    (find-file (make-temp-file (user-real-login-name) 't))
  )

;; Remove all comments from current buffer
(defun remove-comments ()
  (interactive)
  (progn
    (goto-char (point-min))
    (let (kill-ring)
      (comment-kill (count-lines (point-min) (point-max))))
    )
  )

;; Spanish symbols
(defun place-question-mark (symbolType)
  (interactive
   (list
    (string (read-char "(q)uestion (?) or (e)xclamation (!)"))
    )
   )
  (let*
      (
       (beginSymbol (if (equal symbolType "q") "¿" "¡"))
       (endSymbol (if (equal symbolType "q") "?" "!"))
       (case-fold-search nil)
       (max-distance (save-excursion
		   (beginning-of-line)
		   (point)
		   )
		 )
       (no-delimiter (cons -1 -1))
       (possible-places (list
		     no-delimiter
		     (cons (save-excursion (search-backward "," max-distance t)) 1)
		     (cons (save-excursion (search-backward ";" max-distance t)) 1)
		     (cons (save-excursion (search-backward "." max-distance t)) 1)
		     (cons (save-excursion (re-search-backward "[[:upper:]]" max-distance t)) 0)
		     )
		    )
       (closest-delimiter
        (seq-map (lambda (value)
	         (if (car value)
		   value
		 (cons 0 (cdr value))
		 )
	         )
	       possible-places
	       )
        )
       ;;Give the position of the max element
       (distance-only
        (seq-map (lambda (value)
	         (car value))
	       closest-delimiter)
        )
       (max-element-position
        (cl-position (seq-max distance-only) distance-only)
        )
       (max-element
        (nth max-element-position closest-delimiter)
        )
       )
    (if (/= (car max-element) -1)
        (progn
	(save-excursion
	  (insert endSymbol)
	  (goto-char (+ (cdr max-element) (car max-element)))
	  (if (/= (cdr max-element) 0)
	      (search-forward-regexp "[^[:space:]]")
	    )
	  ;; why do i have to do this? no one knows. it simply works
	  (backward-char (cdr max-element))
	  (insert beginSymbol)
	  )
	(forward-char 1)
	)
      (print "No commas, or dots found")
      )
    )
  )

;; Crontab:
;;;Source: https://emacs.stackexchange.com/a/10080/39379
(defun crontab-e ()
    "Run `crontab -e' in a emacs buffer."
    (interactive)
    (with-editor-async-shell-command "crontab -e"))

;; Xah open in external app
(defun xah-open-in-external-app (&optional file)
  "Open the current file or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not file) (list (buffer-file-name)))
           (file (list file)))))

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) myFileList) ) ) ) ) )
(defalias 'open-with 'xah-open-in-external-app)

(defun pretty-comment-delimiter (message)
  (interactive
   (list
    (read-string "Comment message: ")
    )
   )
  (let* (
        (message (concat " " message " "))
        (column-limit fill-column )
        (message-length (length message))
        (comment-length (length comment-start))
        (empty-space (- column-limit message-length))
        (right-space (/ empty-space 2))
        (left-space (- right-space comment-length))
        )
    (progn
      (insert comment-start)
      (insert (make-string left-space ?=))
      (insert message)
      (insert (make-string right-space ?=))
      )
    )
  )

(defmath normalDistribution (x)
  "Calculate the lower tail part of a normal distibution"
  (interactive 1 "Norm. Dist.")
  (- 1
     (utpn x 0 1)
     )
  )

(defmath weibullDistribution (x c a)
  "Calculate the lower tail part of a weibull distibution. First the
X value, then the c value and then the a value."
  (interactive 3 "Weibull Dist.")
  (- 1
     (exp
        (* -1
	 (expt (/ x a) c)
	 )
     )
  )
  )

(defmath exponentialDistribution (x lam)
  "Calculate the lower tail part of an exponential distibution. First the
X value, then the lambda value aka the mean."
  (interactive 2 "Weibull Dist.")
  (- 1
     (exp
      (*
       (* -1
	lam
	)
       x
       )
      )
     )
  )


;;; Definition stored by Calc on Fri Sep  6 17:11:18 2024
(put 'calc-define 'calc-reset '(progn
 (define-key calc-mode-map "zr" 'calc-reset)
))

;;; Definition stored by Calc on Fri Sep  6 17:12:29 2024
(put 'calc-define 'calc-normalDistribution '(progn
 (define-key calc-mode-map "zn" 'calc-normalDistribution)
))

;;; Definition stored by Calc on Thu Sep 12 11:44:47 2024
(put 'calc-define 'calc-frac-mode '(progn
 (define-key calc-mode-map "zf" 'calc-frac-mode)
))

(put 'calc-define 'calc-weibullDistribution '(progn
 (define-key calc-mode-map "zw" 'calc-weibullDistribution)
))

(put 'calc-define 'calc-exponentialDistribution '(progn
 (define-key calc-mode-map "ze" 'calc-exponentialDistribution)
))





(provide 'fabri-utils)
