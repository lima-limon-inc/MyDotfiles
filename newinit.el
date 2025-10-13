; Profile configuration

(setq fabri-profile
      (if (equal system-type 'gnu/linux)
	'personal
	'work))

;; Email address
(setq user-full-name "Tomas Fabrizio Orsi")
(setq user-mail-address
      (if (equal fabri-profile 'personal)
      "torsi@fi.uba.ar"
      "tomas.orsi@lambdaclass.com"
      ))

;;; Get current-day format
(defun current-day ()
  (format-time-string "%Y-%m-%d" (current-time))
  )

(defun current-day-file ()
  (interactive)
  (concat "~/Documents/LearningPath/" (current-day) ".org")
  )

; Package manager settings
(progn
  (require 'package)
  (add-to-list 'package-archives
	          '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  )


; Global look and feel

;; Font
(if (equal fabri-profile 'personal)
    (add-to-list 'default-frame-alist
                 '(font . "-1ASC-Liberation Mono-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1"))
  (set-face-attribute 'default nil :height 160)
  )

;; Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t)
  )

;; Don't use tabs, only use spaces
(setq-default indent-tabs-mode nil)

;; Use · for spaces
(setq whitespace-display-mappings
      '((space-mark ?\ [?·] [?.])))

;; Enable for programming modes
(add-hook 'prog-mode-hook #'whitespace-mode)

;; Enable everything
(setq whitespace-style
      '(face               ;; Enable highlighting using faces
        tabs               ;; highlight tab chars
        tab-mark           ;; show a printable tab mark
        spaces             ;; highlight space chars
        space-mark         ;; show a printable space mark
        trailing           ;; highlight trailing whitespace
        ))

;; Redefine hspace as leading space
(setq whitespace-hspace-regexp "\\(^ +\\)")

(with-eval-after-load 'whitespace
  ;; Make leading whitespaces follow current theme's comment colouring
  (set-face-attribute 'whitespace-hspace nil
                      :foreground (face-foreground 'font-lock-comment-face nil t)
                      :background nil)
  ;; Hide the whitespaces between words
  (set-face-attribute 'whitespace-space nil
                      :foreground (face-background 'default nil t)
                      :background nil)
  ;; Don't chanege anything about trailing whitespaces, just leave it here for reference
  (set-face-attribute 'whitespace-trailing nil
                      :foreground nil
                      :background nil)
  ;; Make leading tabs follow current theme's comment colouring
  (set-face-attribute 'whitespace-tab nil
                      :foreground (face-foreground 'font-lock-comment-face nil t)
                      :background nil))

;; Post
(defun config-is-done ()
  (message "All done")
  )

;; Menus
;;;Disable scrollbar, menu and tool bar
(when (display-graphic-p)
    (progn
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      )
  )

;; Scrolling
;;; Makes scrolling vim like
(setq scroll-step 1)
(setq scroll-margin 1)

;; Line numbers
;;; Line and Relative line numbers
(global-display-line-numbers-mode 1)

(setq display-line-numbers-type 'relative)

(defun relative-numbers ()
  (interactive)
  (customize-option 'display-line-numbers-type))

;; Load server
(load "server")
(unless (server-running-p) (server-start))

;;Tab size
(setq-default tab-width 5)
(setq c-basic-offset 4)

;; Global general customizations
;;; Binds y and n to yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; Get string for subdirectory in emacs directory
(defun my-emacs-dir (directory)
  (concat user-emacs-directory directory))

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist `((".*" . ,(my-emacs-dir "backup")))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      2 ; how many of the newest versions to keep
      kept-old-versions      1) ; and how many of the old

(setq make-backup-files nil) ; stop creating ~ files


;; "Forbidden 80-column number"
(setq-default fill-column 80)


;; Different custom set variables files
(setq custom-file (my-emacs-dir "custom.el"))
(load custom-file 'noerror)

;; Confirm before killing emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;; Follow symblinks without asking
(setq vc-follow-symlinks t)

;; World clock
(setq zoneinfo-style-world-list '(
                                  ("Europe/Rome" "Rome")
                                  ("Europe/Madrid" "Madrid")
                                  ("America/Buenos_Aires" "Buenos Aires")
                                  ("America/Montevideo" "Montevideo")
			                   ))

;; Add newlines at the end of the file
(setq require-final-newline 'visit-save)

;; Misc alias
;;; Undefine
(defalias 'undefun 'fmakunbound)
;;; Reload file
(defalias 'reload-file 'revert-buffer)
;;; Fill mode alias
(defalias 'wrap-region 'fill-paragraph)
;;; Restart emacs
(defalias 'resma 'restart-emacs)
;;; Ispell
(defalias 'ispell-change-language 'ispell-change-dictionary)
(defalias 'set-ispell-language 'ispell-change-dictionary)
;; Dired
(defalias 'unix-find 'find-name-dired)
;; Find file
(defalias 'find-file-recursive 'find-name-dired)

;; Zone when idle
(require 'zone)
(zone-when-idle 600)
(setq zone-programs (remove 'zone-pgm-random-life zone-programs))

;; Byte compile warnings
(setq byte-compile-warnings nil)

;;Async shell command
;;;Source: https://emacs.stackexchange.com/a/58341/39379
(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
;;; Multiple async processes
(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)


;; Editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

; Emacs built in gadgets 
;; Diary
(appt-activate)
(defun check-file-exists-warn (file)
  (let
      (
       (diary-dir (my-emacs-dir "diary")))
    (unless (file-exists-p diary-dir)
      (display-warning 'warning (format "No hay archivo en %s" diary-dir)))
    )
  )

;; (advice-add 'config-is-done
;; 	  :after #'(lambda () (check-file-exists-warn (my-emacs-dir "diary"))))

;; Calendar

;;; Set calendar style
(require 'calendar) 
(calendar-set-date-style 'european)

;;; Set sunset times
(setq calendar-latitude -34.37)
(setq calendar-longitude -58.38)
(setq calendar-location-name "Buenos Aires, Argentina")

;; Start the week on monday
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday

;; Dired
(setq dired-listing-switches "-alhF")
;;; Create new files with leader o
(evil-leader/set-key-for-mode 'dired-mode "o" 'dired-create-empty-file)
(evil-leader/set-key-for-mode 'dired-mode "i" 'open-with)
;;;Enable drag and drop
(setq dired-mouse-drag-files t)
;;;Unbinds g key so that i can use gt to change window
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "g") #'nil)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "p") #'revert-buffer)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "C-o") #'nil)))
(add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "v") #'evil-visual-char)))
(evil-leader/set-key-for-mode 'dired-mode "u" 'dired-toggle-read-only)
(evil-leader/set-key-for-mode 'dired-mode "c" 'projectile-compile-project)
;;; Dired will try to guess destination. If you have to open windows, then it will use the one next to it
(setq dired-dwim-target t)

;; Term
(evil-leader/set-key-for-mode 'term-mode "p" 'term-paste)

;; Shell mode
(evil-leader/set-key-for-mode 'shell-mode "p" 'comint-previous-input)
(evil-leader/set-key-for-mode 'shell-mode "n" 'comint-next-input)

;; Info mode
(evil-leader/set-key-for-mode 'Info-mode "w" 'Info-follow-nearest-node)

;; Grep mode
(evil-leader/set-key-for-mode 'grep-mode "g" 'recompile)
(evil-leader/set-key-for-mode 'grep-mode "n" 'next-error)

;; Compilation mode
(evil-leader/set-key-for-mode 'compilation-mode "n" 'next-error)
(evil-leader/set-key-for-mode 'compilation-mode "c" 'compile)
(evil-leader/set-key-for-mode 'compilation-mode "g" 'recompile)
;; "Next" will go to errors, will skip warnings
(setq compilation-skip-threshold 2)

;; Proceed
(defalias 'top 'proced)
(setq-default proced-auto-update-flag t)
(setq proced-auto-update-interval 2)
(setq proced-enable-color-flag t)

;; Artist mode
(advice-add 'artist-mode :before #'(lambda (x) (progn
				         (when (bound-and-true-p whitespace-mode)
					 (whitespace-mode))
				         (turn-off-evil-mode)
				         )))

(advice-add 'artist-mode-off :after #'(lambda () (progn
					  (when (bound-and-true-p whitespace-mode)
					    (whitespace-mode))
					  (turn-on-evil-mode)
					  )))

;; Emacs - Calc
(defhydra hydra-calc ()
  "Calcs"
  ("c" calc "Calc")
  ("v" full-calc "Full calc")
  )
;; (evil-leader/set-key "" hydra-calc/body)

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

(add-hook 'calc-mode-hook (lambda () (local-set-key (kbd "g t") 'tab-bar-switch-to-next-tab)))
(add-hook 'calc-mode-hook (lambda () (local-set-key (kbd "g T") 'tab-bar-switch-to-prev-tab)))


; Global keybindings
;; Evil
;;;Unbinds evil's ret/space/tab key so that is uses the normal emacs key
(with-eval-after-load 'evil-maps
  ;; (define-key evil-motion-state-map (kbd "SPC") nil)
  ;; (define-key evil-motion-state-map (kbd "RET") nil)
  ;; (define-key evil-motion-state-map (kbd "TAB") nil)
  (define-key evil-motion-state-map (kbd "\\") nil)
  (define-key evil-insert-state-map (kbd "DEL") nil)
  (define-key evil-insert-state-map (kbd "DEL") nil)
  (define-key evil-insert-state-map (kbd "C-i") nil)
  (define-key evil-normal-state-map (kbd "C-i") nil)
  )

;;; enable global-evil-leader-mode before you enable evil-mode, otherwise evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, …).
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  )

(use-package evil
  :init
  (progn
    ;;; Disable control i feature (compatibilty with org mode)
    (setq evil-want-C-i-jump nil)
    (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
    )
  :config
    (evil-mode 1)
  )

;;; Install Hydra first, I use quite often

(use-package hydra
  :config
  )

;;;Makes Ctrl R work
(evil-set-undo-system 'undo-redo)

;;; Sets the leader key to be space
(evil-leader/set-leader "<SPC>")

;;; Splitting window keys
(evil-leader/set-key "v" 'split-window-horizontally)
(evil-leader/set-key "s" 'split-window-vertically)
;;; Move between said windows
(evil-leader/set-key "h" 'evil-window-left)
(evil-leader/set-key "l" 'evil-window-right)
(evil-leader/set-key "k" 'evil-window-up)
(evil-leader/set-key "j" 'evil-window-down)
;;; Shell
(evil-leader/set-key "&" 'async-shell-command)

;; Macos Maximize
(when (equal fabri-profile 'work)
  (defhydra hydra-macos ()
    "Macos functions"
    ("t" toggle-frame-maximized "maximizar")
    )

  (evil-leader/set-key "=" 'hydra-macos/body)
  )

;;; Run :wa with les typing
(evil-leader/set-key "RET" 'evil-write-all)

;;; Find files
(defhydra hydra-files ()
  "Various search file related functions"
  ("f" find-file "find-file")
  ("q" projectile-find-file "projectile find file")
  )

;;;Search files a la vim /
(evil-leader/set-key "f" 'hydra-files/body)

;;; Grepping
(defhydra hydra-grep ()
  "Various search file related functions"
  ("g" counsel-rg "Counsel rg")
  ("q" projectile-grep "projectile grep")
  )

;;;Grep like functions
(evil-leader/set-key "g" 'hydra-grep/body)

;;;New tab
(defhydra hydra-tabs ()
  "Tabs"
  ("e" tab-new "New tab")
  ("c" tab-bar-close-tab "Close tab")
  )

;;;Close tab
(evil-leader/set-key "e" 'hydra-tabs/body)
;;;Kill buffer
(evil-leader/set-key "b" 'kill-current-buffer)
;;;Changes the search to swiper
(define-key evil-normal-state-map (kbd "/") 'swiper)
;;;Map meta x to leader :. A sort of mix of vim and emacs
(evil-leader/set-key ";" 'execute-extended-command)
;;; Buffer related functions
(defhydra hydra-buffer ()
  "Various search file related functions"
  ("r" switch-to-buffer "Switch to buffer")
  ("e" jump-to-register "Jump to register")
  ("n" next-buffer "Next buffer")
  ("p" previous-buffer "Previous buffer")
  )

(evil-leader/set-key "r" 'hydra-buffer/body)
;;;Comment a region out imitating tim pope's plugin
(define-key evil-normal-state-map (kbd "g c") 'comment-or-uncomment-region)
;;;Indent region
(define-key evil-normal-state-map (kbd "g i") 'indent-region)
;;Open the switch tab menu
(define-key evil-normal-state-map (kbd "g s") 'tab-switch)
;;;Open the terminal
(defhydra hydra-shells ()
  "Various shells"
  ("9" eshell "Esheel")
  ("o" shell "Shell")
  ("l" term "Term")
  )
(evil-leader/set-key "9" 'hydra-shells/body)

;;;Cancel backspace in order to force me to not move my hands
(defun nothing-delete ()
  "Functions that does nothing"
  (interactive)
  (message "Do not delete with backspace. It hurts your fingers!. Try using 'diw' or 'x' to delete stuff")
  )

(define-key evil-normal-state-map (kbd "DEL") 'nothing-delete)

(define-key evil-insert-state-map (kbd "C-<backspace>") 'nothing-delete)

(define-key evil-normal-state-map (kbd "C-<backspace>") 'nothing-delete)

(defun nothing-move ()
  "Functions that does nothing"
  (interactive)
  (message "Use hjkl to move")
  )

(define-key evil-normal-state-map (kbd "<up>") 'nothing-move)
(define-key evil-normal-state-map (kbd "<down>") 'nothing-move)
(define-key evil-normal-state-map (kbd "<right>") 'nothing-move)
(define-key evil-normal-state-map (kbd "<left>") 'nothing-move)

;;Registers
;;; Register with files I open often
(set-register ?e (cons 'file (my-emacs-dir "init.el")))

;;;Register with directories to college subject
(defun TPSdir (materia)
  (concat college-directory materia "/TPS"))

(set-register ?d (cons 'file "~/Downloads/"))

(when (equal fabri-profile 'personal)
  )
(when (equal fabri-profile 'work)
  )


; Auxiliary function
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

(if (equal fabri-profile 'personal)
    (defun org-mode-auto-insert (title tag)
      (interactive
       (list
        (read-string "Title (default is file name): ")
        (read-string "Filetag (if any): ")
        )
       )
      (let
	(
	 (org-title (if (equal title "")
		      (concat "#+title: " (separateCaptial (file-name-sans-extension (buffer-name))) "")
		    (concat "#+title: " title "")))
	 )
        (if (equal tag "")
	  nil
	(insert (concat "#+FILETAGS: " ":" tag ":")))
        (insert "\n")
        (insert org-title)
        )
      )
  (defun org-mode-auto-insert (title)
    (interactive
     (list
      (read-string "Title (default is date): ")
      )
     )
    (let
        (
         (org-title (if (equal title "")
		    (concat "* " (current-day) "")
		  (concat "* " title "")))
         )
      (insert org-title)
      (insert "\n")
      (insert "** Misc")
      (insert "\n")
      (insert "*** TODO Daily Meeting")
      )
    )
  )

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

;; Highlight sql words
(defun point-in-comment ()
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun my-capitalize-all-mysql-keywords ()
  (interactive)
  (require 'sql)
  (save-excursion
    (dolist (keywords sql-mode-mysql-font-lock-keywords) 
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (unless (point-in-comment)
          (goto-char (match-beginning 0))
          (upcase-word 1))))))

(defalias 'sql-capitalize 'my-capitalize-all-mysql-keywords)

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
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c /") #'place-question-mark)))
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key (kbd "C-c /") #'place-question-mark)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c /") #'place-question-mark)))

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

;; Comment delimiter
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

; Programming configuration
;; Color line
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Enable auto insert mode
(auto-insert-mode t)

(use-package magit
  :init
  (setq transient-default-level 6)
  :config
  (progn
    (evil-leader/set-key "." 'magit-status)
    (setq magit-blame-styles
          '((margin
             (margin-width . 52)
             (margin-format . ("%H %a %f"))
             (margin-face . magit-blame-margin)
             (margin-body-face . magit-blame-dimmed)
             (show-message . t))))

    (defalias 'magit-co-authored-by 'git-commit-co-authored)
    (defalias 'co-authored-by 'git-commit-co-authored)
    (defalias 'mablame 'magit-blame)
    (defalias 'ma-show-current-blame 'magit-show-commit)

    (defun my-wrap-lines ()
      "Disable `truncate-lines' in the current buffer."
      (setq truncate-lines nil))

    (add-hook 'magit-status-mode-hook #'my-wrap-lines)
    (add-hook 'magit-diff-mode-hook #'my-wrap-lines)

    ;; Protect against accident pushes to main
    (defun query-magit-push-upstream (args)
      (when-let ((branch (magit-get-current-branch)))
        (when (or (string-equal branch "master") (string-equal branch "main"))
	(unless (yes-or-no-p (format "WARNING: ARE YOU SURE YOU WANT TO PUSH \"%s\" BRANCH TO \"%s\"? "
                                       branch
                                       (magit-get "branch" branch "remote")))
	  (user-error "Pushed aborted")))))

    (when (equal fabri-profile 'work)
      (advice-add 'magit-push-current-to-upstream :before #'query-magit-push-upstream)

      (advice-add 'magit-push-current-to-pushremote :before #'query-magit-push-upstream)

      (defun work-collegues (arg)
        (interactive (list (completing-read "Who? "
                                            '(
                                              ("Tomas Fabrizio Orsi <tomas.orsi@lambdaclass>" 0)
                                              ("Leandro Serra <leandro.serra@lambdaclass.com>" 1)
                                              ("Damian Ramirez <damian.ramirez@lambdaclass.com>" 2)
                                              ("Dylan Socolobsky <dylan.socolobsky@lambdaclass.com>" 3)
                                              ("Tomás Grüner <tomas.gruner@lambdaclass.com>" 4)
                                              ("Juan Bono <juan.bono@lambdaclass.com>" 5)
                                              ("Joaquin Carletti <joaquin.carletti@lambdaclass.com>" 6)
                                              ("Maximo Palopoli <maximo.palopoli@lambdaclass.com>" 7)
                                              ("Jeremias Salomon <jeremias.salomon@lambdaclass.com>" 8)
                                              ("Lucas Delgado <lucas.delgado@lambdaclass.com>" 9)
                                              ("Tomas Paradelo <tomas.paradelo@lambdaclass.com>" 10)
                                              ("Ivan Litteri <ivan.litteri@lambdaclass.com>" 11)
                                              ("Federico Borello <federico.borello@lambdaclass.com" 12)
                                              )
                                            ))
                     )
        (insert arg)
        )

      (define-key git-commit-mode-map (kbd "C-c C-n") 'work-collegues)
      )

    )
  )

(use-package git-modes
  :ensure t
  )

;; Raibow delimiters
(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'graphviz-dot-mode-hook #'rainbow-delimiters-mode)
    )
  )

;;Rainbow-mode
(use-package rainbow-mode
  :config
  (progn
    (add-hook 'c++-mode-hook 'rainbow-mode)
    (add-hook 'c-mode-hook 'rainbow-mode)
    (add-hook 'rust-mode-hook 'rainbow-mode)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
    )
  )

;; TODO Highlighter
(use-package hl-todo
  :config
  (progn
    (setq hl-todo-keyword-faces
	'(("TODO"      . "#FF0000")
	  ("FIXME"     . "#FF0000")
	  ("ERROR"     . "#FF0000")
	  ("DEBUG"     . "#A020F0")
	  ("GOTCHA"    . "#FF4500")
	  ("HELP"      . "#F5601B")
	  ("WARNING"   . "#E6DB10")
	  ("ATTENTION" . "#0bb552")
	  ("STUB"      . "#1E90FF")
	  ("IWASHERE"  . "#C60CFA")
	  ("QUESTION"  . "#12E6DB")
	  ("IMPORTANT" . "#FF0019")
	  ("NOTE"      . "#1A02EB")
	  )) 
    (add-hook 'prog-mode-hook #'hl-todo-mode)
    (add-hook 'LaTeX-mode-hook #'hl-todo-mode)
    )
  )

;; Company mode autocompleiton framework
(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-minimum-prefix-length 2
	company-idle-delay 0.0) ;; default is 0.2
    ;;; Hago company mode case sensitive
    (setq company-dabbrev-downcase nil)
    )
  )

;; Flycheck for ui
(use-package flycheck
  :config
  (progn
    (defalias 'show-errors 'flycheck-list-errors)
    )
  )

;; LSP
(use-package lsp-mode
  :init
  (progn
    (setq lsp-keymap-prefix "s-l")
    (setq lsp-ui-sideline-show-diagnostics 1)
    )
  :config
  (progn
    (add-hook 'lsp-mode-hook 'lsp-diagnostics-mode)
   )
  )

;;; Lsp UI
(use-package lsp-ui
  :config
  (setq lsp-completion-enable-additional-text-edit nil)
  )

(use-package lsp-ivy
  :ensure t
  )

;; GDB
;;;Disable company mode in gdb
(add-hook 'gdb-mode-hook (lambda () (company-mode -1)))
;;;Enable many windows by default
(add-hook 'gdb-mode-hook #'gdb-many-windows)
;;; Show main source buffer when using GDB
(setq gdb-show-main t)

;; Language specific configuration

;; Text mode
(evil-leader/set-key-for-mode 'text-mode "6" 'reload-file)

;;;C++
(evil-leader/set-key-for-mode 'c++-mode "c" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'c++-mode "t" 'gdb)
;;;; Opens the other file. Header -> Cpp / Cpp -> Header
(evil-leader/set-key-for-mode 'c++-mode "3" 'ff-find-other-file)
(add-hook 'c++-mode-hook #'lsp)
(evil-leader/set-key-for-mode 'c++-mode "i" 'lsp-treemacs-symbols)
(setq lsp-clients-clangd-args '("--background-index=0" "-j=2" "--header-insertion-decorators=0"))

;;;C
(evil-leader/set-key-for-mode 'c-mode "c" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'c-mode "t" 'gdb)
;;;; Opens the other file. Header -> C / C -> Header
(evil-leader/set-key-for-mode 'c-mode "3" 'ff-find-other-file)
(add-hook 'c-mode-hook #'lsp)
(evil-leader/set-key-for-mode 'c-mode "i" 'lsp-treemacs-symbols)

;;;Go
(add-hook 'go-mode-hook #'lsp)
(evil-leader/set-key-for-mode 'go-mode "i" 'lsp-treemacs-symbols)
;;; ASM
(evil-leader/set-key-for-mode 'asm-mode "c" 'projectile-compile-project)

;;;Elixir
(evil-leader/set-key-for-mode 'elixir-mode "c" 'projectile-compile-project)

;;;Rust
;;;; Shows colors in buffers
(projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                  :project-file "Cargo.toml"
                                  ;; :compile "RUSTFLAGS=-Awarnings cargo build"
                                  :compile (if (equal fabri-profile 'work)
                                             "make"
                                             "cargo build")
                                  :test "cargo test"
                                  :run "cargo run")  
(add-hook 'rust-mode-hook #'lsp)
(evil-leader/set-key-for-mode 'rust-mode "c" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'conf-toml-mode "c" 'projectile-compile-project) 
(setq lsp-rust-clippy-preference "on") 
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

(defun function-return-area ()
  (save-excursion
    (progn
      (beginning-of-defun)
      (let*
          (
           (fn-beginning (point))
           (fn-ending (save-excursion (progn (end-of-defun) (point))))
           (end (- (search-forward "{" fn-ending) 1))
           (start (+ 3
                     (re-search-backward
                      (rx 
                           ;; (sequence (and (not "<") (not "("))
                           (sequence (not "(")
                                     ")")
                           ))))
           (return-text (buffer-substring start end))

           (arrow-relative-pos (string-match-p (regexp-quote "->") return-text))
           (arrow-offset (if arrow-relative-pos arrow-relative-pos 0))
           (arrow-pos (+ arrow-offset start))
           )
        (list start end arrow-pos)
        )
      )
    )
  )

(defun change-rust-return (new-return start end)
  (interactive
   (save-excursion
     (let*
         (
          (function-limits (function-return-area))
          (start (nth 0 function-limits))
          (end (nth 1 function-limits))
          (arrow-pos (nth 2 function-limits))

          (function-start (if arrow-pos arrow-pos start))
          (return-text (buffer-substring function-start end))
          (return-text (string-replace "\n" "" return-text))

          (filter-text (string-trim-right return-text))
          )
       (list
        (read-string "Return value: " (format "%s" filter-text))
        start
        end)
       )
     )
   )
  (save-excursion
    (progn
      (delete-region start end)
      (goto-char start)
      (insert " ")
      (insert new-return)
      (insert " ")
      )
    )
  )
(evil-leader/set-key-for-mode 'rust-mode "t" 'projectile-test-project)
(evil-leader/set-key-for-mode 'rust-mode "y" 'change-rust-return)

;; Apply format on save
(setq rust-format-on-save t)

;;;Cmake
(evil-leader/set-key-for-mode 'cmake-mode "c" 'projectile-compile-project)

;;;Makefile
(evil-leader/set-key-for-mode 'makefile-gmake-mode "c" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'make-mode "c" 'projectile-compile-project) 
(evil-leader/set-key-for-mode 'makefile-mode "c" 'projectile-compile-project)  
(evil-leader/set-key-for-mode 'makefile-bsdmake-mode "c" 'projectile-compile-project)

;;;Python
(use-package lsp-pyright
  :ensure t
  ;; :ensure (install-for 'python)
  :config
  (progn
    (add-hook 'python-mode-hook #'lsp)
    (defalias 'ipython 'run-python)
    (evil-leader/set-key-for-mode 'python-mode "i" 'lsp-treemacs-symbols)
    )
  )
(evil-leader/set-key-for-mode 'python-mode "p" 'run-python)
(evil-leader/set-key-for-mode 'python-mode "c" 'python-shell-send-buffer)

;;;Latex
(evil-leader/set-key-for-mode 'LaTeX-mode "c" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'LaTeX-mode "t" 'TeX-command-master)

(when (equal fabri-profile 'personal)
  (use-package lsp-tex
    ;; :ensure (install-for 'tex)
    :config
    (progn
      (add-to-list 'lsp-language-id-configuration (cons 'LaTeX-mode "latex")) 
      (add-hook 'LaTeX-mode-hook #'lsp)
      (setq lsp-tex-server 'texlab)
      )
    )
  )

(defun move-to-window-come-back (window-name)
  ;; (save-excursion
  ;; (move-beginning-of-line)
  (switch-to-buffer-other-window window-name)
  (previous-buffer)
  (revert-buffer nil 't)
  ;; )
  )

(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "DEL") #'nothing-delete)))

(setq TeX-auto-untabify 't)

(when (equal fabri-profile 'personal)
  (add-hook 'LaTeX-mode-hook (lambda () (set-input-method "spanish-postfix")))
  )


;; Editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

;; Editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

;; Woman mode
(use-package man
  :config
  (progn
    (setq evil-lookup-func #'(lambda () (call-interactively #'woman)))
    (add-hook 'woman-mode-hook (lambda () (local-set-key (kbd "g T") 'tab-bar-switch-to-prev-tab)))
    (add-hook 'woman-mode-hook (lambda () (local-set-key (kbd "g t") 'tab-bar-switch-to-next-tab)))
    )
  )

;; Ansi-color mode
(use-package ansi-color
  :config
  (progn
    (defun my/ansi-colorize-buffer ()
      (let ((buffer-read-only nil))
        (ansi-color-apply-on-region (point-min) (point-max))))

    (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
    )
  )

;; Makes company-mode windows "gui-like". Helps with whitespace-mode's windows
(use-package company-posframe
  :ensure t
  :config
  (progn
    (when (display-graphic-p)
      (company-posframe-mode 1)
      )
    )
  )

;; ;; Whitespace-mode
;; (use-package whitespace 
;;   :config
;;   (progn
;;     (setq whitespace-style (remove 'space-mark whitespace-style))
;;     ;; (setq whitespace-style (remove 'trailing whitespace-style))
;;     ;; (setq whitespace-style (remove 'space-mark))
;;     ;; (setq whitespace-style (remove 'trailing whitespace-style))
;;     (setq whitespace-line-column (* fill-column 2)) ;; Highlight lines if twice the amount of fill-column
;;     ;; (setq whitespace-space-regexp "\\(^ +\\| +$\\)") ;; visualize only leading SPACEs.
;;     ;; (setq whitespace-hspace-regexp "\\(^\xA0+\\|\xA0+$\\)")
;;     (setq whitespace-display-mappings (assq-delete-all 'newline-mark whitespace-display-mappings))
;;     ;; (add-to-list 'whitespace-display-mappings '(newline-mark ?\n [92 ?n ?\n][?$ ?\n]))

;;     (add-hook 'prog-mode-hook #'whitespace-mode)
;;     (add-hook 'markdown-mode-hook #'whitespace-mode)
;;     ;; (add-hook 'text-mode-hook #'whitespace-mode)
;;     )
;;   )

;;;;;;;;;;;;;;;;;;;;;Org mode begin;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Makes lines wrap
(add-hook 'org-mode-hook 'visual-line-mode)
(when (equal fabri-profile 'personal) 
  (add-hook 'org-mode-hook (lambda () (set-input-method "spanish-postfix")))
  )

;;; defalias
(defalias 'org-go-to-link 'org-open-at-point)
(defalias 'org-mode-insert-date 'org-time-stamp) 
(defalias 'org-insert-date 'org-mode-insert-date)
(defalias 'org-time-difference 'org-evaluate-time-range)
(defalias 'delete-lines 'flush-lines)

;;; Org agenda
(setq org-agenda-span 60)

;;; Org agenda clockreport
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))

;;; Dont show done
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))

;;; RETURN will follow links in org-mode files
(setq org-return-follows-link  t)

;;; Shortcuts
(defhydra hydra-org ()
  "Various shells"
  ("`" org-store-link "org-store-link")
  ("m" org-agenda "org-agenda")
  ("," org-capture "org-capture")
  )

(evil-leader/set-key "," 'hydra-org/body)
