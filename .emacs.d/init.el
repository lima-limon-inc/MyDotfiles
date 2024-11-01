; Profile configuration

(setq fabri-profile
      (if (equal system-type 'gnu/linux)
	'personal
	'work))

(setq languages-used (list
      'python
      'c
      'cpp
      'rust
      'go
      'tex
      )
      )

(defun check-lang-used (lang)
  (when (member lang languages-used)
    't
    )
  )

;; Check if the current profile requires installation
(defun install-for (lang)
  (and
   (when (equal fabri-profile 'work) 't) 
   (check-lang-used lang)
   )
  )

;; Email address
(setq user-full-name "Tomas Fabrizio Orsi")
(setq user-mail-address
      (if (equal fabri-profile 'personal)
      "torsi@fi.uba.ar"
      "mail empresa"
      ))


; Package manager settings
(when (equal fabri-profile 'work)
  (progn
    (require 'package)
    (add-to-list 'package-archives
	      '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    )
)

(setq use-package-always-ensure
      (when (equal fabri-profile 'work)
        't) 
      )

; Global look and feel

;; Font
(add-to-list 'default-frame-alist
             '(font . "-1ASC-Liberation Mono-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1"))

;; Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t)
  )

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
(setq-default tab-width 10)
(setq c-basic-offset 4)

;; Global general customizations
;;; Binds y and n to yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; Get string for subdirectory in emacs directory
(defun emacs-dir (directory)
  (concat user-emacs-directory directory))

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '((".*" . (emacs-dir "backup")))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      2 ; how many of the newest versions to keep
      kept-old-versions      1) ; and how many of the old

(setq make-backup-files nil) ; stop creating ~ files


;; Different custom set variables files
(setq custom-file (emacs-dir "custom.el"))
(load custom-file 'noerror)

;; Confirm before killing emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;; World clock
(setq zoneinfo-style-world-list '(
			    ("Europe/Rome" "Rome")
			    ("Europe/Madrid" "Madrid")
			    ("America/Buenos_Aires" "Buenos Aires")
			    ("America/Montevideo" "Montevideo")
			    ))

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

;; Zone when idle
(require 'zone)
(zone-when-idle 300)
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
(evil-leader/set-key "&" 'async-shell-command)


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


;;; Run :wa with les typing
(evil-leader/set-key "RET" 'evil-write-all)

;;; Evil save and close the buffer
(evil-leader/set-key "[" 'evil-save-and-close)

;;;Search files a la vim /
(evil-leader/set-key "f" 'find-file)
;;;Search files regardless of path
(evil-leader/set-key "q" 'projectile-find-file)
;;;Fuzzy find with ripgrep
(evil-leader/set-key "g" 'counsel-rg)
;;;New tab
(evil-leader/set-key "e" 'tab-new)
;;;Use leader key to jump to register
(evil-leader/set-key "C-r" 'jump-to-register)
;;;Kill buffer
(evil-leader/set-key "b" 'kill-this-buffer)
;;;Changes the search to swiper
(define-key evil-normal-state-map (kbd "/") 'swiper)
;;;Map meta x to leader :. A sort of mix of vim and emacs
(evil-leader/set-key ";" 'execute-extended-command)
;;;Comment a region out imitating tim pope's plugin
(define-key evil-normal-state-map (kbd "g c") 'comment-or-uncomment-region)
;;;Indent region
(define-key evil-normal-state-map (kbd "g i") 'indent-region)
;;Open the switch tab menu
(define-key evil-normal-state-map (kbd "g s") 'tab-switch)
;;;Recreate vim's ctrl o behavior
(define-key evil-normal-state-map (kbd "C-o") 'previous-buffer)
;;;Switch buffer
(evil-leader/set-key "r" 'switch-to-buffer)
;;;Open the terminal
(evil-leader/set-key "9" 'eshell)
;;;Close tab
(evil-leader/set-key "4" 'tab-bar-close-tab)


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



;; Global variables
(setq college-directory "~/Documents/Facultad/")

;;Registers
;;; Register with files I open often
(set-register ?e (cons 'file (emacs-dir "init.el")))

;;;Register with directories to college subject
(defun TPSdir (materia)
  (concat college-directory materia "/TPS"))

(set-register ?d (cons 'file "~/Downloads/"))

(set-register ?u (cons 'file (TPSdir "Concu")))
(set-register ?h (cons 'file "~/Documents/Obsidian-Vaults/Hobbies"))
(set-register ?n (cons 'file "~/Documents/Personal/Notas/notes.org"))
(set-register ?f (cons 'file "~/Documents/Facultad/facultad.org"))
(set-register ?p (cons 'file "~/Documents/Personal/daily.org"))
(set-register ?b (cons 'file "~/Scripts/Orgmode/"))
(set-register ?r (cons 'file "~/Documents/Personal/Radio/")) 


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

;; Create temporary dir
(defun tmp-dir ()
  (interactive)
    (find-file (make-temp-file (user-real-login-name) 't))
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


; Programming configuration
;; Color line
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Enable auto insert mode
(auto-insert-mode t)

;;Magit
(use-package magit
  :init 
  (setq transient-default-level 5)
  :config
  (progn
    (evil-leader/set-key "." 'magit-status)
    (setq magit-blame-styles
          '((margin
             (margin-width . 32)
             (margin-format . ("%C %c %f"))
             (margin-face . magit-blame-margin)
             (margin-body-face . magit-blame-dimmed)
             (show-message . t))))

    (defalias 'magit-co-authored-by 'git-commit-co-authored)
    (defalias 'co-authored-by 'git-commit-co-authored)
    (defalias 'mablame 'magit-blame)

    (defun my-wrap-lines ()
      "Disable `truncate-lines' in the current buffer."
      (setq truncate-lines nil))

    (add-hook 'magit-status-mode-hook #'my-wrap-lines)
    (add-hook 'magit-diff-mode-hook #'my-wrap-lines)
    )
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
	  ("DEBUG"     . "#A020F0")
	  ("GOTCHA"    . "#FF4500")
	  ("HELP"      . "#F5601B")
	  ("WARNING"   . "#E6DB10")
	  ("ATTENTION" . "#0bb552")
	  ("STUB"      . "#1E90FF")
	  ("IWASHERE"  . "#C60CFA")
	  ("QUESTION"  . "#12E6DB")
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
    (setq company-minimum-prefix-length 3
	company-idle-delay 0.0) ;; default is 0.2
    ;;; Hago company mode case sensitive
    (setq company-dabbrev-downcase nil)
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

;; Projectile
(use-package projectile
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)
    (evil-leader/set-key "<up>" 'projectile-run-async-shell-command-in-root)
    (evil-leader/set-key "7" 'projectile-kill-buffers)
    )
  )

;; GDB
;;;Disable company mode in gdb
(add-hook 'gdb-mode-hook (lambda () (company-mode -1)))
;;;Enable many windows by default
(add-hook 'gdb-mode-hook #'gdb-many-windows)
;;; Show main source buffer when using GDB
(setq gdb-show-main t)


;; Language specific configuration

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

;;;Rust
;;;; Shows colors in buffers
(projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                  :project-file "Cargo.toml"
                                  :compile "RUSTFLAGS=-Awarnings cargo build"
                                  :test "cargo test"
                                  :run "cargo run")
(add-hook 'rust-mode-hook #'lsp)
(evil-leader/set-key-for-mode 'rust-mode "c" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'conf-toml-mode "c" 'projectile-compile-project) 
(evil-leader/set-key-for-mode 'rust-mode "t" 'projectile-test-project)

;;;Cmake
(evil-leader/set-key-for-mode 'cmake-mode "c" 'projectile-compile-project)

;;;Makefile
(evil-leader/set-key-for-mode 'makefile-gmake-mode "c" 'projectile-compile-project)

;;;Python
(use-package lsp-pyright
  :ensure (install-for 'python)
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
(use-package lsp-tex
  :ensure (install-for 'tex)
  :config
  (progn
    (add-to-list 'lsp-language-id-configuration (cons 'LaTeX-mode "latex")) 
    (add-hook 'LaTeX-mode-hook #'lsp)
    (setq lsp-tex-server 'texlab)
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

				; from enberg on #emacs
(add-hook 'LaTeX-mode-hook
	(lambda ()
	  (add-hook 'compilation-finish-functions
		  (lambda (buf str)
		    (if (null (string-match ".*exited abnormally.*" str))
		        ;;no errors, make the compilation window go away in a few seconds
		        (progn
			(run-at-time
			 "1 sec" nil 'move-to-window-come-back
			 "*compilation*"
			 )
			(message "No Compilation Errors!")))))
	  )
	)
(add-hook 'LaTeX-mode-hook
          (lambda () (local-set-key (kbd "DEL") #'nothing-delete)))

(setq TeX-auto-untabify 't)

(add-hook 'LaTeX-mode-hook (lambda () (set-input-method "spanish-postfix")))



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
       (diary-dir (emacs-dir "diary")))
    (unless (file-exists-p diary-dir)
      (display-warning 'warning (format "No hay archivo en %s" diary-dir)))
    )
  )

(advice-add 'config-is-done
	  :after #'(lambda () (check-file-exists-warn (emacs-dir "diary"))))

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
;;; Dired will try to guess destination. If you have to open windows, then it will use the one next to it
(setq dired-dwim-target t)

;; Term
(evil-leader/set-key-for-mode 'term-mode "p" 'term-paste)

;; Woman mode
(use-package man
  :config
  (progn
    (setq evil-lookup-func #'(lambda () (call-interactively #'woman)))
    (add-hook 'woman-mode-hook (lambda () (local-set-key (kbd "g T") 'tab-bar-switch-to-prev-tab)))
    (add-hook 'woman-mode-hook (lambda () (local-set-key (kbd "g t") 'tab-bar-switch-to-next-tab)))
    )
  )


;; Info mode
(evil-leader/set-key-for-mode 'Info-mode "w" 'Info-follow-nearest-node)

;; Grep mode
(evil-leader/set-key-for-mode 'grep-mode "g" 'recompile)
(evil-leader/set-key-for-mode 'grep-mode "n" 'next-error)

;; Compilation mode
(evil-leader/set-key-for-mode 'compilation-mode "n" 'next-error)
(evil-leader/set-key-for-mode 'compilation-mode "c" 'compile)
(evil-leader/set-key-for-mode 'compilation-mode "g" 'recompile)

;; Proceed
(defalias 'top 'proced)
(setq-default proced-auto-update-flag t)
(setq proced-auto-update-interval 2)
(setq proced-enable-color-flag t)

;; Artist mode
(advice-add 'artist-mode :after #'(lambda (x) (turn-off-evil-mode)))

(advice-add 'artist-mode-off :after #'(lambda () (turn-on-evil-mode)))

;; Emacs - Calc
(evil-leader/set-key "C-c" 'calc)

(evil-leader/set-key "C-v" 'full-calc)

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



;; Org mode
;;;;;;;;;;;;;;;;;;;;;Org mode begin;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set org mode default directory
(setq org-directory "~/Documents/Personal/Notas")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;;;Makes lines wrap
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (set-input-method "spanish-postfix")))

;;; defalias
(defalias 'org-go-to-link 'org-open-at-point)
(defalias 'org-mode-insert-date 'org-time-stamp) 
(defalias 'org-insert-date 'org-mode-insert-date)
(defalias 'org-time-difference 'org-evaluate-time-range)

;;; Org agenda
(setq org-agenda-span 60)

;;; Dont show done
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))

;;; RETURN will follow links in org-mode files
(setq org-return-follows-link  t)

;;;Shortcuts
(evil-leader/set-key "`" 'org-store-link)
(evil-leader/set-key-for-mode 'org-mode "n" 'hydra-footnote/body)
(evil-leader/set-key "m" 'org-agenda)
(evil-leader/set-key "," 'org-capture)
(evil-leader/set-key-for-mode 'org-mode "\\" 'org-timer-set-timer)
(evil-leader/set-key-for-mode 'org-mode "u" 'org-todo)
(evil-leader/set-key-for-mode 'org-mode "i" 'org-toggle-checkbox)
(evil-leader/set-key-for-mode 'org-mode "o" 'org-insert-link)
(evil-leader/set-key-for-mode 'org-mode "t" 'org-set-tags-command)
(evil-leader/set-key-for-mode 'org-mode "p" 'org-set-property)
(evil-leader/set-key-for-mode 'org-mode "d" 'org-deadline)
;; (evil-leader/set-key-for-mode 'org-mode "[" 'org-agenda-file-to-front)
(evil-leader/set-key-for-mode 'org-mode "/" 'org-sparse-tree)
;; (evil-leader/set-key-for-mode 'org-mode "z" 'flyspell-mode)
;; (evil-leader/set-key-for-mode 'org-mode "x" 'flyspell-auto-correct-word)
(evil-leader/set-key-for-mode 'org-mode "]" 'org-latex-preview)
(evil-leader/set-key-for-mode 'org-mode "c" 'org-export-dispatch)
(evil-leader/set-key-for-mode 'org-mode "8" 'org-insert-date)
;; (evil-leader/set-key-for-mode 'org-mode "'" 'mozc-mode)
(evil-leader/set-key-for-mode 'org-mode "6" 'org-toggle-inline-images)
(evil-leader/set-key-for-mode 'org-mode "0" 'hydra-clock/body)
(evil-leader/set-key-for-mode 'org-mode "w" 'org-go-to-link)
(evil-leader/set-key-for-mode 'org-mode "C-w" 'org-mark-ring-goto)
;; (evil-leader/set-key-for-mode 'org-mode "{" 'insert-braces) 
(evil-leader/set-key-for-mode 'org-mode "(" 'insert-parentheses)
;; (evil-leader/set-key-for-mode 'org-mode "0" 'org-clock-in)
;; (evil-leader/set-key-for-mode 'org-mode "-" 'org-clock-out) ;;Moved to hydra clock out
(evil-leader/set-key-for-mode 'org-mode "a" 'org-insert-structure-template)

;;; Beamer
(require 'ox-beamer)

;;; DOnt show repeting taks
(setq org-agenda-show-future-repeats nil)

;;;Org capture templates
(setq org-capture-templates
      ;; Default value
      (if (equal fabri-profile 'personal)
	'(
	  ("t" "Task" entry (file+headline "" "Tasks")
	   "* TODO %?\n  %u\n  %a")
	  ("j" "Journal" entry (file "~/Documents/Personal/Journaling/journal.org")
	   "* %t\n %?")
	  ("e" "Facultad - Examen" entry (file+headline "~/Documents/Facultad/facultad.org" "Examen")
	   "* TODO Parcial - %^{NOMBRE} [%] %^g
DEADLINE: %^{DEADLINE}t
- [ ] Anotarse en el SIU (https://guaraniautogestion.fi.uba.ar/g3w/) %? ")
	  ("p" "Facultad - TP" entry (file+headline "~/Documents/Facultad/facultad.org" "TP")
	   "* TODO TP - %^{NOMBRE} [%] %^g
DEADLINE: %^{DEADLINE}t ")
	  ("a" "Facultad - Tareas" entry (file+headline "~/Documents/Facultad/facultad.org" "Tareas")
	   "* TODO Tareas - %^{NOMBRE} [%] %^g 
DEADLINE: %^{DEADLINE}t ")
	  ("r" "Recordar" entry (file+headline "" "Someday")
	   "* TODO - %^{NOMBRE} [%] %(org-set-tags-command)
%u
%a
%?")
	  ("b" "Blog" entry (file+headline "/home/fabri/Scripts/Orgmode/org/Lo-Que-Se-Viene.org" "Proximo")
	   "* Titulo: - %^{NOMBRE} [%]
%^{IDEA}")
	  )
        '(
	("t" "Task" entry (file+headline "" "Tasks")
	 "* TODO %?\n  %u\n  %a")
	)
        )
      )

;;; Enforce dependencies
(setq org-enforce-todo-dependencies 1)

;;;Org todo redifinitions
(setq org-todo-keywords
      '((sequence "TODO(t)" "Marchando(m)" "Waiting(w)" "|" "Waiting but done(b)" "DONE(d)")
        ))
(setq org-tag-alist '(
		  (:startgroup . nil)
		  ;;Categoria
                      ("facultad" . ?f) ("personal" . ?p)
                      (:endgroup . nil)

		  ;;Hobbies
		  (:startgroup . nil)
		  ("emacs" . ?e) ("linux" . ?l)
                      (:newline)
		  (:endgroup . nil)

		  ;;Misc
		  ("someday" . ?s) ("aprendizaje" . ?a)
		  ))


;;; Log done
(setq org-log-done 'time)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

(add-hook 'org-mode-hook
          (lambda () (local-set-key (kbd "DEL") #'nothing-delete)))

;;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t) ;Usar C mayuscula en el source block. Esto en teoria sirve para c, c++ y d
   ;; (cpp . t)
   (shell . t)
   (dot . t)
   (gnuplot . t)
   (latex . t)
   (python . t)
   (latex . t)
   (http . t)
   ))

;; Hago que despues de ejecutar un bloque de codigo, se haga refresh de las imagenes.
(advice-add 'org-babel-execute-src-block
	  :after #'(lambda (a b) (progn
			    ;; (message "Hello")))) 
			    (org-remove-inline-images)
			    (org-display-inline-images))))

;; Hago que minted sea el paquete que se use para syntax highlight
(setq org-latex-listings 'minted)
(require 'ox-latex)

(add-to-list 'org-export-backends 'md)


(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(require 'ox-man)
(require 'ol-man)

;;; Org latex preview
(setq org-format-latex-options (plist-put org-format-latex-options :scale 8.0))

(setq org-image-max-width 'fill-column)

(use-package org-alert
  :ensure nil
  :config
  (progn
    (org-alert-enable)
    (setq alert-default-style 'libnotify)
    (setq org-alert-interval 300
	org-alert-notify-cutoff 10
	org-alert-notify-after-event-cutoff 10)
    (setq org-clock-sound "~/Media/Music/Bell.wav")
    )
  )

(add-to-list 'auto-insert-alist '(org-mode . (lambda ()
				       (interactive)
				       (call-interactively 'org-mode-auto-insert))))

;;;;;;;;;;;;;;;;;;;;;Org mode end;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Additional extensions
;; Ivy
(use-package ivy
  :config
  (progn
    (ivy-mode)
    (define-key minibuffer-mode-map (kbd "C-RET") 'ivy-immediate-done)
    ))

;; Counsel
(use-package counsel
  :config
  (progn
    (counsel-mode)
    (with-eval-after-load 'counsel
      (setq ivy-initial-inputs-alist nil))
    ))

;; Swiper
(use-package swiper 
  :config
  (progn
    (setq search-default-mode #'char-fold-to-regexp) 
    (setq counsel-grep-base-command "grep -E -n -e -i %s %s")
    (setq lazy-highlight-cleanup nil) 
    )
  )
(add-hook 'messages-buffer-mode-hook (lambda () (local-set-key (kbd "SPC ;") #'execute-extended-command)))
(add-hook 'messages-buffer-mode-hook (lambda () (local-set-key (kbd "SPC ;") #'execute-extended-command)))
(add-hook 'messages-buffer-mode-hook (lambda () (local-set-key (kbd "SPC f") #'counsel-find-file)))
(add-hook 'messages-buffer-mode-hook (lambda () (local-set-key (kbd "SPC h") #'evil-window-left)))
(add-hook 'messages-buffer-mode-hook (lambda () (local-set-key (kbd "SPC l") #'evil-window-right)))
(add-hook 'messages-buffer-mode-hook (lambda () (local-set-key (kbd "SPC") #'nil)))


;; Markdown
(use-package markdown-mode
  :config
  (progn
    (evil-leader/set-key-for-mode 'markdown-mode "o" 'markdown-insert-link)
    (evil-leader/set-key-for-mode 'markdown-mode "i" 'markdown-toggle-gfm-checkbox)
    (evil-leader/set-key-for-mode 'markdown-mode "6" 'markdown-toggle-inline-images) 
    (evil-leader/set-key-for-mode 'markdown-mode "w" 'markdown-follow-link-at-point)
    (setq markdown-enable-math t)
    (add-hook 'markdown-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-<return>") 'markdown-insert-header-atx-1)))
    ;; (advice-add 'markdown-insert-header-atx-1 :after #'(lambda () (kill-line)))
    (setq markdown-asymmetric-header 't)
    (defalias 'mabold 'markdown-insert-bold)
    )
  )


;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

;; Dockerfiles
(use-package dockerfile-mode
  )

;; Yas snippets
(use-package yasnippet
  :config
  (progn
    (yas-global-mode 1)

    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-c k") #'yas-expand)
    (defalias 'yas-snippet-go-to-definition 'yas-visit-snippet-file)
    (defalias 'yas-edit-snipet 'yas-visit-snippet-file)
    )
  )

;; Search on browser
(load-file "~/Scripts/Elisp/Emacs-Quick-Search/quick-search.el")

(quick-search/set-preferred-browser "firefox-bin" "")
(quick-search/add-search-engine "Google" "https://google.com/search?q=" nil)
(quick-search/add-search-engine "Zugaina" "http://gpo.zugaina.org/Search?search=" nil)
(quick-search/add-search-engine "Wikipedia" "https://en.wikipedia.org/w/index.php?search=" nil)
(quick-search/add-search-engine "Gentoo wiki" "https://wiki.gentoo.org/index.php?search=" nil)
(quick-search/add-search-engine "Arch wiki" "https://wiki.archlinux.org/index.php?search=" nil)
(quick-search/add-search-engine "Python" "https://docs.python.org/3/search.html?q=" '(python-mode))
(quick-search/add-search-engine "Oxford Dictionary" "https://www.oxfordlearnersdictionaries.com/definition/english/" '(org-mode latex-mode LaTeX-mode tex-mode markdown-mode))
(quick-search/add-search-engine "Youtube" "https://www.youtube.com/results?search_query=" nil)
(quick-search/add-search-engine "C plus plus" "https://cplusplus.com/search.do?q=" '(c++-mode c-mode))
(quick-search/add-search-engine "Rust" "https://doc.rust-lang.org/std/?search=" '(rust-mode))

(evil-leader/set-key "5" 'quick-search)


; Additional "non essential" extensions
(setq use-package-always-ensure nil) 

;; Drag stuff
(use-package drag-stuff
  :config
  (progn
    (drag-stuff-global-mode 1)
    (define-key drag-stuff-mode-map (kbd "C-<up>") 'drag-stuff-up)
    (define-key drag-stuff-mode-map (kbd "C-<down>") 'drag-stuff-down)
    )
  )

;; Which key
(use-package which-key
  :config
  (which-key-mode)
  )

;;Dashboard
(use-package dashboard
  :config
  (progn
    (dashboard-setup-startup-hook)
    (setq dashboard-items '(
		        (recents  . 5)
                            ;; (bookmarks . 5)
                            ;; (projects . 5)
                            (agenda . 10)
                            (registers . 5)
		        ))
    (setq dashboard-set-file-icons t)
    (evil-leader/set-key-for-mode 'dashboard-mode "SPC" 'dashboard-open)
    ;;; Make dashboard the default when using a client
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    )
  )

;;Gnuplot
(use-package gnuplot
  :init
  (progn 
    (setq gnuplot-use-context-sensitive-completion nil) 
    (setq gnuplot-context-sensitive-mode nil)
    (autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
    (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
    (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))
    ))

;; All the icons

(when (display-graphic-p)
  (use-package all-the-icons
    )

  (use-package all-the-icons-dired
    :config
    ;;; Enable dashboard-dired
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
    )
  )


;; Org-roam
;; (use-package org-roam
;;   :config
;;   (progn 
;;     (setq org-roam-directory "~/Documents/Obsidian-Vaults/Roam")
;;     (org-roam-db-autosync-mode)
;;     )
;;   )

;; Calfw
(use-package calfw
  :config
  (progn
    (require 'calfw-org)
    (require 'calfw-cal)
    (setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday
    (defalias 'org-mode-calendar 'cfw:open-org-calendar)
    (evil-leader/set-key "C-o" #'org-mode-calendar)
    (add-hook 'cfw:calendar-mode-hook (lambda () (local-set-key (kbd "g t") 'tab-bar-switch-to-next-tab)))
    (add-hook 'cfw:calendar-mode-hook (lambda () (local-set-key (kbd "g T") 'tab-bar-switch-to-prev-tab)))
    (add-hook 'cfw:calendar-mode-hook (lambda () (local-set-key (kbd "g") #'nil)))
    ))

;; Graphviz
(use-package graphviz-dot-mode
  :config
  (progn
    (setq graphviz-dot-indent-width 4)
    (add-hook 'graphviz-dot-mode-hook 'company-mode)
    (evil-leader/set-key-for-mode 'graphviz-dot-mode "p" 'graphviz-dot-preview)
    (evil-leader/set-key-for-mode 'graphviz-dot-mode "c" 'compile)
    )
  )

;; PDF TOOLS
(use-package pdf-tools
  :config
  (progn
    ;;; Makes pdfs continuos
    (setq doc-view-continuous t)
    (pdf-tools-install)  ; Standard activation command

    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC ;") #'execute-extended-command)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC f") #'counsel-find-file)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC h") #'evil-window-left)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC l") #'evil-window-right)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC k") #'evil-window-up)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC j") #'evil-window-down)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC r") #'switch-to-buffer)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC b") #'kill-this-buffer)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC e") #'tab-new)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC 4") #'tab-bar-close-tab)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "SPC") #'nil)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "j") 'pdf-view-next-line-or-next-page)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "k") 'pdf-view-previous-line-or-previous-page)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "4") 'pdf-annot-add-highlight-markup-annotation)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "y") 'pdf-view-kill-ring-save)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd ";") 'execute-extended-command)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "g t") 'tab-bar-switch-to-next-tab)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "g T") 'tab-bar-switch-to-prev-tab)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "g") #'nil)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "p") 'revert-buffer)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "p") #'nil)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "b") #'kill-this-buffer)))
    (add-hook 'pdf-view-mode-hook (lambda () (local-set-key (kbd "/") #'isearch-forward)))
    )
  )

;; Hydra
(use-package hydra
  :config
  (progn
    (defhydra hydra-zoom ()
      "Zoom"
      ("i" text-scale-increase "in")
      ("o" text-scale-decrease "out"))

    (evil-leader/set-key "z" 'hydra-zoom/body)

    (defhydra hydra-clock ()
      "Clock"
      ("i" org-clock-in "Clock in")
      ("o" org-clock-out "Clock out")
      ("r" org-clock-report "Clock report")
      ("c" org-timer-set-timer "Countdown")
      ("p" org-timer-pause-or-continue "Pause or continue")
      )

    ;; Evil numbers
    (defhydra hydra-increment-number ()
      "Evil increment numbers"
      ("i" evil-numbers/inc-at-pt "increment")
      ("o" evil-numbers/inc-at-pt-incremental "increment incremental")
      ("d" evil-numbers/dec-at-pt "decrement")
      ("s" evil-numbers/dec-at-pt-incremental "decrement incremental")
      )
    (evil-leader/set-key "1" 'hydra-increment-number/body)


    ;; Footnote
    (defhydra hydra-footnote ()
      "Footnote action"
      ("j" org-footnote-new "include")
      ("k" org-footnote-normalize "normalize")
      )

    (defhydra hydra-ispell ()
      "Ispell actions"
      ("l" set-ispell-language "change language")
      ("r" ispell-region "ispell region")
      ("b" ispell "ispell buffer")
      )
    (defalias 'ispell-full 'hydra-ispell/body)

    )

  )


;; Svg clock
(load-file "~/Scripts/Elisp/emacs-svg-clock/svg-analog-clock.el")
(defalias 'analog-clock 'svg-analog-clock)


;; RFC
(use-package rfc-mode 
  :config
  (progn
    (setq rfc-mode-directory (expand-file-name (emacs-dir "rfc")))

    (evil-leader/set-key-for-mode 'rfc-mode "n" 'rfc-mode-next-section)
    (evil-leader/set-key-for-mode 'rfc-mode "p" 'rfc-mode-previous-section)
    (evil-leader/set-key-for-mode 'rfc-mode "t" 'rfc-mode-goto-section)
    ))


; Final details

;; WARNING KEEP AT THE BOTTOM 
;; NOTE: Meant to add advice to this function
(config-is-done)
