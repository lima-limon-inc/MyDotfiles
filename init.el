; Basic UI changes
;; Font
(add-to-list 'default-frame-alist
             '(font . "-1ASC-Liberation Mono-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

;;Theme
;;; Gruvbox theme
(require 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)

;; Menus
;;;Disable scrollbar, menu and tool bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Scrolling
;;; Makes scrolling vim like
(setq scroll-step 1)
(setq scroll-margin 1)

;; Line numbers
;;; Line and Relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Global general customizations
;;; Binds y and n to yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;;;Registers
;;;;; Register with files I open often
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
;;;;;Register with directories to college subject
(defun TPSdir (materia)
  (setq college-directory "~/Documents/Facultad/")
  (concat college-directory materia "/TPS"))

(set-register ?t (cons 'file (TPSdir "Taller")))
(set-register ?s (cons 'file (TPSdir "Sisop")))
(set-register ?a (cons 'file (TPSdir "TDA")))
(set-register ?j (cons 'file "~/Documents/Japo/"))

;;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      2 ; how many of the newest versions to keep
      kept-old-versions      1) ; and how many of the old

;;; Different custom set variables files
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Evil

;;; enable global-evil-leader-mode before you enable evil-mode, otherwise evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, …).
(global-evil-leader-mode)

;;; Disable control i feature (compatibilty with org mode)
(setq evil-want-C-i-jump nil)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;;;Actually enable evil
(require 'evil)
(evil-mode 1)

;;;;Rebinds
;;;;;Makes Ctrl R work
(evil-set-undo-system 'undo-redo)
;;;;; Sets the leader key to be space
(require 'evil-leader)
(evil-leader/set-leader "<SPC>")
;;;;; Splitting window keys
(evil-leader/set-key "v" 'split-window-horizontally)
(evil-leader/set-key "s" 'split-window-vertically)
;;;;; Move between said windows
(evil-leader/set-key "h" 'evil-window-left)
(evil-leader/set-key "l" 'evil-window-right)
(evil-leader/set-key "k" 'evil-window-up)
(evil-leader/set-key "j" 'evil-window-down)
;;;;;Search files a la vim /
(evil-leader/set-key "f" 'find-file)
;;;;;Fuzzy find with ripgrep
(evil-leader/set-key "g" 'counsel-rg)
;;;;;New tab
(evil-leader/set-key "e" 'tab-new)
;;;;;Use leader key to jump to register
(evil-leader/set-key "y" 'jump-to-register)
;;;;;Kill buffer
(evil-leader/set-key "b" 'kill-this-buffer)
;;;;;Changes the search to swiper
(define-key evil-normal-state-map (kbd "/") 'swiper)
;;;;;Map meta x to leader :. A sort of mix of vim and emacs
(evil-leader/set-key ":" 'execute-extended-command)
;;;;;Comment a region out imitating tim pope's plugin
(define-key evil-normal-state-map (kbd "g c") 'comment-or-uncomment-region)
;;;;;Recreate vim's ctrl o behavior
(define-key evil-normal-state-map (kbd "C-o") 'previous-buffer)

; Emacs related configs
;; Dired
;;; Create new files with leader o
(evil-leader/set-key-for-mode 'dired-mode "o" 'dired-create-empty-file)

;; Ivy
(ivy-mode)

;; Counsel
(counsel-mode)
(with-eval-after-load 'counsel
  (setq ivy-initial-inputs-alist nil))

;; Pdf views
;;; Makes pdfs continuos
(setq doc-view-continuous t)

;; Org mode
;;; Set org mode default directory
(setq org-directory "~/.emacs.d/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;;;Makes lines wrap
(add-hook 'org-mode-hook 'visual-line-mode)
;;;Shortcuts
(evil-leader/set-key "n" 'org-store-link)
(evil-leader/set-key "m" 'org-agenda)
(evil-leader/set-key "," 'org-capture)
(evil-leader/set-key-for-mode 'org-mode "u" 'org-todo)
(evil-leader/set-key-for-mode 'org-mode "i" 'org-toggle-checkbox)
(evil-leader/set-key-for-mode 'org-mode "t" 'org-set-tags-command)
(evil-leader/set-key-for-mode 'org-mode "p" 'org-set-property)
(evil-leader/set-key-for-mode 'org-mode ";" 'org-deadline)
(evil-leader/set-key-for-mode 'org-mode "[" 'org-agenda-file-to-front)
(evil-leader/set-key-for-mode 'org-mode "]" 'org-remove-file)
(evil-leader/set-key-for-mode 'org-mode "c" 'org-export-dispatch)
;;;Org todo redifinitions
(setq org-todo-keywords
      '((sequence "TODO(t)" "Marchando(m)" "Waiting(e)" "|" "DONE(d)")
	(sequence "Parcial(p)" "|" "Esperando nota(n)" "Resultado(r)")))
(setq org-enforce-todo-dependencies 1)
(setq org-log-done 'time)
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

; Programming - package specific
;;Magit
(setq transient-default-level 5)
(require 'magit)
(evil-leader/set-key "." 'magit-status)

;;Tresitter - Possibly not in use
(require 'treesit)

;;Rainbow parenthesis
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; TODO Highlighter
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
(add-hook 'prog-mode-hook #'hl-todo-mode)

;; Company mode autocompleiton framework
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

;; Lsp configuration
(setq lsp-keymap-prefix "s-l")
;;;Show diagnostics in buffer
(setq lsp-ui-sideline-show-diagnostics 1)
(require 'lsp-ui)

(require 'lsp-mode)
(add-hook 'lsp-mode-hook 'lsp-diagnostics-mode)
;;;Language specific lsp servers
;;;;C++
(add-hook 'c++-mode-hook #'lsp)
;;;;C
(add-hook 'c-mode-hook #'lsp)
;;;;Python
(require 'lsp-pyright)
(add-hook 'python-mode-hook #'lsp)
;;;;Racket (not working atm)
;;;;;(require 'racket-langserver)
;;;;;(add-hook 'racket-mode-hook #'lsp)

;; GDB Debugger
;;;Disable company mode in gdb
(add-hook 'gdb-mode-hook (lambda () (company-mode -1)))

;;;Enable many windows by default
(add-hook 'gdb-mode-hook #'gdb-many-windows)
;;; Show main source buffer when using GDB
(setq gdb-show-main t)

;;Language specific configurations
;;;C++
(evil-leader/set-key-for-mode 'c++-mode "c" 'compile)
(evil-leader/set-key-for-mode 'c++-mode "t" 'gdb)

; Misc - package specific
;; Mode for markdown files
(require 'markdown-mode)

;; Mozc aka japanese input
(require 'mozc)
(setq default-input-method "japanese-mozc")

;; Mode for graphviz
(require 'graphviz-dot-mode)
(setq graphviz-dot-indent-width 4)
(add-hook 'graphviz-dot-mode-hook 'company-mode)
(evil-leader/set-key-for-mode 'graphviz-dot-mode "p" 'graphviz-dot-preview)
(evil-leader/set-key-for-mode 'graphviz-dot-mode "c" 'compile)
