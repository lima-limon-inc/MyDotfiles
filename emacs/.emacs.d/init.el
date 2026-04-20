; Profile configuration

(setq fabri-profile
      (let ((env (getenv "FABRI_PROFILE")))
	   (cond
	    ((equal env "personal") 'personal)
	    ((equal env "work") 'work)
	    (t (if (equal system-type 'gnu/linux)
		      'personal
	         'work)))))

;; Email address
(setq user-full-name "Tomas Fabrizio Orsi")
(setq user-mail-address
      (if (equal fabri-profile 'personal)
      "torsi@fi.uba.ar"
      "tomas.orsi@lambdaclass.com"
      ))


; Package manager settings
(use-package package
  :ensure nil
  :config
    (add-to-list 'package-archives
                '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (add-to-list 'package-archives
                '("melpa" . "http://melpa.org/packages/") t)
    ;; Packages from lower-priority archives will not be shown in the menu, if the
    ;; same package is available from a higher-priority archive
    (setq package-archive-priorities '(("gnu" . 10)
                                       ("melpa-stable" . 5)
                                       ("melpa" . 0))))

(use-package use-package-ensure
  :ensure nil
  :config
  ; Always ensure packages are installed
  (setq use-package-always-ensure t))

; Global look and feel

;; Font
(if (equal fabri-profile 'personal)
    (add-to-list 'default-frame-alist
                 (cons 'font (font-xlfd-name
                              (font-spec
                               :family "DejaVu Sans Mono"
                               :size 15))))
    (add-to-list 'default-frame-alist
                 (cons 'font (font-xlfd-name
                              (font-spec
                               :family "DejaVu Sans Mono"
                               :size 16))))
  )

;; Theme, new theme, new world
(load-theme 'modus-vivendi-tinted t)
(set-face-attribute 'font-lock-comment-face nil :foreground "#FF5900")

;; Don't use tabs, only use spaces
(setq-default indent-tabs-mode nil)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Menus
;;;Disable scrollbar, menu and tool bar
(progn
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  )

;; Scrolling
;;; Makes scrolling vim like
(setq scroll-step 1)
(setq scroll-margin 1)

;; Line numbers
;;; Line and Relative line numbers
(global-display-line-numbers-mode 1)

(setq display-line-numbers-type 'relative)

;; Column
(setq column-number-mode t)

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
(let ((backup-dir (my-emacs-dir "backup")))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t)))

(setq make-backup-files t) ; Create backup files
(setq backup-directory-alist `((".*" . ,(my-emacs-dir "backup")))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      2 ; how many of the newest versions to keep
      kept-old-versions      1) ; and how many of the old



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
			    ("America/New_York" "New York")
			    ("America/Buenos_Aires" "Buenos Aires")))

;; Add newlines at the end of the file
(setq require-final-newline 'visit-save)

;; Zone when idle
(when (equal fabri-profile 'work)
  (require 'zone)
  (zone-when-idle 300)
  (setq zone-programs (remove 'zone-pgm-random-life zone-programs)))

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



; Global keybindings
;;; enable global-evil-leader-mode before you enable evil-mode, otherwise
;;; evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*, …).
(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  ;; Sets the leader key to be space
  (evil-leader/set-leader "<SPC>")
  ;; Move between said windows
  (evil-leader/set-key "h" 'evil-window-left)
  (evil-leader/set-key "l" 'evil-window-right)
  (evil-leader/set-key "k" 'evil-window-up)
  (evil-leader/set-key "j" 'evil-window-down)
  ;; Run :wa with les typing
  (evil-leader/set-key "RET" 'evil-write-all)
  ;;Map meta x to leader :. A sort of mix of vim and emacs
  (evil-leader/set-key ";" 'execute-extended-command)
  )

(use-package evil
  ; enable global-evil-leader-mode before you enable evil-mode, otherwise
  ; evil-leader won’t be enabled in initial buffers (*scratch*, *Messages*).
  :after evil-leader
  :init
  :config
    (add-to-list 'evil-emacs-state-modes 'Info-mode)
    (add-to-list 'evil-emacs-state-modes 'world-clock-mode)
    (evil-set-initial-state 'Info-mode 'emacs)
    ; Modes that should be in Emacs mode
    ; Start evil mode
    (evil-mode 1)
    ;;;Makes Ctrl R work
    (evil-set-undo-system 'undo-redo)
  :bind
    (:map evil-motion-state-map
      ("RET" . nil)             ; Unbind C-n and C-p in insert mode so that
    :map evil-insert-state-map
      ("C-n" . nil)             ; Unbind C-n and C-p in insert mode so that
      ("C-p" . nil)             ; corfu takes precedence
      ;; ("C-<backspace>" . nothing-delete) ; Unmap delete
      ;; ("<backspace>" . nothing-delete)   ; Unmap delete
    :map evil-normal-state-map
      ("RET" . nothing)                  ; Unmap delete
      ("DEL" . nothing-delete)           ; Unmap delete
      ("<deletechar>" . nothing-delete)  ; Unmap delete
      ("C-<backspace>" . nothing-delete) ; Unmap delete
      ("g c" . comment-or-uncomment-region) ; g c comments a region
      ("g i" . indent-region)               ; g i indents a region
      ("<up>" . nothing-move)    ; Unbind arrow keys
      ("<down>" . nothing-move)  ; Unbind arrow keys
      ("<right>" . nothing-move) ; Unbind arrow keys
      ("<left>" . nothing-move)  ; Unbind arrow keys
    ))

;; Hydra
;; For multiple level menus
(use-package hydra
  :ensure t
  :config
  )

(add-to-list 'load-path (concat user-emacs-directory "org"))
(require 'fabri-org)


(add-to-list 'load-path (concat user-emacs-directory "utils"))
(require 'fabri-utils)

;;; Window related functions
(defhydra window-movements ()
  "Window movements"
  ("a" split-window-horizontally "Horizontal split")
  ("s" split-window-vertically "Vertical split")
  ("=" toggle-frame-maximized "Maximize")
  ("e" tab-new "New tab")
  ("4" tab-close "Close tab")
  ("b" kill-current-buffer "Kill buffer")
  ("r" consult-buffer "Switch to buffer")
  ("g" better-jumper-jump-backward "Backward")
  ("t" better-jumper-jump-forward "Forward")
  ("v" better-jumper-set-jump "Set jump")
  )
(evil-leader/set-key "v" 'window-movements/body)

;;; Shell
(defhydra shell-commands ()
  "Shell commands"
  ("&" project-async-shell-command "Project command")
  ("e" eshell "Eshell")
  ("t" term "Term")
  ("v" shell "Shell")
  )
(evil-leader/set-key "&" 'shell-commands/body)

(use-package term
  :init
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (evil-set-initial-state 'term-mode 'emacs)
  :bind
  (:map term-mode-map
        ("C-h" . help)
  :map term-raw-map
        ("C-h" . help))
  :ensure nil
  :hook (
         (term-mode . (lambda () (setq-local show-trailing-whitespace nil))))
  )
;; Shell mode config
(evil-leader/set-key-for-mode 'shell-mode "p" 'comint-previous-input)
(evil-leader/set-key-for-mode 'shell-mode "n" 'comint-next-input)

;;; File related functions
(defhydra file-functions ()
  "File functions"
  ("[" evil-save-and-close "Save and close file")
  ("r" reload-file "Reload a file")
  )
(evil-leader/set-key "[" 'file-functions/body)

;;; Find file related functionality
(defhydra find-file-functions ()
  "File functions"
  ("f" find-file "Find file")
  ("q" project-find-file "Project find file")
  )
(evil-leader/set-key "f" 'find-file-functions/body)


;;; Highlighting related functions
(defhydra highlight-functions ()
  "Highlight functions"
  ("h" highlight-regexp "Highlight")
  ("u" highlight-symbol-at-point "Current symbol")
  ("d" unhighlight-regexp "Unhighlight (C-u)")
  )
(evil-leader/set-key "y" 'highlight-functions/body)

;;; Grep related functions
(defhydra grep-functions ()
  "Grepers functions"
  ("g" consult-ripgrep "Ripgrep")
  ("/" consult-line "Swiper")
  ;; ("f" (lambda () (interactive) (setq current-prefix-arg '(4)) (call-interactively 'consult-grep)) "Async grep")
  ("f" consult-grep "Async grep")
  ("r" rgrep "Recursive grep")
  ("h" grep "grep")
  ("u" highlight-functions/body "Highlight menu" :exit t)
  )
(define-key evil-normal-state-map (kbd "/") 'grep-functions/body)

(use-package project
  :ensure nil
  :config
  (evil-leader/set-key "c" 'project-compile)
  )

;; Grep mode
;(setq-default grep-find-ignored-directories
;    (cons "target" grep-find-ignored-directories)
;    )
(use-package grep
  :ensure nil
  :config
  (evil-leader/set-key "c" 'project-compile)
  :bind
  (:map grep-mode-map
        ("g" . recompile)
        ("n" . next-error)))


;;Registers
;;; Register with files I open often
(set-register ?e (cons 'file (my-emacs-dir "init.el")))

; Text mode
(use-package markdown-mode
  )

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c /") #'place-question-mark)))
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key (kbd "C-c /") #'place-question-mark)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c /") #'place-question-mark)))

; Programming configuration
;; Color line
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(defhydra formatting-functions ()
  "Format functions"
  ("w" wrap-region "wrap-region")
)
(evil-leader/set-key "r" 'formatting-functions/body)

;;Magit
(use-package magit
  :custom
  (transient-default-level 7)
  (magit-blame-styles
   '((margin
      (margin-width . 52)
      (margin-format . ("%H %a %f"))
      (margin-face . magit-blame-margin)
      (margin-body-face . magit-blame-dimmed)
      (show-message . t))))
  :config
  (evil-leader/set-key "." 'magit-status)

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

  (advice-add 'magit-push-current-to-upstream :before #'query-magit-push-upstream)

  (advice-add 'magit-push-current-to-pushremote :before #'query-magit-push-upstream))

(use-package forge
  :after magit
  :custom
  (epa-pinentry-mode 'loopback)
  (auth-sources '("~/.authinfo.gpg"))
  )

;; Manage git forges directly from Magit
;; (use-package forge
;;   :after magit
;;   :custom
;;   (auth-sources '("~/.authinfo.gpg"))
;;   (epg-pinentry-mode 'loopback)
;;   (auth-source-debug t)
;;   :config
;;   (epa-file-enable)
;;   )

(use-package git-modes
  :ensure t
  )

;; Raibow delimiters
(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

;;Rainbow-mode
(use-package rainbow-mode
  :config
  (rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode)
  )

;; TODO Highlighter
(use-package hl-todo
  :custom
  (hl-todo-keyword-faces
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
     ("NOTE"      . "#1A02EB")))
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  )

;; Completion mode
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  :bind
  (:map prog-mode-map
   ("<tab>" . completion-at-point)
   :map corfu-map
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   ("<backspace>" . evil-delete-backward-char)
   )

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :custom
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

;; Add extensions
;; (use-package cape
;;   ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
;;   ;; Press C-c p ? to for help.
;;   :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
;;   ;; Alternatively bind Cape commands individually.
;;   ;; :bind (("C-c p d" . cape-dabbrev)
;;   ;;        ("C-c p h" . cape-history)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ...)
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-elisp-block)
;;   ;; (add-hook 'completion-at-point-functions #'cape-history)
;;   ;; ...
;; )


;; Minibuffer
(use-package vertico
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  ;; (context-menu-mode t)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


(use-package better-jumper
  :config
  (add-hook 'prog-mode-hook #'turn-on-better-jumper-mode)
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  ;; :bind (;; C-c bindings in `mode-specific-map'
  ;;        ("C-c M-x" . consult-mode-command)
  ;;        ("C-c h" . consult-history)
  ;;        ("C-c k" . consult-kmacro)
  ;;        ("C-c m" . consult-man)
  ;;        ("C-c i" . consult-info)
  ;;        ([remap Info-search] . consult-info)
  ;;        ;; C-x bindings in `ctl-x-map'
  ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ;;        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;;        ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ;;        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;;        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;;        ;; Custom M-# bindings for fast register access
  ;;        ("M-#" . consult-register-load)
  ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;        ("C-M-#" . consult-register)
  ;;        ;; Other custom bindings
  ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;;        ;; M-g bindings in `goto-map'
  ;;        ("M-g e" . consult-compile-error)
  ;;        ("M-g r" . consult-grep-match)
  ;;        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ;;        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ;;        ("M-g m" . consult-mark)
  ;;        ("M-g k" . consult-global-mark)
  ;;        ("M-g i" . consult-imenu)
  ;;        ("M-g I" . consult-imenu-multi)
  ;;        ;; M-s bindings in `search-map'
  ;;        ("M-s d" . consult-find)                  ;; Alternative: consult-fd
  ;;        ("M-s c" . consult-locate)
  ;;        ("M-s g" . consult-grep)
  ;;        ("M-s G" . consult-git-grep)
  ;;        ("M-s r" . consult-ripgrep)
  ;;        ("M-s l" . consult-line)
  ;;        ("M-s L" . consult-line-multi)
  ;;        ("M-s k" . consult-keep-lines)
  ;;        ("M-s u" . consult-focus-lines)
  ;;        ;; Isearch integration
  ;;        ("M-s e" . consult-isearch-history)
  ;;        :map isearch-mode-map
  ;;        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ;;        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ;;        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ;;        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
  ;;        ;; Minibuffer history
  ;;        :map minibuffer-local-map
  ;;        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ;;        ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :custom
  ;; Tweak the register preview delay
  (register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<") ;; "C-+"

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)


(use-package embark
  :ensure t

  :bind
  (("C-;" . embark-act)         ;; pick some comfortable binding
   ("C-'" . embark-dwim)        ;; good alternative: M-.
   ; ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   )

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)

  :init
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Save consult grep results into a minibuffer
;; Among other things
(use-package embark-consult
  )

;; Configure Tempel
(use-package tempel
  :bind (
         ;; ("C-<tab>" . tempel-next)
         ("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("<tab>" . tempel-next))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))

    ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
    ;; a trigger prefix character in order to prevent Tempel from triggering
    ;; unexpectly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
  )

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection if you want ready-made templates.
(use-package tempel-collection
  :ensure t
  :after tempel
)


(use-package eglot-tempel
  ;; :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))


;; Lsp completion
(use-package eglot
  :ensure nil
  :custom
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :hook (
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         ))

(defhydra eglot-functions () "Eglot functions"
  ("d" eldoc "Documentation" :exit t)
  ("r" eglot-rename "Rename" :exit t)
  ("u" eglot-code-actions "Actions" :exit t)
  )
(evil-leader/set-key "u" 'eglot-functions/body)

;; Eglot extensions
(use-package eglot-x
  :vc (:url "https://github.com/nemethf/eglot-x.git"
       :rev :newest))



;; GDB
;;;Enable many windows by default
(add-hook 'gdb-mode-hook #'gdb-many-windows)
;;; Show main source buffer when using GDB
(setq gdb-show-main t)

;; Programming keybinds

;;; Per-language hydras
(defhydra c-functions () "C functions"
  ("c" project-compile "Compile")
  ("t" gdb "GDB")
  ("3" ff-find-other-file "Other file")
  )
(defhydra c++-functions () "C++ functions"
  ("c" project-compile "Compile")
  ("t" gdb "GDB")
  ("3" ff-find-other-file "Other file")
  )
(defhydra python-functions () "Python functions")
(defhydra rust-functions () "Rust functions"
  ("c" project-compile "Compile")
  )
(defhydra default-prog-functions () "Default prog"
  ("c" project-compile "Compile")
  )


;;; Dispatch to the appropriate language hydra
(defun function-mode-dispatch-auto ()
  "Dispatch to the appropriate language hydra based on the current major mode."
  (interactive)
  (pcase major-mode
    ('c-mode (c-functions/body))
    ('c++-mode (c++-functions/body))
    ('python-mode (python-functions/body))
    ('rust-mode (rust-functions/body))
    (_ (default-prog-functions/body))))

(evil-leader/set-key "c" 'function-mode-dispatch-auto)

;; Editorconfig
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

; Emacs built in gadgets 

;; (provide 'compile)
(use-package compile
  :ensure nil
  :custom
  (compilation-skip-threshold 2)
  :bind
  (:map compilation-mode-map
        ("n" . next-error)
        ("c" . compile)
        ("g" . recompile)))

;; Calendar

(use-package calendar
  :ensure nil
  :custom
  (calendar-latitude -34.37)                               ; Set latitude
  (calendar-longitude -58.38)                              ; Set longitude
  (calendar-location-name "Buenos Aires, Argentina")       ; Location name
  (calendar-week-start-day 1)                              ; 0:Sunday, 1:Monday
  :config
  (calendar-set-date-style 'european) ; Set calendar style
  )



;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alhF")
  ;;; Dired will try to guess destination. If you have to open windows, then it
  ;;; will use the one next to it
  (dired-dwim-target t)
  ;;; Enable drag and drop
  (dired-mouse-drag-files t)
  :bind
  (:map dired-mode-map
        ("e" . dired-create-empty-file)
        ("i" . open-with)
        ("b" . dired-toggle-read-only) ; Edit files as a buffer
        ("g" . nil)                    ; Unbinds g key so that I can use gt to change window
        ("p" . revert-buffer)          ; Refresh buffer
   ))
;; (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "p") #'revert-buffer)))
;; (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "C-o") #'nil)))
;; (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "v") #'evil-visual-char)))


;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-show-ancestor t)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; :bind
  ;; (:map ediff-mode-map
  ;;       ("o" . other-window)))
  )



;; Ansi-color mode
(use-package ansi-color
  :ensure nil
  :init
  (defun my/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook
  (compilation-filter .  my/ansi-colorize-buffer)
  )

;; Emacs cal
(use-package calc
  :init
  (add-to-list 'evil-emacs-state-modes 'calc-mode)
  (evil-set-initial-state 'calc-mode 'emacs)
  :ensure nil
  :bind
  (:map calc-mode-map
        ("g t" . tab-bar-switch-to-next-tab)
        ("g T" . tab-bar-switch-to-prev-tab)
        )
  )

(defhydra calc-functions ()
  "Calc functions"
  ("c" calc "Calc" :exit t)
  ("v" full-calc "Full Calc" :exit t)
  )

(evil-leader/set-key "C-c" 'calc-functions/body)


; Additional extensions
;; WGrep mode - Edit grep buffer
(use-package wgrep
  :ensure t
  )

;; Yaml
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

;; Dockerfiles
(use-package dockerfile-mode
  )

;; JS/TS
(use-package typescript-mode
)

;; Rust mode
(use-package rust-mode
  :hook (
         (rust-mode . eglot-ensure)
         ))

;; Solidy
(use-package solidity-mode
  )

;;Dashboard
(use-package dashboard
  ; Done because :bind defers loading
  :demand t
  :init
  (add-to-list 'evil-emacs-state-modes 'dashboard-mode)
  (setq dashboard-set-file-icons t)
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-items '(
		                ;; (recents  . 5)
                          ;; (bookmarks . 5)
                          (projects . 10)
                          (agenda . 10)
                          (registers . 5)
		                ))
   ;;; Make dashboard the default when using a client
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (add-to-list 'dashboard-footer-messages '"Aguante Banfield")
  (evil-leader/set-key-for-mode 'dashboard-mode "SPC" 'dashboard-open)
  (dashboard-setup-startup-hook)
  :bind
    (:map dashboard-mode-map
      ("f" . find-file)             ; Unbind C-n and C-p in insert mode so that
    )
  )

(use-package all-the-icons
  :ensure t
  )

(use-package all-the-icons-dired
  :ensure t
  :config
  ;;; Enable dashboard-dired
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package all-the-icons-ibuffer :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

;; (use-package rfc-mode
;;   :init
;;   (add-to-list 'evil-emacs-state-modes 'rfc-mode)
;;   :config
;;   (rfc-mode-directory (my-emacs-dir "rfc"))
;;   :hook (
;;          (rfc-mode . (lambda () (setq-local show-trailing-whitespace nil)))))
(use-package htmlize
  )

(use-package adoc-mode
  )

;; RSS feed
(use-package newst-backend
  :ensure nil
  :init
  (add-to-list 'evil-emacs-state-modes 'newsticker-treeview-mode)
  (evil-set-initial-state 'newsticker-treeview-mode 'emacs)
  :config
  (progn
   (add-to-list 'newsticker-url-list '("Andrew Kelley" "https://andrewkelley.me/rss.xml"))
   (add-to-list 'newsticker-url-list '("Ryan Fleury" "https://www.dgtlgrove.com/feed"))
   )
  )


;; ;; Enable auto insert mode
;; (auto-insert-mode t)


;; (defun magit-ediff-read-files (revA revB &optional fileB)
;;   "Read file in REVB, return it and the corresponding file in REVA.
;; When FILEB is non-nil, use this as REVB's file instead of
;; prompting for it."
;;   (unless (and fileB (member fileB (magit-revision-files revB)))
;;     (setq fileB
;;           (or (and fileB
;;                    magit-buffer-log-files
;;                    (derived-mode-p 'magit-log-mode)
;;                    (member "--follow" magit-buffer-log-args)
;;                    (cdr (assoc fileB
;;                                (magit-renamed-files
;;                                 revB
;;                                 (oref (car (oref magit-root-section children))
;;                                       value)))))
;;               (magit-read-file-choice
;;                (format "File to compare between %s and %s"
;;                        revA (or revB "the working tree"))
;;                (magit-changed-files revA revB)
;;                (format "No changed files between %s and %s"
;;                        revA (or revB "the working tree"))))))
;;   (list (or (car (member fileB (magit-revision-files revA)))
;;             (cdr (assoc fileB (magit-renamed-files revB revA)))
;;             (magit-read-file-choice
;;              (format "File in %s to compare with %s in %s"
;;                      revA fileB (or revB "the working tree"))
;;              (magit-changed-files revB revA)
;;              (format "No files have changed between %s and %s"
;;                      revA revB)))
;;         fileB))
