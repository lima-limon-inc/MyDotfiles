;; Set up package.el to work with MELPA
(package-initialize)
(require 'package)
(require 'use-package)

;; Adding Melpa to the list (VIM Awesome like webasite)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("org" . "https://orgmode.org/elpa/"))
;; Enable Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(require 'evil-numbers)
(use-package evil-numbers
  :ensure t)
(global-unset-key (kbd "C-a")) ;Unsets the ctrl a key from it's original keybinding
(global-unset-key (kbd "C-z")) ;Unsets the ctrl a key from it's original keybinding
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-z") 'evil-numbers/dec-at-pt)
;; Enable leader like behaviour from vim
;;(evil-set-leader nil "\\")
(define-prefix-command 'my-window-map)

(let ((map my-window-map))
  (define-key map (kbd "<up>") 'evil-window-up)
  (define-key map (kbd "<down>") 'evil-window-down)
  (define-key map (kbd "<left>") 'evil-window-left)
  (define-key map (kbd "<right>") 'evil-window-right)

  ;; And presumably, for opening/closing
  (define-key map "v" 'evil-window-vsplit)
  (define-key map "s" 'evil-window-split)
  (define-key map "c" 'evil-window-delete)

  ;; Window resize commands TODO: Not working
  (define-key map (kbd ">") 'evil-shift-right)
  (define-key map (kbd "<") 'evil-shift-left)

 ;;Magit commands
  (define-key map "g" 'magit-status)

  ;;Buffer related
  (define-key map "k" 'kill-current-buffer)
  ;(define-key map "b" 'helm-buffer-list)

)

(define-key evil-normal-state-map "\\" 'my-window-map)
;(define-key evil-normal-state-map (kbd "C-f") 'dired)

;; Enable org-mode
(use-package org
  :ensure t)
;; Add here relocation to org mode

(use-package keychain-environment
  :ensure t)
(keychain-refresh-environment)


;; Auto-complete
;; TAB-and-Go customizations
;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   ;(global-auto-complete-mode t)
;;   (ac-set-trigger-key "TAB")
;;   (setq ac-auto-start nil)
;;   :init
;;   ;(setq ac-auto-start nil)
;;   )
;; (ac-config-default)
;; (global-auto-complete-mode t)
;; (add-to-list 'ac-modes 'asm-mode)
;; (add-to-list 'ac-modes 'go-mode)
;; (add-to-list 'ac-modes 'tex-mode)
;; Enable auto completion and configure quitting

(use-package corfu
  ;; Optional customizations
  :ensure t
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-popupinfo-mode t)
  (corfu-info-documentation 0.25)
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
 ;; :hook (prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :bind
    (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))


;; A few more useful configurations...
(use-package emacs
  :ensure t
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 2)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  )


;; Add extensions
;; (use-package cape
;;   ;:defer 10
;;   :bind ("C-c f" . cape-file)
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
;;   (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
;;   (cl-pushnew #'cape-file completion-at-point-functions)
;;   :config
;;   ;; Silence then pcomplete capf, no errors or messages!
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;;   ;; Ensure that pcomplete does not write to the buffer
;;   ;; and behaves as a pure `completion-at-point-function'.
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)


;; (use-package cape
;;   :ensure t
;;   :defer 10
;;   :bind ("C-c f" . cape-file)
;;   :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
;;   (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
;;   (cl-pushnew #'cape-file completion-at-point-functions)
;;   :config
;;   ;; Silence then pcomplete capf, no errors or messages!
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;;   ;; Ensure that pcomplete does not write to the buffer
;;   ;; and behaves as a pure `completion-at-point-function'.
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify));; Helm installation

(use-package helm
  :init
    (require 'helm-config)
    (setq helm-split-window-in-side-p t 
	helm-move-to-line-cycle-in-source t) ;;Makes it so that helm wraps around and does not occupy too much space
    
    :config
    (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
    (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
    (global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
    (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
    (global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
    (global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
    (global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
    (global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
    (global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
    (define-key evil-ex-map "z" 'helm-M-x)  ;; Imrpoved M-X  menu 
    (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
  :ensure t)

;; Line numbering
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Beacon highlights the current line in the buffer
(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

;; Powerline like programm
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful))
;(load-theme 'respectful t)
;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>
(sml/setup)

;; Magit package: Git integration
(use-package magit
  :ensure t)

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(setq x-select-enable-clipboard t) ;; Makes system and emacs clipboard the same
(defalias 'ter 'shell) ;;Renames the command for the shell
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-:") 'helm-M-x))

(scroll-bar-mode -1);; Hides Scroll bar, menu bar, tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)
;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      2 ; how many of the newest versions to keep
      kept-old-versions      1) ; and how many of the old

(set-fringe-mode 0)

;;(define-key evil-normal-state ":" 'helm-M-x)
;;(global-set-key ":" 'helm-M-x)
;;(global-set-key (kbd "M-x") 'evil-ex)

;; Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t)
  :ensure t)

(use-package rainbow-mode ;Shows colors in their respective color
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters ;Shows different color parentheses
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package comment-tags ;Shows different color parentheses
  :ensure t
  :hook (prog-mode . comment-tags-mode)
  :init
  (setq comment-tags-case-sensitive 'nil)
  (setq comment-tags-lighter t)
  )
  
(use-package neotree
  :ensure t)
;(global-set-key (kbd "C-f") 'neotree-toggle)
(define-key evil-normal-state-map (kbd "C-f") 'neotree-toggle)
(setq neo-theme 'ascii)



;; (use-package lsp-mode ;;Todo: Configure later
;;   :ensure t)

;; Try package
(use-package try ;;Package that allows you to try packages without needing to download it
  :ensure t)

;; Languages mode download section
(use-package asm-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package auctex
  :ensure t
  :defer t)

;; Show main source buffer when using GDB
(setq gdb-show-main t)




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(neotree keychain-environment rainbow-delimiters rainbow-mode lsp-mode corfu auctex go-mode magit evil-collection helm use-package evil))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
