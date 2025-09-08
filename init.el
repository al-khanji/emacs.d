;;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'dired-x)
(require 'lma-lib)

(defvar *auto-save-dir-name* "auto-save/")
(defvar *custom-file-name* "custom.el")

(lma/init-use-package)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name *auto-save-dir-name*) t))
        custom-file (no-littering-expand-etc-file-name *custom-file-name*))
  (load custom-file 'noerror 'nomessage))

(use-package fontaine
  :custom
  (fontaine-presets '((small
                       :default-height 120)
                      (smallish
                       :default-height 130)
                      (regular
                       :default-height 140)
                      (large
                       :default-height 170)
                      (large-but-light
                       :default-height 170
                       :default-weight semilight)
                      (t
                       :default-family "Iosevka Fixed"))))

(let ((inhibit-redisplay t))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (when (and (display-graphic-p) (not *think-different*))
    (menu-bar-mode -1))

  (fontaine-set-preset 'regular)

  (when *think-different*
    ;; Make emojis work
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

  (context-menu-mode)
  (setq frame-title-format "%F %* %f")

  (use-package doom-modeline
    :init
    (doom-modeline-mode))

  (use-package ef-themes
    :config
    (load-theme 'ef-spring t)))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; recentf-mode reads some files on startup, delay it for later
  (add-hook 'after-init-hook
            (lambda ()
              (recentf-mode)
              (add-hook 'buffer-list-update-hook
                        'recentf-track-opened-file)))

  ;; no blinky
  (blink-cursor-mode -1)

  (setq inhibit-startup-screen t        ; no startup screen
        sentence-end-double-space nil   ; one space only thx
        ring-bell-function 'ignore      ; no audible bell
        visible-bell t                  ; yes visible bell
        use-dialog-box nil              ; no dialog boxes
        create-lockfiles nil            ; no lock files
        native-comp-async-report-warnings-errors 'silent ; silence the deluge
        dired-dwim-target t        ; come on dired
        make-backup-files nil      ; no backup files
        split-height-threshold 120 ; I really prefer windows side by side
        )

  ;; Work around mutter being stupid when it comes to resizes
  (setq x-gtk-resize-child-frames 'hide)

  ;; Contrary to what many Emacs users have in their configs, you don't need
  ;; more than this to make UTF-8 the default coding system:
  (set-language-environment "UTF-8")

  ;; set-language-environment sets default-input-method, which is unwanted
  (setq default-input-method nil)

  ;; never mix tabs and spaces. Never use tabs, period.
  (customize-set-variable 'indent-tabs-mode nil)

  ;; Show column in modeline
  (column-number-mode)

  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)

  ;; Print some startup statistics
  (add-hook 'after-init-hook 'lma/print-startup-time 100)

  ;; Set WINDOWID for e.g. zenity
  (add-hook 'after-init-hook 'lma/set-WINDOWID)
  (add-function :after after-focus-change-function 'lma/set-WINDOWID)

  ;; Please emacs do the work for me
  (electric-pair-mode)
  (electric-indent-mode))

(use-package bind-key
  :config
  ;; Too close to M-o and not needed
  (unbind-key "s-o")
  ;; Annoying shortcut to suspend-frame
  (when (display-graphic-p)
    (unbind-key "C-z"))
  (when *think-different*
    (bind-key "s-w" 'kill-this-buffer)
    (unbind-key "s-p"))
  (bind-keys
   ("s-[" . previous-window-any-frame)
   ("s-]" . next-window-any-frame)
   ("M-[" . previous-window-any-frame)
   ("M-]" . next-window-any-frame)))

;;; Always reload unchanged buffers if the underlying file changes on disk
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;;; highlight current line in prog and text modes
(use-package hl-line
  :hook ((text-mode prog-mode) . hl-line-mode))

;;; Line numbers in some modes by default
(use-package display-line-numbers
  :custom
  (display-line-numbers-width-start t)
  :hook ((text-mode prog-mode) . display-line-numbers-mode))

;;; Make links clickable
(use-package goto-addr
  :hook (after-init . global-goto-address-mode))

;;; Fix ielm xref
(use-package ielm
  :defer t
  :hook (ielm-mode . (lambda ()
                       (add-hook 'xref-backend-functions 'elisp--xref-backend nil t))))

(use-package gnu-elpa-keyring-update)

(use-package ns-auto-titlebar
  :if *think-different*
  :config
  (ns-auto-titlebar-mode)
  (setq ns-use-proxy-icon nil))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (defvar *homebrew-coreutils-gnubin* "/usr/local/opt/coreutils/libexec/gnubin")
  (when *think-different*
    (lma/add-to-path *homebrew-coreutils-gnubin*)))

(use-package logview
  :defer t
  :custom-face
  (logview-error-entry ((t (:inherit error))))
  (logview-warning-entry ((t (:inherit warning))))
  (logview-information-entry ((t (:inherit default))))
  (logview-debug-entry ((t (:inherit shadow)))))

(use-package org
  :custom
  (org-startup-indented t)
  (org-adapt-indentation t)
  :defer t
  :hook (org-mode . (lambda ()
                      ;; load more export backends
                      (require 'ox-beamer)
                      (require 'ox-md))))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory))
  (org-roam-setup))

(use-package org-noter
  :defer t)

(use-package marginalia
  :bind ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

(use-package ace-window
  :custom
  (aw-swap-invert t)
  :config
  (defun lma/ace-window (arg)
    (interactive "p")
    (cond
     ;; Move buffer to target window, select target window, delete old window
     ((eql arg 64) (aw-select " Move buffer to window killing old window"
                              (lambda (window)
                                (let* ((buffer (current-buffer))
                                       (old-window (selected-window)))
                                  (set-window-buffer window buffer)
                                  (select-window window)
                                  (delete-window old-window)))))
     (t (ace-window arg))))
  :bind (("M-o" . lma/ace-window)))

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0)
  :hook (after-init . which-key-mode))

;; (use-package slime
;;   :defer t
;;   :custom
;;   (slime-net-coding-system 'utf-8-unix)
;;   (slime-compilation-finished-hook 'slime-maybe-list-compiler-notes)
;;   :config
;;   (setq slime-contribs '(slimy-fuzzy slime-fancy slime-asdf slime-quicklisp))
;;   (add-to-list 'slime-lisp-implementations
;;                `(sbcl ("sbcl" "--dynamic-space-size=2048") :coding-system ,slime-net-coding-system)))

(use-package sly)

(use-package sly-macrostep
  :after sly)

(use-package minions
  :config (minions-mode 1))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x p g" . project-magit-status))
  :config
  (defun project-magit-status (&optional prefix)
    "Open magit status for current project. Prompt for project if none current or prefix argument given."
    (interactive "P")
    (magit-status (if (and (project-current) (not prefix))
                      (project-root (project-current))
                    (project-prompt-project-dir)))))

(use-package yaml-mode)

(use-package multiple-cursors
  :defer t
  :bind (:prefix-map multiple-cursors-map
                     :prefix "C-c m"
                     :prefix-docstring "multiple cursors"
                     ("m" . 'mc/edit-lines)
                     ("d" . 'mc/mark-all-dwim)))

(use-package paredit
  :bind (:map lisp-mode-shared-map
              ("RET" . paredit-newline)
              :map paredit-mode-map
              (("RET" . nil)
               ("C-j" . paredit-newline)))
  :hook (((emacs-lisp-mode
           lisp-mode
           lisp-data-mode
           lisp-interaction-mode
           scheme-mode
           ielm-mode
           slime-repl-mode
           sly-mrepl-mode
           eval-expression-minibuffer-setup
           slime-mode
           clojure-mode) . enable-paredit-mode)
	 (slime-repl-mode . (lambda ()
			      (define-key slime-repl-mode-map
				(read-kbd-macro paredit-backward-delete-key) nil)))))

(use-package paren
  :custom
  (show-paren-style 'mixed)
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode 1))

(use-package erlang
  :defer t)

(use-package editorconfig
  :hook (find-file . editorconfig-mode))

(use-package corfu
  :init
  (setq tab-always-indent 'complete
        tab-first-completion 'word-or-paren-or-punct
        corfu-popupinfo-hide nil)
  (corfu-popupinfo-mode)
  (global-corfu-mode)
  :custom
  (corfu-popupinfo-delay 0))

;; (use-package cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package vertico
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :bind (([remap isearch-forward] . consult-line)
         ([remap isearch-backward] . consult-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap yank-pop] . consult-yank-pop)
         ([remap project-switch-to-buffer] . consult-project-buffer))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package eglot-booster
  :init
  (unless (package-installed-p 'eglot-booster)
    (package-vc-install "https://github.com/jdtsmith/eglot-booster"))
  :after eglot
  :config (eglot-booster-mode))

(use-package eglot
  :defer t
  :custom
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  :hook ((c-mode c++-mode erlang-mode python-mode) . eglot-ensure))

(use-package google-c-style
  :hook (c-mode-common . (lambda ()
                           (c-add-style "Google" google-c-style t))))

(use-package clang-format
  :defer t)

(use-package treemacs
  :defer t
  :bind (:map treemacs-mode-map
         ;; Disable dragging, too easy to do things accidentally
         ([drag-mouse-1] . nil)))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package esup
  :defer t)

(use-package gcmh
  :config
  (add-hook 'after-init-hook 'gcmh-mode))

;;; Add to bashrc:
;;; if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
;;;   . ~/.emacs.d/vterm/bash.rc
;;; fi
(use-package vterm
  :defer t
  :custom
  (vterm-always-compile-module t)
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-buffer-name-string "vterm %s")
  (vterm-copy-mode-remove-fake-newlines t))

(use-package with-editor
  :defer t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . 'with-editor-export-editor)
  :bind (([remap async-shell-command] . 'with-editor-async-shell-command)
         ([remap shell-command] . 'with-editor-shell-command)))

(use-package saveplace
  :config
  (save-place-mode))

(use-package cmake-mode
  :defer t)

(use-package wgrep)

(use-package wgrep-deadgrep)

(use-package markdown-mode)

(use-package bazel)

(use-package csv-mode
  ;; Match .csv or .tsv, case-insensitive
  :mode ("(?i)\\.\\(csv\\|tsv\\)\\'" . csv-mode)
  :hook ((csv-mode . lma/csv-setup))
  :config
  (defun lma/csv-setup ()
    "Custom CSV/TSV mode setup without assuming headers."
    (setq-local csv-align-padding 1)  ;; one space between columns
    (csv-align-mode 1)))              ;; auto-align columns

(server-start)

(load "site-config" 'noerror)

(provide 'init)

(put 'upcase-region 'disabled nil)
