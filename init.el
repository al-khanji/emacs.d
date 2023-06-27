;;;; init.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eshell)
(require 'dired-x)

;;; basic package setup
(progn
  (require 'package)
  (dolist (package-archive '(("melpa" . "https://melpa.org/packages/")
                             ("gnu" . "https://elpa.gnu.org/packages/")
                             ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
    (add-to-list 'package-archives package-archive))

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package gnu-elpa-keyring-update)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage))

(use-package doom-themes
  :config
  (load-theme 'doom-spacegrey t))

(when (find-font (font-spec :family "Iosevka Term"))
 (set-face-attribute 'default nil :font "Iosevka Term-16"))

;;; Contrary to what many Emacs users have in their configs, you don't need
;;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;;; set-language-environment sets default-input-method, which is unwanted
(setq default-input-method nil)

;;; lsp-mode documentation suggests tweaking this
(setq read-process-output-max (* 8 1024 1024)) ;; 8 MB

(setq inhibit-startup-screen t
      sentence-end-double-space nil
      ring-bell-function 'ignore
      visible-bell t
      use-dialog-box nil
      create-lockfiles nil
      compilation-scroll-output t
      native-comp-async-report-warnings-errors 'silent
      dired-dwim-target t
      make-backup-files nil)

;;; Always reload unchanged buffers if the underlying file changes on disk
(global-auto-revert-mode t)

;;; never mix tabs and spaces. Never use tabs, period.
;;; We need the setq-default here because this becomes
;;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(setq vc-handled-backends nil)

;;; Not set by default on macOS
(unless (getenv "DISPLAY")
  (setenv "DISPLAY" ":0"))

;;; Show column in modeline
(column-number-mode)

;;; highlight current line in prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'hl-line-mode))

;;; Line numbers everywhere, except ...
(global-display-line-numbers-mode t)
;;; ... for some modes
(dolist (mode '(org-mode-hook
                treemacs-mode-hook
                comint-mode-hook
                vterm-mode-hook
                sly-mrepl-mode-hook
                osm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Clickable links
(dolist (mode '(text-mode-hook prog-mode-hook vterm-mode-hook))
  (add-hook mode 'goto-address-mode))

(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; Some utility functions
(progn
  (cl-defun add-to-path (p &optional (append nil))
    "Add P to path variables: exec-path eshell-path-env $PATH.

Prepends by default, append by setting APPEND to non-nil."
    (interactive "GDirectory: \nP")

    (add-to-list 'exec-path p append)
    (let ((new-paths (string-join (append (unless append
                                            (list p))
                                          (eshell-get-path)
                                          (when append
                                            (list p)))
                                  path-separator)))
      (setq-default eshell-path-env new-paths)
      (setenv "PATH" new-paths)))

  (defun package-find-reqs (pkg)
    "Looks up the requirements for PKG from PACKAGE-ARCHIVE-CONTENTS.

 Returns a list of tuples (NAME VERSION) if found, otherwise nil. "
    (pcase (assoc pkg package-archive-contents)
      (`(,name ,desc) (package-desc-reqs desc))))

  (cl-defun fill-line (char &optional (width fill-column))
    (interactive "cFill character: \nP")
    (message "filling %c to column %d" char width)
    (save-excursion
      (end-of-line)
      (while (< (current-column) width)
        (insert-char char)))))

;;; macOS specials
(defvar *think-different* (eq system-type 'darwin))
(defvar *homebrew-coreutils-gnubin* "/usr/local/opt/coreutils/libexec/gnubin")

(when *think-different*
  ;; Make emojis work
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  ;; Make ⌘-w close the current
  (bind-key "s-w" #'kill-this-buffer)
  (unbind-key "C-z")
  (unbind-key "s-o"))

(unless *think-different*
  (menu-bar-mode 0))

(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode)
  (setq ns-use-proxy-icon nil))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (when *think-different*
    (add-to-path *homebrew-coreutils-gnubin*)))

;;; Custom github bits and pieces
(progn
  (defvar *local-github-dir* (expand-file-name "~/github"))
  (defvar *github-urls* '((:https "https://github.com/")
                          (:ssl "git@github.com:")))
  (defvar *git-config-alist* '(("core.autocrlf" . "input")
                               ("rebase.stat" . "true")
                               ("color.ui" . "auto")
                               ("core.pager" . "\"less -FRSX\"")
                               ("alias.di" . "diff")
                               ("alias.ci" . "commit")
                               ("alias.co" . "checkout")
                               ("alias.ann" . "blame")
                               ("alias.st" . "status")))

  (cl-defun config-my-git (&optional (config-alist *git-config-alist*))
    (interactive)
    (when (executable-find "git")
      (pcase-dolist (`(,k . ,v) config-alist)
        (shell-command (format "git config --global %s %s" k v)))))

  (defun local-github-subdir (dir)
    (expand-file-name dir *local-github-dir*))

  (defun github-clone (repo &optional arg)
    (interactive "sRepository: \nP")
    (pcase-let* ((scheme (if arg :ssl :https))
                 (`(,scheme ,url) (assoc scheme *github-urls*))
                 (`(,owner ,project) (split-string repo "/"))
                 (src (concat url repo ".git"))
                 (dst (local-github-subdir owner)))
      (message "github-clone %s -> %s" src dst)
      (magit-clone-regular src dst nil))))

;;; Erlang stuff
(progn
  (defvar *erlang-binary* (executable-find "erl"))

  (defun erl-eval-print (expr)
    (interactive "sErlang expression: ")
    (catch 'no-erl
      (unless (and (stringp *erlang-binary*)
                   (executable-find *erlang-binary*))
        (when (called-interactively-p 'any)
          (message (format "bad *erlang-binary* => %s" *erlang-binary*)))
        (throw 'no-erl nil))
      (with-temp-buffer
        (let* ((printable-filter-fn (concat "fun (F) when is_float(F) -> erlang:float_to_binary(F);"
                                            "    (I) when is_integer(I) -> erlang:integer_to_binary(I);"
                                            "    (X) -> X end"))
               (apply-printable-expr (format "erlang:apply(%s, [%s])" printable-filter-fn expr))
               (eval-str (format "io:format(\"~s\", [%s])" apply-printable-expr))
               (exit-code (call-process *erlang-binary* nil (current-buffer) nil
                                        "-noinput" "-eval" eval-str "-s" "erlang" "halt" "-env" "ERL_CRASH_DUMP" "/dev/null"))
               (output (buffer-string)))
          (when (called-interactively-p 'any)
            (message output))
          (when (zerop exit-code)
            output))))))

(use-package logview
  :ensure t
  :custom-face
  (logview-error-entry ((t (:inherit error))))
  (logview-warning-entry ((t (:inherit warning))))
  (logview-information-entry ((t (:inherit default))))
  (logview-debug-entry ((t (:inherit shadow)))))

(use-package org
  :custom
  (org-startup-indented t)
  (org-adapt-indentation t)
  :config
  ;; load more export backends
  (dolist (backend '(ox-beamer ox-md))
    (require backend)))

(use-package org-contrib
  :after org
  :config
  (require 'ox-confluence))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update nil)
  (auto-package-update-hide-results t)
  :config
  (add-hook 'after-init-hook #'auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package beacon
  :config
  (beacon-mode 1)
  :custom
  (beacon-blink-when-focused t)
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-blink-when-point-moves-horizontally 20))

(use-package ivy
  :bind ("C-s" . swiper)
  :config
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  (push (cons 'counsel-M-x "") ivy-initial-inputs-alist))

(use-package ivy-rich
  :after (counsel ivy)
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package amx)

(use-package counsel
  :after (amx)
  :config
  (counsel-mode +1))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package projectile
  :after (ivy)
  :init
  (projectile-mode +1)
  :custom
  (projectile-indexing-method 'hybrid)
  (projectile-enable-caching t)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq frame-title-format `((buffer-file-name "%f" "%b") " [" (:eval (projectile-project-name)) "]")))

(use-package counsel-projectile
  :after (counsel projectile)
  :init
  (counsel-projectile-mode))

(defun lma-ace-window (arg)
  (interactive "p")
  (cl-case arg
    ;; Move buffer to target window, select target window, delete old window
    (64 (aw-select " Move buffer to window killing old window"
                   (lambda (window)
                     (let* ((buffer (current-buffer))
                            (old-window (selected-window)))
                       (set-window-buffer window buffer)
                       (select-window window)
                       (delete-window old-window)))))
    (t (ace-window arg))))

(use-package ace-window
  :custom
  (aw-swap-invert t)
  :bind (("M-o" . lma-ace-window)))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package sly
  :config
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "2048") :coding-system utf-8-unix))))

(use-package sly-macrostep
  :after sly
  :config
  (load "sly-macrostep-autoloads"))

(use-package ag)

(use-package rg)

(use-package company
  :after (ag rg)
  :custom
  (lsp-completion-provider :capf)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  :hook ((ielm-mode prog-mode sly-mrepl-mode) . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)))

;; (use-package eglot
;;   :custom
;;   (eglot-extend-to-xref t)
;;   (eglot-confirm-server-initiated-edits nil))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package minions
  :config (minions-mode 1))

(use-package magit
  :bind (("C-x g" . magit-status))
  :custom
  (magit-git-executable (executable-find "git")))

(use-package yaml-mode)

(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines)
         ("C-c m d" . #'mc/mark-all-dwim)))

(use-package paredit
  :bind (:map lisp-mode-shared-map
              ("RET" . paredit-newline))
  :config
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
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

(use-package erlang)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((erlang-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-lens-enable nil)
  :commands lsp)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp-deferred))))

(use-package clang-format)

(use-package lsp-ui
  :after (lsp-mode)
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-peek-enable t
        lsp-ui-peek-show-directory t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip)))))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (define-key treemacs-mode-map [drag-mouse-1] nil)
  :hook (after-init . treemacs)
  :bind (:map global-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t d"   . treemacs-select-directory)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag)))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package esup
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0))

(use-package gcmh
  :custom
  (gcmh-high-cons-threshold (* 2 1024 1024 1024))
  :config
  (gcmh-mode 1))

;;; fix M-. (aka xref-find-definitions) in ielm
(use-package ielm
  :hook (ielm-mode . (lambda ()
                       (add-hook 'xref-backend-functions #'elisp--xref-backend nil t))))

(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-buffer-name-string "vterm %s")
  (vterm-copy-mode-remove-fake-newlines t))

(use-package with-editor
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . 'with-editor-export-editor)
  :bind (([remap async-shell-command] . 'with-editor-async-shell-command)
         ([remap shell-command] . 'with-editor-shell-command)))

(use-package saveplace
  :config
  (save-place-mode))

(use-package osm
  :bind (("C-c m h" . osm-home)
   	 ("C-c m s" . osm-search)
   	 ("C-c m v" . osm-server)
   	 ("C-c m t" . osm-goto)
   	 ("C-c m x" . osm-gpx-show)
   	 ("C-c m j" . osm-bookmark-jump))
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))

(use-package minimap
  :custom
  (minimap-window-location 'right))

(use-package cmake-mode)

(use-package default-text-scale)

(load "~/.emacs.d/site-config" 'noerror)

(server-start)

(provide 'init)

(put 'scroll-left 'disabled nil)

