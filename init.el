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

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)

  (setq use-package-always-ensure t
        use-package-compute-statistics t))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage))

(defvar *think-different* (eq system-type 'darwin))
(defvar *homebrew-coreutils-gnubin* "/usr/local/opt/coreutils/libexec/gnubin")

(defun lma/set-font (family height &optional slant)
  (setq slant (or slant 'normal))
  (when-let ((font (find-font (font-spec :family family :slant slant))))
    (set-face-attribute 'default nil :font font :height height)))

(let ((inhibit-redisplay t))
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (when (and (display-graphic-p) (not *think-different*))
    (menu-bar-mode -1))

  (lma/set-font "Iosevka Term" 160)

  (when *think-different*
    ;; Make emojis work
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

  (use-package ef-themes
    :config
    (load-theme 'ef-spring t)))

;;; no blinky
(blink-cursor-mode -1)

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

;;; Work around mutter being stupid when it comes to resizes
(setq x-gtk-resize-child-frames 'hide)

;;; Contrary to what many Emacs users have in their configs, you don't need
;;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;;; set-language-environment sets default-input-method, which is unwanted
(setq default-input-method nil)

;;; never mix tabs and spaces. Never use tabs, period.
(customize-set-variable 'indent-tabs-mode nil)

;;; Always reload unchanged buffers if the underlying file changes on disk
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;;; Show column in modeline
(column-number-mode)

;;; highlight current line in prog and text modes
(use-package hl-line
  :hook ((text-mode prog-mode) . hl-line-mode))

;;; Line numbers in some modes by default
(use-package display-line-numbers
  :hook ((text-mode prog-mode) . display-line-numbers-mode))

;;; Make links clickable
(use-package goto-addr
  :hook (after-init . global-goto-address-mode))

;;; Some utility functions
(progn
  (defun project-save-buffers (&optional arg)
    "Save modified file-visiting buffers of current project.
Based on `save-some-buffers', refer to its documentation about interactive
behavior and the optional argument ARG."
    (interactive "P")
    (save-excursion
      (when-let* ((project (project-current))
                  (buffers (project-buffers project))
                  (predicate (lambda ()
                               (memq (current-buffer) buffers))))
        (save-some-buffers arg predicate))))
  
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

(use-package ielm
  :defer t
  :hook (ielm-mode . (lambda ()
                       (add-hook 'xref-backend-functions 'elisp--xref-backend nil t))))

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

(use-package bind-key
  :config
  ;; Too close to M-o and not needed
  (unbind-key "s-o")
  ;; Annoying shortcut to suspend-frame
  (when (display-graphic-p)
    (unbind-key "C-z"))
  (when *think-different*
    (bind-key "s-w" 'kill-this-buffer))
  (bind-keys
   ("s-[" . previous-window-any-frame)
   ("s-]" . next-window-any-frame)))

(use-package gnu-elpa-keyring-update)

(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode)
  (setq ns-use-proxy-icon nil))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (when *think-different*
    (add-to-path *homebrew-coreutils-gnubin*)))

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

(use-package marginalia
  :bind ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

(recentf-mode)
(add-hook 'buffer-list-update-hook 'recentf-track-opened-file)

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
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0)
  :hook (after-init . which-key-mode))

(use-package sly
  :defer t
  :config
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "2048") :coding-system utf-8-unix))))

(use-package sly-macrostep
  :after sly)

(use-package minions
  :config (minions-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status))

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
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-popupinfo-delay 0.25))

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
  (setq enable-recursive-minibuffers t))

(use-package savehist
  :init
  (savehist-mode))

(use-package eglot
  :defer t
  :custom
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  (eglot-report-progress 'messages)
  :config
  (add-to-list 'eglot-server-programs
               `((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ,(eglot-alternatives
                     '("ccls" "clangd"))))
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

(use-package gcmh)

(use-package vterm
  :defer t
  :custom
  (vterm-kill-buffer-on-exit nil)
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

(load "~/.emacs.d/site-config" 'noerror 'nomessage)

(server-start)

(provide 'init)

