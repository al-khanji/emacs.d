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

  (setq use-package-always-ensure t))

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage))

(use-package ef-themes
  :config
  (load-theme 'ef-spring t))

(when (find-font (font-spec :family "Iosevka Term"))
  (set-face-attribute 'default nil :font "Iosevka Term-16"))

(use-package use-package-ensure-system-package)

(use-package gnu-elpa-keyring-update)

;;; Contrary to what many Emacs users have in their configs, you don't need
;;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;;; set-language-environment sets default-input-method, which is unwanted
(setq default-input-method nil)

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

(bind-key "s-[" 'previous-window-any-frame)
(bind-key "s-]" 'next-window-any-frame)

;;; no blinky
(blink-cursor-mode -1)

;;; Work around mutter being stupid when it comes to resizes
(setq x-gtk-resize-child-frames 'hide)

;;; Always reload unchanged buffers if the underlying file changes on disk
(global-auto-revert-mode t)

;;; never mix tabs and spaces. Never use tabs, period.
;;; We need the setq-default here because this becomes
;;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

;;; Show column in modeline
(column-number-mode)

;;; highlight current line in prog and text modes
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'hl-line-mode))

;;; Line numbers in some modes by default
(dolist (hook '(text-mode-hook prog-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;;; Make links clickable
(global-goto-address-mode)

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
  ;; Too close to M-o and not needed
  (unbind-key "s-o"))

(bind-key (if *think-different* "s-w" "C-w") 'kill-this-buffer)

;;; Annoying shortcut to suspend-frame
(when (display-graphic-p)
    (unbind-key "C-z"))

(menu-bar-mode (if *think-different* 1 0))

(add-hook 'ielm-mode-hook
 (lambda ()
   (add-hook 'xref-backend-functions 'elisp--xref-backend nil t)))

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

(use-package beacon
  :config
  (beacon-mode 1)
  :custom
  (beacon-blink-when-focused t)
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-blink-when-point-moves-horizontally 20))

(use-package marginalia
  :bind ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

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
  :config
  (which-key-mode))

(use-package sly
  :ensure-system-package sbcl
  :config
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl") "--dynamic-space-size" "2048") :coding-system utf-8-unix))))

(use-package sly-macrostep)

(use-package minions
  :config (minions-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure-system-package git)

(use-package yaml-mode)

(use-package multiple-cursors
  :bind (("C-c m m" . 'mc/edit-lines)
         ("C-c m d" . 'mc/mark-all-dwim)))

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

(use-package erlang)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package corfu
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("RET" . nil))
  :custom
  (corfu-popupinfo-delay 0.25))

;; (use-package vertico
;;   :init
;;   (vertico-mode))

;; (use-package savehist
;;   :init
;;   (savehist-mode))

(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  (eglot-report-progress 'messages))

(defun project-save-buffers (&optional arg)
  "Save modified file-visiting buffers of current project.
Based on `save-some-buffers', refer to its documentation about interactive
behavior and the optional argument ARG."
  (interactive "P")
  (save-excursion
    (when-let* ((project (project-current))
                (buffers (project-buffers project)))
      (save-some-buffers arg
                         (lambda ()
                           (memq (current-buffer) buffers))))))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (eglot-ensure))))

(use-package google-c-style
  :config
  (c-add-style "Google" google-c-style)
  (dolist (mode '(c-mode c++-mode))
    (push `(,mode . "Google") c-default-style)))

(use-package clang-format)

(use-package treemacs
  :config
  ;; Disable dragging, too easy to move things around accidentally
  (define-key treemacs-mode-map [drag-mouse-1] nil))

(use-package treemacs-magit)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package esup)

(use-package gcmh)

(use-package vterm
  :ensure-system-package cmake
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

(use-package cmake-mode)

(load "~/.emacs.d/site-config" 'noerror)

(server-start)

(provide 'init)

