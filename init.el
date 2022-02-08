;;; init.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eshell)

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; set-language-enviornment sets default-input-method, which is unwanted
(setq default-input-method nil)

(setq inhibit-startup-screen t
      sentence-end-double-space nil
      ring-bell-function 'ignore
      visible-bell t
      use-dialog-box nil
      create-lockfiles nil
      compilation-scroll-output t
      frame-title-format `((buffer-file-name "%f" "%b")
                           " [" (:eval (projectile-project-name)) "]"))

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

;; Not set by default on macOS
(unless (getenv "DISPLAY")
  (setenv "DISPLAY" ":0"))

;(delete-selection-mode t)
(column-number-mode)

;; Line numbers everywhere, except ...
(global-display-line-numbers-mode t)
;; ... for some modes
(dolist (mode '(org-mode-hook
                treemacs-mode-hook
                comint-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Some utility functions
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


;; basic package setup
(progn
  (require 'package)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  
  (unless package-archive-contents
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package doom-themes
  :config
  (load-theme 'doom-spacegrey t))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; macOS specials
(progn
  (defvar *think-different* (eq system-type 'darwin))
  (defvar *homebrew-coreutils-gnubin* "/usr/local/opt/coreutils/libexec/gnubin")

  (when *think-different*
    ;; Make emojis work
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
    ;; Make ⌘-w close the current
    (bind-key "s-w" #'kill-this-buffer)
    (unbind-key "C-z"))

  (use-package ns-auto-titlebar
    :if *think-different*
    :config
    (ns-auto-titlebar-mode)
    (setq ns-use-proxy-icon nil))

  ;; Get exec path from shell on mac, by default some dirs are missing
  (use-package exec-path-from-shell
    :if *think-different*
    :config  
    (exec-path-from-shell-initialize)
    (add-to-path *homebrew-coreutils-gnubin*)))

;; Custom github bits and pieces
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

;; Erlang stuff
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

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file 'noerror 'nomessage))

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
        ivy-count-format "(%d/%d) "))

(use-package ivy-rich
  :after (counsel ivy)
  :config
  (ivy-rich-mode +1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package counsel
  :config
  (counsel-mode +1))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package sly
  :config
  (setq sly-lisp-implementations
        `((sbcl (,(executable-find "sbcl")) :coding-system utf-8-unix))))

(use-package ag)

(use-package rg)

(use-package company
  :after (ag rg)
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)))

;; (use-package slime
;;   :bind ("C-c s" . slime-selector)
;;   :config
;;   (setq slime-lisp-implementations '(("sbcl" ("/usr/local/bin/sbcl"))))
;;   (slime-setup '(slime-fancy slime-company)))

;; (use-package slime-company
;;   :after (slime company)
;;   :config (setq slime-company-completion 'fuzzy
;;                 slime-company-after-completion 'slime-company-just-one-space))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :custom
  (magit-git-executable (executable-find "git")))

(use-package yaml-mode)

(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines)
         ("C-c m d" . #'mc/mark-all-dwim)))

(use-package paredit
  :bind (:map lisp-mode-shared-map
              ("RET" . paredit-newline))
  :hook (((emacs-lisp-mode
           lisp-mode
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
  :diminish
  :config
  (editorconfig-mode 1))

;(add-to-path "/usr/local/opt/llvm/bin")

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((erlang-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode))
  :commands lsp)

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
                              (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  
  (treemacs-hide-gitignored-files-mode nil)
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
  :diminish
  :custom
  (gcmh-high-cons-threshold (* 2 1024 1024 1024))
  :config
  (gcmh-mode 1))

(server-start)

(provide 'init)

