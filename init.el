(setq inhibit-startup-screen t
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      compilation-scroll-output t
      frame-title-format `((buffer-file-name "%f" "%b")))

;; emacs *insists* on this being on its own line with hard-coded user name
(setq inhibit-startup-echo-area-message "louai")

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

;; Use utf-8 everywhere
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Not set by default on macOS
(unless (getenv "DISPLAY")
  (setenv "DISPLAY" ":0"))

(delete-selection-mode t)
(column-number-mode)

;; Line numbers everywhere, except ...
(global-display-line-numbers-mode t)
;; ... for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; UI Settings
(when (window-system)
  (setq default-frame-alist (append '((width . 0.7)
                                      (height . 0.7)
                                      (left . 0.5)
                                      (top . 0.5)
                                      (vertical-scroll-bars . nil)
                                      (horizontal-scroll-bars . nil)
                                      (ns-transparent-titlebar . t))
                                    default-frame-alist))
  (modify-frame-parameters (selected-frame) default-frame-alist))

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

;; Some utility functions
(progn
  (cl-defun add-to-path (p &optional (append nil))
    "Add P to path variables: exec-path eshell-path-env $PATH.

Prepends by default, append by setting APPEND to non-nil."
    (interactive "GDirectory: \nP")
    (add-to-list 'exec-path p append)
    (require 'eshell)
    (let* ((new-paths (list (list p) (eshell-get-path)))
           (new-paths (if append (reverse new-paths) new-paths))
           (new-paths (apply #'append new-paths))
           (new-paths (string-join new-paths path-separator)))
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

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package doom-themes
  :config
  (load-theme 'doom-spacegrey t))

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
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package slime
  :bind ("C-c s" . slime-selector)
  :config
  (setq slime-lisp-implementations '(("sbcl" ("/usr/local/bin/sbcl"))))
  (slime-setup))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location)))

(use-package slime-company
  :config
  (add-to-list 'slime-contribs 'slime-company))

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

(use-package woman)

(use-package erlang
  :custom (erlang-root-dir (erl-eval-print "code:root_dir()"))
  :config
  (require 'erlang-start)
  (pcase (erl-eval-print "code:root_dir()")
    ('nil)
    (lib-dir (add-to-list 'woman-manpath (expand-file-name "man" lib-dir)))))

;; (use-package erlang
;;   :ensure nil
;;   :if (executable-find "erl")
;;   :load-path (lambda () (concat (erl-eval-print "code:lib_dir(tools)") "/emacs"))
;;   :custom (erlang-root-dir (erl-eval-print "code:root_dir()"))
;;   :init
;;   (add-to-list 'exec-path (concat (erl-eval-print "code:root_dir()") "/bin")))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(dolist (p '("al-khanji/erlang_ls/_build/default/bin"
             "al-khanji/erlang_ls/_build/dap/bin"))
  (add-to-path (local-github-subdir p)))

(use-package lsp-ui)

(use-package lsp-mode
  :ensure nil
  :pin manual
  :preface
  (pcase-dolist (`(,req-name ,req-version) (package-find-reqs 'lsp-mode))
    (unless (package-installed-p req-name) (package-install req-name 'dont-select)))
  (mapcar (lambda (project) (add-to-list 'load-path (local-github-subdir project)))
          '("al-khanji/lsp-mode" "al-khanji/lsp-mode/clients")))

(provide 'init)

