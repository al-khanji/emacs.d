(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file 'noerror)

(setq inhibit-startup-screen t
      sentence-end-double-space nil
      ring-bell-function 'ignore
      use-dialog-box nil
      compilation-scroll-output t)

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

;; Stop with all the littering
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; Not set by default on macOS
(unless (getenv "DISPLAY")
  (setenv "DISPLAY" ":0"))

(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)

;; disable toolbar
(when (window-system)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)
  (dolist (e '((width . 0.7)
               (height . 0.7)
               (left . 0.5)
               (top . 0.5)
               (vertical-scroll-bars . nil)
               (horizontal-scroll-bars . nil)
               (ns-transparent-titlebar . t)
               (ns-use-proxy-icon . nil)))
    (add-to-list 'default-frame-alist e))
  (modify-frame-parameters (selected-frame) default-frame-alist))

;; basic package setup
(progn 
  (require 'package)
  (dolist (e '(("gnu"   . "https://elpa.gnu.org/packages/")
               ("melpa" . "https://melpa.org/packages/")
               ("org"   . "https://orgmode.org/elpa/")))
    (add-to-list 'package-archives e))
  (setq package-user-dir (expand-file-name "packages" user-emacs-directory))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))

  (setq use-package-always-ensure t))

(use-package doom-themes
  :config
  (load-theme 'doom-spacegrey t))

;; macOS specials
(progn
  (defvar *think-different* (memq window-system '(mac ns x)))

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
    (exec-path-from-shell-initialize)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package diminish
  :config (diminish 'eldoc-mode))

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

(use-package magit)

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

;; Erlang stuff
(progn
  (defvar *erlang-binary* (executable-find "erl"))

  (defun erl-eval-print (expr)
    (interactive "sErlang expression: ")
    (catch 'no-erl
      (unless (and *erlang-binary* (executable-find *erlang-binary*))
        (message "*erlang-binary* not set or incorrect - set it manually or add `erl` to $PATH")
        (throw 'no-erl nil))
      (let* ((printable-fun-str (concat "fun (F) when is_float(F) -> erlang:float_to_binary(F);"
                                        "    (I) when is_integer(I) -> erlang:integer_to_binary(I);"
                                        "    (X) -> X end"))
             (expr-str (format "erlang:apply(%s, [%s])" printable-fun-str expr))
             (eval-str (format "io:format(\"~s\", [%s])." expr-str))
             (cmd-str (format "%s -noinput -eval '%s' -s erlang halt" *erlang-binary* eval-str))
             (result (shell-command-to-string cmd-str)))
        (when (called-interactively-p 'any)
	  (message result))
        result)))

  (use-package erlang
    :custom (erlang-root-dir (erl-eval-print "code:root_dir()")))

  ;; (use-package erlang
  ;;   :ensure nil
  ;;   :if (executable-find "erl")
  ;;   :load-path (lambda () (concat (erl-eval-print "code:lib_dir(tools)") "/emacs"))
  ;;   :custom (erlang-root-dir (erl-eval-print "code:root_dir()"))
  ;;   :init
  ;;   (add-to-list 'exec-path (concat (erl-eval-print "code:root_dir()") "/bin")))
  )

;; Custom github bits and pieces
(progn
  (defvar *local-github-dir* "~/github")
  (defvar *github-urls* '((:https "https://github.com/")
                          (:ssl "git@github.com:")))

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

(defun add-to-path (p)
  (add-to-list 'exec-path p)
  (setq-default eshell-path-env (concat p ":" eshell-path-env))
  (setenv "PATH" (concat p ":" (getenv "PATH"))))

(dolist (p '("al-khanji/erlang_ls/_build/default/bin"
             "al-khanji/erlang_ls/_build/dap/bin"))
  (add-to-path (local-github-subdir p)))

(use-package lsp-mode
  :ensure nil
  :pin manual
  :preface
  (dolist (p '(dash
               f
               ht
               lv
               markdown-mode
               spinner
               flycheck
               ert-runner
               espuds
               ecukes
               undercover
               deferred
               el-mock))
    (package-install p 'dont-select))
  (dolist (d '("al-khanji/lsp-mode"
               "al-khanji/lsp-mode/clients"))
    (add-to-list 'load-path (local-github-subdir d))))

(provide 'init)

