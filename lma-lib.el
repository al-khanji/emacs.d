;;; lma-lib.el ---                                   -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eshell)
(require 'package)

(defvar *think-different* (eq system-type 'darwin))

(defvar *github-urls* '((:https "https://github.com/")
                        (:ssl "git@github.com:")))

(defvar *local-github-dir* (expand-file-name "~/github"))

(defvar *git-config-alist* '(("core.autocrlf" . "input")
                             ("rebase.stat" . "true")
                             ("color.ui" . "auto")
                             ("core.pager" . "\"less -FRSX\"")
                             ("alias.di" . "diff")
                             ("alias.ci" . "commit")
                             ("alias.co" . "checkout")
                             ("alias.ann" . "blame")
                             ("alias.st" . "status")))

(defvar *erlang-binary* (executable-find "erl"))

(defun lma/init-use-package ()
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

(defun lma/set-font (family height &optional slant)
  (setq slant (or slant 'normal))
  (when-let ((font (find-font (font-spec :family family :slant slant))))
    (set-face-attribute 'default nil :font font :height height)))

(defun lma/project-save-buffers (&optional arg)
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

(defun lma/add-to-path (p &optional append)
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

(defun lma/package-find-reqs (pkg)
  "Looks up the requirements for PKG from PACKAGE-ARCHIVE-CONTENTS.
 Returns a list of tuples (NAME VERSION) if found, otherwise nil. "
  (pcase (assoc pkg package-archive-contents)
    (`(,name ,desc) (package-desc-reqs desc))))

(cl-defun lma/fill-line (char &optional (width fill-column))
  (interactive "cFill character: \nP")
  (message "filling %c to column %d" char width)
  (save-excursion
    (end-of-line)
    (while (< (current-column) width)
      (insert-char char))))

(defun lma/github-clone (repo &optional arg)
  (interactive "sRepository: \nP")
  (pcase-let* ((scheme (if arg :ssl :https))
               (`(,scheme ,url) (assoc scheme *github-urls*))
               (`(,owner ,project) (split-string repo "/"))
               (src (concat url repo ".git"))
               (dst (local-github-subdir owner)))
    (message "github-clone %s -> %s" src dst)
    (vc-git-clone src dst nil)))

(cl-defun lma/config-my-git (&optional (config-alist *git-config-alist*))
  (interactive)
  (when (executable-find "git")
    (pcase-dolist (`(,k . ,v) config-alist)
      (shell-command (format "git config --global %s %s" k v)))))

(defun lma/local-github-subdir (dir)
  (expand-file-name dir *local-github-dir*))

(defun lma/erl-eval-print (expr)
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
          output)))))

(defun lma/print-startup-time ()
  (message "Emacs loaded in %f seconds with %d gcs."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(defun lma/set-WINDOWID (&optional frame)
  (setenv "WINDOWID" (frame-parameter frame 'window-id)))

(provide 'lma-lib)
