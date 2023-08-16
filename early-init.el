;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(defmacro lma/override-during-init (variable-name override-value &optional depth)
  (let ((g (gensym)))
    `(let ((,g ,variable-name))
       (setq ,variable-name ,override-value)
       (add-hook 'after-init-hook (lambda ()
                                    (setq ,variable-name ,g)) ,depth))))

;;; We enable gcmh-mode in init.el
(lma/override-during-init gc-cons-threshold most-positive-fixnum -100)

;;; From Doom Emacs
;;; Resizing the Emacs frame can be a terribly expensive part of changing the
;;; font. By inhibiting this, we halve startup times, particularly when we use
;;; fonts that are larger than the system default (which would resize the frame).
(lma/override-during-init frame-inhibit-implied-resize t 100)

;;; From Doom Emacs
;;; Get rid of "For information about GNU Emacs..." message at startup, unless
;;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(modify-all-frames-parameters '((width . 0.7)
                                (height . 0.7)
                                (left . 0.5)
                                (top . 0.5)
                                (vertical-scroll-bars . nil)
                                (horizontal-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (menu-bar-lines . 0)
                                (ns-transparent-titlebar . t)))


