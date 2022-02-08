;;; early-init.el -*- lexical-binding: t; -*-

(let ((gc-cons-threshold-original gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold gc-cons-threshold-original)
              (when (package-installed-p 'gcmh)
                (gcmh-mode 1)))
            100))

(setq default-frame-alist (append '((width . 0.7)
                                    (height . 0.7)
                                    (left . 0.5)
                                    (top . 0.5)
                                    (vertical-scroll-bars . nil)
                                    (horizontal-scroll-bars . nil)
                                    (ns-transparent-titlebar . t)
                                    (cursor-type . bar))
                                  default-frame-alist))

