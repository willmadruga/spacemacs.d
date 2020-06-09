;;; packages.el --- Personal Layer packages File for Spacemacs
;;
;; Copyright (c) 2020 William Madruga
;;
;; Author: William MAdruga
;; URL: https://github.com/wmadruga/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst wmad-packages
  '())

;; (defun wmad/init-equake ()
;;   (use-package equake
;;     :defer t
;;     :config
;;     (global-set-key (kbd "C-x C-c") 'equake-check-if-in-equake-frame-before-closing) ; prevent accidental frame-closure
;;     (setq equake-size-width 0.99) ; set width a bit less than full-screen (prevent 'overflow' on multi-monitor)
;;     ;; set distinct face for Equake: white foreground with dark blue background, and different font
;;     (set-face-attribute 'equake-buffer-face 'nil :inherit 'default :family "DejaVu Sans Mono" :background "#000022" :foreground "white")))
