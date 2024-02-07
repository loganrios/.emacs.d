(setq lexical-binding t)
(setq gc-cons-threshold 50000000)

;;;; Straight

(setq straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; GC

(straight-use-package 'gcmh)

(require 'gcmh)

(gcmh-mode 1)

(setq gcmh-idle-delay 10
      gcmh-high-cons-threshold #x6400000)
