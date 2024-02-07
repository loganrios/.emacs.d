(setq lexical-binding t)

;;;; Defaults
;; sensible defaults shark
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq-default indent-tabs-mode nil)

(defun my-prompt-for-save-with-parents ()
  (when-buffer-file-name
   (let ((dir (file-name-directory buffer-file-name)))
     (when (and (not (file-exists-p dir))
                (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
       (make-directory dir t)))))

(add-hook 'before-save-hook 'my-prompt-for-save-with-parents)

(setq require-final-newline t)
(setq inhibit-startup-message t
      initial-scratch-message nil)

(global-font-lock-mode t)
(global-auto-revert-mode t)

(recentf-mode)

;; mitigate littering
(setq auto-save-defaults nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; visual
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode +1)
(setq inhibit-startup-screen t)
(global-prettify-symbols-mode 1) ;; currently only lambda

;; line numbers
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(org-mode)
  "Major modes on which to display line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(add-hook 'prog-mode-mook 'toggle-truncate-lines)

(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100)

(set-window-scroll-bars (minibuffer-window) nil nil)

(defun +frame-title ()
  (let ((project (project-current)))
    (if project
        (project-name project)
      (buffer-name))))

(setq-default frame-title-format '(:eval (+frame-title)))

;; custom *Messages* startup
(defun display-startup-echo-area-message ()
  (message "Let the hacking begin!"))

;; Configure `straight'
(defalias 'sup 'straight-use-package)

;;;; Theme

(sup 'doom-themes)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(load-theme 'doom-gruvbox t)

(doom-themes-org-config)

;;;; Font

(when (member "Comic Code Ligatures" (font-family-list))
  (set-frame-font "Comic Code Ligatures-14"))

;;;; Modeline

(sup 'telephone-line)

(telephone-line-mode 1)

;;;; Completion

(sup 'vertico)

(vertico-mode)

;; hacking bc i really want extensions
;; but not to pull in use-package
(add-to-list 'load-path "~/.emacs.d/straight/repos/vertico/extensions/")
(load "vertico-directory.el")

(add-hook 'rfn-eshadow-update-overlay 'vertico-directory-tidy)
(keymap-set vertico-map "DEL" #'vertico-directory-delete-char)

(sup 'consult)

(sup 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;;;; Programming Generic

(add-hook 'prog-mode-hook 'toggle-truncate-lines)

;;;; Lisping Generic

(sup 'rainbow-delimiters)

;;;; Git

(sup 'magit)

(sup 'git-modes)

;;;; Dired

(setq dired-clean-up-buffers-too t)
(setq dired-dwim-target t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;;; Org

(setq org-hide-leading-stars t
      org-startup-indented t
      org-startup-folded nil)

(setq org-src-fontify-natively t)

(setq org-src-window-setup 'current-window)

(setq org-adapt-indentation nil)

(setq org-insert-heading-respect-content t)

(setq org-src-preserve-indentation t)

(add-hook 'org-mode-hook 'visual-line-mode)

(require 'org-tempo) ;; <s style easy templates

;;;; Markdown

(sup 'markdown-mode)

;;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;;;; Common Lisp

(sup 'sly)

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;;;; JSON

(sup 'json-mode)

;;;; Editing

(load-file (expand-file-name "modal.el" user-emacs-directory))

;;;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))
