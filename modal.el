(straight-use-package 'meow)
(require 'meow)

(defun scroll-half-page-down ()
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
   (interactive)
   (scroll-down (/ (window-body-height) 2)))

(meow-motion-overwrite-define-key
 ;; Use e to move up, n to move down.
 ;; Since special modes usually use n to move down, we only overwrite e here.
 '("e" . meow-prev)
 '("<escape>" . ignore))

(meow-define-keys 'insert
  '("C-o" . meow-insert-exit))

(meow-leader-define-key
 ;; To execute the originally e in MOTION state, use SPC e.
 '("e" . "H-e")

 '("?" . meow-cheatsheet)
 '("." . find-file)
 '("," . consult-buffer)
 '("SPC" . project-find-file)
 '("/" . consult-ripgrep)
 '(";" . comment-dwim)

 ;; Buffer
 '("b e" . eval-buffer)
 '("b q" . kill-this-buffer)
 '("b r" . rename-buffer)
 '("b i" . ibuffer)
 '("b w" . delete-trailing-whitespace)

 ;; File
 '("f r" . recentf-open-files)

 ;; Seems important enough on its own
 '("s" . save-buffer)

 ;; Window
 '("w v" . split-window-right)
 '("w s" . split-window-below)
 '("w o" . other-window)
 '("w q" . delete-window)
 '("w b" . maximize-window)

 '("1" . meow-digit-argument)
 '("2" . meow-digit-argument)
 '("3" . meow-digit-argument)
 '("4" . meow-digit-argument)
 '("5" . meow-digit-argument)
 '("6" . meow-digit-argument)
 '("7" . meow-digit-argument)
 '("8" . meow-digit-argument)
 '("9" . meow-digit-argument)
 '("0" . meow-digit-argument))

(meow-normal-define-key
 '("0" . meow-expand-0)
 '("1" . meow-expand-1)
 '("2" . meow-expand-2)
 '("3" . meow-expand-3)
 '("4" . meow-expand-4)
 '("5" . meow-expand-5)
 '("6" . meow-expand-6)
 '("7" . meow-expand-7)
 '("8" . meow-expand-8)
 '("9" . meow-expand-9)
 '("-" . negative-argument)
 '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
 '("." . meow-bounds-of-thing)
 '("[" . meow-beginning-of-thing)
 '("]" . meow-end-of-thing)
 '("/" . meow-visit)
   
 '("q" . meow-quit)
 '("w" . meow-next-word) '("W" . meow-next-symbol)
 '("f" . meow-find)
 '("p" . meow-yank)
 '("b" . meow-back-word) '("B" . meow-back-symbol)
 '("j" . meow-join)
 '("l" . meow-line) '("L" . meow-goto-line)
 '("u" . meow-undo) '("U" . meow-undo-in-selection)
 '("y" . meow-save)
 '("'" . meow-repeat)

 '("a" . meow-append) '("A" . meow-open-below)
 '("r" . meow-replace) '("R" . undo-redo)
 '("s" . meow-insert) '("S" . meow-open-above)
 '("t" . meow-till)
 '("g" . meow-cancel-selection) '("G" . meow-grab)
 '("m" . meow-left) '("M" . meow-left-expand)
 '("n" . meow-next) '("N" . meow-next-expand)
 '("e" . meow-prev) '("E" . meow-prev-expand)
 '("i" . meow-right) '("I" . meow-right-expand)
 '("o" . meow-block) '("O" . meow-to-block)

 '("z" . meow-pop-selection)
 '("x" . meow-delete) '("X" . meow-backward-delete)
 '("c" . meow-change)
 '("d" . meow-kill)
 '("v" . meow-search)
 '("k" . scroll-half-page-down) '("K" . scroll-half-page-up)
 '("h" . meow-mark-word) '("H" . meow-mark-symbol))

(meow-global-mode 1)
