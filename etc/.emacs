;;; .emacs 

;;; Commentary:

;; An initialisation file for GNU Emacs.
;; This is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Code:


;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Miscellaneous settings
(setq-default column-number-mode t)
(setq-default python-indent-offset 4)
(setq-default c-default-style "linux" c-basic-offset 4)
(setq-default ispell-local-dictionary "british")
(setq-default inhibit-startup-screen t)
(setq-default cursor-type 'bar)

;; Custom bindings
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c ^") 'master-window)
(global-set-key (kbd "C-c }") 'master-window-horizontally)
(global-set-key (kbd "C-c <up>") 'select-line-up)
(global-set-key (kbd "C-c <down>") 'select-line-down)
(global-set-key (kbd "C-c i") 'string-insert-rectangle)

;; Hooks
(add-hook 'LaTeX-mode-hook '(flyspell-mode t))


;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun master-window ()
  "Expand current window vertically to take up 70% of frame text area."
  (interactive)
  (window-resize nil (- (floor (* 0.7 (frame-height))) (window-height))))

(defun master-window-horizontally ()
  "Expand current window horizontally to take up 70% of frame text area."
  (interactive)
  (window-resize nil (- (floor (* 0.7 (frame-width))) (window-width)) t))

(defun select-line-up ()
  "Select a line from end to start."
  (interactive)
  (set-mark (line-end-position))
  (beginning-of-line)
  (activate-mark))

(defun select-line-down ()
  "Select a line from start to end, including newline."
  (interactive)
  (set-mark (line-beginning-position))
  (forward-line)
  (activate-mark))


;; EXTERNAL PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; use-package setup: note packages must be installed before use!
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")


;;; .emacs ends here
