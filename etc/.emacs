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
(setq python-indent-offset 4)
(setq c-default-style "linux" c-basic-offset 4)
(setq ispell-dictionary "british")
(setq inhibit-startup-screen t)
(setq mouse-autoselect-window t)
(setq org-agenda-files '("~/Documents/Notes/"))

;; Appearance
(setq-default cursor-type 'bar)
(column-number-mode t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'fringe nil :background nil)
(modify-all-frames-parameters
 '((font . "Roboto Mono-14")
   (internal-border-width . 16)))

;; Custom bindings
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c ^") 'master-window)
(global-set-key (kbd "C-c }") 'master-window-horizontally)
(global-set-key (kbd "C-c <up>") 'select-line-up)
(global-set-key (kbd "C-c <down>") 'select-line-down)
(global-set-key (kbd "C-c i") 'string-insert-rectangle)
(global-set-key (kbd "C-c p") 'password-store-copy)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'eval-region)

;; Hooks
(add-hook 'text-mode-hook '(flyspell-mode t))
(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))


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


;; PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; use-package setup: note external packages must be installed before use!
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

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package which-key
  :config (which-key-mode 1))

(use-package slime
  :init (setq inferior-lisp-program "sbcl"))

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))


;;; .emacs ends here
