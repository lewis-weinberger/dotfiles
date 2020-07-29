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


;; MACROS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro if-system (system &rest body)
  "Evaluate BODY only on SYSTEM"
  (declare (indent defun))
  `(when (string-equal system-type ,system)
     ,@body))


;; GENERAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Miscellaneous settings
(setq python-indent-offset 4)
(setq c-default-style "linux" c-basic-offset 4)
(setq ispell-dictionary "british")
(setq inhibit-startup-screen t)
(setq mouse-autoselect-window t)
(setq org-agenda-files '("~/Documents/Notes/"))
(setq frame-resize-pixelwise t)

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

;; MacOS specific
(if-system "darwin"
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

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
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))


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
  :init
  (setq inferior-lisp-program "sbcl")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(use-package geiser
  :init
  (if-system "darwin"
    (setq geiser-racket-binary "/Applications/Racket v7.5/bin/racket")))

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

(use-package acme-theme
  :config
  (load-theme 'acme t))

;;; .emacs ends here
