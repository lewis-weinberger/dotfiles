;;; init.el --- drvog's Emacs config

;;; Commentary:
;; An initialisation file for Emacs!

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

;; VANILLA CUSTOMIZATION ------------------------------------------------------

;; No scroll-bar
(scroll-bar-mode -1)

;; No menu-bar
(menu-bar-mode -1)

;; No tool-bar
(tool-bar-mode -1)

;; Show column number
(column-number-mode 1)

;; Line numbers
(when (version<= "26.0.50" emacs-version)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode))

;; ibuffer
(define-key global-map [remap list-buffers] 'ibuffer)

;; Tabs
(setq tab-width 4)
(setq-default c-basic-offset 4)

;; PACKAGES -------------------------------------------------------------------

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-insert-state-cursor '(bar "red"))
  (setq evil-normal-state-cursor '(box "red"))
  (setq evil-emacs-state-cursor '(box "white"))
  (setq evil-search-module 'evil-search)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  (dolist (mode '(special-mode
                  dired-mode
                  org-mode
                  calc-mode
                  magit-mode
		  eshell-mode
		  ielm-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (evil-mode 1))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; Dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;; Base16 colour scheme
(use-package base16-theme
  :ensure t
  :init
  (setq base16-distinct-fringe-background nil)
  :config
  (load-theme 'base16-default-dark t))

;; Syntax Checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; Inline error display
(use-package flycheck-inline
  :ensure t
  :config
  (global-flycheck-inline-mode))

;; Rust
(use-package rust-mode
  :ensure t
  :defer t
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (use-package flycheck-rust
    :ensure t
    :hook (flycheck-mode . flycheck-rust-setup)))

;; Racket
(use-package racket-mode
  :ensure t)

;; LaTex
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (setq TeX-auto-save t)
  (setq TeX-PDF-mode t))

;; Octave-mode
(use-package octave
  :ensure t
  :mode ("\\.m\\'" . octave-mode))

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Deadgrep
(use-package deadgrep
  :ensure t
  :config
  (global-set-key (kbd "C-c g") #'deadgrep))

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (use-package all-the-icons
    :ensure t)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 1)
  (setq dashboard-set-navigator t)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5))))

;; Turn off bold fonts
(mapc
  (lambda (face)
    (set-face-attribute face nil :weight 'normal))
  (face-list))

;;; init.el ends here
