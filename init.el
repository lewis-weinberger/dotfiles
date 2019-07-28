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

;; Wider fringes
(fringe-mode 16)

;; Indicate column 80 position when in prog-mode
;; https://emacs.stackexchange.com/a/149
(defun prog-mode-header-line ()
  "Use the header line to indicate column 80."
  (setq header-line-format
        (list "  " (make-string 79 ? ) "↓")))
(add-hook 'prog-mode-hook #'prog-mode-header-line)

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
  (package-install 'use-package)
  )

;; Enable use-package
(eval-when-compile
  (require 'use-package)
  )

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
                  magit-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  (evil-mode 1)
  )

;; Dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  )

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
  (load-theme 'base16-eighties t))

;; Syntax Checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

;; Rust-mode
(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :config
  (setq rust-format-on-save t)
  (use-package flycheck-rust
    :ensure t
    :defer t
    :init
    :hook (flycheck-mode . flycheck-rust-setup)
    )
  )

;; LaTex
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq font-latex-fontify-script nil)
  (setq font-latex-fontify-sectioning 'color)
  (setq TeX-auto-save t)
  (setq TeX-PDF-mode t)
  )

;; Smart modeline
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (setq sml/theme 'dark)
  (sml/setup)
  )

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; init.el ends here
