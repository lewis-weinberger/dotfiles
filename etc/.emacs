;;; Emacs startup file

;;; Commentary:
;; A minimal initialisation file for Emacs, with a few basic customisations.

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

;; Show column number
(column-number-mode 1)

;; Cursor style vertical bar
(setq-default cursor-type 'bar)

;; Selecting regions a line at a time
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

;; Select whole line (including newline)
(global-set-key (kbd "C-c <up>") 'select-line-up)
(global-set-key (kbd "C-c <down>") 'select-line-down)

;; Insert block
(global-set-key (kbd "C-c i") 'string-insert-rectangle)

;; Code style
(setq-default python-indent-offset 4)
(setq-default c-default-style "linux" c-basic-offset 4)

;; Spellchecking in Latex
(setq-default ispell-local-dictionary "british")
(add-hook 'LaTeX-mode-hook '(flyspell-mode t))

;; No splash screen
(setq inhibit-startup-screen t)

;;; .emacs ends here
