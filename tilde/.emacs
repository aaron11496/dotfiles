(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun condense-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match " " nil nil))))))

(defun my-custom-frames ()
  "Set the frames to three even-width columns."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun ipdb-trace ()
  "Handy for debugging Python code."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(set-fringe-mode 0)
(column-number-mode 1)
(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [M-f12] 'revert-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)
(global-set-key (kbd "C-s-d") 'ipdb-trace)
(global-set-key [f7] 'my-custom-frames)

(global-auto-revert-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(transient-mark-mode t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; npm install jsonlint -g
;; npm install jshint -g
;; The javascript checker, javascript-jshint, might require `node' to be installed
;; as `node' and not `nodejs'. Fix it using something like this:
;; sudo ln -s /usr/bin/nodejs /usr/bin/node

(global-flycheck-mode 1)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-highlighting-mode 'lines)
(setq flycheck-display-errors-delay 0)

; Illegal1 = 0.123456789 '"[](){} !@#$%^&*
; ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 abcdefghijklmnopqrstuvwxyz
;!@#$%^&*()[]{}<>-_=+\|;:'",./?
;; (set-frame-font "Inconsolata-11")
;; (set-frame-font "Terminus-12")
(set-frame-font "UbuntuMono-11")
;; (set-frame-font "AndaleMono-10")
;; (set-frame-font "Monaco-10")
;; (set-frame-font "DejaVuSansMono-10")
;; (set-frame-font "DroidSansMono-10")

(set-face-attribute 'default nil
                    :background "black"
                    :foreground "light gray"
                    :stipple nil
                    :box nil
                    :strike-through nil
                    :overline nil
                    :underline nil
                    :slant 'normal
                    :weight 'normal
                    :inverse-video nil)

(set-face-attribute 'vertical-border nil :foreground "dim gray")

(set-face-attribute 'region nil :background "navy")

(set-face-attribute 'font-lock-builtin-face nil :foreground "sandy brown")
(set-face-attribute 'font-lock-comment-face nil :foreground "dim gray")
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "medium blue")
(set-face-attribute 'font-lock-constant-face nil :foreground "pale violet red")
(set-face-attribute 'font-lock-doc-face nil :foreground "khaki")
(set-face-attribute 'font-lock-function-name-face nil :foreground "firebrick")
(set-face-attribute 'font-lock-keyword-face nil :foreground "steel blue")
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "khaki")
(set-face-attribute 'font-lock-string-face nil :foreground "forest green")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "indian red")

; To make modeline look like a button, use `:box '(:line-width 2 :style released-button)'
(set-face-attribute 'mode-line nil :foreground "gray60" :background "gray8" :box nil)
(set-face-attribute 'mode-line-inactive nil :foreground "gray40" :background "gray4" :box nil)
(set-face-attribute 'mode-line-highlight nil :background "gray20" :box nil)

(set-face-attribute 'flycheck-info nil :background "lime green" :underline nil :inherit nil)
(set-face-attribute 'flycheck-warning nil :background "midnight blue" :underline nil :inherit nil)
(set-face-attribute 'flycheck-error nil :background "dark red" :underline nil :inherit nil)
