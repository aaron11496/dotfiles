;;
;; GENERAL CONFIG
;;

(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(scroll-bar-mode -1)
(set-fringe-mode 0)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward)
(setq x-select-enable-clipboard t)
(setq-default sort-fold-case t)
;;(setq-default fill-column 70)
(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(tool-bar-mode -1)
(tooltip-mode -1)
(transient-mark-mode t)

;; save backup files to a single dir
(defun make-backup-file-name (file)
  (concat "~/.emacs_backups/" (file-name-nondirectory file) "~"))


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . shell-script-mode))

(eval-after-load "sql" '(progn (sql-set-product 'postgres)))

;;
;; PACKAGE CONFIG
;;

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)

(use-package diminish
  :config
  ;(diminish 'auto-complete-mode)
  )

(use-package flycheck
  :config
  (global-flycheck-mode 1)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-display-errors-delay 0)
  :diminish flycheck-mode
  )

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  )

(use-package flx-ido
  :config
  (ido-mode t)
  (flx-ido-mode t)
  (setq ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq flx-ido-use-faces nil)
  (defadvice ido-find-file (after find-file-sudo activate)
    "Make ido-mode find file as root if necessary."
    (unless (and buffer-file-name
                 (file-writable-p buffer-file-name))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
  )

(use-package smex
  ;; C-h f, while Smex is active, runs describe-function on the currently selected command.
  ;; M-. jumps to the definition of the selected command.
  ;; C-h w shows the key bindings for the selected command. (Via where-is.)
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package fill-column-indicator
  :config
  (setq fci-rule-color "dark slate gray")
  (setq fci-rule-use-dashes t)
  )

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char)
  )

(use-package anzu
  :init
  (anzu-mode)
  )

(use-package image+
  :init
  (imagex-global-sticky-mode)
  )


(use-package buffer-move
  :bind
  ("C-S-<up>" . buf-move-up)
  ("C-S-<down>" . buf-move-down)
  ("C-S-<left>" . buf-move-left)
  ("C-S-<right>" . buf-move-right)
  )

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  )

(use-package beacon
  :config
  (beacon-mode 1)
  :diminish beacon-mode
  )

(use-package jedi
  :config
  (jedi:setup)
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (define-key jedi-mode-map (kbd "C-'") 'jedi:complete)
  (define-key jedi-mode-map (kbd "<C-tab>") nil)
  )

(use-package robe-mode
  :disabled t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  )

(use-package re-builder
  :config
  (setq reb-re-syntax 'string)
  )

(use-package projectile
  :config
  (projectile-global-mode)
  :diminish projectile-mode
  )

;;
;; CUSTOM FUNCTIONS
;;

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG - 1 lines first.  If
  point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun ipdb-trace ()
  "Handy for debugging Python code."
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))


(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
   Prefixed with negative \\[universal-argument], sorts in reverse.

   The variable `sort-fold-case' determines whether alphabetic case
   affects the sort order.

   See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


(defun three-columns ()
  "Set the frames to three even-width columns."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (next-multiframe-window)
  (switch-to-next-buffer)
  (split-window-horizontally)
  (next-multiframe-window)
  (switch-to-next-buffer)
  (balance-windows))

(defun four-columns ()
  "Set the frames to three even-width columns."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (next-multiframe-window)
  (switch-to-next-buffer)
  (split-window-horizontally)
  (next-multiframe-window)
  (switch-to-next-buffer)
  (split-window-horizontally)
  (next-multiframe-window)
  (switch-to-next-buffer)
  (balance-windows))


;;
;; KEYBOARD SHORTCUTS
;;

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [f7] 'three-columns)
(global-set-key [f8] 'four-columns)
(global-set-key [M-f12] 'revert-buffer)
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

(add-hook 'python-mode-hook (lambda() (local-set-key (kbd "C-s-d") 'ipdb-trace)))

;; This is mapped to ESC ESC ESC "keyboard-escape-quit", which
;; destroys other windows which is annoying when you accidentally
;; press it instead of C-p C-p C-p
(global-unset-key (kbd "C-[ C-[ C-["))

;;
;; COLOR SCHEMES
;;

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

(set-face-attribute 'vertical-border nil :foreground "gray16")

(set-face-attribute 'region nil :background "navy")

(set-face-attribute 'font-lock-builtin-face nil :foreground "sandy brown")
(set-face-attribute 'font-lock-comment-face nil :foreground "dim gray")
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "medium blue")
(set-face-attribute 'font-lock-constant-face nil :foreground "pale violet red")
(set-face-attribute 'font-lock-doc-face nil :foreground "khaki")
(set-face-attribute 'font-lock-function-name-face nil :foreground "firebrick")
(set-face-attribute 'font-lock-keyword-face nil :foreground "steel blue")
(set-face-attribute 'font-lock-type-face nil :weight 'bold :foreground "cyan")
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "khaki")
(set-face-attribute 'font-lock-string-face nil :foreground "forest green")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "indian red")

; To make modeline look like a button, use `:box '(:line-width 2 :style released-button)'
(set-face-attribute 'mode-line nil :foreground "gray60" :background "gray8" :box nil)
(set-face-attribute 'mode-line-inactive nil :foreground "gray40" :background "gray4" :box nil)
(set-face-attribute 'mode-line-highlight nil :background "gray20" :box nil)

(set-face-attribute 'flycheck-info nil :background "dark green" :underline nil :inherit nil)
(set-face-attribute 'flycheck-warning nil :background "midnight blue" :underline nil :inherit nil)
(set-face-attribute 'flycheck-error nil :background "dark red" :underline nil :inherit nil)

;;
;; STARTUP
;;
(when (equal command-line-args (list "emacs")) (three-columns))
