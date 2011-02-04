`(require 'cl)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/predictive")

(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-aaron)
(color-theme-aaron)

(defun condense-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match " " nil nil))))))

(require 'term)
(defun visit-ansi-term ()
  "Rename, restart if killed, or create and switch to an ansi-term buffer"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(defun my-custom-frames ()
  "Set the frames to three even-width columns, my current normal setup"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(global-set-key (kbd "RET") 'newline-and-indent)
;(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
;(global-set-key (kbd "M-c") 'whitespace-cleanup)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [M-f12] 'revert-buffer)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-s-d")
                (lambda () (interactive)
                  (insert "import ipdb; ipdb.set_trace()")))
(global-set-key [f2] 'visit-ansi-term)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'my-custom-frames)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

(set-frame-font "Terminus-12")
(column-number-mode 1)

; Setup menu's etc.
(show-paren-mode t)
(scroll-bar-mode nil)
(tool-bar-mode nil)
(menu-bar-mode nil)
(tooltip-mode nil)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/pluto/pycloud/apps/emacs/bin/lintrunner.py"
            (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook
          '(lambda () (if (not (null buffer-file-name)) (flymake-mode))))

;(autoload 'yaml-mode "yaml-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.ya?ml$", yaml-mode))

;(autoload 'css-mode "css-mode" nil t)
;(setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist))

; TO COMPILE: $ emacs --batch --eval '(byte-compile-file "js2.el")'
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))

; necessary?
;(add-to-list 'auto-mode-alist '("\\.\\(html\\|rng\\|xhtml\\)$" . html-mode))




;; http://user.it.uu.se/~mic/pager.el
;;(require 'pager)
