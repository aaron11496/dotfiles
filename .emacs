; Illegal1 = 0.123456789 '"[](){} !@#$%^&*
;(set-frame-font "Terminus-12")
(set-frame-font "Monospace-10")

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
        (term-cmd "/usr/bin/zsh")
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
  "Set the frames to three even-width columns"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun ipdb-trace ()
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-s-d") 'ipdb-trace)
(global-set-key [M-f12] 'revert-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key [C-tab] 'next-multiframe-window)
(global-set-key [C-S-iso-lefttab] 'previous-multiframe-window)
(global-set-key [f2] 'visit-ansi-term)
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'my-custom-frames)
(global-auto-revert-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq x-select-enable-clipboard t)

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
      (list "~/.emacs.d/bin/lintrunner.py"(list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook
          '(lambda () (if (not (null buffer-file-name)) (flymake-mode))))
(require 'python)

(autoload 'css-mode "css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'cython-mode "cython-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)$" . yaml-mode))
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun recompile-everything-under-the-sun ()
  (interactive)
  (dolist (path load-path)
    (byte-recompile-directory path 0)))
(set-fringe-mode 1)
(column-number-mode 1)
(show-paren-mode t)
(scroll-bar-mode nil)
(tool-bar-mode nil)
(menu-bar-mode nil)
(tooltip-mode nil)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

; Custom colors for rst mode
(custom-set-faces
 '(rst-level-1-face ((t (:foreground "Red"))) t)
 '(rst-level-2-face ((t (:foreground "Orange"))) t)
 '(rst-level-3-face ((t (:foreground "Yellow"))) t)
 '(rst-level-4-face ((t (:foreground "Green"))) t)
 '(rst-level-5-face ((t (:foreground "Blue"))) t)
 '(rst-level-6-face ((t (:foreground "Purple"))) t))
 ;; (rst-level-7-face ((t (:foreground "LightSteelBlue"))) t)
 ;; (rst-level-8-face ((t (:foreground "LightSalmon"))) t)
(custom-set-variables
 '(rst-level-face-base-light 0))

(put 'downcase-region 'disabled nil)
