`(require 'cl)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/predictive")

(defmacro try-this (&rest body)
  `(unwind-protect
       (let (retval (gensym))
         (condition-case ex
             (setq retval (progn ,@body))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)))

(defmacro try-independently (&rest body)
  (let (retval (gensym))
    (dolist (x body retval) ()
            (push `(try-this ,x) retval))
    (setq retval (reverse retval))
    (push 'progn retval)))

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

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
;(global-set-key (kbd "M-c") 'whitespace-cleanup)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "<M-f12>") 'revert-buffer)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-s-d")
                (lambda () (interactive)
                  (insert "import ipdb; ipdb.set_trace()")))
(global-set-key [mouse-16] 'revert-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;(try-this
 (require 'color-theme)
 (color-theme-initialize)
 (require 'color-theme-aaron)
 (color-theme-aaron);)

;(try-this
; (set-frame-font "Menlo-12"))
 (set-frame-font "Terminus-12");)

;(try-this
 (column-number-mode 1);)

;(try-this
 (require 'ido)
 (ido-mode t)
 (setq ido-enable-flex-matching t);)
;(try-this
; (setq default-frame-alist '((font . "Monaco-9"))))

;(try-this
 ;(require 'sql)
 ;(defalias 'sql-get-login 'ignore))


; Setup menu's etc.
;(try-independently
 (show-paren-mode t)
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (tooltip-mode -1)

 (setq inhibit-startup-message t)
 (setq require-final-newline t)
 (setq ring-bell-function 'ignore)
 (setq-default indent-tabs-mode nil);)

;(require 'uniquify)
;(setq uniquify-buffer-name-style 'forward)g

(fset 'yes-or-no-p 'y-or-n-p)

;(try-this
; Flymake as you work
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

; Flymake on save
; (when (load "flymake" t)
;   (defun flymake-pylint-init ()
;     (list "/pluto/pycloud/apps/emacs/bin/lintrunner.py"
;           (list buffer-file-name)))
;   (defun flymake-display-warning (warning)
;     "Display a warning to the user, using lwarn"
;     (message warning));
;   (add-to-list 'flymake-allowed-file-name-masks
;               '("^[^\*]+\\.py$" flymake-pylint-init)))

 (add-hook 'python-mode-hook
           '(lambda () (if (not (null buffer-file-name)) (flymake-mode))));)

;(try-this
 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode));)

;(try-this
 (autoload 'yaml-mode "yaml-mode" nil t)
 (add-to-list 'auto-mode-alist '("\\.ya?ml$", yaml-mode));)

;(try-this
 (autoload 'css-mode "css-mode" nil t)
 (setq auto-mode-alist (append '(("\\.css$" . css-mode)) auto-mode-alist));)

; TO COMPILE: $ emacs --batch --eval '(byte-compile-file "js2.el")'
;(try-this
 (autoload 'js2-mode "js2" nil t)
 (add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))
 (add-to-list 'auto-mode-alist '("\\.\\(html\\|rng\\|xhtml\\)$" . html-mode));)


;(try-this
; (server-start))

;(defun recompile-everything-under-the-sun ()
;  (interactive)
;  (dolist (path load-path)
;    (byte-recompile-directory path 0)))
