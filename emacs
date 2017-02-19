;; No startup message
(setq inhibit-startup-screen t)

;; No more backup
(setq make-backup-files nil)

;; headers trigger C++ mode
(add-to-list 'auto-mode-alist '("\\.h" . c++-mode))

;; Show whitespaces
(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;; Syntax color
(font-lock-mode t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration 5)

;; melpa.milkbox.net
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Jedi (python autcompletion)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(set-foreground-color "white")
(set-background-color "black")

;; Line highlight
(global-hl-line-mode t)
(custom-set-faces '(highlight ((t (:weight bold)))))

;; Encoding style
(set-terminal-coding-system 'latin-1)
(set-language-environment 'latin-1)
(set-keyboard-coding-system 'latin-1)

;; --------
;; MODELINE
;; --------

;; Display numbers of rows and columns
(line-number-mode 1)
(column-number-mode 1)

;; show current function in modeline
(which-func-mode t)

;; Show time in modeline
(display-time)

;; Show Matching Parentheses
(show-paren-mode t)

;; Highlight selection
(transient-mark-mode t)

;; Default indentation
(setq c-indentation-style "sh-mode")

;; Do not check for old-style (K&R) function declarations;
;; this speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; No menu bar
(menu-bar-mode nil)

;; when moving the cursor to the bottom/top of screen, scroll up/down 2 lines
(setq scroll-step 5)

;; when moving page up/down, keep 1 line in common
(setq next-screen-context-lines 5)

;; Working Del key
(and
 window-system
 (functionp 'normal-erase-is-backspace-mode)
 (normal-erase-is-backspace-mode nil))

;; ---------------
;;  LANGUAGE MODS
;; ---------------

;; Shortcuts
(global-set-key "\C-c\C-g"  'goto-line)
(global-set-key "\C-c\C-c"  'comment-or-uncomment-region)
(global-set-key [home]      'beginning-of-line)
(global-set-key [end]       'end-of-line)
(global-set-key [f12]       'jedi:goto-definition)
(global-set-key [M-left]    'pop-global-mark)
(global-set-key [C-up]      'shrink-window)
(global-set-key [C-down]    'enlarge-window)
(global-set-key [C-left]    'shrink-window-horizontally)
(global-set-key [C-right]   'enlarge-window-horizontally)

;; French keyboard
(setq default-input-method 'french-postfix)
(global-set-key "\C-x\C-g" 'toggle-input-method)
(add-hook 'message-mode-hook 'toggle-input-method)

;; --------
;;  MACROS
;; --------
(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)
  (insert (format-time-string "%D %-I:%M %p")))

(defun timestamp ()
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y")))

(defun today ()
  "Insert string for today's date nicely formatted in American style, e.g. Sunday, September 17, 2000."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))
