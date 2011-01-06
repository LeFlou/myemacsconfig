;; Last modified
;; Thu Jan 06 21:32:06 CET 2011

;; Thanks to:
;; JanBorsodi.emacs (http://www.dotemacs.de/dotfiles/JanBorsodi/JanBorsodi.emacs.html)
;; KDE (http://websvn.kde.org/trunk/KDE/kdesdk/scripts/kde-emacs/)
;; emacswiki.org (http://www.emacswiki.org/cgi-bin/wiki?QtMode)

;; REMINDER
;; M-x name-last-kbd-macro
;; M-x insert-kbd-macro
;; (global-set-key (kbd "C-c a") 'my-macro)

;; No startup message
(setq inhibit-startup-screen t)

;; Disabling backup
(setq backup-inhibited t)

;; rightheader macro
;; open src/myclass.cpp
;; then M-x rightheader to open headers/myclass.h
(fset 'rightheader "\C-[xbuffer-menu\C-m\C-[OF\C-@\C-[[1;5D\C-[[1;5D\C-[w\C-xk\C-m\C-x3\C-xo\C-x\C-f\C-?\C-?\C-?\C-?headers/\C-y\C-?\C-?\C-?h\C-m\C-xo")

;; ajout au PATH-Emacs
(add-to-list 'load-path "~/.emacs.d/")

;; Autocomplete mode
(require 'auto-complete-config)
(require 'auto-complete-etags)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

;; Show whitespaces
(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;; Syntax color
(font-lock-mode t)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration 5)

(set-face-foreground   'font-lock-string-face "Yellow")
(set-face-foreground   'font-lock-comment-face  "OrangeRed")
(set-face-foreground   'font-lock-keyword-face  "Cyan")
(set-face-bold-p       'font-lock-keyword-face t)
(set-face-foreground   'font-lock-type-face     "Wheat")
;; (set-face-underline-p  'font-lock-type-face t)
(set-face-foreground   'font-lock-function-name-face    "Blue")
(set-face-bold-p       'font-lock-function-name-face t)
(set-face-foreground   'font-lock-variable-name-face    "Green")
;; reference
;; (set-face-foreground   'font-lock-constant-face "White")
;; (set-face-background   'font-lock-constant-face "BlueViolet")
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

;; Python
;; http://launchpadlibrarian.net/21781107/python-mode.el
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; php
;; http://www.emacswiki.org/emacs/PhpMode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Shortcuts
(global-set-key "\C-c\C-r"      'iwb)
(global-set-key "\C-c\C-g"      'goto-line)
(global-set-key "\C-c\C-c"      'comment-or-uncomment-region)
(global-set-key "\C-p"          'rightheader)
(global-set-key [home]          'beginning-of-line)
(global-set-key [end]           'end-of-line)
(global-set-key [f12]           'iwb)

;; Jeux de claviers
(setq default-input-method 'french-postfix)
(global-set-key "\C-x\C-g" 'toggle-input-method)
(add-hook 'message-mode-hook 'toggle-input-method)

;; ------
;; MACROS
;; ------

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  )

;; Usage
;; $ emacs
;; M-x my-class-generator MyClass
(defun my-class-generator (classname)
  (setq src_dir "src")
  (setq headers_dir "headers")
  (interactive "sEnter classname: ")
  (if (and
       (file-accessible-directory-p src_dir)
       (file-accessible-directory-p headers_dir))
      (progn
        (message "Generating class...")
        (find-file (concat src_dir "/" (downcase classname) ".cpp"))
        (insert (concat "#include \"" (downcase classname) ".h\"\n\n"))
        (insert (concat classname "::" classname "(void)\n{\n}\n\n"))
        (insert (concat classname "::~" classname "(void)\n{\n}\n"))
        (save-buffer)
        (split-window-horizontally)
        (other-window -1)
        (find-file (concat "../" headers_dir "/" (downcase classname) ".h"))
        (insert (concat "#ifndef " (upcase classname) "_H_\n"))
        (insert (concat "#define " (upcase classname) "_H_\n\n"))
        (insert (concat "class " classname "\n{\npublic:\n"))
        (insert (concat classname "(void);\n"))
        (insert (concat "~" classname "(void);\n"))
        (insert "};\n\n#endif")
        (indent-region (point-min) (point-max) nil)
        (save-buffer)
        (other-window -1)
        (message "Generating class... done"))
    (message (concat src_dir " and " headers_dir " directories must exist."))
    )
  )

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

;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer
print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; Will align c/c++ variable declarations in the selected region
;; Example:
;; int a;
;; const QString b;
;; static unsigned int c;
;;
;; will become:
;; int                 a;
;; const QString       b;
;; static unsigned int c;
(defun align-vars(beg end)
  "Aligns c/c++ variable declaration names on the same column, with beginning and end taken from selected region."
  (interactive "r")
  (save-excursion
    (let (bol eol expr-end
              (max-col 0) col
              poslist curpos)
      (goto-char end)
      (if (not (bolp))
          (setq end (line-end-position)))
      (goto-char beg)
      (while (and (> end (point)) (not (eobp)))
        (setq bol (line-beginning-position))
        (setq eol (line-end-position))
        (beginning-of-line)
        (setq expr-end (point))
        (if (search-forward-regexp "^[^/][^/]\\([a-zA-Z][a-zA-Z]*\\)[ \t]+[^;]" eol t)
            (let ()
              (setq expr-end (match-end 1))
              (while (search-forward-regexp "\\([a-zA-Z][a-zA-Z]*\\)[ \t]+[^;]" eol t)
                (setq expr-end (match-end 1)))
              (goto-char expr-end)
              (setq col (current-column))
              (if (search-forward-regexp (concat "\\(\\*\\|&[ \t]*\\)?"
                                                 "\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)\\([\[][0-9]+[\]]\\)?"
                                                 "\\([ \t]*,[ \t]*\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)\\([\[][0-9]+[\]]\\)?\\)*"
                                                 "[ \t]*;$") eol t)
                  (let ((name-col-end 0))
                    (if (eq (match-beginning 2) (match-beginning 0))
                        (setq name-col-end 1))
                    (setq poslist (cons (list expr-end col (match-beginning 0) name-col-end) poslist))
                    (if (> col max-col)
                        (setq max-col col))
                    (beginning-of-next-line))
                (beginning-of-next-line)))
          (beginning-of-next-line)))
      (setq curpos poslist)
      (while curpos
        (let* ((pos (car curpos))
               (col (car (cdr pos)))
               (col-end (car (cdr (cdr pos))))
               (col-end-name (car (cdr (cdr (cdr pos)))))
               (abs-pos (car pos)))
          (goto-char abs-pos)
          (delete-region abs-pos col-end)
          (insert-string (make-string (+ (+ (- max-col col) 1) col-end-name) 32)))
        (setq curpos (cdr curpos))))))

;; Aligns all variable declarations in this buffer
(defun align-vars-buffer()
  "Aligns c/c++ variable declaration names on the same column in this buffer."
  (interactive)
  (save-excursion
    (let (beg end)
      (beginning-of-buffer)
      (setq beg (point))
      (end-of-buffer)
      (setq end (point))
      (align-vars beg end))))

;; Checks for known classes and adds includes on the top if none are present
                                        ;(defun insert-include( buffer buf )
(defun insert-include()
  "Insert #include on the top of the file if certain class names are found in the file"
  (interactive)
  (if (string-equal mode-name "C++")
      (let ((includes project-include-classes)
            (include)
            (include-classes)
            (include-class)
            (include-file)
            (class-exists nil))
        (while includes
          (setq include (car includes))
          (setq include-classes (car include))
          (setq include-file (car (cdr include)))
          (setq class-exists nil)
          (while (and (not class-exists) include-classes)
            (setq include-class (car include-classes))
            (save-excursion
              (beginning-of-buffer)
              (if (search-forward-regexp (concat "\\<" include-class "\\>") nil t)
                  (setq class-exists t)))
            (setq include-classes (cdr include-classes)))
          (if class-exists
              (let ((already-present nil))
                (save-excursion
                  (beginning-of-buffer)
                  (if (search-forward-regexp (concat "^#include[ \t]+"
                                                     include-file
                                                     "[ \t]*\n") nil t )
                      (setq already-present t)))
                (if (not already-present)
                    (save-excursion
                      (goto-char (end-of-include-place))
                      (insert-string (concat "#include " include-file "\n"))))))
          (setq includes (cdr includes))))))



;; Cursor stands still on scroll
;; (setq scroll-preserve-screen-position t)
;; (setq cursor-in-non-selected-windows t)

;; Copy on cursor position, not mouse one
;; (setq mouse-yank-at-point t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "lightslateblue" "magenta" "cyan" "white"])
 '(canlock-password "e722586516e941d6dfddd0e88b7623046bec29aa")
 '(compilation-scroll-output t)
 '(compile-auto-highlight 5)
 '(executable-chmod 755)
 '(gnus-buttonized-mime-types (quote (".*/.*")))
 '(gnus-show-threads t)
 '(gnus-thread-hide-subtree t)
 '(grep-command "grep -i -n -e "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "tan"))))
 '(gnus-header-content-face ((t (:foreground "green3"))) t)
 '(gnus-header-from-face ((((class color) (background dark)) (:foreground "SeaGreen3"))) t)
 '(gnus-header-name-face ((((class color) (background dark)) (:foreground "Green"))) t)
 '(gnus-header-subject-face ((((class color) (background dark)) (:foreground "spring green"))) t)
 '(info-xref ((t (:foreground "magenta" :weight bold))))
 '(message-header-name-face ((((class color) (background dark)) (:inherit gnus-header-name-face))) t)
 '(message-header-other-face ((((class color) (background dark)) (:inherit gnus-header-content-face))) t)
 '(mode-line ((((type x w32 mac) (class color)) (:background "slateblue" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(mouse ((t (:background "white"))))
 '(show-paren-match ((((class color)) (:background "DarkSlateBlue"))))
 '(tooltip ((((class color)) (:background "lightyellow" :foreground "black")))))

;;;; syntax-highlighting for Qt
;;;; (based on work by Arndt Gulbrandsen, Troll Tech)
;; (defun jk/c-mode-common-hook ()
;;   "Set up c-mode and related modes.
;;    Includes support for Qt code (signal, slots and alikes)."

;; (c-set-style "stroustrup")
;; (c-toggle-auto-hungry-state 1)

;; (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
;;                                "\\|protected slot\\|private\\|private slot"
;;                                "\\)\\>")
;;       c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
;;                                "\\|public slots\\|protected slots\\|private slots"
;;                                "\\)\\>[ \t]*:"))
;; (progn
;;   (font-lock-add-keywords 'c++-mode
;;                           '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
;;   (make-face 'qt-keywords-face)
;;   (set-face-foreground 'qt-keywords-face "BlueViolet")
;;   (font-lock-add-keywords 'c++-mode
;;                           '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
;;   (font-lock-add-keywords 'c++-mode
;;                           '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
;;   (font-lock-add-keywords 'c++-mode
;;                           '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
;;   ))
;; (add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)

;; Qt keywords
;; (c-add-style "qt-gnu" '("gnu"(c-access-key . "\\<\\(signals\\|public\\|protected\\|private\\|public slots\\|protected slots\\|private slots\\):")
;;                      (c-basic-offset . 4)))

(add-to-list 'auto-mode-alist '("\.h$" . c++-mode))
(setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
(font-lock-add-keywords 'c++-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))

;; Do not check for old-style (K&R) function declarations;
;; this speeds up indenting a lot.
(setq c-recognize-knr-p nil)

;; Switch fromm *.<impl> to *.<head> and vice versa
(defun switch-cc-to-h ()
  (interactive)
  (when (string-match "^\\(.*\\)\\.\\([^.]*\\)$" buffer-file-name)
    (let ((name (match-string 1 buffer-file-name))
          (suffix (match-string 2 buffer-file-name)))
      (cond ((string-match suffix "c\\|cc\\|C\\|cpp")
             (cond ((file-exists-p (concat name ".h"))
                    (find-file (concat name ".h"))
                    )
                   ((file-exists-p (concat name ".hh"))
                    (find-file (concat name ".hh"))
                    )
                   ))
            ((string-match suffix "h\\|hh")
             (cond ((file-exists-p (concat name ".cc"))
                    (find-file (concat name ".cc"))
                    )
                   ((file-exists-p (concat name ".C"))
                    (find-file (concat name ".C"))
                    )
                   ((file-exists-p (concat name ".cpp"))
                    (find-file (concat name ".cpp"))
                    )
                   ((file-exists-p (concat name ".c"))
                    (find-file (concat name ".c"))
                    )))))))
