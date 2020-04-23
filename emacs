;; This history of this .emacs is in the repo
;; sso://user.git.corp.google.com/jba/homedir
;; which currently is in ~/repos/homedir.
;; Alos, search for "dot-emacs" in my email to find old and/or obsolete stuff.

(message "Loading jba's .emacs...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial stuff.

(require 'package)

(require 'google)

;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-enable nil)
 '(package-selected-packages
   (quote
    (company-lsp company dockerfile-mode expand-region smartparens lsp-ui fuzzy fzf gxref multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:foreground "white"))))
 '(company-tooltip-annotation ((t (:foreground "yellow"))))
 '(company-tooltip-common ((t (:foreground "white") (:background "blue")))))

(setq version (string-to-number emacs-version))

(setq on-X (let ((d (getenv "DISPLAY")))
	     (not (or (null d) (string= d "")))))

(defmacro += (stringvar &rest vals)
  (list 'setq stringvar (cons 'concat (cons stringvar vals))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global settings.

(setq load-path (append load-path
                        '("~/.emacs.d/lisp"
                          )))


(dolist (shellfile '(".bashrc" ".bash_env" ".bash_aliases" ".bash_functions"))
  (add-to-list 'auto-mode-alist (cons (concat "\\" shellfile) 'shell-script-mode)))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.htmlt$" . html-mode))

(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq frame-title-format '("emacs:  %b   (" default-directory ")"))

(when (not on-X)
  (dolist (face '(minibuffer-prompt
                  link
                  font-lock-function-name-face))
    (set-face-foreground face "cyan"))
  (set-face-foreground 'mode-line-inactive "gray")
)


(setq jba-mode-line-format
  (list
   mode-line-modified " "
   mode-line-buffer-identification
   " [" '(:eval (or (citc-client-name buffer-file-name) (magit-get-current-branch))) "] "
   '(:eval (google3-dir buffer-file-name)) " "
   mode-line-position
   mode-line-modes))


(add-hook 'find-file-hook '(lambda ()
			     (setq mode-line-format jba-mode-line-format)))


(setq-default fill-column 80)

(setq kill-ring-max 300)

(global-set-key "\C-x\C-k"
  '(lambda () (interactive) (kill-buffer nil)))

(global-set-key "\C-c\C-l" 'goto-line)

(global-set-key "\C-z" 'undo)

(global-unset-key "\C-\\") ;; bound to toggle-input-method

(global-set-key "\M-n" 'cyclebuffer-backward)
(global-set-key "\M-p" 'cyclebuffer-forward)
(global-set-key "\C-\M-l" '(lambda ()
                             (interactive)
                             (set-window-buffer nil (other-buffer))))

(global-set-key "\C-x1" 'delete-other-windows-vertically)
(global-set-key "\C-x9" 'delete-other-windows)

;; Move by logical lines, not visual lines.
(setq line-move-visual nil)

(put 'eval-expression 'disabled nil)
(put 'set-input-method 'disabled t)

(setq sentence-end-double-space nil)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Use the gnome/freedesktop.org magic for browser choice and options.
;; From Roland McGrath on the google emacs-users group.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions.

(defun camel-case-identifier-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (null bounds)
	(error "no symbol at point"))
    (let ((cw (camel-case-identifier (buffer-substring (car bounds) (cdr bounds)))))
      (delete-region (car bounds) (cdr bounds))
      (insert cw))))

(defun camel-case-identifier (id)
  (apply #'concat (mapcar #'capitalize (split-string id "_"))))

(defun remove-control-Ms ()
  "Strip ^M's from a file"
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "
*$" ""))

(defun jba-compile (prefix)
  (interactive "P")
  (if prefix
      (call-interactively 'compile)
    (save-some-buffers t) ; save all buffers w/o prompting
    (compile compile-command)))


(defun search-for-symbol (name)
  (interactive (list (read-string "Search for symbol: "
                                  (get-symbol-around-point))))
  (grep (format "grep -n '\\<%s\\>' *.h *.cpp" name)))

(defun revbuf ()
  "Revert buffer without asking questions"
  (interactive)
  (revert-buffer nil t))

(defun triple-pane-80 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun jba-make-window-80-columns ()
  (interactive)
  (enlarge-window (- 80 (window-width)) t))

(defun jba-widen-window ()
  (interactive)
  (enlarge-window 5 t))

(defun set-window-width (width)
  (interactive "Nwidth: ")
  (enlarge-window (- width (window-width)) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Separator lines.

(defvar sep-line-initial "")
(defvar sep-line-middle "-")
(defvar sep-line-final "")

(defun sep-line-setup (ini mid fin)
  (make-local-variable 'sep-line-initial)
  (make-local-variable 'sep-line-middle)
  (make-local-variable 'sep-line-final)
  (setq sep-line-initial ini)
  (setq sep-line-middle mid)
  (setq sep-line-final fin))

(defun separator-line (&optional endcol)
  "Inserts as line of string, to fill-column or endcol"
  (interactive)
  (sep-line sep-line-initial
                sep-line-middle
                sep-line-final
                (or endcol fill-column 78)))

(defun sep-line (initial middle final end-column)
  (insert initial)
  (if (string= middle "")
      (beep)
    (progn
      (while (< (current-column) end-column)
            (insert middle))
      (insert final)
      (newline))))

(global-set-key "\M-:" 'separator-line)
;;; Think of it as Meta-shift-;

(defun insert-heading-comment (text)
  ;; uses sep-line-initial, -middle, -final
  (interactive "sHeading: ")
  (open-line 1)
  (beginning-of-line)
  (let* ((heading-len (length text))
             (line-len fill-column)
             (ends-len (+ (length sep-line-initial) (length sep-line-final)))
             ;; total space available for middle chars:
             (mid-len (- line-len heading-len ends-len 2)) ;; -2 for spaces
             (total-n-mid (/ mid-len (length sep-line-middle)))
             ;; # of middle strings to put on each side of heading:
             (n-mid (/ total-n-mid 2)))
      (if (< n-mid 1)
              (error "heading too long"))
      (insert sep-line-initial)
      (insert-n n-mid sep-line-middle)
      (insert " ")
      (insert text)
      (insert " ")
      (insert-n n-mid sep-line-middle)
      (if (= (mod total-n-mid 2) 1)
              (insert sep-line-middle))
      (insert sep-line-final)
      (center-line)
      (next-line 1)))


(defun insert-n (n string)
  (let ((i 1))
    (while (<= i n)
      (insert string)
      (setq i (1+ i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick, simple bookmarks.

(require 'breadcrumb)

;; Don't save them.
(setq bc-bookmark-file nil)

(setq bc-bookmark-limit 100)

;; Don't set them on tag operations.
(setq bc-bookmark-hook-enabled nil)

(defun jba-next-bookmark (p)
  (interactive "P")
  (if p
      (bc-local-next)
    (bc-next)))

(define-prefix-command 'ctl-c-ctl-b-map)

(define-key global-map "\C-c\C-b" 'ctl-c-ctl-b-map)

(define-key global-map "\C-c\C-bm" 'bc-set)
(define-key global-map "\C-c\C-bn" 'jba-next-bookmark)
(define-key global-map "\C-c\C-bp" 'bc-previous)
(define-key global-map "\C-c\C-bl" 'bc-list)
(define-key global-map "\C-c\C-bc" '(lambda ()
				      (interactive)
				      (bc-clear)
				      (message "Bookmarks cleared.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit.

(require 'magit)

(global-set-key  "\C-c\C-m" #'magit-status)
(global-set-key  "\C-c\C-b" '(lambda ()
			       (interactive)
			       (let ((default-directory (magit-get-top-dir))
				     (magit-uninteresting-refs (cons "^refs/tags"
								     magit-uninteresting-refs)))
				 (call-interactively #'magit-checkout))))

;; (dolist (face '(magit-tag magit-log-head-label-tags magit-log-reflog-label-commit
;; 			  magit-log-head-label-bisect-skip
;; 			  magit-log-reflog-label-merge magit-log-reflog-label-amend))
;;   (set-face-background face nil))

(setq magit-last-seen-setup-instructions "1.4.0")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous features.

(read-abbrev-file)


;; For dabbrev expansion
(setq case-replace nil)
(setq dabbrev-case-fold-search nil)
(setq tags-case-fold-search nil)
(setq case-fold-search t)

(define-advice try-expand-dabbrev (:around (fn &rest args))
  (let ((case-fold-search nil))
    (apply fn args)))

;; Hippie-expand source at
;; https://github.com/emacs-mirror/emacs/blob/emacs-25/lisp/hippie-exp.el.

(setq hippie-expand-verbose t)

(global-set-key "\M-/" 'hippie-expand)

(require 'ido)
(ido-mode)

(setq ido-max-dir-file-cache 0) ; caching unreliable

(windmove-default-keybindings 'shift)   ; shift-arrows move between windows
;; Also bind ctrl-arrows for contexts (e.g. tmux) that don't see shift-arrows.
(global-set-key "\M-[D" 'windmove-left)
(global-set-key "\M-[C" 'windmove-right)
(global-set-key "\M-[A" 'windmove-up)
(global-set-key "\M-[B" 'windmove-down)


(require 'find-file-in-project)

(global-set-key "\C-x\C-p" 'ffip)

(defvar jba-associated-shell-pid nil)

(defun jba-set-default-directory (pid)
  (interactive (list (or jba-associated-shell-pid (read-number "shell PID: "))))
  (setq jba-associated-shell-pid pid)
  (let ((dir (file-truename (format "/proc/%d/cwd" pid))))
    (setq default-directory dir)
    (message "default directory set to %s" dir)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode hooks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go mode.

(require 'google-go)

;(setq gofmt-command "goimports")

(add-hook 'go-mode-hook #'jba-go-mode)
(add-hook 'go-mode-hook #'eglot-ensure)

;(add-hook 'go-mode-hook #'yas-minor-mode)
;(add-hook 'go-mode-hook #'lsp)



(defun jba-go-mode ()
  (setq tab-width 4)
  (jba-setup-font-lock-mode)
  (jba-setup-go-eglot)
  ;; (if show-ws-highlight-tabs-p
  ;;     (show-ws-toggle-show-tabs))
  ;(add-to-list 'xref-backend-functions 'gxref-xref-backend)
  ;; September 2019: since go 1.13 was released, flycheck's go vet is not working.
;  (add-to-list 'flycheck-disabled-checkers 'go-vet)
  ;; Remove annoying highlighting from xref.
  ;; (setq
  ;;  xref-after-jump-hook (remove 'xref-pulse-momentarily xref-after-jump-hook)
  ;;  xref-after-return-hook (remove 'xref-pulse-momentarily xref-after-return-hook))
  (define-key go-mode-map "\M-z" 'jba-compile)
  (define-key go-mode-map "" 'jba-godoc)
  (define-key go-mode-map "{" #'jba-go-open-brace)
  (define-key go-mode-map "}" #'go-mode-insert-and-indent)
  (define-key go-mode-map  [?\C-\\] #'jba-switch-to-other-file)
  (define-key go-mode-map  "\C-c\C-t" #'jba-go-test)
  ;(define-key go-mode-map "\M-\?" #'jba-go-grep-symbol)
;  (define-key go-mode-map "\C-cc" #'jba-go-select-call)
  ;(define-key go-mode-map "\C-ce" #'go-guru-expand-region)
  (setq hippie-expand-try-functions-list
	'(jba-try-expand-skeleton
	  try-expand-dabbrev
	  try-expand-all-abbrevs
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-complete-file-name-partially
	  try-complete-file-name
	  ;try-expand-list  (too aggressive for me)
	  ;try-expand-line
	  try-complete-lisp-symbol-partially
	  try-complete-lisp-symbol))
;  (setq ac-sources '(ac-source-go))

  )

(defun jba-setup-go-eglot ()
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
)


(defun jba-setup-go-lsp ()
  (define-key lsp-mode-map "\C-c\C-c" lsp-command-map)
  (define-key go-mode-map  "\M-\?" #'lsp-find-references)
  (setq lsp-ui-doc-enable nil)
)





(add-hook 'compilation-mode-hook #'(lambda ()
				     (define-key compilation-mode-map "\M-z" 'jba-compile)))

(add-hook 'sql-mode-hook #'(lambda ()
			     (define-key sql-mode-map "\M-z" 'jba-compile)
			     (define-key sql-mode-map  [?\C-\\] #'jba-switch-to-other-file)))






;; (defun jba-go-select-call ()
;;   (interactive)
;;   (let* ((enclosing-regions (go-guru--enclosing-unique))
;; 	 (block (seq-find (lambda (b) (string-prefix-p "function call" (cdr (assoc 'desc b)))) enclosing-regions)))
;;     (when block
;; 	(go-guru--goto-byte (1+ (cdr (assoc 'start block))))
;; 	(set-mark (byte-to-position (1+ (cdr (assoc 'end block)))))
;; 	(setq deactivate-mark nil))))



(defun jba-expand-skel-string (s abbrev-table)
  (let* ((sym (abbrev-symbol s abbrev-table))
	 (func (symbol-function sym)))
    (and func (funcall func))))


(defun jba-try-expand-skeleton (old)
  (if old
      nil
    (he-init-string (he-dabbrev-beg) (point))
    (if (or (jba-expand-skel-string he-search-string local-abbrev-table)
	    (jba-expand-skel-string he-search-string global-abbrev-table))
	(progn
	  (he-substitute-string "")
	  t))))

(defun jba-go-open-brace ()
  (interactive)
  (if (or (inside-string) (inside-comment))
      (insert "{")
    ;; else
    (expand-abbrev)
    (backward-char)
    (cond
     ((looking-at " ")
      (forward-char)
      (insert "{")
      (newline-and-indent))
     ((looking-at ")")
      (forward-char)
      (insert " {")
      (newline-and-indent))
     (t
      (forward-char)
      (insert "{")))))


(defun inside-string ()
  (nth 3 (syntax-ppss)))

(defun inside-comment ()
  (nth 4 (syntax-ppss)))


(define-skeleton go-skeleton-test
  "a Go unit test"
  nil
  "func Test"
  _
  "(t *testing.T) {\n"
  >
  "t.SkipNow()\n}"
  )

(define-skeleton go-skeleton-if-err-return
  "return on error"
  nil
  "if err != nil {\n"
  >
  "return " _ "err\n"
  > -1 "}"
  )

(define-skeleton go-skeleton-if-t-fatal
  "t.Fatal on error"
  nil
  "if err != nil {\n"
  >
  "t.Fatal(err)\n"
  > -1 "}"
  )

(define-skeleton go-skeleton-if-log-fatal
  "log fatal on error"
  nil
  "if err != nil {\n"
  >
  "log.Fatal(err)\n"
  > -1 "}"
  )

(define-skeleton go-skeleton-if-err-t-fatal
  "if err := XXX; err != nil { t.Fatal(err) }"
  nil
  "if err := " _ "; err != nil {\n"
  >
  "t.Fatal(err)\n"
  > -1 "}"
  )


(defun gogrep (pat prefix)
  (interactive "MPattern: \nP")
  (jba-go-grep default-directory pat prefix))

(defun gogrep-repo (pat prefix)
  (interactive "MPattern: \nP")
  (jba-go-grep (jba-git-repo-root) pat prefix))


(defun jba-go-grep (dir pat prefix)
  (let* ((cmd (format
	       "find . -type d -name .git -prune -o %s -name '*.go' -exec grep --color -nH --null -e %s \\{\\} +"
	       (if prefix ;; exclude test files
		   "\\! -name '*_test.go' -a"
		 "")
	       (shell-quote-argument pat)))
	 (default-directory dir))
    (grep cmd)))


(defun jba-go-grep-symbol (prefix)
  (interactive "P")
  (let* ((sym (jba-symbol-at-point))
	 (ex (if prefix
		 "--exclude=*_test.go "
	       ""))
	 (cmd (concat "grep -r --color -nH --include='*.go' '\\<" sym "\\>' " ex "."))
	 (default-directory (jba-git-repo-root)))
    (grep cmd)))


(defun jba-gxref--find-symbol (symbol &rest args)
  ;; Same as gxref--find-symbol, but without the shell-quote-argument.
  (let* ((process-args
          (append args
                  (list "-x" "-a" symbol)))
         (global-output (gxref--global-to-list process-args)))
    (remove nil
            (mapcar #'gxref--make-xref-from-gtags-x-line global-output)
            )))

;(require 'gxref)

(defun jba-fuzzify (s)
  (let ((ci (apply #'concat (seq-map #'jba-case-insensitive s))))
    (concat ".*" ci ".*")))


;; (cl-defmethod xref-backend-definitions ((_backend (eql gxref)) symbol)
;;   (jba-gxref--find-symbol (jba-fuzzify symbol) "-d"))


;; Defining a qualident (qualified identfier) as a "thing".
;; See thingatpt.el, filename stuff, for the origin of this.
(defvar thing-at-point-qualident-chars ".[:alnum:]"
  "Characters allowable in qualified identifiers.")

(put 'qualident 'end-op
     (lambda ()
       (re-search-forward (concat "\\=[" thing-at-point-qualident-chars "]*")
                          nil t)))
(put 'qualident 'beginning-op
     (lambda ()
       (if (re-search-backward (concat "[^" thing-at-point-qualident-chars "]")
                               nil t)
           (forward-char)
         (goto-char (point-min)))))

(defun jba-go-test (prefix)
  (interactive "P")
  (save-some-buffers t) ; save all buffers w/o prompting
  (let ((cmd "go test -v"))
    (if prefix
	(setq cmd (concat cmd " -short")))
    (setq compile-command cmd)
    (compile compile-command)))


(defun jba-godoc (package symbol)
  (interactive (jba-godoc-args))
  (let ((s (if (null symbol)
               ""
             (concat "#" symbol))))
    (browse-url (format "http://godoc/pkg/%s%s" package s))))

(defun jba-godoc-args ()
  (let* ((bounds (bounds-of-thing-at-point 'qualident))
         (default (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (if (string-equal default "")
        (setq default nil))
    (let* ((qualident (read-string-with-default "Symbol" default))
           (strings (string-split "\\." qualident)))
      (if (= (length strings) 1)
          ;; just a package
          (list (car strings) nil)
        ;; a package and a symbol
        strings))))


(defun jba-switch-to-other-file ()
  (interactive)
  (let ((other-filename (jba-other-filename buffer-file-name)))
    (if other-filename
	(find-file other-filename))))

(defun jba-other-filename (filename)
  (cond
   ((string-match "\\(.*\\)_test.go$" filename)
    (concat (match-string 1 filename) ".go"))
   ((string-match "\\(.*\\).go$" filename)
    (concat (match-string 1 filename) "_test.go"))
   ((string-match "\\(.*\\)_test.go2$" filename)
    (concat (match-string 1 filename) ".go2"))
   ((string-match "\\(.*\\).go2$" filename)
    (concat (match-string 1 filename) "_test.go2"))
   ((string-match "\\(.*\\).down.sql$" filename)
    (concat (match-string 1 filename) ".up.sql"))
   ((string-match "\\(.*\\).up.sql$" filename)
    (concat (match-string 1 filename) ".down.sql"))
   (t
    nil)))




;; (require 'go-autocomplete)

;; (defun ac-trigger-key-command (&optional force)
;;   (interactive "P")
;;   (let (started)
;;     (when (or force (ac-trigger-command-p last-command))
;;       (message "AC...")
;;       (setq started (auto-complete-1 :triggered 'trigger-key))
;;       (message "AC...done."))
;;     (unless started
;;       (ac-fallback-command 'ac-trigger-key-command))))



;; ;;; Tweak godef-describe and godef-jump so they work if point
;; ;;; is just beyond a word.
;; (define-advice godef-describe (:around (fn point))
;;   (if (not (looking-at "\\w"))
;;       (setq point (- point 1)))
;;   (funcall fn point))

;; (define-advice godef-jump (:around (fn point))
;;   (if (not (looking-at "\\w"))
;;       (setq point (- point 1)))
;;   (funcall fn point))


;;; disabled for gopls
;(require 'google-ycmd)


;; (define-advice xref-find-definitions (:before (&rest args))
;;   (jba-set-tags-file-for-current-buffer))



;;; End Go mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++ mode.

(add-hook 'c++-mode-hook 'jba-setup-font-lock-mode)
(add-hook 'c++-mode-hook '(lambda ()
                            ;;(define-key c++-mode-map "\M-z" 'google-compile)
                            (define-key c++-mode-map "\M-z" 'jba-compile)
                            (define-key c++-mode-map "\C-c\C-l" 'goto-line)
                            (define-key c++-mode-map "\M-." 'gtags-find-tag)
			    (define-key c++-mode-map [?\C-\\] 'google-rotate-among-files-h-and-cc)))

(defun jba-c++-switch-to-other-source-file ()
  (interactive)
  (let* ((basename (file-name-sans-extension buffer-file-name))
             (ext (file-name-extension buffer-file-name))
             (other-exts (if (string= ext "h") '("cpp" "cc") '("h")))
             (found nil))
    (while (and other-exts (not found))
      (let ((filename (format "%s.%s" basename (car other-exts))))
            (if (file-exists-p filename)
                (progn
                  (find-file filename)
                  (setq found t))
              (setq other-exts (cdr other-exts)))))
    (if (not found)
            (message "could not find other source file"))))


(defun jba-insert-include-guard-and-namespace (ns)
  (interactive)
  (let ((guard-name
         (format "%s_%s_H"
                 ns
                 (upcase (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name))))))
    (save-excursion
      (beginning-of-buffer)
      (insert "#ifndef ")
      (insert guard-name)
      (insert "\n#define ")
      (insert guard-name)
      (insert "\n\n")
      (insert (format "namespace %s {\n\n" ns))
      (end-of-buffer)
      (insert (format "} // end namespace %s\n" ns))
      (insert "#endif\n"))))

(defun jba-insert-include-guard ()
  (interactive)
  (let ((guard-name (format "%s_H" (upcase (file-name-sans-extension
                                            (file-name-nondirectory buffer-file-name))))))
    (save-excursion
      (beginning-of-buffer)
      (insert "#ifndef ")
      (insert guard-name)
      (insert "\n#define ")
      (insert guard-name)
      (insert "\n\n")
      (end-of-buffer)
      (insert "#endif\n"))))

(defun jba-find-class-files (class-name)
  (interactive "sName: ")
  (find-file (format "%s.cpp" class-name))
  (if (not (save-excursion (beginning-of-buffer) (looking-at "#include")))
      (insert (format "#include \"%s.h\"\n" class-name)))
  (find-file (format "%s.h" class-name))
  (if (not (save-excursion (beginning-of-buffer) (looking-at "#ifndef")))
      (progn
            (jba-insert-include-guard)
            (beginning-of-buffer)
            (forward-line 3)
            (insert (format "class %s\n" class-name))
            (insert "{\n};\n\n"))))

;(global-set-key "\C-c\C-f" 'jba-find-class-files)

(defun jba-member-decl-to-def ()
  "Take the member function declaration (prototype) on the current line and
   change it into the skeleton of a member function definition."
  (interactive)
  ;; Clean up initial whitespace.
  (beginning-of-line)
  (delete-horizontal-space)
  ;; Obtain class name from buffer name and use it to qualify the function name.
  (let ((class-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (end-of-line)
    (backward-sexp 2)
    (insert class-name)
    (insert "::"))
  ;; Remove the semicolon at the end and insert braces.
  (end-of-line)
  (delete-backward-char 1)
  (insert "\n{\n}\n")
  )

(defun jba-def-to-proto ()
  "Create a prototype from the function definition."
  (interactive)
  (beginning-of-line)
  (let ((def (buffer-substring (point) (save-excursion (end-of-line) (point)))))
    (insert def)
    (insert ";\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python mode.

(setq python-mode-hook
      '(lambda ()
             (jba-setup-font-lock-mode)
	     (set-face-foreground font-lock-string-face
				  (if on-X "dim gray" "yellow"))
	     ;;(whitespace-mode 1)
	     ;;(setq whitespace-style '(face tabs trailing lines-tail))
	     ;;(set-face-foreground whitespace-line "white")
	     ;;(set-face-background whitespace-line "purple")
	     ;;(whitespace-mode 1)
	     (show-ws-toggle-show-trailing-whitespace)
	     (show-ws-toggle-show-tabs)
             (let ((mode-map (if (>= version 22) python-mode-map py-mode-map)))
               (define-key mode-map "\C-c\C-l" 'goto-line)
               (define-key mode-map "\M-z" 'jba-compile)
	       (define-key mode-map "\C-\M-a" 'jba-py-up-class)
	       (define-key mode-map "\M-\?" #'jba-py-grep-symbol)
	       (define-key mode-map "\C-c\C-a" 'jba-py-which-class)
	       (define-key mode-map  [?\C-\\] #'jba-py-switch-to-other-source-file)
	       )
	     (setq py-python-command "python")
	     (setq python-indent 4)
	     (setq python-continuation-offset 4)
	     (jba-setup-max-column 79)
	     ;; TODO: add something to font-lock-keywords to highlight doc comments
	     ))

(defun jba-py-up-class ()
  "Go up to the enclosing class, or if at a class decl, the previous class."
  (interactive)
  (if (looking-at "^class")
      (forward-char -1))
  (goto-char (jba-py-point-at-class)))

(defun jba-py-which-class ()
  (interactive)
  (save-excursion
    (jba-py-up-class)
    (forward-word 2)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
	   (name (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (message "%s" name))))

(defun jba-py-point-at-class ()
  (save-excursion
    (let ((done nil)
	  (pt nil))
      (while (not done)
	(python-nav-backward-up-list)
	(if (looking-at "^class")
	    (setq done t))
	(if (eql (point) pt)
	    (error "not in a class"))
	(setq pt (point)))
      (point))))


(defvar py-cloud-dirs '(
			("bigquery/google/cloud/bigquery" . "bigquery/tests/unit")
			("firestore/google/cloud/firestore_v1beta1" . "firestore/tests/unit")
			))

(defun jba-py-switch-to-other-source-file ()
  (interactive)
  (let* ((repo-root "/usr/local/google/home/jba/git-repos/jba/google-cloud-python")
	 (base (file-name-base buffer-file-name))
	 (is-test (string-prefix-p "test_" base))
	 (other-base (if is-test (substring base 5) (concat "test_" base)))
	 (dir (file-name-directory buffer-file-name)))
    (if (not (string-prefix-p repo-root dir))
	(error "dir ~s not in repo root ~s" dir repo-root))
    (let* ((reldir (substring dir (+ (length repo-root) 1) (- (length dir) 1)))
	   (other-dir nil))
      (dolist (dirs py-cloud-dirs)
	(let ((ed (if is-test (cdr dirs) (car dirs)))
	      (od (if is-test (car dirs) (cdr dirs))))
	  (message "reldir=%s, ed=%s" reldir ed)
	  (if (string-equal reldir ed) (setq other-dir od))))
      (let ((other-filename (concat repo-root "/" other-dir "/" other-base ".py")))
	(find-file other-filename)))))

(defun jba-py-grep-symbol (prefix)
  (interactive "P")
  (let* ((sym (jba-symbol-at-point))
	 (cmd (concat "grep --color -nH '\\<" sym "\\>' *.py")))
    (grep cmd)))



(defun read-string-with-default (prompt-prefix default)
  (let ((prompt (if default
                    (format "%s (default: %s): " prompt-prefix default)
                  (format "%s: " prompt-prefix))))
    (read-string prompt nil nil default)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java mode.

(c-add-style "jba-google"
	     '("google"
	       (c-offsets-alist
		(arglist-cont-nonempty . ++)
		(arglist-close . ++)
		))
)

(setq java-mode-hook
      '(lambda ()
	 (define-key java-mode-map "\M-z" 'jba-compile)
         (define-key java-mode-map "\C-c\C-l" 'goto-line)
         ;; This is for javascript mode, which uses the string face for
         ;; comments.
         (set-face-foreground font-lock-string-face "dark green")
	 (define-key java-mode-map "\C-c\C-f" 'google-java-format-buffer)
	 (define-key java-mode-map "\C-c\C-j" 'google-imports-jade)
	 (define-key java-mode-map [?\C-\\] 'google-rotate-among-files)
	 (jba-setup-max-column 100)
	 (c-set-style "jba-google")
         ))


;; White background.
(defun jba-setup-font-lock-mode ()
  (interactive)
  (font-lock-mode 1)
  (set-face-foreground font-lock-comment-face "dark green")
  (set-face-foreground font-lock-comment-delimiter-face "dark green")
  (let ((faces (list font-lock-keyword-face
                     font-lock-string-face
                     font-lock-constant-face
                     font-lock-builtin-face
                     font-lock-variable-name-face
                     font-lock-type-face
                     )))
    (while (not (null faces))
      (set-face-foreground  (car faces) "unspecified")
      (set-face-attribute (car faces) nil :weight 'normal)
      (setq faces (cdr faces))))
  )

(defun jba-setup-max-column (col)
  (setq fill-column col)
  (highlight-beyond-fill-column)
  (set-face-background 'highlight-beyond-fill-column-face "red")
  (set-face-underline 'highlight-beyond-fill-column-face nil))


(defun checkpoint ()
  "Save a copy of this file, to a new name"
  (interactive)
  (let ((filename (create-checkpoint-filename buffer-file-name)))
    (write-region (point-min) (point-max) filename)
    (message "Checkpointed to %s" filename)))

(defvar checkpoint-directory "my-checkpoints")

(defun create-checkpoint-filename (fname)
  (let* ((cp-char ?#)
         (basename (file-name-sans-extension (file-name-nondirectory fname)))
         (dir (file-name-as-directory (or checkpoint-directory (file-name-directory fname))))
         (ext (file-name-extension fname))
         (max 0))
    (if (not (file-exists-p dir))
            (make-directory dir))
    (let ((checkpoints (directory-files dir nil
                                        (format "%s%c[0-9]+\\.%s$"
                                                basename cp-char ext))))
      (while checkpoints
            (let* ((cp (car checkpoints))
                   (start (+ (string-match (string cp-char) cp) 1))
                   (end   (- (length cp) (+ (length ext) 1)))
                   (num   (string-to-int (substring cp start end))))
              (if (> num max)
                  (setq max num))
              (setq checkpoints (cdr checkpoints))))
      (format "%s%s%c%d.%s"
                  dir basename cp-char (+ max 1) ext))))


(defun find-duplicate-lines ()
  (interactive)
  (let ((done nil)
            line)
    (while (not done)
      (search-forward-regexp "^.*$")
      ;; ignore whitespace at end of line
      (setq line (format "%s[ \\t]*$"
                         (buffer-substring (match-beginning 0) (match-end 0))))
      (forward-line 1)
      (if (looking-at line)
              (progn
                (push-mark (point) t t)
                (setq done t)
                (while (looking-at line)
                  (forward-line 1)))))))



;(global-set-key "\C-c\C-s" 'jba-shell)

(defun jba-shell ()
  (interactive)
  (if (get-buffer "*shell*")
      (switch-to-buffer-other-frame "*shell*")
    (select-frame (make-frame '((name . "SHELL")
                                                (width . 100)
                                                (height . 42))))
    (shell)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous modes.

(setq text-mode-hook
      '(lambda ()
         (turn-on-auto-fill)
         (abbrev-mode 1)
         ;; (define-key text-mode-map "\t"
         ;;   '(lambda () (interactive) (insert-tab)))
         (sep-line-setup "" "-" "")
         ))



(setq comint-mode-hook
      '(lambda ()
             (define-key comint-mode-map "\C-\M-l" 'switch-to-other-buffer)
             ;(setq comint-completion-addsuffix '("\\" . " "))       ; Make tab completion in shell use a backslash.
             (set-face-foreground 'comint-highlight-prompt
                                  (if on-X "blue" "yellow"))
             ))

(setq emacs-lisp-mode-hook
      '(lambda ()
         (jba-setup-font-lock-mode)))

(setq protobuf-mode-hook
      '(lambda ()
	 (define-key protobuf-mode-map "\M-z" 'jba-compile)
         (define-key protobuf-mode-map "\C-c\C-l" 'goto-line)
	 ))

(defvar json-mode-hook nil)

(define-derived-mode json-mode js-mode "JSON"
  "Major mode for editing JSON files"
  (if (not show-ws-highlight-tabs-p)
      (show-ws-toggle-show-tabs))
)


(setq scheme-mode-hook
      '(lambda ()
             (put 'dotimes 'scheme-indent-function 1)
             (put 'dolist 'scheme-indent-function 1)))


(add-hook 'sh-mode-hook #'(lambda ()
			    (add-hook 'before-save-hook #'(lambda ()
							    (untabify (point-min) (point-max))
							    (delete-trailing-whitespace))
				      nil t)))

(add-hook 'js-mode-hook '(lambda () (setq indent-tabs-mode nil)))


(setq gfm-mode-hook '(lambda ()
		       (google-maybe-show-trailing-whitespace)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities.

(defun get-symbol-around-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (not (null bounds))
            (buffer-substring (car bounds) (cdr bounds)))))


(defun jba-git-repo-root ()
  (string-trim-right (shell-command-to-string "git rev-parse --show-toplevel")))


(defun jba-symbol-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
	(default (if bounds
		     (buffer-substring-no-properties (car bounds) (cdr bounds)))))
     (read-string-with-default "Symbol" default)))

(defun jba-case-insensitive (char)
  (if (= (downcase char) (upcase char))
      (string char)
    (format "[%c%c]" (upcase char) (downcase char))))

(defun join-list (lst sep)
  (let ((result ""))
    (while lst
      (setq result (concat result sep (car lst)))
      (setq lst (cdr lst)))
    result))

(defun count-chars (string char)
  (let ((i 0)
            (count 0))
    (while (< i (length string))
      (if (= (aref string i) char)
              (setq count (1+ count)))
      (setq i (1+ i)))
    count))

(defun collect-advice (func)
  (let (ads)
    (advice-mapc #'(lambda (f p) (setq ads (cons f ads))) func)
    ads))


(defun clear-advice (func)
  (dolist (a (collect-advice func))
    (advice-remove func a)))



(defun jba-find-tag-file (filename)
  "Find the nearest TAGS file by looking up from FILENAME's directory."
  (let ((dir (file-name-directory filename))
	(tagfile nil)
	(homedir (getenv "HOME")))
    (while (and (>= (length dir) (length homedir)) (null tagfile))
      (let ((tf (concat dir "TAGS")))
	(if (file-exists-p tf)
	    (setq tagfile tf)
	  (setq dir (file-name-directory (substring dir 0 -1))))))
    tagfile))

(defun jba-set-tags-file-for-current-buffer ()
  (let ((tf (jba-find-tag-file buffer-file-name)))
    (if tf
	(setq tags-file-name tf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google.

(require 'rotate-among-files)

(define-key global-map [?\C-\;] 'google-rotate-among-files)

;; For google-maybe-show-trailing-whitespace
(push 'protobuf-mode google-trailing-whitespace-modes)
(push 'gfm-mode google-trailing-whitespace-modes)
(push 'yaml-mode google-trailing-whitespace-modes)

(set-face-background 'trailing-whitespace "yellow2")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google3.

(setq p4-use-p4config-exclusively t)

(defvar citc-path (concat "/google/src/cloud/" user-login-name "/"))

(defun citc-client-name (path)
  (if (string-prefix-p citc-path path)
      (let ((segments (split-string path "/")))
	(nth 5 segments))
    nil))

(defun google3-dir (path)
  (if (string-match "/google3/\\(.*\\)/.*" path)
      (concat "//" (match-string 1 path))
    ""))


;; Turning g4d aliases into environment variables, for find-file.
;; (defun g4d-aliases-to-env-vars ()
;;     (let ((loading-buffer (find-file-noselect "~/.g4d")))
;;       (with-current-buffer loading-buffer
;; 	(goto-char (point-min))
;; 	(while (< (point) (point-max))
;; 	  (let* ((line (buffer-substring (point) (save-excursion (end-of-line) (point))))
;; 		 (words (delete "" (split-string line "[ \t]+"))))
;; 	    (when (and (= (length words) 3) (equal (car words) "alias"))
;; 	      (let* ((var (substring (nth 1 words) 1))
;; 		     (val (substring (nth 2 words) 1)))
;; 		(setenv var val)))
;; 	    (forward-line 1)))
;;       (kill-buffer loading-buffer))))

;; (g4d-aliases-to-env-vars)

(defvar google3-package-alist
  '(("context"	"go/context/context")
    ("creds"	"security/credentials/go/creds")
    ("proto"	"net/proto2/go/proto")
    ("rpc"	"net/rpc/go/rpc")
    ("rpcprod"	"net/rpc/go/rpcprod")
    ("google"   "base/go/google")
    ("log"	"base/go/log")
    ("runfiles" "base/go/runfiles")
    ("flag"	"base/go/flag")
    ))


(defun glaze ()
  (interactive)
  (shell-command "glaze" nil nil))

(setq borg-mode-hook
      '(lambda ()
	 (define-key c-mode-map "\C-c\C-l" 'goto-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '.emacs)

(message "Loading jba's .emacs...done.")

;;; .emacs ends here
