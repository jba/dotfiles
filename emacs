;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(message "Loading jba's .emacs...")

;; Search for "dot-emacs" in my email to find old and/or obsolete stuff.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial stuff.

(setq version (string-to-int emacs-version))

(setq on-X (let ((d (getenv "DISPLAY")))
	     (not (or (null d) (string= d "")))))

(defmacro += (stringvar &rest vals)
  (list 'setq stringvar (cons 'concat (cons stringvar vals))))

(setq load-path (append load-path
                        '("~/.emacs.d/lisp"
                          )))


(dolist (shellfile '(".bashrc" ".bash_env" ".bash_aliases"))
  (add-to-list 'auto-mode-alist (cons (concat "\\" shellfile) 'shell-script-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance.

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

;; this causes a type error.
;;  (setq dired-mode-hook
;;        '(lambda ()
;;  	 (if on-X
;;  	     (set-face-foreground dired-directory "cyan"))))

(defun triple-pane-80 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))


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


(setq-default fill-column 85)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google-specific.

(require 'google)

;(load-file "/home/build/public/eng/elisp/google.el")
(setq p4-use-p4config-exclusively t)

(require 'rotate-among-files)

(define-key global-map [?\C-\;] 'google-rotate-among-files)

;; For google-maybe-show-trailing-whitespace
(setq google-trailing-whitespace-modes
      (cons 'protobuf-mode
            google-trailing-whitespace-modes))

(set-face-background 'trailing-whitespace "yellow2")

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
(defun g4d-aliases-to-env-vars ()
    (let ((loading-buffer (find-file-noselect "~/.g4d")))
      (with-current-buffer loading-buffer
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (let* ((line (buffer-substring (point) (save-excursion (end-of-line) (point))))
		 (words (delete "" (split-string line "[ \t]+"))))
	    (when (and (= (length words) 3) (equal (car words) "alias"))
	      (let* ((var (substring (nth 1 words) 1))
		     (val (substring (nth 2 words) 1)))
		(setenv var val)))
	    (forward-line 1)))
      (kill-buffer loading-buffer))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various settings.

(setq kill-ring-max 300)

(global-set-key "\C-x\C-k"
  '(lambda () (interactive) (kill-buffer nil)))

(global-set-key "\C-c\C-l" 'goto-line)

(global-set-key "\C-z" 'undo)
;(global-set-key "\C-z" 'suspend-emacs)

(global-unset-key "\C-\\") ;; bound to toggle-input-method

;(global-set-key "\C-c!" 'toggle-read-only)

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

;; Support for saving the previous state.
;; (desktop-load-default)
;; (desktop-read)


;(transient-mark-mode 1)

;(set-face-background 'region "blue")

(defun revbuf ()
  "Revert buffer without asking questions"
  (interactive)
  (revert-buffer nil t))


(read-abbrev-file)

;; For dabbrev expansion
(setq case-replace nil)
(setq dabbrev-case-fold-search nil)

(setq tags-case-fold-search nil)

(require 'ido)
(ido-mode)

(setq ido-max-dir-file-cache 0) ; caching unreliable

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;; Use the gnome/freedesktop.org magic for browser choice and options.
;; From Roland McGrath on the google emacs-users group.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open"
)

(setq sentence-end-double-space nil)

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



(windmove-default-keybindings 'shift)   ; shift-arrows move between windows
;; Also bind ctrl-arrows for contexts (e.g. tmux) that don't see shift-arrows.
(global-set-key "\M-[D" 'windmove-left)
(global-set-key "\M-[C" 'windmove-right)
(global-set-key "\M-[A" 'windmove-up)
(global-set-key "\M-[B" 'windmove-down)


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode hooks.

(setq text-mode-hook
      '(lambda ()
         (turn-on-auto-fill)
         (abbrev-mode 1)
         ;; (define-key text-mode-map "\t"
         ;;   '(lambda () (interactive) (insert-tab)))
         (sep-line-setup "" "-" "")
         ))




;; (defvar *text-indent-regexp* "[-*]\\|[0-9]+[./]")
;;  ;; / for dates

;; ;(defun text-indent-line ()
;; ;  "If the most recent line not beginning with whitespace begins with digit,
;; ;   * or -, then use indent-relative.  Otherwise, use indent-to-left-margin."
;; ;  (let ((indent-func
;; ;           (save-excursion (forward-line -1)
;; ;                                   (while (looking-at "[ \t]")
;; ;                                      (forward-line -1))
;; ;               (if (looking-at *text-indent-regexp*)
;; ;                       'indent-relative
;; ;                       'indent-to-left-margin))))
;; ;    (funcall indent-func)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C mode.

;; (setq c-mode-hook
;;       '(lambda ()
;;              (setq c-basic-offset 4)
;;              (setq tab-width 8)
;;              (setq indent-tabs-mode nil) ; always use spaces, never tabs
;;              (push '(case-label . +) c-offsets-alist)
;;              (define-key c-mode-map "\C-c\C-s" 'jba-shell)
;;              (abbrev-mode 1)
;;              ;(find-file-noselect "~/Code/jni-dabbrev-words.txt")
;;              (define-key c-mode-map "\M-z"
;;                '(lambda () (interactive) (save-some-buffers t) (recompile)))
;; ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++ mode.

(add-hook 'c++-mode-hook 'jba-setup-font-lock-mode)
(add-hook 'c++-mode-hook '(lambda ()
                            ;;(define-key c++-mode-map "\M-z" 'google-compile)
                            (define-key c++-mode-map "\M-z" 'jba-compile)
                            (define-key c++-mode-map "\C-c\C-l" 'goto-line)
                            (define-key c++-mode-map "\M-." 'gtags-find-tag)
			    (define-key c++-mode-map [?\C-\\] 'google-rotate-among-files-h-and-cc)))



;; (setq c++-mode-hook
;;       '(lambda ()
;;              (setq c-basic-offset 4)
;;              (setq tab-width 4)
;;              (setq indent-tabs-mode nil) ; always use spaces, never tabs
;;              (push '(case-label . +) c-offsets-alist)
;;              (push '(substatement-open . 0) c-offsets-alist)
;;              (jba-setup-font-lock-mode)
;;              (define-key c++-mode-map "\C-c\C-s" 'jba-shell)
;;              (define-key c++-mode-map [?\C-\;] 'jba-c++-switch-to-other-source-file)
;;              ; for putty, where C-; doesn't work:
;;              (define-key c++-mode-map [?\C-\\] 'jba-c++-switch-to-other-source-file)
;;              (define-key c++-mode-map "\M-z" 'jba-compile)
;;              (define-key c++-mode-map "\C-c\C-u" '(lambda () (interactive) (insert "using namespace std;\n")))
;;              (define-key c++-mode-map "\M-'" 'search-for-symbol)
;; ;;           (define-key c++-mode-map "{"
;; ;;             '(lambda () (interactive) (jba-java-open-brace t)))
;; ;;           (define-key c++-mode-map [?\C-{]
;; ;;             '(lambda () (interactive) (jba-java-open-brace nil)))
;;              (define-key c++-mode-map "\C-c." 'jba-square-brackets-to-dot)
;;              (define-key c++-mode-map "\C-c\C-l" 'goto-line)
             ;; ))


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



(defun jba-compile (prefix)
  (interactive "P")
  (if prefix
      (compile)
    (save-some-buffers t) ; save all buffers w/o prompting
    (compile compile-command)))


;; Note: this also removes namespace prefixes from the thing inside the square
;; brackets (it was written to change attributes to data members), so it
;; may not be right for general use.
;; (defun jba-square-brackets-to-dot ()
;;   (interactive)
;;   (if (search-forward "[" (+ (point) 200)  t)
;;       (progn
;;             (delete-backward-char 1)
;;             (insert ".")
;;             (if (looking-at "cmo::")
;;                 (delete-char 5))
;;             (if (looking-at "attr::")
;;                 (delete-char 6))
;;             (search-forward "]" (+ (point) 200))
;;             (delete-backward-char 1))))


;; (setq makefile-mode-hook

;;       '(lambda ()

;;              (define-key makefile-mode-map "\M-z" 'jba-compile)))



;; (setq compilation-mode-hook

;;       '(lambda ()

;;              (define-key compilation-mode-map "\M-z" 'jba-compile)))


(defun search-for-symbol (name)
  (interactive (list (read-string "Search for symbol: "
                                  (get-symbol-around-point))))
  (grep (format "grep -n '\\<%s\\>' *.h *.cpp" name)))

(defun get-symbol-around-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if (not (null bounds))
            (buffer-substring (car bounds) (cdr bounds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python mode.

(setq python-mode-hook
      '(lambda ()
             (jba-setup-font-lock-mode)
	     (set-face-foreground font-lock-string-face
				  (if on-X "dim gray" "yellow"))
	     ;; (whitespace-mode 1)
	     ;; (setq whitespace-style '(face tabs trailing lines-tail))
	     ;; (set-face-foreground whitespace-line "white")
	     ;; (set-face-background whitespace-line "purple")
	     ;; (whitespace-mode 1)
             (let ((mode-map (if (>= version 22) python-mode-map py-mode-map)))
               (define-key mode-map "\C-c\C-l" 'goto-line)
               (define-key mode-map "\M-z" 'jba-compile)
               (setq py-python-command "python")
               (setq python-indent 2)
	       (setq python-continuation-offset 4)
               ;; TODO: add something to font-lock-keywords to highlight doc comments
               )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Go mode.

(require 'google-go)

(setq gofmt-command "goimports")

(add-hook 'go-mode-hook 'jba-go-mode)

(defun jba-go-mode ()
  (setq tab-width 4)
  (jba-setup-font-lock-mode)
  (define-key go-mode-map "\M-z" 'jba-compile)
  (define-key go-mode-map "" 'jba-godoc)
  (define-key go-mode-map "{" #'jba-go-open-brace)
  (define-key go-mode-map "}" #'go-mode-insert-and-indent)
  (define-key go-mode-map  [?\C-\\] #'jba-go-switch-to-other-source-file)
  (define-key go-mode-map  "\C-c\C-t" #'jba-go-test)
  (define-key go-mode-map "\M-\?" #'jba-go-grep-symbol)
  (define-key go-mode-map  "\C-c\C-e"
    #'(lambda () (interactive)
	(insert "if err != nil { log.Fatal(err) }")
	(newline-and-indent)))
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
	  try-complete-lisp-symbol)))

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
    (insert "{"))))


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

(define-skeleton go-skeleton-if-log-fatal
  "log fatal on error"
  nil
  "if err != nil {\n"
  >
  "log.Fatal(err)\n"
  > -1 "}"
  )

(defun jba-go-grep-symbol (sym)
  (interactive (jba-symbol-at-point))
  (grep (concat "grep --color -nH '\\<" sym "\\>' *.go"))
)

(defun jba-symbol-at-point ()
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (default (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (if (string-equal default "")
        (setq default nil))
    (list (read-string-with-default "Symbol" default))))



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


(defun jba-go-switch-to-other-source-file ()
  (interactive)
  (let* ((other-filename-base (if (string-match "\\(.*\\)_test.go" buffer-file-name)
				  (match-string 1 buffer-file-name)
				(concat (file-name-sans-extension buffer-file-name) "_test")))
	 (other-filename (concat other-filename-base ".go")))
    (find-file other-filename)))


(defun read-string-with-default (prompt-prefix default)
  (let ((prompt (if default
                    (format "%s (default: %s): " prompt-prefix default)
                  (format "%s: " prompt-prefix))))
    (read-string prompt nil nil default)))


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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comint mode.

(setq comint-mode-hook
      '(lambda ()
             (define-key comint-mode-map "\C-\M-l" 'switch-to-other-buffer)
             ;(setq comint-completion-addsuffix '("\\" . " "))       ; Make tab completion in shell use a backslash.
             (set-face-foreground 'comint-highlight-prompt
                                  (if on-X "blue" "yellow"))
             ))

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
	 (setq fill-column 100)
	 (highlight-beyond-fill-column)
	 (set-face-background 'highlight-beyond-fill-column-face "red")
	 (set-face-underline 'highlight-beyond-fill-column-face nil)
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


(defun remove-control-Ms ()
  "Strip ^M's from a file"
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "
*$" ""))


(setq emacs-lisp-mode-hook
      '(lambda ()
         (jba-setup-font-lock-mode)))

(defun glaze ()
  (interactive)
  (shell-command "glaze" nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq protobuf-mode-hook
      '(lambda ()
	 (define-key protobuf-mode-map "\M-z" 'jba-compile)
         (define-key protobuf-mode-map "\C-c\C-l" 'goto-line)
	 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json mode

(defvar json-mode-hook nil)

(define-derived-mode json-mode js-mode "JSON"
  "Major mode for editing JSON files"
  (if (not show-ws-highlight-tabs-p)
      (show-ws-toggle-show-tabs))
)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))


(message "Loading jba's .emacs...done.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OBSOLETE.

;; (setq Info-mode-hook
;;       '(lambda ()
;;              (set-face-foreground 'info-xref "yellow")
;;              (set-face-foreground 'info-header-xref "yellow")
;;              ))

;; (setq shell-mode-hook
;;       '(lambda ()
;;              (define-key shell-mode-map "\C-\M-l" 'switch-to-other-buffer)
;;              (define-key shell-mode-map "\C-c\C-s" 'other-frame)
;;              ;(set-face-foreground 'comint-highlight-prompt "yellow")
;;              ))

;; (defun copy-buffer ()
;;   (interactive)
;;   (copy-region-as-kill (point-min) (point-max))
;;   (message "Buffer copied."))



;; (defun hilite-word (string)
;;   (interactive "sString: ")
;;   (setq regexp (format "\\b%s\\b" string))
;;   (setq font-lock-keywords (cons regexp font-lock-keywords))
;;   (font-lock-fontify-buffer))



;; (defun unhilite-word (string)
;;   (interactive "sString: ")
;;   (setq regexp (format "\\b%s\\b" string))
;;   (setq font-lock-keywords (delete regexp font-lock-keywords))
;;   (font-lock-fontify-buffer))


;; ;; This one is for a black background.
;; (defun jba-setup-font-lock-mode ()
;;   (font-lock-mode 1)

;;   ;(set-face-foreground font-lock-comment-face "snow4")
;;   ;(set-face-underline-p font-lock-string-face nil)
;;   (set-face-foreground font-lock-comment-face "green")
;;   (if (> version 21)
;;       (set-face-foreground font-lock-comment-delimiter-face "green"))
;;   (let ((faces (list font-lock-keyword-face
;;                              font-lock-string-face
;;                              font-lock-constant-face
;;                              font-lock-builtin-face
;;                              font-lock-reference-face
;;                              font-lock-variable-name-face
;;                              font-lock-type-face
;;                              )))
;;     (while (not (null faces))
;;       (set-face-foreground  (car faces) "white")
;;       (set-face-attribute (car faces) nil :weight 'normal)
;;       (setq faces (cdr faces)))))

;; (defun jba-java-open-brace (newline)
;;   (expand-abbrev)
;;   (backward-char)
;;   (while (looking-at " ")
;;     (delete-char 1)
;;     (backward-char))
;;   (forward-char)
;;   (if (at-beginning-of-line) ; this case for C++ function style
;;       (progn (insert "{")
;;                  (newline-and-indent))
;;     ;; else
;;     (insert " {")
;;     (if (not newline)
;;             (insert " ")
;;       ;; else, put in a newline
;;       (if (or (= (point) (point-max))
;;                   (next-line-is-blank)
;;                   (next-line-is-close-brace))
;;               (newline-and-indent)
;;             (forward-line)
;;             (c-indent-command)))))

;; (defun next-line-is-blank ()
;;   ;; really just tests if next line is empty
;;   (save-excursion
;;     (forward-line)
;;     (let ((start (point)))
;;       (end-of-line)
;;       (= (- (point) start) 0))))

;; (defun next-line-is-close-brace ()
;;   (save-excursion
;;     (forward-line)
;;     (looking-at "[ \t]*}")))

;; (defun java-run-buffer ()
;;   (interactive)
;;   (let ((classname (file-name-nondirectory (file-name-sans-extension
;;                                                                 buffer-file-name))))
;;     (call-process "start" nil 0 nil "command.com" "/c" "java" classname)
;;     ))

;; (defun java-copy-for-PowerPoint ()
;;   (interactive)
;;   (if buffer-read-only
;;       (toggle-read-only))
;;   (setq tab-width 4)
;;   (let ((start (point-min))
;;             (end   (point-max)))
;;     (tabify start end)
;;     (copy-region-as-kill start (point-max)) ;; max may change
;;     (untabify start (point-max))))

;; (defun untabify-buffer ()
;;   (interactive)
;;   (untabify (point-min) (point-max)))


;; (global-set-key "\C-x\C-j" 'find-java-source-file)

;; (defun find-java-class (name arg)
;;   (interactive "MClass name: \nP")
;;   (if (string-match "\\.java$" name)
;;       (setq name (substring name 0 -5)))
;;   (let ((fname (concat name ".java"))
;;             (cori (if arg "interface" "class")))
;;     (find-file fname)
;;     (if (not (file-exists-p fname))
;;             (insert (format "public %s %s " cori name)))))

;; ;(global-set-key "\C-c\C-f" 'find-java-class)
;; (global-set-key "\C-x\C-t" '(lambda () (interactive) (recenter 0)))

;; (defun define-property (name type)
;;   (interactive "sName: \nsType: ")
;;   (c-indent-command)
;;   (let ((varname (lowercase-first-letter name))
;;             (fname (cap-first-letter name)))
;;     (insert (format "private %s %s;\n" type varname))
;;     (newline-and-indent)
;;     (insert (format "public %s get%s() { return %s; }\n" type fname varname))
;;     (newline-and-indent)
;;     (insert (format "public void set%s(%s x) { %s = x; }\n" fname type varname))
;;     (newline-and-indent)
;;     ))

;; (defun write-getter ()
;;   (interactive)
;;   (let* ((varname (get-symbol-around-point))
;;              (s (format "XXX get%s() { return %s; }"
;;                             (cap-first-letter varname)
;;                             varname)))
;;     (forward-line)
;;     (insert "\n")
;;     (insert s)))

;; (defun write-setter ()
;;   (interactive)
;;   (let* ((varname (get-symbol-around-point))
;;              (s (format "void set%s(T x) { %s = x; }"
;;                             (cap-first-letter varname)
;;                             varname)))
;;     (forward-line)
;;     (insert "\n")
;;     (insert s)))

;; (defun cap-first-letter (string)
;;   (concat (char-to-string (capitalize (aref string 0)))
;;               (substring string 1)))

;; (defun lowercase-first-letter (string)
;;   (concat (char-to-string (downcase (aref string 0)))
;;               (substring string 1)))

;; (defun insert-exception-class (name)
;;   (interactive "MException name: ")
;;   (insert (format "class %s extends Exception {" name))
;;   (newline-and-indent)
;;   (insert (format "%s() {}" name))
;;   (newline-and-indent)
;;   (insert (format "%s(String message) { super(message); }" name))
;;   (newline)
;;   (insert "}\n"))

;; (defun write-exception-class ()
;;   (interactive)
;;   (let* ((exc-name (get-symbol-around-point))
;;              (filename (concat exc-name ".java")))
;;     (find-file filename)
;;     (if (not (file-exists-p filename))
;;             (insert-exception-class exc-name))))

;; (defun get-exception-name-from-error-buffer ()
;;   (save-excursion
;;     (set-buffer (get-buffer "*compilation*"))
;;     (search-forward "unreported exception ")
;;     (let ((start (point)))
;;       (search-forward ";")
;;       (buffer-substring start (- (point) 1)))))

;; (defun insert-throws-clause-from-error ()
;;   (interactive)
;;   (let* ((full-ex (get-exception-name-from-error-buffer))
;;              (ex (car (last (split-string full-ex "\\.")))))
;;     ;; go to start of method
;;     (beginning-of-line)
;;     (search-forward ")")
;;     (insert " throws ")
;;     (insert ex)))

;; (defun break-at-throws ()
;;   ;; Must be on function header line
;;   (interactive)
;;   (beginning-of-line)
;;   (search-forward "throws")
;;   (forward-word -1)
;;   (insert "\n")
;;   (tab-to-tab-stop)
;;   (tab-to-tab-stop))

;; (defun jba-fori-after-abbrev ()
;;   (backward-word 2)
;;   (backward-char 1)
;;   )

;; (defun jba-toggle-selective-display ()
;;   (interactive)
;;   (if (null selective-display)
;;       (set-selective-display (+ c-basic-offset 1))
;;     (set-selective-display nil)))

;; (defun capitalize-identifier ()
;;   (interactive)
;;   (skip-syntax-forward " ")
;;   (capitalize-region (point) (+ (point) 1)))

;; ;; (defun capitalize-identifier ()
;; ;;   (interactive)
;; ;;   (let ((old-cfs case-fold-search))
;; ;;     (unwind-protect
;; ;;          (let ((end (save-excursion
;; ;;                           (setq case-fold-search nil)
;; ;;                           (while (looking-at "[A-Z]")
;; ;;                             (forward-char))
;; ;;                           (while (looking-at "[a-z_]")
;; ;;                             (forward-char))
;; ;;                           (point))))
;; ;;            (capitalize-region (point) end)
;; ;;            (goto-char end))
;; ;;       (setq case-fold-search old-cfs))))


;; (defun jba-find-class-tag (name)
;;   (interactive (find-tag-interactive "Class name: "))
;;   (find-tag-regexp (concat "\\(class\\|interface\\)[ \\t]+" name "[ \\t\\n]")))

;; ;(global-set-key  "\M-." 'jba-find-class-tag)



;; (defun rename-class (new-name)
;;   (interactive "MNew name: ")
;;   (let ((old-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
;;     (beginning-of-buffer)
;;     (query-replace old-name new-name)
;;     (rename-file buffer-file-name (concat buffer-file-name ".old"))
;;     (write-file (concat new-name ".java"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation.

;; (setq java-project-alist
;;       '()
;;              (project (name salesdemo)
;;                           (package-root "c:/DSCode/salesdemo/projects/src")
;;                           (class-directory
;;                            "c:/DSCode/salesdemo/projects/classes")
;;                           (jars ("c:/DataSynapse/jdriver/DSDriver.jar"))
;;              ))
;; ))


;; (defvar java-compile-flags "-g -deprecation -source 1.4")
;; (defvar java-separate-source-and-object-trees nil)
;; (defvar jdk-home (getenv "java_home"))
;; (defvar classpath nil)

;; If using packages but you want to have sources and class files
;; in the same directory, do the following (assuming pkg is one level deep):
;;          (setq java-compile-flags "-sourcepath ..")

;; If using packages with separate src and classes dirs, do the following:
;; (Assuming the package was two levels deep)
;;          (setq java-compile-flags "-sourcepath ../.. -d ../../../classes")


;; (setq current-project nil)

;; (defun java-compile-buffer ()
;;   "Compile the buffer as a Java program"
;;   (interactive)
;;   (save-buffer)
;;   (normal-mode)             ; to reparse initial -*- line
;;   (let ((project current-project))
;;     (if (null project)
;;             (setq project (inherited-directory-assoc
;;                                (file-name-directory buffer-file-name)
;;                                java-project-alist)))
;;     (if (null project)
;;             (java-standard-compile)
;;       (let ((command (project-get project 'compile-function)))
;;             (message "Using project %s" (project-get project 'name))
;;             (if (null command)
;;                 (java-project-compile project)
;;               (funcall command))))))


;; (defun inherited-directory-assoc (key alist)
;;   "Keys are directories; look up the key, matching on parent directories."
;;   (if (or (string= key "/") (string-match "^[A-Za-z]:/$" key))
;;       nil
;;     (let ((pair (assoc key alist)))
;;       (if pair
;;               (cadr pair)
;;             (inherited-directory-assoc
;;              (file-name-as-directory (file-truename (concat key "..")))
;;              alist)))))

;; (defun ds-java-compile ()
;;   (compile "lccompile.bat"))


;; (defun java-project-compile (project)
;;     (let ((cp (project-get project 'classpath))
;;               (dest (project-get project 'class-directory))
;;               (cflags "-g"))
;;       (if cp (+= cflags " -classpath " cp))
;;       (if dest
;;               (progn
;;                 (setq cflags (concat cflags " -d " dest))
;;                 (make-directory dest t)))
;;       (javac cflags)))


;; (defun java-standard-compile ()
;;   (let ((cflags (java-assemble-standard-compile-flags)))
;;     (javac cflags)))

;; (defun java-assemble-standard-compile-flags ()
;;   (let ((pkg (get-buffer-java-package))
;;             (cflags ""))
;;     (if (null pkg)
;;             (progn
;;               (setq cflags java-compile-flags)
;;               (if (not (null classpath))
;;                   (+= cflags " -classpath " classpath)))
;;       ;; Set up flags for package compilation.
;;       (let* ((dotcount (count-chars pkg ?.))
;;                  (up-path (make-up-path (+ dotcount 1))))
;;             (if java-separate-source-and-object-trees
;;                 (let ((destination up-path "/classes"))
;;                   (+= cflags " -sourcepath " up-path)
;;                   (+= cflags " -d " destination)
;;                   (make-directory (format "%s/%s" destination
;;                                           (package-to-path pkg)) t)))
;;             (+= cflags " -classpath " (if (null classpath)
;;                                           up-path
;;                                         (concat up-path ";" classpath)))))
;;     cflags))



;; (defun javac (flags)
;;   (compile (format "%s/bin/javac %s %s"
;;                            jdk-home
;;                            flags
;;                            (file-name-nondirectory buffer-file-name))))


;; (defun alist-to-flags (alist)
;;   (let ((result ""))
;;     (dolist (pair alist)
;;       (setq result (format "%s -%s %s" result (car pair) (cadr pair))))
;;     result))

;; (defun get-buffer-java-package ()
;;   (if (null (search-for-package-declaration))
;;       nil
;;     (buffer-substring (match-beginning 1) (match-end 1))))

;; (defun search-for-package-declaration ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (re-search-forward "package \\([A-Za-z0-9_.]+\\);" nil t)))

;; (defun set-package (pkg)
;;   (interactive "MPackage name: ")
;;   (if (null (search-for-package-declaration))
;;       (progn
;;             (goto-char (point-min))
;;             (insert (format "package %s;\n\n" pkg)))
;;     ;; package name is match #1
;;     (kill-region (match-beginning 1) (match-end 1))
;;     (goto-char (match-beginning 1))
;;     (insert pkg)))

;; (defun make-up-path (n)
;;   (if (<= n 0)
;;       ""
;;     (let ((result ".."))
;;       (while (> n 1)
;;             (setq result (concat result "/.."))
;;             (setq n (1- n)))
;;       result)))


;; (defun package-to-path (pkg)
;;   (subst-char-in-string ?. ?/ pkg))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Projects.

;; ;;; A project is a list of the form (project name (attr val) (attr val) ...)
;; (setq testproj '(project test (root "c:/testdir") (package-root "c:/testdir")
;;                                      (jars ("c:/DataSynapse/jdriver/DSDriver.jar"))
;;                                      ))

;; (setq project-attribute-function-alist
;;       '((classpath project-classpath)))

;; (defun project-name (proj)
;;   (cadr proj))

;; (defun project-get (proj attr)
;;   (let ((pair (assoc attr (cddr proj))))
;;     (if pair
;;             (cadr pair)
;;       (let ((func (cadr (assoc attr project-attribute-function-alist))))
;;             (if func
;;                 (funcall func proj)
;;               nil)))))

;; (defun project-classpath (proj)
;;   (concat (project-get proj 'package-root)
;;               (join-list (project-get proj 'jars) ";")))

;; (defun get-project (name)
;;   (let ((lst java-project-alist)
;;             (result nil))
;;     (while (and lst (null result))
;;       (let ((proj (cadr (car lst))))
;;             (if (equal name (project-name proj))
;;                 (setq result proj)))
;;       (setq lst (cdr lst)))
;;     result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme mode.  (For STk)

(setq scheme-mode-hook
      '(lambda ()
             (put 'dotimes 'scheme-indent-function 1)
             (put 'dolist 'scheme-indent-function 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Holes.

;; (defvar hole-start "<<")
;; (defvar hole-end   ">>")

;; (defvar hole-regexp (concat hole-start "[^>]*" hole-end))

;; (defun jba-search-forward-hole ()
;;   (interactive)
;;   (forward-char 1)
;;   (search-forward-regexp hole-regexp)
;;   (push-mark (match-beginning 0))
;;   (exchange-point-and-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (eval-after-load
;;   "filecache"
;;    '(jba-file-cache-initialize))


;; (defun jba-file-cache-initialize ()
;;    (interactive)
;;    (message "Loading file cache...")
;;    (file-cache-add-file "~/.emacs")
;;    (file-cache-add-file "c:/Program Files/Dragon Systems/NaturallySpeaking/Users/Jonathan/current/emacs.dvc")
;;    (file-cache-add-directory "c:/livecluster/source/java/com/livecluster/")
;;    )

;; (autoload 'p4-edit "p4"  "open file for edit using Perforce" t)

;; (autoload 'p4-add  "p4"  "add file to Perforce" t)

;; (global-set-key "\C-xpe" 'p4-edit)

;; (global-set-key "\C-xpa" 'p4-add)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timeclock mode.

;;; line format: date in-time out-time lunch-hours excess

;; (defconst tc-workday-hours 8.5)

;; (defun tc-date (line) (nth 0 line))
;; (defun tc-time-in (line) (nth 1 line))
;; (defun tc-time-out (line) (nth 2 line))
;; (defun tc-lunch (line) (nth 3 line))
;; (defun tc-excess-hours (line) (nth 4 line))

;; (defun get-current-line ()
;;   (buffer-substring (line-beginning-position) (line-end-position)))

;; (defun tc-string-to-minutes (s)
;;   (let ((ss (split-string s ":")))
;;     (if (/= (length ss) 2)
;;             (error "malformed time: ~s" s))
;;     (+ (* 60 (string-to-int (car ss)))
;;        (string-to-int (cadr ss)))))

;; (defun tc-current-line ()
;;   (let* ((s (split-string (get-current-line)))
;;              (in (tc-time-in s))
;;              (out (tc-time-out s))
;;              (lunch (tc-lunch s))
;;              (excess (tc-excess-hours s)))
;;     (list (tc-date s)
;;               (and in (tc-string-to-minutes in))
;;               (and out (+ (tc-string-to-minutes out) (* 12 60)))
;;               (and lunch (* (string-to-int lunch) 60))
;;               (and excess (string-to-int excess)))))


;; (defun tc-compute-minutes-worked ()
;;   (let ((line (tc-current-line)))
;;     (- (- (tc-time-out line) (tc-time-in line))
;;        (tc-lunch line))))

;; (defun tc-compute-excess-minutes ()
;;   (- (tc-compute-minutes-worked) (* tc-workday-hours 60)))

;; (defun tc-insert-excess ()
;;   (interactive)
;;   (let ((ex-hrs (/ (tc-compute-excess-minutes) 60.0)))
;;     (end-of-line)
;;     (delete-horizontal-space)
;;     (insert "\t")
;;     (if (> ex-hrs 0)
;;             (insert "+"))
;;     (insert (format "%g" ex-hrs))))

;; (defun tc-total-excess ()
;;   (interactive)
;;   (let ((beginning (region-beginning))
;;             (end (region-end))
;;             (total 0))
;;     (goto-char beginning)
;;     (while (< (point) end)
;;       (if (not (looking-at "[ \t\n]"))
;;               (let ((line (tc-current-line)))
;;                 (incf total (tc-excess-hours line))))
;;       (forward-line 1))
;;     total))



;; (defun find-newest-file (dir)
;;   (interactive "DDirectory: ")
;;   (let ((files (directory-files dir t "server.*")))
;;     (if (null files)
;;             (error "no files in %s" dir)
;;       (progn
;;             (let ((newest (car files))
;;                   (f (cdr files)))
;;               (while (not (null f))
;;                 (if (file-newer-or-same-p (car f) newest)
;;                         (setq newest (car f)))
;;               (setq f (cdr f)))
;;             (find-file newest))))))

;; (defun file-newer-or-same-p (file1 file2)
;;   (or (file-newer-than-file-p file1 file2)
;;       (not (file-newer-than-file-p file2 file1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hiding lines.

;; How to hide things in emacs:
;; Set selective-display to t, meaning everything from ^M (control-M)
;; to end of line is invisible.  Then replace a line's previous
;; newline with ^M.

;; From progmodes/hideif.el.

;; (defun jba-flag-region (from to flag)
;;   "Hides or shows lines from FROM to TO, according to FLAG.
;; If FLAG is \\n (newline character) then text is shown, while if FLAG is \\^M
;; \(control-M) the text is hidden."
;;   (let ((modp (buffer-modified-p)))
;;     (unwind-protect (progn
;;                               (subst-char-in-region from to
;;                                           (if (= flag ?\n) ?\^M ?\n)
;;                                           flag t) )
;;       (set-buffer-modified-p modp))
;;     ))

;; (defun jba-hide-region (from to)
;;   (jba-flag-region from to ?\^M))

;; (defun jba-show-region (from to)
;;   (jba-flag-region from to ?\n))

;; (defun jba-hide-line (start)
;;   ;; given start position of line
;;   (if (not (= start (point-min)))
;;       (jba-hide-region (- start 1) start)))

;; (defun hide-lines-matching-regexp (regexp)
;;   (interactive "MRegexp: ")
;;   (setq selective-display t)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (while (re-search-forward regexp nil t)
;;       (let ((start (progn (goto-char (match-beginning 0)) (beginning-of-line)
;;                        (point))))
;;             (jba-hide-line start)
;;             (goto-char (match-end 0))))))


;; (defun hide-lines-not-matching-regexp (regexp)
;;   (interactive "MRegexp: ")
;;   (setq selective-display t)
;;   (let ((prev-end (point-min)))
;;     (save-excursion
;;       (beginning-of-buffer)
;;       (while (re-search-forward regexp nil t)
;;             (let ((start (progn (goto-char (match-beginning 0))
;;                              (beginning-of-line) (point))))
;;               (jba-hide-region prev-end (- start 1))
;;               (setq prev-end (progn (goto-char (match-end 0))
;;                                  (end-of-line) (point)))))
;;       (jba-hide-region prev-end (point-max)))))

;; (defun show-all ()
;;   (interactive)
;;   (jba-show-region (point-min) (point-max))
;;   (setq selective-display nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time manipulation in log files.

;; (require 'parse-time)
;; (require 'time-date)

;; (defconst log-time-string-length 23)

;; (defun log-time-string-from-line-at (pos)
;;   "Extract the time string from the current line of the log file.
;;    Assumes the time string is the first two whitespace-separated words.
;;   "
;;   (let ((beg (save-excursion (goto-char pos) (line-beginning-position))))
;;     (buffer-substring beg (+ beg log-time-string-length))))


;; (defun millis-from-log-time-string (time-string)
;;   "Convert the final digits after the comma to a integer."
;;   (let ((pos (string-match "," time-string)))
;;     (if (null pos)
;;             (error "No millis in log time string")
;;       (string-to-int (substring time-string (+ pos 1))))))


;; (defun time-from-log-time-string (time-string)
;;   "Get the emacs-lisp time from a time string."
;;   ;; parse-time-string handles everything except millis. It ignores stuff
;;   ;; at end. It also has three extra items.
;;   (let* ((lst    (parse-time-string time-string))
;;          (t1       (apply 'encode-time (butlast lst 3)))
;;          (micros (* (millis-from-log-time-string time-string) 1000)))
;;     ;; We have a two-element time that is up to the seconds.  Add the
;;     ;; microseconds.
;;     (list (car t1) (cadr t1) micros)))


;; (defun secs-from-log-line-at (pos)
;;   (time-to-seconds (time-from-log-time-string
;;                     (log-time-string-from-line-at pos))))


;; (defun log-time-diff (beg end)
;;   (interactive "r") ; get the region
;;   (let ((s1 (secs-from-log-line-at beg))
;;             (s2 (secs-from-log-line-at (- end 1))))
;;     (message "%.3f seconds" (- s2 s1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq require-final-newline nil)

;; (setq inhibit-default-init t) ;; Don't load default.el because it says (setq require-final-newline 'query)



;; ;; Useful stuff from /usr/share/emacs/site-lisp/default.el:



;; (when window-system

;;   ;; enable wheelmouse support by default

;;   (mwheel-install)

;;   ;; make switching frames works properly under the default click-to-focus

;;   (setq focus-follows-mouse nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang.

;; (require 'erlang-start)

;; (setq erlang-mode-hook
;;       '(lambda ()
;;           (jba-setup-font-lock-mode)
;;           (define-key erlang-mode-map "\C-c\C-l" 'goto-line)
;;           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D.

;; (load-library "d-mode")

;; (setq d-mode-hook
;;       '(lambda ()
;;           (setq c-basic-offset 4)
;;           (push '(case-label . +) c-offsets-alist)
;;           (jba-setup-font-lock-mode)
;;           ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go.


;; (setq go-mode-hook
;;       '(lambda ()
;;              (jba-setup-font-lock-mode)
;;              ;(set-face-foreground font-lock-function-name-face "white")
;;              (set-face-bold-p font-lock-function-name-face nil)
;;              (setq tab-width 4)
;;              (global-set-key "\M-z" #'jba-compile)
;;              ;; Highlight function declarations, but not function calls.
;;              (setq go-mode-font-lock-keywords
;;                    (remove-if #'(lambda (x) (and (eq (car (last x)) 'font-lock-function-name-face) (not (string-match-p "func" (car x)))))
;;                                       go-mode-font-lock-keywords))
;;              ))

;; (defun jba-insert-and-indent (key) ;; gets rid of delay
;;   (interactive (list (this-command-keys)))
;;   (insert key)
;;   (indent-according-to-mode))


;; (add-to-list 'load-path "/home/jba/go/misc/emacs" t)

;; (require 'go-mode-load)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML Mode

;; (defvar jba-font-lock-code-face
;;   (make-face 'jba-font-lock-code-face))


;; (setq html-mode-hook
;;       '(lambda ()
;;              (define-key html-mode-map [backspace]
;;                'backward-delete-char-untabify)
;;              (define-key html-mode-map [?\C-c ?\C-/] 'jba-close-tag)
;;              (define-key html-mode-map "\C-j" 'newline-and-indent)
;;              (define-key html-mode-map "\C-c\C-j"
;;                '(lambda () (interactive) (insert "<BR>\n")))
;;              (define-key html-mode-map "\C-c\C-p"
;;                '(lambda () (interactive)
;;                   (if (not (at-beginning-of-line))
;;                           (insert "\n"))
;;                   (insert "<P>\n")))
;;              (define-key html-mode-map [?\C->] 'jba-make-tag)
;;              (define-key html-mode-map "\C-c\C-h"
;;                '(lambda () (interactive) (wrap-line-in-tags "<H1>" "</H1>")))
;;              (define-key html-mode-map "\C-c<"
;;                '(lambda () (interactive) (insert "&lt;")))
;;              (define-key html-mode-map "\C-c>"
;;                '(lambda () (interactive) (insert "&gt;")))
;;              (define-key html-mode-map "\C-c "
;;                '(lambda () (interactive) (insert "&nbsp;")))
;;              (define-key html-mode-map "\C-c\C-d" 'insert-dash)
;;              (define-key html-mode-map "\C-c\C-v"
;;                '(lambda () (interactive) (wrap-region-in-tags
;;                                           (region-beginning) (region-end)
;;                               "<BLOCKQUOTE><PRE>\n" "</PRE></BLOCKQUOTE>")))
;;              (define-key html-mode-map "\C-ct"
;;                '(lambda () (interactive) (tag-region-or-last-word "tt")))
;;              (define-key html-mode-map "\C-ci"
;;                '(lambda () (interactive) (tag-region-or-last-word "i")))
;;              (define-key html-mode-map "\C-c\C-i"
;;                '(lambda () (interactive) (tag-region-or-last-word "i")))
;;              (setq browse-url-browser-function 'browse-url-netscape6)
;;              (define-key html-mode-map "\C-cb"
;;                '(lambda () (interactive)
;;                   (browse-url-of-buffer (current-buffer))))
;;              (define-key html-mode-map "\C-c\C-s" 'jba-shell)
;;              (auto-fill-mode 1)
;; ;;           (font-lock-mode 1)
;; ;;           (set-face-foreground font-lock-string-face "black")
;; ;;           (set-face-foreground jba-font-lock-code-face "medium purple")
;; ;;           (setq font-lock-keywords
;; ;;                 (append
;; ;;                      '(("<%.*%>" . jba-font-lock-code-face)
;; ;;                        ("<%.*" . jba-font-lock-code-face))
;; ;;                      (cdr font-lock-keywords)))
;; ;;           (font-lock-fontify-buffer)
;;              ))



;; (setq sgml-mode-hook
;;       '(lambda ()
;;              (define-key sgml-mode-map [?\C->] 'jba-make-tag)
;;              (define-key sgml-mode-map [?\C-c ?\C-/] 'jba-close-tag)
;;              ))

;; (defun insert-dash ()
;;   (interactive)
;;   (insert "&#151;"))

;; (defun tag-region-or-last-word (tag)
;;   (let ((start-tag (format "<%s>" tag))
;;             (end-tag   (format "</%s>" tag)))
;;     (if mark-active
;;             (wrap-region-in-tags (region-beginning)
;;                                          (region-end)
;;                                          start-tag
;;                                          end-tag)
;;       (progn
;;             (wrap-region-in-tags (save-excursion
;;                                            (forward-word -1)
;;                                            (point))
;;                                          (point)
;;                                          start-tag
;;                                          end-tag)
;;             (forward-char (length end-tag))))))



;; (defun wrap-line-in-tags (pretag posttag)
;;   (interactive)
;;   (wrap-region-in-tags (save-excursion (beginning-of-line) (point))
;;                                (save-excursion (end-of-line) (point))
;;                                pretag
;;                                posttag))


;; (defun wrap-region-in-tags (beg end pretag posttag)
;;   (save-excursion
;;     (setq endm (make-marker))
;;     (set-marker endm end)
;;     (goto-char beg)
;;     (insert pretag)
;;     (goto-char endm)
;;     (insert posttag)))


;; (defun at-beginning-of-line ()
;;   (= (char-before) ?\n))


;; (defun jba-make-tag ()
;;   (interactive)
;;   (insert ">")
;;   (save-excursion
;;     (backward-word 1)
;;     (insert "<")))

;; (defun jba-close-tag ()
;;   (interactive)
;;   (let ((tag (last-unclosed-tag)))
;;     (insert "</")
;;     (insert tag)
;;     (insert ">")))

;; (defvar self-closing-tags
;;   '(
;;      "br" "li" "option" "input" "jsp:setProperty" "jsp:getProperty"
;;     "hr" "img" "p"
;;     ))

;; (defun last-unclosed-tag ()
;;   (save-excursion
;;     (let ((tag-stack '())
;;               (done nil)
;;               tag)
;;       (while (not done)
;;             (setq tag (previous-tag))
;;             (if (not (member (downcase tag) self-closing-tags))
;;                 (progn
;;                   (if (is-close-tag tag)
;;                           (push (substring tag 1) tag-stack)
;;                         ;; we have an open tag
;;                         (if (or (null tag-stack)
;;                                     (not (string= (upcase tag)
;;                                                       (upcase (car tag-stack))))
;;                             (setq done t)
;;                           ;; pop tag stack
;;                           (setq tag-stack (cdr tag-stack)))))))
;;       tag)))


;; (defun previous-tag ()
;;   (search-backward-regexp "<\\(/?[-A-Za-z0-9:]+\\)[ >]")
;;   (buffer-substring (match-beginning 1) (match-end 1)))

;; (defun is-close-tag (s)
;;   (= (string-to-char s) ?/))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; temporary, for re-doing the javadoc of com.livecluster.*


;; (defun change-javadoc-formats ()
;;   (interactive)
;;   (beginning-of-buffer)
;;   (while (re-search-forward "^[ \t]*\\*[ \t]*@property-name" nil t)
;;     (beginning-of-line)
;;     (insert " */\n/**\n")
;;     (forward-line 1))
;;   (beginning-of-buffer)
;;   (if (search-forward "@name " nil t)
;;       (replace-match ""))
;;   (if (search-forward "@description " nil t)
;;       (replace-match ""))
;;   (while (search-forward "@property-" nil t)
;;     (replace-match "@"))

;;   (beginning-of-buffer)
;;   (while (search-forward "@range" nil t)
;;     (beginning-of-line)
;;     (insert " * @category \n")
;;     (forward-line 1))

;;   (beginning-of-buffer)
;;   (while (search-forward "*/" nil t)
;;     (forward-line -1)
;;     (if (looking-at "[ \t]*\\*[ \t]*$")
;;             (kill-line 1))
;;     (forward-line 2))

;;   (beginning-of-buffer)
;;   (while (re-search-forward "@range[ \t]*$" nil t)
;;     (beginning-of-line)
;;     (kill-line 1))
;; )

;; (if (getenv "DISPLAY")
;;     (progn
;;       ;(set-default-font "10x20")
;;       )
;;   (set-default-font "fixedsys"))


;; (if (and (>= version 22) (not (getenv "DISPLAY")))
;;     (progn
;;       (set-face-foreground 'minibuffer-prompt "yellow")
;;       (set-face-foreground 'font-lock-function-name-face "yellow")
;;       ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer cycling -- obsoleted by cyclebuffer?

;; (defvar jba-buflist nil)

;; (defun switch-to-other-buffer ()
;;   (interactive)
;;   (switch-to-buffer (other-buffer))
;;   (setq jba-buflist (cdr (cdr (buffer-list)))))

;; (defun next-buffer ()
;;   "Cycles around all the buffers, starting from 'other' one.
;;    Do it right after a ctrl-L."
;;   (interactive)
;;   (if (null jba-buflist)
;;       (setq jba-buflist (buffer-list)))
;;   (while (not (is-user-buffer (car jba-buflist)))
;;       (setq jba-buflist (cdr jba-buflist)))
;;     (switch-to-buffer (car jba-buflist))
;;     (setq jba-buflist (cdr jba-buflist)))


;; (defun is-user-buffer (buffer)
;;   (not (eq (aref (buffer-name buffer) 0) ? )))

;; (global-set-key "\C-\M-l" 'switch-to-other-buffer)

;; ;; while my Alt-Ctl-L key is broken...

;; (global-set-key [?\C-\M-;] 'switch-to-other-buffer)

;; (global-set-key "\C-\M-m" 'next-buffer)


;; ;(defun switch-to-other-buffer ()
;; ;  (interactive)
;; ;  (switch-to-buffer (other-buffer)))

;; ;(global-set-key "\C-\M-l" 'switch-to-other-buffer)


;(push (cons "\\.h\\'"  'c++-mode) auto-mode-alist)
;(push (cons "\\.tk\\'"  'tcl-mode) auto-mode-alist)
;(push (cons "\\.jsp" 'html-mode) auto-mode-alist)
;(push (cons "\\.stk" 'scheme-mode) auto-mode-alist)
;(push (cons "\\.rb" 'ruby-mode) auto-mode-alist)
;(push (cons "\\.cs$" 'java-mode) auto-mode-alist) ; C#
;(push (cons "\\.d$" 'd-mode) auto-mode-alist)
;(push (cons "Makefile.*" 'makefile-mode) auto-mode-alist)

;(push (cons "python" 'python-mode) interpreter-mode-alist)
;(push (cons "python2.4" 'python-mode) interpreter-mode-alist)
;(push (cons "python2.5.2" 'python-mode) interpreter-mode-alist)
;(push (cons "escript" 'erlang-mode) interpreter-mode-alist)

;; (push ".class" completion-ignored-extensions)
;; (push ".java~" completion-ignored-extensions)
;; (push ".tcl~"  completion-ignored-extensions)
;; (push ".os"    completion-ignored-extensions)

;;; (setq completion-ignored-extensions
;       (remove ".log" completion-ignored-extensions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quick, simple bookmarks.
(require 'breadcrumb)

;; Don't save them.
(setq bc-bookmark-file nil)

(setq bc-bookmark-limit 100)

;; Don't set them on tag operations.
(setq bc-bookmark-hook-enabled nil)

(define-key global-map "\C-x\C-m" 'bc-set)

(defun jba-next-bookmark (p)
  (interactive "P")
  (if p
      (bc-local-next)
    (bc-next)))

(define-key global-map "\C-x\C-n" 'jba-next-bookmark)
(define-key global-map "\C-x\C-j" 'jba-next-bookmark)

(define-key global-map "\C-x\C-p"
  '(lambda (p)
     (interactive "P")
     (if p
	 (bc-local-previous)
       (bc-previous))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq borg-mode-hook
      '(lambda ()
	 (define-key c-mode-map "\C-c\C-l" 'goto-line)))

(g4d-aliases-to-env-vars)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hippie-expand.

;;; Source at https://github.com/emacs-mirror/emacs/blob/emacs-25/lisp/hippie-exp.el.

(setq hippie-expand-verbose t)

(global-set-key "\M-/" 'hippie-expand)
