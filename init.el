(require 'cl)
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	;; ("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defmacro define-key-tree (keymap &rest body)
  (labels ((rec (node)
		(let ((key (car node))
		      (rem (cdr node)))
		  (cond ((symbolp (caar rem))
			 (list (cons key (cadar rem))))
			(t (mapcar (lambda (def)
				     (cons (concat key (car def))
					   (cdr def)))
				   (mapcan #'rec rem)))))))
    `(progn ,@(mapcar (lambda (def)
			(let ((key (car def))
			      (fun (cdr def)))
			  `(define-key ,keymap ,key ',fun)))
		      (mapcan #'rec body)))))

(defmacro aif (expr then &optional else)
  `(let ((it ,expr))
     (if it ,then ,else)))

(defmacro awhen (expr &rest then)
  `(aif ,expr (progn ,@then)))

(defvar done-package-refresh-contentsp nil)

(defun refresh-and-package-install (package)
  (unless done-package-refresh-contentsp
    (setq done-package-refresh-contentsp t)
    (package-refresh-contents))
  (package-install package))

(defun require-or-install (package)
  (unless (package-installed-p package)
    (refresh-and-package-install package)))



(progn ;theme settings
  (require-or-install 'monokai-theme)
  (load-theme 'monokai t))

(progn ;native comp settings
  (setq native-comp-async-report-warnings-errors nil))

(progn ;backup files settings
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(progn ;parenthes settings
  (require-or-install 'rainbow-delimiters)
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1)))
  (electric-pair-mode 1))

(progn ;scroll settings
  (setq scroll-margin 10)
  (setq scroll-step 1))

(progn ;evil settings
  (require-or-install 'evil)
  (require-or-install 'evil-escape)
  (require-or-install 'evil-terminal-cursor-changer)
  (require-or-install 'undo-tree)
  (custom-set-variables '(evil-undo-system 'undo-tree)
			'(undo-tree-auto-save-history nil))
  (global-undo-tree-mode)
  (evil-mode 1)
  (evil-escape-mode 1)
  (setq evil-escape-delay 1)
  (unless (display-graphic-p)
    (etcc-on)
    (evil-terminal-cursor-changer-activate)
    (setq evil-motion-state-cursor 'box
	  evil-visual-state-cursor 'box
	  evil-normal-state-cursor 'box
	  evil-insert-state-cursor 'bar
	  evil-emacs-state-cursor 'hbar))
  (define-key input-decode-map "2~" [SpacE])
  (evil-define-key 'normal compilation-mode-map "q" 'quit-window)
  (define-key-tree
    evil-normal-state-map
    ("j" 'evil-next-visual-line)
    ("k" 'evil-previous-visual-line)
    (" "
     ("b" ; buffer
      ("l" 'next-buffer)
      ("h" 'previous-buffer)
      ("d" 'kill-buffer)
      ("o" 'ibuffer))
     ("o" ; online judge
      ("d" 'online-judge-download)
      ("t" 'online-judge-test)
      ("s" 'online-judge-submit))
     ("'" 'multi-vterm)
     (";" 'eval-expression)
     ("w" ; window
      ("h" 'evil-window-left)
      ("j" 'evil-window-down)
      ("k" 'evil-window-up)
      ("l" 'evil-window-right)
      ("d" 'delete-window)
      ("D" 'delete-other-windows)
      ("s" 'split-window-horizontally)
      ("v" 'split-window-vertically)
      ("<" 'evil-window-decrease-width)
      (">" 'evil-window-increase-width)
      ("-" 'evil-window-decrease-height)
      ("+" 'evil-window-increase-height)
      ("H" 'evil-window-move-far-left)
      ("L" 'evil-window-move-far-right)
      ("K" 'evil-window-move-very-top)
      ("J" 'evil-window-move-very-bottom)
      ("f" 'toggle-frame-maximized))
     ("t" ; tab
      ("l" 'tab-bar-switch-to-next-tab)
      ("h" 'tab-bar-switch-to-prev-tab)
      ("L" 'tab-bar-move-tab)
      ("H" 'tab-bar-move-tab-backward)
      ("d" 'tab-bar-close-tab)
      ("n" 'tab-bar-new-tab))
     ("r" ; tab-line
      ("l" 'tab-line-switch-to-next-tab)
      ("h" 'tab-line-switch-to-prev-tab))
     ("f" ; file
      ("t" 'neotree))
     (" " 'execute-extended-command))
    (";" 'evil-ex)))


(progn ;etc settings
  (require-or-install 'xclip)
  (xclip-mode 1)
  (column-number-mode 1)
  (global-display-line-numbers-mode 1) ;show line number on left
  (setq gc-cons-threshold 12800000)
  (setq inhibit-startup-message t)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (setq backup-directory-alist '((".*" . "~/.emacs_auto/backup")))
  (setq auto-save-file-name-transforms '((".*" "~/.emacs_auto/tmp" t)))
  (setq create-lockfiles nil)
  (add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
  (setq ring-bell-function 'ignore) ; no bell!
  (put 'erase-buffer 'disabled nil))

(progn ;terminal settings
  (require-or-install 'vterm)
  (require-or-install 'multi-vterm)
  (setq-default truncate-lines nil)
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (evil-define-key 'normal vterm-mode-map "p"
		(lambda ()
		  (interactive)
		  (vterm-end-of-line)
		  (vterm-yank)))
	      (define-key vterm-mode-map "\C-c\C-d" 'vterm-send-C-d)
	      (define-key vterm-mode-map "\C-c\C-a" 'vterm-send-C-a))))

(progn ; projectile settings
  (require-or-install 'projectile)
  (projectile-mode 1))

(progn ;lsp settings
  (require-or-install 'lsp-mode)
  (require-or-install 'lsp-ui)
  (require-or-install 'lsp-metals)
  (require-or-install 'flycheck)
  (require-or-install 'yasnippet)
  (setq lsp-ui-sideline-show-hover t) ; show document in hover
  (add-hook 'lsp-mode-hook
	    (lambda ()
	      (lsp-ui-mode 1)
	      (yas-minor-mode 1)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("d" 'lsp-describe-thing-at-point)))
		("g"
		 ("d" 'xref-find-definitions)))
	      (setq lsp-prefer-capf t)
	      (setq read-process-output-max (* 1024 1024))
	      (setq lsp-idle-delay 0.1)))
  (add-hook 'lsp-ui-mode-hook
	    (lambda ()
	      (setq lsp-ui-doc-show-with-cursor t)))
  (add-hook 'lsp-treemacs-generic-mode-hook
	    (lambda ()
	      (evil-define-key 'normal lsp-treemacs-generic-map (kbd "TAB") 'treemacs-TAB-action)))
  (add-hook 'lsp-after-apply-edits-hook
	    (lambda (operation)
	      (when (eq operation 'rename)
		(save-buffer)))))

(progn ;company settings
  (require-or-install 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (add-hook 'company-mode-hook
	    (lambda ()
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous))))

(progn ;rust settings
  (require-or-install 'rust-mode)
  (require-or-install 'cargo)
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin")) ;path to rust analyzer
  (add-to-list 'auto-mode-alist '("\\.rs$'" . rust-mode))
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (setq lsp-rust-server 'rust-analyzer)
	      (lsp 1)
	      (cargo-minor-mode 1)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (setq rust-format-on-save t)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("c" ;cargo
		  ("r" 'cargo-process-run)
		  ("t" 'cargo-process-test)
		  ("c" 'cargo-process-check)
		  ("a" 'cargo-process-add)))))))

(progn ;c++ settings
  (add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
		      :major-modes '(c++-mode)
		      :remote? t
		      :server-id 'clangd-remote)))
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (lsp)
	      (setq tab-width 4
		    c-basic-offset 4
		    indent-tabs-mode nil
		    lsp-enable-indentation nil))))

(progn ;cmake settings
  (require-or-install 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode)))

(progn ;C# settings
  (require-or-install 'csharp-mode)
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
  (add-hook 'csharp-mode-hook
	    (lambda ()
	      (lsp)
	      (add-hook 'before-save-hook
			'lsp-format-buffer)
	      (setq tab-width 4
		    c-basic-offset 4
		    indent-tabs-mode nil))))

(progn ;paredit settings
  (require-or-install 'paredit)
  (add-hook 'paredit-mode-hook
	    (lambda ()
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("h" 'paredit-backward-slurp-sexp)
		  ("l" 'paredit-forward-slurp-sexp)
		  ("H" 'paredit-backward-barf-sexp)
		  ("L" 'paredit-forward-barf-sexp)
		  ("k" 'paredit-splice-sexp)
		  ("j" 'paredit-wrap-sexp)))))))

(progn ;common-lisp settings
  (require-or-install 'slime)
  (require-or-install 'slime-company)
  (require-or-install 'paredit)
  (setq slime-company-completion 'fuzzy)

  (defun slime-qlot-exec (directory)
    (interactive (list (read-directory-name "Project directory: ")))
    (slime-start :program "qlot"
		 :program-args '("exec" "ros" "-S" "." "run")
		 :directory directory
		 :name 'qlot
		 :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

  (add-hook 'sldb-mode-hook
	    (lambda ()
	      (evil-define-key 'normal sldb-mode-map (kbd "RET") 'sldb-default-action)
	      (evil-define-key 'normal sldb-mode-map "q" 'sldb-quit)
	      (evil-define-key 'normal sldb-mode-map "v" 'sldb-show-source)))

  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (load (expand-file-name "~/.roswell/helper.el"))
	      (slime-mode 1)
	      (paredit-mode 1)
	      (define-key evil-normal-state-map "gd" 'slime-edit-definition)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("e"
		   ("d" 'slime-eval-defun)
		   ("r" 'slime-eval-region)
		   ("b" 'slime-eval-buffer))
		  ("c"
		   ("d" 'slime-compile-defun)
		   ("r" 'slime-compile-region)
		   ("f" 'slime-compile-file))))))
	    (add-hook 'slime-mode-hook
		      (lambda ()
			(require 'slime-autoloads)
			(setq slime-default-lisp 'sbcl)
			(setq slime-lisp-implementations
			      '((sbcl ("ros"
				       "-e" ; ignore shebang
				       "(set-dispatch-macro-character
 #\\# #\\! 
(lambda (stream character n) (declare (ignore character n)) (read-line stream nil nil t) nil))"
				       "run") :coding-system utf-8-unix)))
					;(load (expand-file-name "~/.roswell/helper.el"))
			(slime-setup '(slime-fancy slime-company slime-banner slime-repl))))))


(progn ;clojure settings
  (require-or-install 'clojure-mode)
  (require-or-install 'cider)
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (paredit-mode 1)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("'" 'cider-jack-in)
		  ("e"
		   ("d" 'cider-eval-defun-at-point)
		   ("b" 'cider-eval-buffer))
		  ("p"
		   ("b" 'cider-load-buffer))
		  ("t"
		   ("l" 'cider-test-run-loaded-tests))))))))

(progn ;emacs-lisp settings
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode 1)
	      (rainbow-delimiters-mode 1))))

 (progn ;markdown settings
   (require-or-install 'flycheck)
   (require-or-install 'add-node-modules-path)
   (require-or-install 'markdown-mode)
   (require-or-install 'markdown-preview-mode)
   (require-or-install 'websocket)
   (require-or-install 'web-server)
   (require-or-install 'uuidgen)
   (flycheck-define-checker textlint
     "A linter for prose."
     :command ("npx" "textlint" "--format" "unix" source-inplace)
     :error-patterns
     ((warning line-start (file-name) ":" line ":" column ": "
 	      (id (one-or-more (not (any " "))))
 	      (message (one-or-more not-newline)
 		       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
 	      line-end))
     :modes (text-mode markdown-mode))
   (add-to-list 'flycheck-checkers 'textlint)
   (add-hook 'markdown-mode-hook (lambda ()
 				  (setq markdown-command "multimarkdown")
 				  (flycheck-mode)
 				  (add-node-modules-path))))

(progn ;tab-bar settings
  (tab-bar-mode 1)
  (mapcar
   (lambda (n)
     (define-key evil-normal-state-map (format " t%d" n) `(lambda () (interactive) (tab-bar-select-tab ,n))))
   (list 1 2 3 4 5 6 7 8 9))
  (custom-set-variables
   '(tab-bar-tab-hints 1)
   '(tab-bar-tab-name-function 'tab-bar-tab-name-truncated)))

(progn ;scheme settings
  (modify-coding-system-alist 'process' "gosh" '(utf-8 . utf-8))
  (setq scheme-program-name "gosh -i")
  (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
  (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
  (add-hook 'scheme-mode-hook
	    (lambda ()
	      (defun scheme-send-buffer ()
		(interactive)
		(let ((pos (count-lines 1 (point)))
		      (start nil)
		      (end nil))
		  (evil-goto-first-line)
		  (setq start (point))
		  (evil-goto-line)
		  (setq end (point))
		  (scheme-send-region start end)
		  (evil-goto-line pos)))
	      (paredit-mode 1)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("e"
		   ("d" 'scheme-send-definition)
		   ("b" 'scheme-send-buffer))))
		("\C-c"
		 ("s" 'scheme-other-window)))
	      (define-key-tree
		evil-visual-state-map
		(" "
		 ("l"
		  ("e"
		   ("r" 'scheme-send-region)))))))

  (defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*")) 
    (run-scheme scheme-program-name)
    (other-window 1))
  )

(progn ;haskell settings
  (require-or-install 'haskell-mode)
  (require-or-install 'lsp-haskell)
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cable$" . haskell-mode))
  (add-hook 'haskell-mode-hook
	    (lambda ()
	      (lsp 1))))

(progn ;matlab settings
  (require-or-install 'ein)
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list
   'auto-mode-alist
   '("\\.m$" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

(progn ;git settings
  (require-or-install 'magit)
  (setenv "GIT_EDITOR" "emacs")
  (add-hook 'shell-mode-hook 'with-editor-export-git-editor)
  (define-key evil-normal-state-map " g" 'magit-status))

(progn ;sql settings
  (add-hook 'sql-mode-hook
	    (lambda ()
	      (setq tab-width 4
		    indent-tabs-mode nil))))

(progn ;neotree settings
  (require-or-install 'neotree)
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "v") 'neotree-quick-look)
	      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
	      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	      (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
	      (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

(progn ;golang settings ;https://qiita.com/kod314/items/2232d480411c5c2ab002
  (require-or-install 'go-mode)
  (require-or-install 'company-go)
  (add-to-list 'exec-path (expand-file-name "/usr/local/go/bin/"))
  (add-to-list 'exec-path (expand-file-name "/home/endered/go/bin/"))
  (add-hook 'go-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'lsp-format-buffer)
			    (setq indent-tabs-mode nil)
			    (setq c-basic-offset 4)
			    (setq tab-width 4)
			    (setq lsp-enable-snippet nil)
			    (lsp 1)
			    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
			    (setq completion-ignore-case t)
			    (setq company-dabbrev-downcase nil)
			    (define-key company-active-map (kbd "C-n") 'company-select-next)
			    (define-key company-active-map (kbd "C-p") 'company-select-previous))))

(progn ;typescript
  (require-or-install 'typescript-mode)
  (add-hook 'typescript-mode-hook
	    (lambda ()
	      (lsp)
	      (lsp-deferred)
	      (setq tab-width 2
		    c-basic-offset 2
		    js-indent-level 2
		    typescript-indent-level 2
		    indent-tabs-mode nil)
	      (add-hook 'before-save-hook
			'lsp-format-buffer))))


(progn ;elm settings
  (require-or-install 'elm-mode)
  (add-hook 'elm-mode-hook
	    (lambda ()
	      (lsp))))


(progn ;makefile settings
  (add-hook 'makefile-mode-hook (lambda ()
				  (setq c-basic-offset 4)
				  (setq tab-width 4))))


(progn ;scala settings
  (require-or-install 'scala-mode)
  (require-or-install 'lsp-metals)
  (add-hook 'scala-mode-hook
	  (lambda ()
	    ;; (add-hook 'before-save-hook 'lsp-format-buffer)
	    (lsp 1)
	    (lsp-lens-mode 1)
	    (setq lsp-prefer-flymake nil)
	    (setq lsp-completion-provider :capf)))

  )

(progn ;java settings
  (require-or-install 'lsp-java)
  (add-hook 'java-mode-hook
	    (lambda ()
	      (setq c-basic-offset 4)
	      (setq tab-width 4)
	      (setq indent-tabs-mode nil)
	      (lsp 1))))

(progn ; js mode
  (add-hook 'js-mode-hook
	    (lambda ()
	      (lsp 1)
	      (setq tab-width 2
		    c-basic-offset 2
		    js-indent-level 2
		    indent-tabs-mode nil)))
  )

(progn ;jupyter node book settings
  (require-or-install 'ein)
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  )

(progn
  (require-or-install 'nix-mode)
  )

(progn ;; mode-line settings
  (require-or-install 'doom-modeline)
  (doom-modeline-mode 1))

(progn ;; customize
  (setq custom-file "~/.emacs.d/custom.el")
  (if (file-exists-p custom-file)
      (load custom-file)))


(setq evil-normal-state-modes
      (append evil-emacs-state-modes
              evil-insert-state-modes
              evil-normal-state-modes
              evil-motion-state-modes))

(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)
