(require 'cl)
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	;; ("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-install-upgrade-built-in t)

(package-initialize)

(defmacro apply-define-key-tree (op &rest body)
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
			  `(,@op ,key ',fun)))
		      (mapcan #'rec body)))))

(defmacro define-key-tree (keymap &rest body)
  `(apply-define-key-tree (define-key ,keymap) ,@body))

(defmacro evil-define-key-tree (state keymap &rest body)
  `(apply-define-key-tree (evil-define-key ,state ,keymap) ,@body))

(defmacro evil-local-set-key-tree (state &rest body)
  `(apply-define-key-tree (evil-local-set-key ,state) ,@body))

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
  (install-if-not-exists package)
  (require package))

(defun install-if-not-exists (package)
  (unless (package-installed-p package)
    (refresh-and-package-install package)))

(defun only-install (package)
  (unless (package-installed-p package)
    (refresh-and-package-install package)))


(progn ; my package manager
  (defvar my/managed-git-packages nil)
  (defvar my/git-root "~/.emacs.d/gits")
  (unless (file-exists-p my/git-root)
    (mkdir my/git-root))

  (defun git-package-path (name)
    (format "%s/%s/" my/git-root name))

  (defun my/ensure-git-package-exist (name path url)
    (unless (file-exists-p path)
      (let ((default-directory my/git-root))
	(shell-command (format "git clone %s %s" url name)))
      t))

  (defun my/register-git-package (name url &optional install)
    (let* ((install-path (git-package-path name))
	   (installed (my/ensure-git-package-exist name install-path url)))
      (setf (alist-get name my/managed-git-packages) (list url install))
      (when (and installed install)
	(package-install-file install-path))
      install-path))

  (defun my/update-git-packages ()
    (interactive)
    (pcase-dolist (`(,name ,url ,install) my/managed-git-packages)
      (when install
	(let ((default-directory (git-package-path name)))
	  (shell-command "git pull --rebase")
	  (package-install-file default-directory))))))

(progn ;; theme settings
  (install-if-not-exists 'monokai-theme)
  (load-theme 'monokai t))

(progn ;native comp settings
  (setq native-comp-async-report-warnings-errors nil))

(progn ;backup files settings
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(progn ;parenthes settings
  (install-if-not-exists 'rainbow-delimiters)
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode 1)))
  (electric-pair-mode 1))

(progn ;scroll settings
  (setq scroll-margin 10)
  (setq scroll-step 1))

(progn ;evil settings
  (install-if-not-exists 'evil)
  (install-if-not-exists 'evil-escape)
  (install-if-not-exists 'evil-terminal-cursor-changer)
  (install-if-not-exists 'undo-tree)
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
  (defun my/revert-buffer ()
    (interactive)
    (revert-buffer t t))
  (define-key evil-normal-state-map (kbd "\C-f") (lambda () (interactive) (evil-next-line 10)))
  (define-key evil-normal-state-map (kbd "\C-b") (lambda () (interactive) (evil-previous-line 10)))
  (define-key-tree
   evil-normal-state-map
   ("j" 'evil-next-visual-line)
   ("k" 'evil-previous-visual-line)
   (" "
    ("b"				; buffer
     ("l" 'next-buffer)
     ("h" 'previous-buffer)
     ("r" 'my/revert-buffer)
     ("d" 'kill-buffer)
     ("o" 'ibuffer))
    ("o"				; online judge
     ("d" 'online-judge-download)
     ("t" 'online-judge-test)
     ("s" 'online-judge-submit))
    ("'" 'multi-vterm)
    (";" 'eval-expression)
    ("w"				; window
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
    ("t"				; tab
     ("l" 'tab-bar-switch-to-next-tab)
     ("h" 'tab-bar-switch-to-prev-tab)
     ("L" 'tab-bar-move-tab)
     ("H" 'tab-bar-move-tab-backward)
     ("d" 'tab-bar-close-tab)
     ("n" 'tab-bar-new-tab))
    ("r"				; tab-line
     ("l" 'tab-line-switch-to-next-tab)
     ("h" 'tab-line-switch-to-prev-tab))
    (" " 'execute-extended-command)
    ("c"				; compile
     ("m" 'compile)			;make
     )
    ("g"				; go (move)
     ("r" 'xref-find-references))
    ("s"				; settings
     ("c" 'toggle-enable-clipboard)))
   (";" 'evil-ex)))


(progn ;etc settings
  (column-number-mode 1)
  (global-display-line-numbers-mode 1) ;show line number on left
  (scroll-bar-mode -1) ;disable scroll bar
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

(progn ;clipboard settings
  (install-if-not-exists 'xclip)
  (xclip-mode 1)
  (setq select-enable-clipboard nil)
  (defun toggle-enable-clipboard ()
    (interactive)
    (let ((current select-enable-clipboard))
      (setq select-enable-clipboard (not current))
      (if current
	  (message "Clipboard was disabled")
	(message "Clipboard was enabled")))))

(progn ;terminal settings
  (install-if-not-exists 'vterm)
  (install-if-not-exists 'multi-vterm)
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
  (install-if-not-exists 'projectile)
  (projectile-mode 1))

(progn ;; eglot
  (require 'flymake)
  (set-face-attribute 'flymake-error nil :underline `(:color "red"))
  (set-face-attribute 'flymake-warning nil :underline `(:color "yellow"))
  (my/register-git-package 'eglot-booster "https://github.com/jdtsmith/eglot-booster" t)
  (with-eval-after-load 'eglot
    (require 'eglot-booster nil t)
    (add-to-list 'save-some-buffers-default-predicate 'save-some-buffers-root)
    (defun my/eglot-format-buffer ()
      (interactive)
      (save-buffer)
      (eglot-format-buffer)
      (save-buffer))
    (defun my/eglot-code-action-organize-imports ()
      (interactive)
      (save-buffer)
      (eglot-code-action-organize-imports (point))
      (save-buffer))
    (defun my/eglot-rename (newname)
      (interactive
       (list (read-from-minibuffer
	      (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
					     "unknown symbol"))
	      nil nil nil nil
	      (symbol-name (symbol-at-point)))))
      (save-some-buffers t) ;; save all file even if not need to save for rename
      (eglot-rename newname)
      (save-some-buffers t))
    (evil-define-key-tree
     'normal
     eglot-mode-map
     (" "
      ("l"
       ("f" 'my/eglot-format-buffer)
       ("r" 'my/eglot-rename)
       ("v" 'eldoc-doc-buffer)
       ("o" 'my/eglot-code-action-organize-imports)
       ("a" 'eglot-code-actions)
       ("p" 'flymake-show-project-diagnostics))))
    (eglot-booster-mode))
  (define-key-tree
   evil-normal-state-map
   (" "
    ("m" ;; mode
     ("l" 'eglot)))))

(progn ;; lsp-mode settings
  (install-if-not-exists 'lsp-mode)
  (install-if-not-exists 'lsp-ui)
  (install-if-not-exists 'yasnippet)
  (defun my/start-lsp-with-log ()
    "Command for workaround at TypeScript Language Server's bug with Tramp"
    (interactive)
    (setq lsp-log-io t)
    (lsp)
    (setq lsp-log-io nil)) 
  
  (with-eval-after-load 'lsp-mode
    (yas-global-mode 1)
    (setq lsp-eldoc-render-all t)
    (setq lsp-inlay-hint-enable t)
    (setq lsp-prefer-flymake nil)
    (setq lsp-keep-workspace-alive nil)
    (add-hook 'lsp-after-apply-edits-hook
	      (lambda (operation)
		(when (or (eq operation 'rename) (eq operation 'format))
		  (save-buffer))))
    (defun my/lsp-format-buffer ()
      (interactive)
      (lsp-format-buffer)
      (save-buffer))
    ;; below hook is workaround for enable lsp-mode-map
    ;; without workaround, we must dive into evil insert state
    (add-hook 'lsp-mode-hook (lambda () (evil-insert-state) (evil-normal-state)))
    (evil-define-key* 'normal lsp-mode-map
      " lf" 'my/lsp-format-buffer
      " lr" 'lsp-rename
      " lv" 'eldoc-doc-buffer
      " lo" 'lsp-organize-imports
      " la" 'lsp-execute-code-action
      " gr" 'lsp-find-references
      " gi" 'lsp-find-implementation))
  (with-eval-after-load 'lsp-ui
    (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-mouse nil))
  (define-key-tree
   evil-normal-state-map
   (" "
    ("m" ;; mode
     ("L" 'lsp)))))

(progn					;company settings
  (install-if-not-exists 'company)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))
  (define-key evil-normal-state-map " mc" 'global-company-mode))

(progn ;rust settings
  (install-if-not-exists 'rust-mode)
  (install-if-not-exists 'cargo)
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin")) ;path to rust analyzer
  (add-to-list 'auto-mode-alist '("\\.rs$'" . rust-mode))
  (defun my/find-rust-project-root (dir)
    (when-let ((root (locate-dominating-file dir "Cargo.toml")))
      (list 'vc 'Git root)))
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (cargo-minor-mode 1)
	      (setq-local project-find-functions (list #'my/find-rust-project-root))
	      (define-key-tree
	       evil-normal-state-map
	       (" "
		("c"			;cargo
		 ("r" 'cargo-process-run)
		 ("t" 'cargo-process-test)
		 ("c" 'cargo-process-check)
		 ("a" 'cargo-process-add)))))))

(progn ;c++ settings
  (add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (setq tab-width 4
		    c-basic-offset 4
		    indent-tabs-mode nil))))

(progn ;cmake settings
  (install-if-not-exists 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode)))

(progn ;paredit settings
  (install-if-not-exists 'paredit)
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
  (install-if-not-exists 'slime)
  (install-if-not-exists 'company)
  (install-if-not-exists 'slime-company)
  (install-if-not-exists 'paredit)

  (define-key evil-normal-state-map " ms" 'slime-mode)

  (defun slime-qlot-exec (directory)
    (interactive (list (read-directory-name "Project directory: ")))
    (slime-start :program "qlot"
		 :program-args '("exec" "sbcl")
		 :directory directory
		 :name 'qlot
		 :env (list (concat "PATH=" (mapconcat 'identity exec-path ":")))))

  (add-hook 'sldb-mode-hook
	    (lambda ()
	      (evil-define-key 'normal sldb-mode-map (kbd "RET") 'sldb-default-action)
	      (evil-define-key 'normal sldb-mode-map "q" 'sldb-quit)
	      (evil-define-key 'normal sldb-mode-map "v" 'sldb-show-source)))

  (add-hook 'slime-mode-hook
	    (lambda ()
	      (require 'slime-autoloads)
	      (setq slime-company-completion 'fuzzy)
	      (slime-setup '(slime-fancy slime-company slime-repl))
	      (evil-define-key 'normal lisp-mode-map "gd" 'slime-edit-definition)))

  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (paredit-mode 1)
	      (setq inferior-lisp-program "sbcl")
	      (evil-define-key-tree
	       'normal
	       lisp-mode-map
	       (" "
		("l"
		 ("e"
		  ("d" 'slime-eval-defun)
		  ("r" 'slime-eval-region)
		  ("b" 'slime-eval-buffer))
		 ("c"
		  ("d" 'slime-compile-defun)
		  ("r" 'slime-compile-region)
		  ("f" 'slime-compile-file))))))))


(progn ;clojure settings
  (install-if-not-exists 'clojure-mode)
  (install-if-not-exists 'cider)
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
   (install-if-not-exists 'flycheck)
   (install-if-not-exists 'markdown-mode)
   (install-if-not-exists 'markdown-preview-mode)
   (install-if-not-exists 'websocket)
   (install-if-not-exists 'web-server)
   (install-if-not-exists 'uuidgen)
   (with-eval-after-load 'markdown-mode
     (evil-define-key 'normal gfm-view-mode-map "q" 'delete-window))
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
   (add-to-list 'flycheck-checkers 'textlint))

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
  (install-if-not-exists 'haskell-mode)
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cable$" . haskell-mode)))

(progn ;matlab settings
  (install-if-not-exists 'ein)
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list
   'auto-mode-alist
   '("\\.m$" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

(progn ;git settings
  (install-if-not-exists 'magit)
  (setenv "GIT_EDITOR" "emacs")
  (add-hook 'shell-mode-hook 'with-editor-export-git-editor)
  (define-key-tree
   evil-normal-state-map
   (" "
    ("m" ;mode
     ("g" 'magit-status)))))

(progn ;sql settings
  (add-hook 'sql-mode-hook
	    (lambda ()
	      (setq tab-width 4
		    indent-tabs-mode nil))))

(progn ;golang settings ;https://qiita.com/kod314/items/2232d480411c5c2ab002
  (install-if-not-exists 'go-mode)
  (install-if-not-exists 'company-go)
  (add-to-list 'exec-path (expand-file-name "/usr/local/go/bin/"))
  (add-to-list 'exec-path (expand-file-name "/home/endered/go/bin/"))
  (add-hook 'go-mode-hook (lambda ()
			    (setq indent-tabs-mode nil)
			    (setq c-basic-offset 4)
			    (setq tab-width 4)
			    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
			    (setq completion-ignore-case t)
			    (setq company-dabbrev-downcase nil)
			    (define-key company-active-map (kbd "C-n") 'company-select-next)
			    (define-key company-active-map (kbd "C-p") 'company-select-previous))))

(progn ;typescript
  (install-if-not-exists 'typescript-mode)
  (add-hook 'typescript-mode-hook
	    (lambda ()
	      (setq tab-width 2
		    c-basic-offset 2
		    js-indent-level 2
		    typescript-indent-level 2
		    indent-tabs-mode nil)
	      (add-hook 'before-save-hook))))


(progn ;elm settings
  (install-if-not-exists 'elm-mode))


(progn ;makefile settings
  (add-hook 'makefile-mode-hook (lambda ()
				  (setq c-basic-offset 4)
				  (setq tab-width 4))))


(progn ;scala settings
  (install-if-not-exists 'scala-mode)
  (install-if-not-exists 'lsp-metals)
  (add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))

  (defun my/lsp-metals-use-remote ()
    "Re-register lsp-metals client for remote developing.
  Since, lsp-metals package strongly depend to lsp client id `metals`. To use lsp-metals on tramp needs recycling client id."
    (interactive)
    (require 'lsp-metals)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection 'lsp-metals--server-command)
		      :major-modes '(scala-mode scala-ts-mode)
		      :priority 1
		      :multi-root lsp-metals-multi-root
		      :initialization-options '((decorationProvider . t)
						(inlineDecorationProvider . t)
						(didFocusProvider . t)
						(executeClientCommandProvider . t)
						(doctorProvider . "html")
						(statusBarProvider . "on")
						(debuggingProvider . t)
						(treeViewProvider . t)
						(quickPickProvider . t)
						(inputBoxProvider . t)
						(commandInHtmlFormat . "vscode"))
		      :notification-handlers (ht ("metals/executeClientCommand" #'lsp-metals--execute-client-command)
						 ("metals/publishDecorations" #'lsp-metals--publish-decorations)
						 ("metals/treeViewDidChange" #'lsp-metals-treeview--did-change)
						 ("metals-model-refresh" #'lsp-metals--model-refresh)
						 ("metals/status" #'lsp-metals--status-string))
		      :request-handlers (ht ("metals/quickPick" #'lsp-metals--quick-pick)
					    ("metals/inputBox" #'lsp-metals--input-box))
		      :action-handlers (ht ("metals-debug-session-start" (-partial #'lsp-metals--debug-start :json-false))
					   ("metals-run-session-start" (-partial #'lsp-metals--debug-start t)))
		      :server-id 'metals
		      :remote? t
		      :initialized-fn (lambda (workspace)
					(with-lsp-workspace workspace
					  (lsp--set-configuration
					   (lsp-configuration-section "metals"))))
		      :after-open-fn (lambda ()
				       (add-hook 'lsp-on-idle-hook #'lsp-metals--did-focus nil t))
		      :completion-in-comments? t
		      :download-server-fn #'lsp-metals--download-server)))

  (defun my/find-scala-project-root (dir)
    (when-let ((root (locate-dominating-file dir "build.sbt")))
      (list 'vc 'Git root)))
  (add-hook 'scala-mode-hook
	    (lambda ()
	      (setq-local project-find-functions (list #'my/find-scala-project-root))))
  (with-eval-after-load 'lsp-metals
    (setf lsp-metals-inlay-hints-enable-type-parameters t)
    (setf lsp-metals-inlay-hints-enable-inferred-types t)
    (setf lsp-metals-server-args '("-Dmetals.client=emacs"))
    (evil-define-key* 'normal lsp-mode-map " gs" 'lsp-metals-goto-super-method))
  (with-eval-after-load 'eglot
    (with-eval-after-load 'scala-mode
      (setq-default eglot-workspace-configuration '(:metals (:inlayHints (
									  :inferredTypes (:enable t)
									  :implicitArguments (:enable t)
									  :typeParameters (:enable t)))))
      (setf (alist-get 'scala-mode eglot-server-programs) '("metals" "-Dmetals.client=emacs")))))

(progn ;java settings
  (add-hook 'java-mode-hook
	    (lambda ()
	      (setq c-basic-offset 4)
	      (setq tab-width 4)
	      (setq indent-tabs-mode nil))))

(progn ; js mode
  (add-hook 'js-mode-hook
	    (lambda ()
	      (setq tab-width 2
		    c-basic-offset 2
		    js-indent-level 2
		    indent-tabs-mode nil)))
  )

(progn ;; typescript
  (install-if-not-exists 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
  (with-eval-after-load 'web-mode
    (setq web-mode-code-indent-offset 2)
    (with-eval-after-load 'lsp-mode
      (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset)))
  (add-hook
   'web-mode-hook
   (lambda ()
     (indent-tabs-mode -1)
     ))
  )

(progn					;jupyter node book settings
  (install-if-not-exists 'ein)
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  )

(progn
  (install-if-not-exists 'nix-mode)
  )

(progn ;; mode-line settings
  (install-if-not-exists 'doom-modeline)
  (add-to-list
   'mode-line-misc-info
   `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-buffer-file-name-style 'file-name)
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

(progn ;; SATySFi settings
  (let ((p (my/register-git-package 'satysfi "https://github.com/gfngfn/satysfi.el")))
    (add-to-list 'load-path p)
    (require 'satysfi))
  (setq satysfi-command "satysfi")
  (setq satysfi-pdf-viewer-command "zathura --fork")
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 '(satysfi-mode . ("satysfi-language-server"))))
  (evil-define-key-tree
   'normal
   satysfi-mode-map
   (" "
    ("s"; satysfi
     ("t" 'satysfi-mode/typeset)
     ("o" 'satysfi-mode/open-pdf)))))


(progn ; tmux
  (defun refresh-environment-variable ()
    (interactive)
    (when (getenv "TMUX")
      (let ((display (string-trim (shell-command-to-string "tmux show-environment | grep '^DISPLAY' | sed -e 's/^.*=//'"))))
	(unless (string-equal display "")
	  (setenv "DISPLAY" display))))))

(progn ; compile
  (add-hook
   'compilation-start-hook
   (lambda (proc)
     (switch-to-buffer-other-frame "*compilation*"))))

(progn ; LaTeX
  (require 'tex-mode)
  (add-hook
   'tex-mode-hook
   (lambda ()
     (evil-local-set-key-tree
      'normal
      (" "
       ("l"
	("c" 'compile)
	("r" 'recompile)))))))

(progn ; graphviz
  (install-if-not-exists 'graphviz-dot-mode))


(progn ; treemacs
  (install-if-not-exists 'treemacs)
  (add-hook 'treemacs-mode-hook
	    (lambda ()
	      (evil-define-key* 'normal treemacs-mode-map
		(kbd "TAB") 'treemacs-TAB-action
		(kbd "RET") 'treemacs-RET-action
		(kbd "M-RET") 'treemacs-visit-node-close-treemacs
		"=" 'treemacs-extra-wide-toggle
		"q" 'treemacs-quit
		"c" 'treemacs-create-file
		"C" 'treemacs-create-dir
		"d" 'treemacs-delete-file
		"r" 'treemacs-rename-file
		"p" 'treemacs-add-project-to-workspace
		"H" 'treemacs-root-up
		"L" 'treemacs-root-down
		"R" 'treemacs-refresh))))

(progn ; neotree
  (install-if-not-exists 'neotree)
  (defun my/open-neotree-on-project-root ()
    (interactive)
    (let ((dir (projectile-project-root)))
      (when dir
	(neotree-dir dir))))
  (defun my/neotree-refresh ()
    (interactive)
    (let ((remote-file-name-inhibit-cache t))
      (neotree-refresh)))
  (define-key-tree
   evil-normal-state-map
   (" "
    ("f"
     ("t" 'neotree)
     ("T" 'my/open-neotree-on-project-root)
     ("f" 'neotree-find))))
  (with-eval-after-load 'neotree
    (evil-define-key* 'normal neotree-mode-map
      "A" 'neotree-stretch-toggle
      (kbd "TAB") 'neotree-enter
      (kbd "RET") 'neotree-enter
      "d" 'neotree-delete-node
      "c" 'neotree-create-node
      "r" 'neotree-rename-node
      "C" 'neotree-change-root
      "H" 'neotree-hidden-file-toggle
      "R" 'my/neotree-refresh)))

(progn ; tramp
  (with-eval-after-load 'tramp
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
    (setq remote-file-name-inhibit-cache nil)
    (setq remote-file-name-inhibit-locks nil))
  (with-eval-after-load 'tramp-sh
    (setq tramp-use-ssh-controlmaster-options t)
    (unless (file-exists-p "~/.emacs.d/.tramp/")
      (make-directory "~/.emacs.d/.tramp/"))
    (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=~/.emacs.d/.tramp/tramp-%%r@%%n:%%p-%%j -o ControlPersist=10")))

(progn ; special mode
  (add-hook 'special-mode-hook
	    (lambda ()
	      (evil-define-key 'normal special-mode-map "q" 'quit-window))))


(progn ; pdf settings
  (install-if-not-exists 'pdf-tools)
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (display-line-numbers-mode -1)
	      (evil-define-key* 'normal pdf-view-mode-map
		"j" 'pdf-view-next-line-or-next-page
		"J" 'pdf-view-next-page
		"k" 'pdf-view-previous-line-or-previous-page
		"K" 'pdf-view-previous-page
		"=" 'pdf-view-enlarge
		"+" 'pdf-view-enlarge
		"-" 'pdf-view-shrink
		"<" 'beginning-of-buffer
		"s" 'pdf-view-fit-width-to-window
		"a" 'pdf-view-fit-height-to-window))))


(progn ; direnv
  (install-if-not-exists 'envrc)
  (envrc-global-mode))

(progn					; dired settings
  (with-eval-after-load 'dired
    (define-key dired-mode-map " " nil)
    (evil-define-key
      'normal dired-mode-map
      "h" 'dired-up-directory
      "l" 'dired-find-file
      "g" nil
      "gg" 'evil-goto-first-line
      "G" 'evil-goto-line
      ";" 'evil-ex
      "n" 'evil-search-next
      "N" 'evil-search-previous)))

(progn ; font settings
  (when (display-graphic-p)
    (create-fontset-from-ascii-font
     "Cica:weight=regular:slant=normal"
     nil
     "Cica")
    (set-fontset-font "fontset-Cica" 'unicode "Cica:weight=regular:slant=normal" nil 'append)
    ;; it is necessary for avoid bug described at https://github.com/bling/fzf.el/issues/116
    (setq face-font-rescale-alist '(("Cica" . 1.0)))
    (add-to-list 'default-frame-alist '(font . "fontset-Cica"))))

(progn ; typst-ts-settings
  (my/register-git-package 'typst-ts-mode "https://git.sr.ht/~meow_king/typst-ts-mode" t)
  (my/register-git-package 'typst-preview "https://github.com/havarddj/typst-preview.el" t)
  (with-eval-after-load 'eglot
    (with-eval-after-load 'typst-ts-mode
      (setq-default eglot-workspace-configuration '(:formatterMode "typstyle"))
      (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist"))))))

(progn ; vertico settings
  (install-if-not-exists 'vertico)
  (install-if-not-exists 'consult)
  (install-if-not-exists 'orderless)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (vertico-mode)
  (define-key evil-normal-state-map " fp" 'consult-fd)
  (define-key evil-normal-state-map " sr" 'consult-ripgrep))




(progn ;; lsp-bridge
  (install-if-not-exists 'markdown-mode)
  (install-if-not-exists 'yasnippet)
  (add-to-list 'load-path "~/.emacs.d/lisp/lsp-bridge/")

  (defun my/enable-lsp-bridge-mode ()
    (interactive)
    (require 'lsp-bridge)
    (global-lsp-bridge-mode)
    (lsp-bridge-mode)
    (yas-global-mode 1)
    (setq lsp-bridge-enable-log nil)
    (setq lsp-bridge-enable-hover-diagnostic t)
    (setq lsp-bridge-enable-diagnostics t)
    (setq lsp-bridge-enable-inlay-hint t)

    (setq lsp-bridge-remote-start-automatically t)
    (setq lsp-bridge-enable-with-tramp t)
    (setq lsp-bridge-remote-python-command "python3")
    (setq lsp-bridge-remote-python-file "~/.emacs.d/lisp/lsp-bridge/lsp_bridge.py")
    (setq lsp-bridge-remote-log "~/.emacs.d/lsp-bridge/remote.log")

    (evil-define-key 'normal lsp-bridge-mode-map
      " lr" 'lsp-bridge-rename
      " lf" 'lsp-bridge-code-format
      " la" 'lsp-bridge-code-action
      " lv" 'lsp-bridge-show-documentation
      " lp" 'lsp-bridge-diagnostic-list
      )

    (evil-define-key 'insert acm-mode-map
      (kbd "C-n") 'acm-select-next
      (kbd "C-p") 'acm-select-prev
      (kbd "C-d") 'acm-doc-toggle
      (kbd "C-j") 'acm-doc-scroll-down
      (kbd "C-k") 'acm-doc-scroll-up)

    (evil-define-key 'normal lsp-bridge-ref-mode-map
      "j" 'lsp-bridge-ref-jump-next-keyword
      "k" 'lsp-bridge-ref-jump-prev-keyword
      (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
      "q" 'lsp-bridge-ref-quit))


  (define-key-tree
   evil-normal-state-map
   (" "
    ("m" ;; mode
     ("b" 'my/enable-lsp-bridge-mode)))))


(progn ;; ssh settings
  (defun my/get-remote-host ()
    (when-let (name (buffer-file-name))
      (when (file-remote-p name 'host)
	(tramp-file-name-host (tramp-dissect-file-name name)))))
  (defun my/port-forwarding (connect-to direction port-from location-to port-to)
    (interactive
     (let* ((connect-to (if-let (default (my/get-remote-host))
			    (read-string (format "Where to connect? (defualt is %s): " default) nil nil default)
			  (read-string "Where to connect?: " nil nil default)))
	    (direction (read-string "Port forwarding direction (default is -L): " nil nil "-L"))
	    (port-from (read-string "Please input port-from: " nil nil nil))
	    (location-to (read-string "Please input location-to (default is localhost): " nil nil "localhost"))
	    (port-to (read-string (format "Please input port-to (default is %s): " port-from) nil nil port-from)))
       (list connect-to direction port-from location-to port-to)))
    (start-process "port-forwarding" nil "ssh" direction (format "%s:%s:%s" port-from location-to port-to) connect-to))
  (define-key evil-normal-state-map " up" 'my/port-forwarding))


(progn ;;bookmark settings
  (define-key evil-normal-state-map " mB" 'bookmark-bmenu-list)
  (with-eval-after-load 'bookmark
    (evil-define-key 'normal bookmark-bmenu-mode-map (kbd "RET") 'bookmark-bmenu-this-window)))

(progn ;; tree sitter settings
  (install-if-not-exists 'tree-sitter-langs)
  (global-tree-sitter-mode))

(progn ;; fold settings
  (let ((p (my/register-git-package 'ts-fold "https://github.com/emacs-tree-sitter/ts-fold" t)))
    (add-to-list 'load-path p)
    (require 'ts-fold)
    (global-ts-fold-mode))
  (define-key evil-normal-state-map "zc" 'ts-fold-close)
  (define-key evil-normal-state-map "zo" 'ts-fold-open)
  (define-key evil-normal-state-map "zO" 'ts-fold-open-recursively)
  (define-key evil-normal-state-map "zm" 'ts-fold-close-all)
  (define-key evil-normal-state-map "zr" 'ts-fold-open-all))

(progn ;; org settings
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     (append org-babel-load-languages
	     '((forth . t))))))
