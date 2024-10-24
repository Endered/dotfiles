(require 'cl)
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	;; ("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("gnu" . "https://elpa.gnu.org/packages/")))
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
  (unless (package-installed-p package)
    (refresh-and-package-install package))
  (require package))

(defun only-install (package)
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
     (" " 'execute-extended-command)
     ("c" ; compile
      ("m" 'compile);make
      )
     ("g" ; go (move)
      ("r" 'xref-find-references)))
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
  (require 'flymake)
  (set-face-attribute 'flymake-error nil :underline `(:color "red"))
  (set-face-attribute 'flymake-warning nil :underline `(:color "yellow"))
  'flymake-warning
  (evil-define-key-tree
   'normal
   eglot-mode-map
   (" "
    ("l"
     ("f" 'eglot-format-buffer)
     ("r" 'eglot-rename)
     ("v" 'eldoc-doc-buffer))))
  (define-key-tree
   evil-normal-state-map
   (" "
    ("m" ;mode
     ("l" 'eglot)))))

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
  (defun my/find-rust-project-root (dir)
    (when-let ((root (locate-dominating-file dir "Cargo.toml")))
      (list 'vc 'Git root)))
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (cargo-minor-mode 1)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
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
  (require-or-install 'cmake-mode)
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake$" . cmake-mode)))

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
  (require-or-install 'company)
  (only-install 'slime-company)
  (require-or-install 'paredit)
  (require 'slime-autoloads)
  (setq slime-company-completion 'fuzzy)
  (slime-setup '(slime-fancy slime-company slime-repl))

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
	      (slime-setup '(slime-fancy slime-company))
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
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cable$" . haskell-mode)))

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
			    (setq indent-tabs-mode nil)
			    (setq c-basic-offset 4)
			    (setq tab-width 4)
			    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
			    (setq completion-ignore-case t)
			    (setq company-dabbrev-downcase nil)
			    (define-key company-active-map (kbd "C-n") 'company-select-next)
			    (define-key company-active-map (kbd "C-p") 'company-select-previous))))

(progn ;typescript
  (require-or-install 'typescript-mode)
  (add-hook 'typescript-mode-hook
	    (lambda ()
	      (setq tab-width 2
		    c-basic-offset 2
		    js-indent-level 2
		    typescript-indent-level 2
		    indent-tabs-mode nil)
	      (add-hook 'before-save-hook))))


(progn ;elm settings
  (require-or-install 'elm-mode))


(progn ;makefile settings
  (add-hook 'makefile-mode-hook (lambda ()
				  (setq c-basic-offset 4)
				  (setq tab-width 4))))


(progn ;scala settings
  (require-or-install 'scala-mode))

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

(progn ;; ripgrep settings
  (require-or-install 'rg)
  (define-key-tree
   evil-normal-state-map
   (" "
    ("s"
     ("r" 'rg)
     ("l" 'rg-literal))))
  (add-hook 'rg-mode-hook
	    (lambda ()
	      (message "HELLO")
	      (evil-define-key-tree
	       'normal
	       rg-mode-map
	       ("j" 'compilation-next-error)
	       ("k" 'compilation-previous-error)
	       ("i" 'rg-rerun-toggle-ignore)
	       ("c" 'rg-rerun-toggle-case)
	       ("q" 'quit-window)
	       ("n" 'next-error-no-select)
	       ("p" 'previous-error-no-select)))))

(progn ;; SATySFi
  (load "~/.emacs.d/lisp/satysfi.el")
  (require 'satysfi)
  (setq satysfi-command "satysfi")
  (setq satysfi-pdf-viewer-command "zathura --fork")
  (require-or-install 'lsp-mode)
  (add-to-list 'lsp-language-id-configuration '(satysfi-mode . "satysfi"))
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
  (evil-define-key-tree
   'normal
   satysfi-mode-map
   (" "
    ("s"; satysfi
     ("t" 'satysfi-mode/typeset)
     ("o" 'satysfi-mode/open-pdf))))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "satysfi-language-server")
		    :activation-fn (lsp-activate-on "satysfi")
		    :server-id 'satysfi-language-server)))



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
