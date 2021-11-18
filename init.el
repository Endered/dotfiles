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

(defconst my-packages '(rust-mode
			company
			lsp-mode
			lsp-ui
			cargo
			evil
			evil-escape
			spacemacs-theme
			monokai-theme
			csharp-mode
			rainbow-delimiters
			slime
			slime-company
			paredit
			flycheck
			add-node-modules-path
			ivy
			matlab-mode
			magit
			neotree
			haskell-mode
			go-mode
			company-go
			clojure-mode
			cider
			evil-terminal-cursor-changer
			vterm
			)) ;enumerate my packages

(let ((uninstalled (remove-if 'package-installed-p 
			      my-packages)))
  (when uninstalled
    (message "There are uninstalled packages")
    (package-refresh-contents)
    (dolist (package uninstalled)
      (print package)
      (package-install package))))


(progn ;theme settings
  ;(load-theme 'spacemacs-dark t)
  (load-theme 'monokai t))


(progn ;backup files settings
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(progn ;parenthes settings
  ;(show-paren-mode 1) ;highlight correspond parenthes
  (rainbow-delimiters-mode 1) ;change parenthes color
  )

(progn ;scroll settings
  (setq-default scroll-margin 10) ; scroll off
  (setq-default scroll-conservatively 1) ; number of line at scroll
  )


(progn ;etc
  (column-number-mode 1)
  (global-display-line-numbers-mode 1) ;show line number on left
  (setq gc-cons-threshold 12800000)
  (setq inhibit-startup-message t)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  )

(progn ;terminal
  (setq-default truncate-lines t) ; disable line wrap at default
  (add-hook 'vterm-mode-hook
            (lambda ()
	      (evil-define-key 'normal vterm-mode-map "p" 'vterm-yank))))

(progn ;company settings
  (add-hook 'after-init-hook 'global-company-mode) ;enable company in all mode
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(progn ;rust settings
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin")) ;path to rust analyzer
  (add-to-list 'auto-mode-alist '("\\.rs$'" . rust-mode))
  (add-hook 'rust-mode-hook
	    (lambda ()
	      (add-hook 'lsp-mode-hook
			(lambda ()
			  (setq lsp-rust-server 'rust-analyzer)
			  (lsp-ui-mode 1)))
	      (lsp 1)
	      (cargo-minor-mode 1)
	      (electric-pair-mode 1)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (setq rust-format-on-save t)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("c" ;cargo
		  ("r" 'cargo-process-run)
		  ("c" 'cargo-process-check)
		  ("a" 'cargo-process-add)))))))

(progn ;c++ settings
  (add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (hs-minor-mode t)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (lsp)
	      (lsp-ui-mode)
	      (electric-pair-mode 1)
	      (setq tab-width 4
		    c-basic-offset 4
		    lsp-prefer-capf t
		    indent-tabs-mode nil
		    company-minimum-prefix-length 2
		    lsp-enable-indentation nil
		    lsp-clients-clangd-executable "clangd-12"
		    read-process-output-max (* 1024 1024)
		    lsp-idle-delay 2.000))))

(progn ;C# settings
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
  (add-hook 'csharp-mode-hook
	    (lambda ()
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (add-hook 'before-save-hook
			'lsp-format-buffer)
	      (lsp)
	      (lsp-ui-mode)
	      (electric-pair-mode 1)
	      (setq tab-width 4
		    c-basic-offset 4
		    indent-tabs-mode nil))))

(progn ;common-lisp settings
  (add-hook 'lisp-mode-hook
	    (lambda ()
	      (hs-minor-mode 1)
	      (load (expand-file-name "~/.roswell/helper.el"))
	      (slime-mode 1)
	      (rainbow-delimiters-mode 1)
	      (paredit-mode 1)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("e"
		   ("d" 'slime-eval-defun)
		   ("r" 'slime-eval-region)
		   ("b" 'slime-eval-buffer))
		  ("h" 'paredit-backward-slurp-sexp)
		  ("l" 'paredit-forward-slurp-sexp)
		  ("H" 'paredit-backward-barf-sexp)
		  ("L" 'paredit-forward-barf-sexp)
		  ("k" 'paredit-splice-sexp)
		  ("j" 'paredit-wrap-sexp)))))
	    (add-hook 'slime-mode-hook
		      (lambda ()
			(setq slime-default-lisp 'sbcl)
			(setq slime-lisp-implementations
			      '((sbcl ("ros"
				       "-e" ; ignore shebang
				       "(set-dispatch-macro-character
 #\\# #\\! 
(lambda (stream character n) (declare (ignore character n)) (read-line stream nil nil t) nil))"
				       "-L" "sbcl" "-Q" "run") :coding-system utf-8-unix)))
			;(load (expand-file-name "~/.roswell/helper.el"))
			(slime-setup '(slime-fancy slime-company)))))) 

(progn ;clojure settings
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (hs-minor-mode 1)
	      (slime-mode 1)
	      (rainbow-delimiters-mode 1)
	      (paredit-mode 1)
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
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
		   ("l" 'cider-test-run-loaded-tests))
		  ("h" 'paredit-backward-slurp-sexp)
		  ("l" 'paredit-forward-slurp-sexp)
		  ("H" 'paredit-backward-barf-sexp)
		  ("L" 'paredit-forward-barf-sexp)
		  ("k" 'paredit-splice-sexp)
		  ("j" 'paredit-wrap-sexp)))))))

(progn ;emacs-lisp settings
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode 1)
	      (rainbow-delimiters-mode 1))))

(progn ;markdown settings
  (flycheck-define-checker textlint
    "A linter for prose."
    :command ("textlint" "--format" "unix" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message (one-or-more not-newline)
		       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
	      line-end))
     :modes (text-mode markdown-mode))
  (add-to-list 'flycheck-checkers 'textlint)
  (add-hook 'markdown-mode-hook (lambda ()
				  (flycheck-mode)
				  (add-node-modules-path))))

(progn ;tab-bar settings
  (tab-bar-mode 1))

(progn ; evil settings
  (evil-mode 1)
  (evil-escape-mode 1)
  (unless (display-graphic-p)
    (etcc-mode 1)
    (evil-terminal-cursor-changer-activate)
    (setq evil-motion-state-cursor 'box
	  evil-visual-state-cursor 'box
	  evil-normal-state-cursor 'box
	  evil-insert-state-cursor 'bar
	  evil-emacs-state-cursor 'hbar))
  (define-key-tree
    evil-normal-state-map
    ("j" 'evil-next-visual-line)
    ("k" 'evil-previous-visual-line)
    (" "
     ("b" ; buffer
      ("l" 'next-buffer)
      ("h" 'previous-buffer)
      ("d" 'kill-buffer))
     ("o" ; online judge
      ("d" 'online-judge-download)
      ("t" 'online-judge-test)
      ("s" 'online-judge-submit))
     ("'" 'vterm)
     (";" 'eval-expression)
     ("w" ; window
      ("h" 'evil-window-left)
      ("j" 'evil-window-down)
      ("k" 'evil-window-up)
      ("l" 'evil-window-right)
      ("d" 'delete-window)
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
      ("d" 'tab-bar-close-tab)
      ("n" 'tab-bar-new-tab))
     ("r" ; tab-line
      ("l" 'tab-line-switch-to-next-tab)
      ("h" 'tab-line-switch-to-prev-tab))
     ("f" ; file
      ("t" 'neotree))
     (" " 'execute-extended-command))
    (";" 'evil-ex)))

(progn 
  (defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (set-exec-path-from-shell-PATH));setup PATH

(defun concat-path (top &rest rest)
  (reduce (lambda (a b)
	    (concat (file-name-as-directory a)
		    b))
	  (cons top rest)))

(defun online-judge-download (url)
  (interactive "sProblem URL: ")
  (labels ((remove-test-directory
	    ()
	    (let ((test-directory (concat-path default-directory "test")))
	      (when (file-directory-p test-directory)
		(delete-directory test-directory t)))))
    (remove-test-directory)
    (awhen (get-buffer "online-judge")
	   (kill-buffer it))
    (pop-to-buffer "online-judge")
    (switch-to-buffer "online-judge")
    (async-shell-command (format "oj d %s" url) (get-buffer "online-judge"))
    (view-mode 1)))


(defun online-judge-test ()
  (interactive)
  (labels ((get-appropriate-command
	    (file-name)
	    (some (lambda (cons)
		    (and (string-match (car cons) file-name)
			 (cdr cons)))
		  '(("\.cpp" . "make \n oj t")
		    ("\.lisp" . "oj t -c 'sbcl --script main.lisp'")
		    ("\.py" . "oj t -c 'python3 main.py'")))))
    (let ((command (get-appropriate-command (file-name-nondirectory
					     (buffer-file-name)))))
      (when command
	(awhen (get-buffer "online-judge")
	       (kill-buffer it))
	(pop-to-buffer "online-judge")
	(switch-to-buffer "online-judge")
	(async-shell-command command (get-buffer "online-judge"))
	(view-mode 1))))
  nil)


(defun online-judge-submit (url)
  (interactive "sProblem URL: ")
  (let ((command (format "oj s -w 0 -y %s %s" url (buffer-name))))
    (awhen (get-buffer "online-judge")
	   (kill-buffer it))
    (pop-to-buffer "online-judge")
    (switch-to-buffer "online-judge")
    (async-shell-command command (get-buffer "online-judge"))
    (view-mode 1)))


(progn
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (toggle-frame-maximized) ;maximize frame at startup
	      )))

(progn ;this is typing counter
  (let ((file-path (expand-file-name "~/.emacs.d/myinits/typing-logger.el")))
    (when (file-exists-p file-path)
      (load-file file-path)
      (add-hook 'pre-command-hook
		(lambda ()
		  (when evil-insert-state-minor-mode
		    (add-typing-log))))
      (add-hook 'kill-emacs-hook 'store-typing-log))))


(progn ;scheme
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
	      (hs-minor-mode 1)
	      (rainbow-delimiters-mode 1)
	      (paredit-mode 1)
	      (define-key-tree
		evil-normal-state-map
		(" "
		 ("l"
		  ("e"
		   ("d" 'scheme-send-definition)
		   ("b" 'scheme-send-buffer))
		  ("h" 'paredit-backward-slurp-sexp)
		  ("l" 'paredit-forward-slurp-sexp)
		  ("H" 'paredit-backward-barf-sexp)
		  ("L" 'paredit-forward-barf-sexp)
		  ("k" 'paredit-splice-sexp)
		  ("j" 'paredit-wrap-sexp))))
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

  (define-key global-map
    "\C-cs" 'scheme-other-window))

(progn ;haskell
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.cable$" . haskell-mode)))

(progn ;matlab
  (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
  (add-to-list
   'auto-mode-alist
   '("\\.m$" . matlab-mode))
  (setq matlab-indent-function t)
  (setq matlab-shell-command "matlab"))

(progn ;git
  (setenv "GIT_EDITOR" "emacsclient")
  (add-hook 'shell-mode-hook 'with-editor-export-git-editor)
  (define-key evil-normal-state-map " g" 'magit-status)
  )

(progn ;neotree
  (add-hook 'neotree-mode-hook
	    (lambda ()
	      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
	      (define-key evil-normal-state-local-map (kbd "v") 'neotree-quick-look)
	      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
	      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	      (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
	      (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

(progn ;golang ;https://qiita.com/kod314/items/2232d480411c5c2ab002
  (add-to-list 'exec-path (expand-file-name "/usr/local/go/bin/"))
  (add-to-list 'exec-path (expand-file-name "/home/endered/go/bin/"))
  (add-hook 'go-mode-hook (lambda ()
			    (hs-minor-mode 1)
			    (setq gofmt-command "goimports")
			    (add-hook 'before-save-hook 'gofmt-before-save)
			    (setq indent-tabs-mode nil)
			    (setq c-basic-offset 4)
			    (setq tab-width 4)
			    (setq lsp-enable-snippet nil)
			    (company-mode)
			    (lsp)
			    (add-hook 'lsp-mode-hook
				      (lambda ()
					(lsp-ui)
					(lsp-deferred)))
			    (electric-pair-mode 1)
			    (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
			    (setq company-idle-delay 0) ; 遅延なしにすぐ表示
			    (setq company-minimum-prefix-length 3) ; デフォルトは4
			    (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
			    (setq completion-ignore-case t)
			    (setq company-dabbrev-downcase nil)
			    (define-key company-active-map (kbd "C-n") 'company-select-next)
			    (define-key company-active-map (kbd "C-p") 'company-select-previous))))

(progn ;makefile
  (add-hook 'makefile-mode-hook (lambda ()
				  (setq c-basic-offset 4)
				  (setq tab-width 4)))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(disable-mouse rainbow-delimiters spacemacs-theme lsp-ui leaf-keywords key-chord hydra evil-escape evil el-get company cargo blackout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)

(setq ring-bell-function 'ignore)
