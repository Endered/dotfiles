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
			csharp-mode
			rainbow-delimiters
			slime
			slime-company
			paredit)) ;enumerate my packages

(let ((uninstalled (remove-if 'package-installed-p 
			      my-packages)))
  (when uninstalled
    (message "There are uninstalled packages")
    (package-refresh-contents)
    (dolist (package uninstalled)
      (print package)
      (package-install package))))


(progn ;theme settings
  (load-theme 'spacemacs-dark t))


(progn ;backup files settings
  (setq make-backup-files nil)
  (setq auto-save-default nil))

(progn ;parenthes settings
  (show-paren-mode 1) ;highlight correspond parenthes
  )

(progn ;scroll settings
  (setq-default scroll-margin 10) ; scroll off
  (setq-default scroll-conservatively 1) ; number of line at scroll
  )


(progn ;etc
  (column-number-mode 1)
  (global-display-line-numbers-mode 1) ;show line number on left
  (setq gc-cons-threshold 12800000)
  )


(progn ;company settings
  (add-hook 'after-init-hook 'global-company-mode) ;enable company in all mode
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(progn ;rust settings
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/rust-analyzer"))
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (add-to-list 'auto-mode-alist '("\\.rs$'" . rust-mode))
  (add-hook 'rust-mode-hook 'lsp)
  (add-hook 'rust-mode-hook 'lsp-rust-server)
  (add-hook 'rust-mode-hook 'lsp-ui-mode)
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(progn ;c++ settings
  (add-to-list 'auto-mode-alist '("\\.cpp$" . c++-mode))
  (add-hook 'c++-mode-hook
	    (lambda ()
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
	      (lsp)
	      (lsp-ui-mode)
	      (electric-pair-mode 1)
	      (setq tab-width 4
		    c-basic-offset 4
		    indent-tabs-mode nil
		    lsp-enable-indentation nil
		    lsp-clients-clangd-executable "clangd"))))

(progn ;C# settings
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
  (add-hook 'csharp-mode-hook
	    (lambda ()
	      (define-key evil-insert-state-map "\C-n" 'company-select-next)
	      (define-key evil-insert-state-map "\C-p" 'company-select-previous)
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
			      '((sbcl ("ros" "-L" "sbcl" "-Q" "run") :coding-system utf-8-unix)))
			(load (expand-file-name "~/.roswell/helper.el"))
			(slime-setup '(slime-fancy slime-company))))))

(progn ;emacs-lisp settings
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode 1)
	      (rainbow-delimiters-mode 1))))

(progn ;tab-bar settings
  (tab-bar-mode 1))

(progn ; evil settings
  (evil-mode 1)
  (evil-escape-mode 1)
  (define-key-tree
    evil-normal-state-map
    ("j" 'evil-next-visual-line)
    ("k" 'evil-previous-visual-line)
    (" "
     ("b"
      ("l" 'next-buffer)
      ("h" 'previous-buffer)
      ("d" 'kill-buffer))
     ("o"
      ("d" 'online-judge-download)
      ("t" 'online-judge-test)
      ("s" 'online-judge-submit))
     ("'" 'open-terminal)
     (";" 'eval-expression)
     ("w"
      ("h" 'evil-window-left)
      ("j" 'evil-window-down)
      ("k" 'evil-window-up)
      ("l" 'evil-window-right)
      ("d" 'delete-window)
      ("s" 'split-window-horizontally)
      ("v" 'split-window-vertically))
     ("t"
      ("l" 'tab-bar-switch-to-next-tab)
      ("h" 'tab-bar-switch-to-prev-tab)
      ("d" 'tab-bar-close-tab)
      ("n" 'tab-bar-new-tab))
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

(defun open-terminal ()
  (interactive)
  (term "/bin/bash"))


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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters spacemacs-theme lsp-ui leaf-keywords key-chord hydra evil-escape evil el-get company cargo blackout)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
