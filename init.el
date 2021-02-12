(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://melpa.org/packages/") t)
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
  (setq scroll-margin 10) ; scroll off
  (setq scroll-conservatively 1) ; number of line at scroll
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
	      (lsp)
	      (lsp-ui-mode)
	      (setq tab-width 4
		    c-basic-offset 4
		    indent-tabs-mode nil
		    lsp-enable-indentation nil
		    lsp-clients-clangd-executable "clangd"))))

(progn ;C# settings
  (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
  (add-hook 'csharp-mode-hook
	    (lambda ()
	      (lsp)
	      (lsp-ui-mode))))

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
