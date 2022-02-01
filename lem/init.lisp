(in-package :lem-user)

(load (uiop:merge-pathnames* ".lem/util" (uiop/common-lisp:user-homedir-pathname)))
(load (uiop:merge-pathnames* ".lem/vim" (uiop/common-lisp:user-homedir-pathname)))

(setf (variable-value 'lem.line-numbers:line-numbers :global) t)

(define-color-theme "monokai" ()
  (foreground "#eeeeee")
  (background "#262626")
  (cursor :foreground "#262626" :background "#eeeeee")
  (syntax-warning-attribute :foreground "#87005f" :background "#262626")
  (syntax-string-attribute :foreground "#d7d787" :background "#262626")
  (syntax-comment-attribute :foreground "#666666" :background "#262626")
  (syntax-keyword-attribute :foreground "#5fd7ff" :background "#262626")
  (syntax-constant-attribute :foreground "#5fd7ff" :background "#262626")
  (syntax-function-name-attribute :foreground "#afd700" :background "#262626")
  (syntax-variable-attribute :foreground nil :background "#262626")
  (syntax-type-attribute :foreground nil :background "#262626")
  (syntax-builtin-attribute :foreground nil :background "#262626"))

(load-theme "monokai")

(lem-vi-mode:vi-mode)

(settings.util:define-key-tree lem-vi-mode.core:*command-keymap*
  (";" 'lem-vi-mode.ex:vi-ex)
  ("g"
   ("d" 'lem.language-mode::find-definitions))
  ("Space"
   ("Space" 'execute-command)
   (";" 'lem-lisp-mode::self-lisp-eval-string)
   ("w"
    ("h" 'lem:window-move-left)
    ("j" 'lem:window-move-down)
    ("k" 'lem:window-move-up)
    ("l" 'lem:window-move-right)
    ("s" 'lem-vi-mode::split-active-window-horizontally)
    ("v" 'lem-vi-mode::split-active-window-vertically))
   ("b"
    ("h" 'lem:previous-buffer)
    ("l" 'lem:next-buffer)
    ("d" 'lem:kill-buffer))))

(settings.util:define-key-tree lem-vi-mode.core:*insert-keymap*
  ("Return" 'lem.language-mode:newline-and-indent))

(lem-lisp-mode.paren-coloring:toggle-paren-coloring)
(lem.show-paren::toggle-show-paren)

(add-hook 
 lem-lisp-mode:*lisp-mode-hook*
 (lambda ()
   (define-key lem-lisp-mode:*lisp-mode-keymap* "Space" nil)
   (lem-paredit-mode:paredit-mode)
   (settings.util:define-key-tree lem-vi-mode.core:*command-keymap*
     ("Space"
      ("l"
       ("l" 'lem-paredit-mode:paredit-slurp)
       ("L" 'lem-paredit-mode:paredit-barf)
       ("j" 'lem-paredit-mode::paredit-wrap)
       ("k" 'lem-paredit-mode::paredit-splice)
       ("e"
        ("d" 'lem-lisp-mode:lisp-eval-defun)
        ("b" 'lem-lisp-mode:lisp-load-file)))))
   (settings.util:define-key-tree lem-vi-mode.core:*insert-keymap*
     ("Space" 'lem-lisp-mode.autodoc::lisp-insert-space-and-autodoc))))

(settings.vim:vim-noremap "fd" "<Escape>") 
(settings.vim:vim-map "<Space>w" "[WINDOW]")
(settings.vim:vim-noremap "[WINDOW]" "<Space>w")
(settings.vim:vim-noremap "[WINDOW]v" ":vs<Return>")
(settings.vim:vim-noremap "[WINDOW]s" ":split<Return>")
(settings.vim:vim-noremap "[WINDOW]h" "<C-w>h")
(settings.vim:vim-noremap "[WINDOW]j" "<C-w>j")
(settings.vim:vim-noremap "[WINDOW]k" "<C-w>k")
(settings.vim:vim-noremap "[WINDOW]l" "<C-w>l")
(settings.vim:vim-noremap "[WINDOW]d" ":q<Return>")