(require 'web-mode)
(require 'lsp-mode) ;; higher keybind's priority rather than lsp-mode
(require 'apheleia)

(setf (alist-get 'biome apheleia-formatters)
      '("biome" "format" "--stdin-file-path" filepath))

(defun format-by-biome ()
  (interactive)
  (apheleia-format-buffer 'biome (lambda () (save-buffer))))

(defvar my-biome-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key 'normal map " lf" 'format-by-biome)
    map))

(define-minor-mode my-biome-minor-mode
  "The minor mode for provide biome's keybindings. This is used for web development"
  :lighter " Biome"
  :keymap my-biome-minor-mode-map)

(add-hook 'web-mode-hook 'my-biome-minor-mode)

(provide 'my-biome)
