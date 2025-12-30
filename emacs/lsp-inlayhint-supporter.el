;;;  -*- lexical-binding: t -*-
(require 'lsp)

(defun lsp-inlayhint-supporter-resolve ()
  (interactive)
  (lsp-request-async
   "textDocument/inlayHint"
   (lsp-make-inlay-hints-params
    :text-document (lsp--text-document-identifier)
    :range (lsp-make-range :start (lsp-point-to-position (window-start))
			   :end (lsp-point-to-position (window-end nil t))))
   (lambda (res)
     (let ((hints (make-hash-table :test #'equal))
	   (hint-key ?A)
	   (current-line (1- (line-number-at-pos (point)))))
       (dolist (hint res)
	 (-let* (((&InlayHint :position :label) hint)
		 (label (lsp--label-from-inlay-hints-response label))
		 ((&Position :line) position))
	   (when (and (stringp label) (eq line current-line))
	     (puthash (char-to-string hint-key) `((label . ,(format " resolve (%s)" label)) (hint . ,hint)) hints)
	     (setf hint-key (1+ hint-key)))))
       (let ((selected-hint
	      (completing-read
	       "HEY"
	       (lambda (str pred action)
		 (let ((result (complete-with-action action hints str pred)))
		   (if (eq action 'metadata)
		       `(metadata ,@ (cdr result)
				     (annotation-function . ,(lambda (key) (cdr (assoc 'label (gethash key hints)))))
				     (display-sort-function . sort))
		     result))))))
	 (lsp-inlayhint-supporter--resolve-hint (cdr (assoc 'hint (gethash selected-hint hints)))))))
   :mode 'unchanged))

(defun lsp-inlayhint-supporter--make-goto-definition-thunk (filename line character)
  (lambda ()
    (cond
     ((string-match "^file://" filename)
      (find-file (replace-regexp-in-string "^file://" "" filename))
      (next-line line)
      (forward-char character))
     (t
      (message "Invalid filename! %s" filename)))))

(defun lsp-inlayhint-supporter--make-execute-command-thunk (command arguments?)
  (lambda ()
    (lsp-workspace-command-execute command arguments?)))

(defun lsp-inlayhint-supporter--resolve-hint (inlayhint)
  (lsp-request-async
   "inlayHint/resolve"
   inlayhint
   (lambda (res)
     (let ((hints (make-hash-table :test #'equal))
	   (hint-key ?A))
       (-let* (((&InlayHint :label) res))
	 (cl-typecase label
	   (string nil)
	   (vector
	    (seq-doseq (part label)
	      (-let (((&InlayHintLabelPart :value :location? :command?) part))
		(when location?
		  (-let* (((&Location :uri :range) location?)
			  ((&Range :start) range)
			  ((&Position :line :character) start))
		    (puthash (char-to-string hint-key)
			     `((label . ,(format " Goto Definition (%s)" value))
			       (action . ,(lsp-inlayhint-supporter--make-goto-definition-thunk uri line character)))
			     hints)
		    (setq hint-key (1+ hint-key))))
		(when command?
		  (-let* (((&Command :title :command :arguments?) command?))
		    (message "NYAAAAAAAAAAA %s" command)
		    (puthash (char-to-string hint-key)
			     `((label . ,(format " %s (%s)" title value))
			       (action . ,(lsp-inlayhint-supporter--make-execute-command-thunk command arguments?)))
			     hints)
		    (setq hint-key (1+ hint-key))))))))
       (cond ((hash-table-empty-p hints)
	      (error "There is no Action!"))
	     (t (let ((selected-hint
		       (completing-read
			"HEY"
			(lambda (str pred action)
			  (let ((result (complete-with-action action hints str pred)))
			    (if (eq action 'metadata)
				`(metadata ,@ (cdr result)
					      (annotation-function . ,(lambda (key) (cdr (assoc 'label (gethash key hints)))))
					      (display-sort-function . sort))
			      result))))))
		  (funcall (cdr (assoc 'action (gethash selected-hint hints)))))))))
   :mode 'unchanged)))

(provide 'lsp-inlayhint-supporter)
