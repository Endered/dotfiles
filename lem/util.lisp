(defpackage :settings.util
  (:use :cl :lem)
  (:export :parse-key-tree
           :define-key-tree))

(in-package :settings.util)

(defun parse-key-tree (tree)
  (destructuring-bind (str . other) tree
    (if (eq 'quote (caar other))
        (list (cons str (car other)))
        (mapcar (lambda (child)
                  (destructuring-bind (str2 . symbol) child
                    (cons (format nil "~a ~a" str str2) symbol)))
                (mapcan #'parse-key-tree other)))))

(defmacro define-key-tree (keymap &body forms)
  `(progn
     ,@(mapcar (lambda (form)
                 (destructuring-bind (str . symbol) form
                   `(define-key ,keymap ,str ,symbol)))
               (mapcan #'parse-key-tree forms))))

(in-package :lem-user)
