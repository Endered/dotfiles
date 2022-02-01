(in-package :lem)

(defpackage :settings.vim
  (:use :cl :lem)
  (:export :vim-map
           :vim-noremap
           :vim-reset-typing-timer
           :vim-typing-timeout-p
           :*buf*
           :*table*))

(in-package :settings.vim)

(let ((file "~/.config/common-lisp/source-registry.conf"))
  (when (probe-file file)
    (asdf:initialize-source-registry
     (with-open-file (in file)
       (read in)))))

(ql:quickload :cl-input-interface)

(defun parse-string (string)
  (labels ((rec (str)
             (when (zerop (length str))
               (return-from rec nil))
             (when (not (eq (aref str 0) #\<))
               (return-from rec (cons (string (aref str 0))
                                      (rec (subseq str 1)))))
             (let ((position (position #\> str)))
               (assert position)
               (cons (subseq str 1 position)
                     (rec (subseq str (1+ position)))))))
    (format nil "~{~a~^ ~}" (rec string))))

(defun make-vim-map (source destination response)
  (setf source (coerce (lem::parse-keyspec (parse-string source)) 'vector))
  (setf destination (coerce (lem::parse-keyspec (parse-string destination)) 'vector))
  (assert (plusp (length source)))
  (let* ((rev (reverse source))
         (first-char (aref rev 0))
         (remain (subseq rev 1)))
    (reduce (lambda (a b)
              (lambda (s)
                (if (equal b s)
                    (list nil a)
                    (list nil))))
            remain
            :initial-value
            (lambda (s)
              (if (equal s first-char)
                  `((t ,@(map 'list response destination)))
                  (list nil))))))

(defparameter *table* (cl-ii:make-input-handler))
(defparameter *buf* nil)
(defparameter *last-typing* (get-internal-real-time))
(defparameter *typing-timeout* 0.15)

(defun make-map (source destination)
  (make-vim-map source destination #'cl-ii:input-callback))
(defun make-noremap (source destination)
  (make-vim-map source destination #'cl-ii:input-response))

(defun vim-map (source destination)
  (cl-ii:input-handler-add-bind
   *table*
   (make-map
    source
    destination)))
(defun vim-noremap (source destination)
  (cl-ii:input-handler-add-bind
   *table*
   (make-noremap
    source
    destination)))

(defun vim-reset-typing-timer ()
  (setf *last-typing* (get-internal-real-time)))

(defun vim-typing-timeout-p ()
  (> (/ (- (get-internal-real-time) *last-typing*)
        internal-time-units-per-second)
     *typing-timeout*))

(in-package :lem)

(defun read-event (&optional timeout)
  (unless timeout (setf timeout 0.01))
  (when  settings.vim:*buf* (return-from read-event (pop settings.vim:*buf*)))
  (let ((res (receive-event timeout)))
    (unless (key-p res)
      (when (settings.vim:vim-typing-timeout-p)
        (setf settings.vim:*buf*
              (append settings.vim:*buf*
                      (cl-ii:input-handler-flush settings.vim:*table*))))
      (return-from read-event res))
    (settings.vim:vim-reset-typing-timer)
    (setf settings.vim:*buf* (append settings.vim:*buf*
                                     (cl-ii:input-handler-input settings.vim:*table*
                                                                res)))
    nil))

(in-package :lem-user)
(get-internal-real-time)
