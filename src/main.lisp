(defpackage wcells
  (:use :cl :trivial-types))
(in-package wcells)

(defclass cell ()
  ((val
      :accessor cell-ref-noop
      :initarg :val)
   (subs
      :accessor cell-subs
      :type (proper-list cell)
      :initform '())
   (updater
     :accessor cell-updater
     :type function
     :initarg :updater))
  (:documentation "A reactive cell"))

;; This is the magic
(defparameter *cell-self* '()
  "The current cell")

(defmethod cell-ref ((cell cell))
  (when *cell-self*
    (setf (cell-subs cell)
          (remove-duplicates
                 (cons *cell-self* (cell-subs cell))
                 :test #'eql)))
  (cell-ref-noop cell))

(defun update-cell (cell)
  "Update a cell to match its `updater`"
  (let ((*cell-self* cell))
    (setf (cell-ref-noop cell) (funcall (cell-updater cell)))
    (dolist (sub (cell-subs cell))
       (update-cell sub))))

(defmacro cell-set (cell-code &body body)
  "Set a cell to a value"
  (let ((cell (gensym "cell")))
    `(let ((,cell ,cell-code))
       (setf (cell-updater ,cell) (lambda () ,@body))
       (update-cell ,cell))))

(defmacro cell (&body body)
  "Defines a cell"
  (let ((res (gensym "res-cell")))
    `(let* ((,res (make-instance 'cell
                    :val '()
                    :updater (lambda () ,@body))))
       (update-cell ,res)
       ,res)))
