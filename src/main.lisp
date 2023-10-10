(defpackage wcells
  (:use :cl :trivial-types))
(in-package wcells)

(defclass cell ()
  ((val
      :accessor cell-ref
      :initarg :val)
   (subs
      :accessor cell-subs
      :type (proper-list function)
      :initform '()))
  (:documentation "A reactive cell"))

;; This is the magic
(defparameter *cell-context* '()
  "The function to calculate and set the current cell")

(defmethod (cl:setf cell-ref) :after (val (cell cell))
  (declare (ignore val))
  (dolist (sub (cell-subs cell))
     (funcall sub)))

(defmethod cell-ref :after ((cell cell))
  (when *cell-context*
    (setf (cell-subs cell)
          (merge 'list
                 (cell-subs cell)
                 (list *cell-context*)
                 #'eql))))

(defmacro cell (&body body)
  "Defines a cell"
  (let ((res (gensym "res-cell")))
    `(let* ((,res (make-instance 'cell :val '()))
            (*cell-context*
              (lambda () (setf (cell-ref ,res) (progn ,@body)))))
       (funcall *cell-context*)
       ,res)))
