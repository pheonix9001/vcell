(defpackage vcell
  (:use :cl :trivial-types)
  (:nicknames :v)
  (:export
    cell
    cell-ref
    cell-ref-noop
    cell-set
    defcell))
(in-package vcell)

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

(defmethod print-object ((cell cell) stream)
  (print-unreadable-object (cell stream :type t)
    (print-object (cell-ref-noop cell) stream)))

;; This is the magic
(defparameter *cell-self* '()
  "The current cell")

(defmethod cell-ref ((cell cell))
  (when *cell-self*
    (setf (cell-subs cell)
          (adjoin *cell-self* (cell-subs cell))))
  (cell-ref-noop cell))

(defun update-cell (cell)
  "Update a cell to match its `updater`"

  (let ((oldsubs (cell-subs cell))
        (*cell-self* cell))
    (setf (cell-ref-noop cell) (funcall (cell-updater cell)))

    ;; Remove all subscribers
    (setf (cell-subs cell) '())
    (dolist (sub oldsubs)
       ;; This will add it back in
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
    `(let ((,res (make-instance 'cell
                    :val '()
                    :updater (lambda () ,@body))))
       (update-cell ,res)
       ,res)))

(defmacro defcell (name value &optional doc)
  "Sugar to define/redefine a cell"
  (if (boundp name)
    `(cell-set ,name ,value)
    `(defvar ,name (cell ,value) ,doc)))
