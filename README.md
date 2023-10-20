# Vcell
Powerful dataflow in Lisp

## Demonstration
Let us begin with a simple example
```lisp
(defcell *age* 10)

*age* ; => #<VCELL:CELL 10>
(cell-ref *age*) ; => 10
```

Now let's get fancy
```lisp
(defcell *can-drive-p* (< 18 (cell-ref *age*)))
(defcell *message* (if (cell-ref *can-drive-p*) "Ok" "Wait that's illegal!"))

(cell-ref *message*) ; => "Wait that's illegal!"
(cell-ref *can-drive-p*) ; => NIL
```

Now this is where the magic comes in
```lisp
(cell-set *age* 20) ; Set the value of a cell dynamically
(cell-ref *message*) ; => "Ok"

;; Do it again
(cell-set *age* 11)
(cell-ref *message*) ; => "Wait that's illegal!"
```

And we can create an observer
```lisp
(defcell *give-warning* (format t "If you are trying to drive: ~a" (cell-ref *message*)))

(cell-set *age* 20) ; => If you are trying to drive: Ok
(cell-set *age* 10) ; => If you are trying to drive: Wait that's illegal!
```

And `defcell` supports redefinition!
```lisp
;; Give message only when someone is trying to drive
(defvar *trying-to-drive-p* nil)

(defcell *give-warning*
  (when *trying-to-drive-p*
    (princ (cell-ref *message*))))

(cell-set *age* 20) ; No output
(cell-set *age* 10) ; No output
(setf *trying-to-drive-p* t)
(cell-set *age* 20) ; => Ok
(cell-set *age* 10) ; => Wait that's illegal!
```
## License
