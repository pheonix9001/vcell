# `v:cell`
Powerful dataflow in Lisp

## Demonstration
Let us begin with a simple example
```lisp
(v:defcell *age* 10)

*age* ; => #<V:CELL 10>
(v:cell-ref *age*) ; => 10
```

Now let's get fancy
```lisp
(v:defcell *can-drive-p* (< 18 (v:cell-ref *age*)))
(v:defcell *message* (if (v:cell-ref *can-drive-p*) "Ok" "Wait that's illegal!"))

(v:cell-ref *message*) ; => "Wait that's illegal!"
(v:cell-ref *can-drive-p*) ; => NIL
```

Now this is where the magic comes in
```lisp
(v:cell-set *age* 20) ; Set the value of a cell dynamically
(v:cell-ref *message*) ; => "Ok"

;; Do it again
(v:cell-set *age* 11)
(v:cell-ref *message*) ; => "Wait that's illegal!"
```

And we can create an observer
```lisp
(v:defcell *give-warning* (format t "If you are trying to drive: ~a" (v:cell-ref *message*)))

(v:cell-set *age* 20) ; => If you are trying to drive: Ok
(v:cell-set *age* 10) ; => If you are trying to drive: Wait that's illegal!
```

And `defcell` supports redefinition!
```lisp
;; Give message only when someone is trying to drive
(defvar *trying-to-drive-p* nil)

(defcell *give-warning*
  (when *trying-to-drive-p*
    (princ (v:cell-ref *message*))))

(v:cell-set *age* 20) ; No output
(v:cell-set *age* 10) ; No output
(setf *trying-to-drive-p* t)
(v:cell-set *age* 20) ; => Ok
(v:cell-set *age* 10) ; => Wait that's illegal!
```

## License
