(in-package :lsquic)

(defun make-pointer-to (type val)
  (cffi:foreign-alloc type :initial-element val))

;; TODO: Seriously? This is bullshit. Need a better way.
;;
;; CFFI requires that all arguments to c-funcs are pointers. When a
;; c-func takes a simple scalar value you end up with silly things
;; like this.

(defmacro with-pointer-to (binding-form &body body)
  (let ((name-value (first binding-form))
        (type (second binding-form)))
    `(let ((,(first name-value) (make-pointer-to ,type ,(second name-value))))
       (unwind-protect
            (progn
              ,@body)
         (cffi:foreign-free ,(first name-value))))))

(defmacro with-pointer-to-int (binding-form &body body)
  `(with-pointer-to (,binding-form :int)
     ,@body))

(defmacro safe-foreign-alloc (type)
  `(static-vectors:fill-foreign-memory (foreign-alloc ,type) (foreign-type-size ,type) 0))
