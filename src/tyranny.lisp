(in-package :lsquic)

(defun make-pointer-to-int (int-value)
  (cffi:foreign-alloc :int :initial-element int-value))


;; TODO: Seriously? This is bullshit. Need a better way.
;;
;; CFFI requires that all arguments to c-funcs are pointers. When a
;; c-func takes a simple scalar value you end up with silly things
;; like this.
(defmacro with-pointer-to-int (binding-form &body body)
  (let ((name (first binding-form))
        (value (second binding-form)))
    `(let ((,name (make-pointer-to-int ,value)))
       (unwind-protect
            (progn
              ,@body)
         (cffi:foreign-free ,name)))))

(defmacro safe-foreign-alloc (type)
  `(static-vectors:fill-foreign-memory (foreign-alloc ,type) (foreign-type-size ,type) 0))
