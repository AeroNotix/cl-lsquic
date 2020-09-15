(defpackage :cffi-helpers
  (:use :cl)
  (:export
   #:make-pointer-to
   #:with-pointer-to
   #:with-pointer-to-int
   #:safe-foreign-alloc
   #:with-initialize-foreign-struct))

(in-package :cffi-helpers)

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

(defun safe-foreign-alloc (type &key (count 1))
  (static-vectors:fill-foreign-memory (cffi:foreign-alloc type :count count) (cffi:foreign-type-size type) 0))

(defmacro with-initialize-foreign-struct (type &body body)
  (let ((struct-field-names (foreign-slot-names `(:struct ,type))))
  `(let ((s (safe-foreign-alloc '(:struct ,type))))
     (with-foreign-slots (,struct-field-names s  '(:struct ,type))
       ,@body))))
