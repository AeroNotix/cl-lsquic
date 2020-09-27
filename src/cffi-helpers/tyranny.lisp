(defpackage :cffi-helpers
  (:use :cl)
  (:export
   #:make-pointer-to
   #:with-pointer-to
   #:with-pointer-to-int
   #:safe-foreign-alloc
   #:with-initialize-foreign-struct-with-fill
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

(defun safe-foreign-alloc (type &key (count 1) (fill-value 0))
  (static-vectors:fill-foreign-memory
   (cffi:foreign-alloc type :count count)
   (* count (cffi:foreign-type-size type))
   fill-value))

(defmacro with-initialize-foreign-struct (type &body body)
  (let* ((struct-field-names (cffi:foreign-slot-names (list :struct type)))
         (struct-sym (gensym)))
    `(let ((,struct-sym (safe-foreign-alloc (list :struct ',type))))
       (cffi:with-foreign-slots (,struct-field-names ,struct-sym (:struct ,type))
         (progn ,@body))
       ,struct-sym)))

(defmacro with-initialize-foreign-struct-with-fill (type fill-value &body body)
  (let* ((struct-field-names (cffi:foreign-slot-names (list :struct type)))
         (struct-sym (gensym)))
    `(let ((,struct-sym (safe-foreign-alloc (list :struct ',type) :fill-value ,fill-value)))
       (cffi:with-foreign-slots (,struct-field-names ,struct-sym (:struct ,type))
         (progn ,@body))
       ,struct-sym)))
