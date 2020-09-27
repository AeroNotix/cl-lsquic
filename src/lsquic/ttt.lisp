(in-package :lsquic)


(cffi:defcstruct swb
  (buf :pointer))

(cffi:defcstruct sswb
  (st :pointer))


(defun make-swb ()
  (let ((s (foreign-alloc '(:struct swb))))
    (with-foreign-slots ((buf) s (:struct swb))
      (setf buf (cffi:foreign-string-alloc "Hello there")))
    s))

(defun print-swb (s)
  (with-foreign-slots ((buf) s (:struct swb))
    (cffi:foreign-funcall "printf" :string "%s\n" :pointer buf)
    (cffi:foreign-funcall "fflush" :int 0)))

(defun make-sswb (n)
  (let* ((sw (foreign-alloc '(:struct sswb)))
         (swbs (loop for i upto n collect (make-swb)))
         (pswbs (cffi:foreign-alloc :pointer :initial-contents swbs)))
    (with-foreign-slots ((st) sw (:struct sswb))
      (setf st pswbs))
    (with-foreign-slots ((st) sw (:struct sswb))
      (dotimes (i n)
        (print-swb "~A~%" (mem-aref st :pointer n))))))

