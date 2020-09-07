(defpackage :lsquic
  (:use :cl :cffi)
  (:export
   #:lispify))

(in-package :lsquic)

(cffi:define-foreign-library lsquic
  (:unix (:or "lsquic/src/liblsquic/liblsquic.so")))

(defun lispify (name flag &optional (package *package*))
  "Borrowed from: https://github.com/mtstickney/cl-libqmlbind/blob/24df1d4248c8962eaadb9dc180e25afa14c7492d/cl-libqmlbind.lisp"
  (intern (string-upcase (map 'string (lambda (c)
                                        (case c
                                          (#\_ #\-)
                                          (t c)))
                              name))
          package))

(cffi:use-foreign-library lsquic)
