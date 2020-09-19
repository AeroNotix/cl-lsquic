(defpackage :dns
  (:use :cl :cffi)
  (:export
   #:initialize-library))
(in-package :dns)

(cffi:define-foreign-library dns
  (:unix (:or "c-src/libdns.so")))

(defun initialize-library ()
  (cffi:use-foreign-library dns))
