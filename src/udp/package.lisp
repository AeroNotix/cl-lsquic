(defpackage :udp
  (:use :cl :cffi)
  (:export
   #:initialize-library))
(in-package :udp)

(cffi:define-foreign-library udp
  (:unix (:or "c-src/libudp.so")))

(defun initialize-library ()
  (cffi:use-foreign-library udp))
