(defpackage :http3
  (:use :cl :cffi :cffi-helpers)
  (:export
   #:client
   #:quic-connect
   #:packets-in))

(in-package :http3)

(cffi:define-foreign-library lsquic
  (:unix (:or "lsquic/src/liblsquic/liblsquic.so")))

(cffi:define-foreign-library dns
  (:unix (:or "c-src/libdns.so")))

(cffi:define-foreign-library udp
  (:unix (:or "c-src/libudp.so")))

(cffi:use-foreign-library lsquic)
(cffi:use-foreign-library dns)
(cffi:use-foreign-library udp)
