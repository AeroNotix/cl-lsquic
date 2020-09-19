(defpackage :lsquic
  (:use :cl :cffi :cffi-helpers)
  (:export
   #:engine

   #:set-context
   #:process-conns
   #:packets-in
   #:new-engine
   #:quic-connect
   #:client
   #:logger-initialize))

(in-package :lsquic)

(eval-when (:compile-toplevel :load-toplevel)
  (unless (fboundp 'lispify)
    (defun lispify (name flag &optional (package *package*))
      "Borrowed from: https://github.com/mtstickney/cl-libqmlbind/blob/24df1d4248c8962eaadb9dc180e25afa14c7492d/cl-libqmlbind.lisp"
      (intern (string-upcase (map 'string (lambda (c)
                                            (case c
                                              (#\_ #\-)
                                              (t c)))
                                  name))
              package)))
  (unless (fboundp 'chomp-lsquic)
    (defparameter prefixes-to-remove '("lsquic_"))
    (defun chomp-lsquic (name flag &optional (package *package*))
      (lispify
       (let ((name (string-downcase name)))
         (first
          (mapcar
           (lambda (s)
             (if (< (length name) (length s))
                 name
                 (if (string= (subseq name 0 (length s)) s)
                     (subseq name (length s))
                     name)))
           prefixes-to-remove))) flag package))))

(cffi:define-foreign-library lsquic
    (:unix (:or "lsquic/src/liblsquic/liblsquic.so")))
(cffi:use-foreign-library lsquic)

(dns:initialize-library)
(udp:initialize-library)
