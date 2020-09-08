(in-package :lsquic)


(defcallback my-log-buf :int ((ctx :pointer) (buf :pointer) (len :int))
  (format t "~A~%" (foreign-string-to-lisp buf :count len))
  len)

(defparameter logger-if
  (progn
    (let ((logger (foreign-alloc '(:struct lsquic-logger-if))))
      (with-foreign-slots ((log-buf) logger (:struct lsquic-logger-if))
        (setf log-buf (callback my-log-buf)))
      logger)))
