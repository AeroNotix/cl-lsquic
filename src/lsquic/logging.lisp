(in-package :lsquic)


(defcallback my-log-buf :int ((ctx :pointer) (buf :pointer) (len :int))
  (format t "~A~%" (foreign-string-to-lisp buf :count len))
  len)

(defun new-logger ()
  (let ((logger (foreign-alloc '(:struct logger-if))))
    (with-foreign-slots ((log-buf) logger (:struct logger-if))
      (setf log-buf (callback my-log-buf)))
    logger))

(defun logger-initialize (log-level)
  (when log-level
    (with-foreign-string (ll log-level)
      (set-log-level ll))
    (logger-init
     (new-logger)
     (cffi:null-pointer)
     (foreign-enum-value 'logger-timestamp-style :LLTS-HHMMSSUS))))
