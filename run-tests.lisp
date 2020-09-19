(in-package #:cl-user)

(defmacro with-silence (&body body)
  `(let ((*standard-output* (make-broadcast-stream))
         (*error-output* (make-broadcast-stream))
         (*trace-output* (make-broadcast-stream)))
     ,@body))

(with-silence
  (ql:quickload :fiveam))

(asdf:test-system :http3/test)
