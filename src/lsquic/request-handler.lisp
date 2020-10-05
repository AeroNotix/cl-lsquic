(in-package :lsquic)

(defclass request-handler ()
  ((request :initarg :request :accessor request)
   (response-parser :initform (fast-http:make-parser (fast-http:make-http-response)) :accessor response-parser)
   (completed-signal :initform (bt:make-semaphore) :accessor completed-signal)))

(defmethod signal-completed ((request-handler request-handler))
  (bt:signal-semaphore (completed-signal request-handler)))

(defmethod wait-for-response ((request-handler request-handler))
  (bt:wait-on-semaphore (completed-signal request-handler))
  (response-parser request-handler))
