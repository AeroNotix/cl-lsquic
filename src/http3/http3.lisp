(in-package :http3)

(define-condition cannot-create-engine (error)
  ()
  (:documentation "Signalled when attempting to create an LSQUIC engine but the engine is NULL"))

(define-condition invalid-settings (error)
  ()
  (:documentation "Signalled when passed the settings for this QUIC client are invalid"))

(define-condition invalid-quic-version (error)
  ((requested :initarg :requested
              :initform (error "Invalid")
              :reader requested)
   (available :initarg :available
              :initform (error "Invalid")
              :reader available))
  (:documentation "Signalled when passed a QUIC version string we do not support or understand"))

(defclass client (lsquic:client)
  ((process-conns-lock :accessor process-conns-lock :initform (bordeaux-threads:make-lock "process-conns"))))

(defmethod connect (client)
  (lsquic:connect client))

(defmethod make-request (client &keys host port headers path body verb)
  (let* ((request (make-instance lsquic:request
                                 :path path
                                 :headers headers
                                 :authority host
                                 :verb verb
                                 :body body))
         (pipe (lsquic:new-stream client request)))
    (wait-for-response pipe)))

(defmacro request-no-body (verb)
  (let* ((sym-name (symbol-name verb))
         (method-name (intern (format nil "MAKE-~A-REQUEST" sym-name))))
    `(defmethod ,method-name (client &keys host port headers path)
       (make-request
        client :host host :port port :headers header :body nil :verb ,verb))))

(defmacro request-with-body (verb)
  (let* ((sym-name (symbol-name verb))
         (method-name (intern (format nil "MAKE-~A-REQUEST" sym-name))))
    `(defmethod ,method-name (client &keys host port headers path body)
       (make-request
        client :host host :port port :headers header :body body :verb ,verb))))

(request-no-body get)
(request-no-body head)
(request-no-body delete)
(request-no-body connect)
(request-no-body trace)
(request-with-body post)
(request-with-body put)
(request-with-body patch)
