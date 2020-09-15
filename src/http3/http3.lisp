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
  (lsquic:set-context client)
  (lsquic:new-engine client)
  (lsquic:quic-connect client)
  (lsquic:process-conns client)
  (lsquic:packets-in client))

