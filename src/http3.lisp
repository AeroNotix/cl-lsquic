;; TODO: Make this a sub-package, e.g. lsquic.http3
(in-package :lsquic)

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

(defclass http3-client ()
  ((engine-version  :initarg :quic-version :initform (error "You are required to supply a QUIC protocol version string"))
   (engine-settings :initform (foreign-alloc '(:struct lsquic-engine-settings)))
   (engine-api      :initform (foreign-alloc '(:struct lsquic-engine-api)))))

;; TODO: the `lsquic-str2ver` doesn't work. It signals an error
;; because the return value doesn't cast back to what the enum was
;; defined as...
;; e.g. `(lsquic-str2ver "Q050" (make-pointer-to-int 4))` is an error. WTF?
(defparameter version-map
  (alexandria:alist-hash-table
   ;; We only support this for now, because it's all I care
   ;; about. Adding support for others is just a case of adding the
   ;; correct mapping here.
   '(("Q050" . :lsqver-050)) :test #'equal))

(defun str->quic-version (str)
  (let ((key (gethash str version-map)))
    (if key
        (foreign-enum-value 'lsquic-version key)
        (error 'invalid-quic-version :requested str :available version-map))))

(defmethod initialize-instance :after ((client http3-client) &key)
  (with-slots (engine-settings engine-version) client
    (lsquic-engine-init-settings engine-settings LSENG-HTTP)
    ;; TODO: write a with-foreign-alloc macro, if can't borrow one
    ;; from somewhere, instead of this foreign-alloc that we don't
    ;; free.
    (let ((errbuf (foreign-alloc :char :count 256)))
      (with-foreign-slots ((es-versions) engine-settings (:struct lsquic-engine-settings))
        (setf es-versions (logior es-versions (ash 1 (str->quic-version engine-version))))
        (when (not (eq 0 (lsquic-engine-check-settings engine-settings LSENG-HTTP errbuf (make-pointer-to-int 256))))
          (error 'invalid-settings))))))

(defmethod close-client ((client http3-client))
  (with-slots (engine-settings engine-api) client
    (foreign-free engine-settings)
    (foreign-free engine-api)))
