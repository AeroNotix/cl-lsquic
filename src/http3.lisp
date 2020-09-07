;; TODO: Make this a sub-package, e.g. lsquic.http3
(in-package :lsquic)

(define-condition library-error-null-p (error)
  ((slot-name :initarg :slot-name))
  (:documentation "Signalled when a pointer is null, but shouldn't be"))

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

(defmacro safe-foreign-alloc (type)
  `(static-vectors:fill-foreign-memory (foreign-alloc ,type) (foreign-type-size ,type) 0))

(defclass http3-client ()
  ((engine-version  :initarg :quic-version :initform (error "You are required to supply a QUIC protocol version string"))
   (engine-settings :initform (safe-foreign-alloc '(:struct lsquic-engine-settings)))
   (engine-api      :initform (safe-foreign-alloc '(:struct lsquic-engine-api)))))

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

(defparameter already-lsquic-global-init nil)

(defun str->quic-version (str)
  (let ((key (gethash str version-map)))
    (if key
        (foreign-enum-value 'lsquic-version key)
        (error 'invalid-quic-version :requested str :available version-map))))

(defmacro check-null-p (sym)
  `(when (cffi:null-pointer-p engine-settings)
    (error 'library-error :slot-name ,sym)))

(defmethod initialize-instance :after ((client http3-client) &key)
  (unless already-lsquic-global-init
    (setf already-lsquic-global-init t)
    (lsquic-global-init LSQUIC-GLOBAL-CLIENT))
  (with-slots (engine-settings engine-version engine-api) client
    (check-null-p engine-settings)
    (check-null-p engine-api)
    (lsquic-engine-init-settings engine-settings LSENG-HTTP)
    ;; TODO: write a with-foreign-alloc macro, if can't borrow one
    ;; from somewhere, instead of this foreign-alloc that we don't
    ;; free.
    (let ((errbuf (foreign-alloc :char :count 256)))
      (with-foreign-slots ((es-versions) engine-settings (:struct lsquic-engine-settings))
        (with-pointer-to-int (bufsize 256)
          (setf es-versions (logior es-versions (ash 1 (str->quic-version engine-version))))
          (when (not (eq 0 (lsquic-engine-check-settings engine-settings LSENG-HTTP errbuf bufsize)))
            (error 'invalid-settings))
          (lsquic-logger-init logger-if (weird-pointers:save :nil) (foreign-enum-value 'lsquic-logger-timestamp-style :LLTS-HHMMSSUS))
          (with-foreign-slots ((ea-settings ea-packets-out ea-packets-out-ctx ea-stream-if ea-stream-if-ctx) engine-api (:struct lsquic-engine-api))
            (setf ea-settings engine-settings)
            (setf ea-stream-if client-callbacks)
            (setf ea-stream-if-ctx (weird-pointers:save client))
            ;(setf ea-packets-out (callback cb-packets-out))
            (setf ea-packets-out-ctx (weird-pointers:save client)))
          (let ((engine (lsquic-engine-new LSENG-HTTP engine-api)))
            (check-null-p engine)))))))

(defmethod close-client ((client http3-client))
  (with-slots (engine-settings engine-api) client
    (foreign-free engine-settings)
    (foreign-free engine-api)))
