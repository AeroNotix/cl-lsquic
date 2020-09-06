;; TODO: Make this a sub-package, e.g. lsquic.http3
(in-package :lsquic)

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
   '(("Q050" . :lsqver-050)) :test #'equal))

(defun str->quic-version (str)
  (let ((key (gethash str version-map)))
    (when key
      (foreign-enum-value 'lsquic-version key))))

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
          (format t "fucked it"))))))

(defmethod close-client ((client http3-client))
  (with-slots (engine-settings) client
    (foreign-free engine-settings)))
