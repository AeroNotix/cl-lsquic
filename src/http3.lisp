;; TODO: Make this a sub-package, e.g. lsquic.http3
(in-package :lsquic)

(defclass http3-client ()
  ((engine-settings :initform (foreign-alloc '(:struct lsquic-engine-settings)))))

(defmethod initialize-instance :after ((client http3-client) &key)
  (with-slots (engine-settings) client
    (lsquic-engine-init-settings engine-settings LSENG-HTTP)))

(defmethod close-client ((client http3-client))
  (with-slots (engine-settings) client
    (foreign-free engine-settings)))
