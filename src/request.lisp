(in-package :http3)


(defclass header ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)))

(defclass request ()
  ((path :initarg :path :accessor path)
   (headers :initarg :headers :accessor headers)
   (body :initarg :body :accessor body)
   (verb :initarg :verb :accessor verb)))

(defmethod lsxpack-headers ((request request))
  (let* ((header-count (length (headers request)))
         (lsxpack-header (safe-foreign-alloc '(:struct lsxpack-header) :count header-count))
         (lsquic-headers (with-initialize-foreign-struct lsquic-http-headers
                           (setf count header-count)
                           (setf headers lsxpack-header))))
    lsquic-headers))


