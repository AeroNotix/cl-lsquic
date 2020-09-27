(in-package :lsquic)


(defclass header ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)))

(defclass request ()
  ((path :initarg :path :initform "/" :accessor path)
   (scheme :initarg :scheme :initform "https" :accessor scheme)
   (headers :initarg :headers :initform nil :accessor headers)
   (authority :initarg :authority :accessor authority)
   (verb :initarg :verb :initform "GET" :accessor verb)
   (body :initarg :body :accessor body)))

(defun make-lsxpack-header (el header)
  (with-foreign-slots ((buf name-len name-offset val-len val-offset) el (:struct lsxpack-header))
    (setf buf (foreign-string-alloc (format nil "~A~A" (name header) (value header))))
    (setf name-len (length (name header)))
    (setf name-offset 0)
    (setf val-len (length (value header)))
    (setf val-offset (length (name header)))))

(defmethod required-headers ((request request))
  (list
   (make-instance 'header :name ":method" :value (verb request))
   (make-instance 'header :name ":scheme" :value (scheme request))
   (make-instance 'header :name ":path" :value (path request))
   (make-instance 'header :name ":authority" :value (authority request))))

(defmethod lsxpack-headers ((request request))
  (let* ((all-headers (nconc (required-headers request) (headers request)))
         (count-hdrs (length all-headers))
         (pheaders (cffi:foreign-alloc '(:struct lsxpack-header) :count count-hdrs))
         (http-headers (cffi:foreign-alloc '(:struct http-headers))))

    (dotimes (i count-hdrs)
      (let ((hdr (mem-aptr pheaders '(:struct lsxpack-header) i)))
        (make-lsxpack-header hdr (elt all-headers i))))

    (with-foreign-slots ((count headers) http-headers (:struct http-headers))
      (setf count count-hdrs)
      (setf headers pheaders))

    http-headers))
