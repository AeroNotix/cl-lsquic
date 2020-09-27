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

(defun make-lsxpack-header (header)
  (with-initialize-foreign-struct lsxpack-header
    (setf buf (foreign-string-alloc (format nil "~A~A" (name header) (value header))))
    (setf name-len (length (name header)))
    (setf name-offset 0)
    (setf val-len (length (value header)))
    (setf val-offset (length (name header)))))

(defmethod required-headers ((request request))
  (list
   (make-instance 'header :name ":verb" :value (verb request))
   (make-instance 'header :name ":path" :value (path request))
   (make-instance 'header :name ":authority" :value (authority request))
   (make-instance 'header :name ":scheme" :value (scheme request))))

(defmethod lsxpack-headers ((request request))
  (let* ((all-headers (nconc (required-headers request) (headers request)))
         (lheaders (mapcar #'make-lsxpack-header all-headers))
         (pheaders (cffi:foreign-alloc '(:struct lsxpack-header) :count (length all-headers)))
         (http-headers (cffi:foreign-alloc '(:struct http-headers))))
    (with-foreign-slots ((count headers) http-headers (:struct http-headers))
      (setf headers pheaders)
      (setf count 4))
    (dotimes (i (length all-headers))
      (setf (mem-aref pheaders '(:struct lsxpack-header) i) (mem-ref (elt lheaders i) '(:struct lsxpack-header))))
    (view-header http-headers)
    http-headers))
