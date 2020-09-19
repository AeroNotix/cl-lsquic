(in-package :lsquic)


(defclass header ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)))

(defclass request ()
  ((path :initarg :path :accessor path)
   (scheme :initarg :scheme :accessor scheme)
   (headers :initarg :headers :accessor headers)
   (authority :initarg :authority :accessor authority)
   (verb :initarg :verb :accessor verb)
   (body :initarg :body :accessor body)))

(defmethod make-lsxpack-header ((header header))
  (with-initialize-foreign-struct lsquic:lsxpack-header
    (setf lsquic:buf
          (foreign-string-alloc
           (format nil "~A~A" (name header) (value header))))
    (setf lsquic:name-len (length (name header)))
    (setf lsquic:name-offset 0)
    (setf lsquic:val-len (length (value header)))
    (setf lsquic:val-offset (length (name header)))))

(defmethod required-headers ((request request))
  (list
   (make-instance 'header :name ":verb" :value (verb request))
   (make-instance 'header :name ":path" :value (path request))
   (make-instance 'header :name ":authority" :value (authority request))
   (make-instance 'header :name ":scheme" :value (scheme request))))

(defmethod lsxpack-headers ((request request))
  (let* ((all-headers (nconc (required-headers request) (headers request)))
         (header-count (length all-headers))
         (lsxpack-headers (cffi:foreign-alloc
                           :pointer
                           :count header-count
                           :initial-contents (mapcar #'make-lsxpack-header all-headers))))
    (with-initialize-foreign-struct lsquic::http-headers
      (setf lsquic:count header-count)
      (setf lsquic:headers lsxpack-headers) lsxpack-headers)))
