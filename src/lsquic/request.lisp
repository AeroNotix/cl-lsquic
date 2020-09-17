(in-package :lsquic)


(defclass header ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)))

(defmethod make-lsxpack-header ((header header))
  (format t "name len ~D value len ~D~%" (length (name header)) (length (value header)))
  (let ((lheader (with-initialize-foreign-struct lsquic:lsxpack-header
                   (setf lsquic:buf (foreign-string-alloc (format nil "~A~A" (name header) (value header)) :null-terminated-p nil))
                   (setf lsquic:name-len (length (name header)))
                   (setf lsquic:name-offset 0)
                   (setf lsquic:val-len (length (value header)))
                   (setf lsquic:val-offset (length (name header)))
                   )))

    (with-foreign-slots ((name-len name-offset val-len val-offset) lheader (:struct lsquic:lsxpack-header))
        (format t "name len ~D name offset ~D value len ~D value offset ~D~%" name-len name-offset val-len val-offset))

    ;; (let ((buf (foreign-string-alloc (format nil "~A~A" (name header) (value header)))))

    ;;   (lsxpack-header-set-offset2
    ;;    lheader
    ;;    buf
    ;;    0
    ;;    (length (name header))
    ;;    (length (name header))
    ;;    (length (value header))))

    ;; (with-foreign-slots ((name-len name-offset val-len val-offset) lheader (:struct lsquic:lsxpack-header))
    ;;   (format t "name len ~D name offset ~D value len ~D value offset ~D~%" name-len name-offset val-len val-offset))
    (format t "~A~%" (lsxpack-header-get-name lheader))
    (format t "~A~%" (lsxpack-header-get-value lheader))
    lheader))

(defclass request ()
  ((path :initarg :path :accessor path)
   (scheme :initarg :scheme :accessor scheme)
   (headers :initarg :headers :accessor headers)
   (authority :initarg :authority :accessor authority)
   (verb :initarg :verb :accessor verb)
   (body :initarg :body :accessor body)))

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
