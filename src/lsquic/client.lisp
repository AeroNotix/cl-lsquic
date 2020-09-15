(in-package :lsquic)

(define-condition library-error-null-p (error)
  ((slot-name :initarg :slot-name))
  (:documentation "Signalled when a pointer is null, but shouldn't be"))

(defmacro check-null-p (sym)
  `(when (cffi:null-pointer-p ,sym)
     (error 'library-error-null-p :slot-name ,sym)))

;; TODO: the `lsquic-str2ver` doesn't work. It signals an error
;; because the return value doesn't cast back to what the enum was
;; defined as...
;; e.g. `(lsquic:str2ver "Q050" (make-pointer-to-int 4))` is an error. WTF?
(defparameter version-map
  (alexandria:alist-hash-table
   ;; We only support this for now, because it's all I care
   ;; about. Adding support for others is just a case of adding the
   ;; correct mapping here.
   '(("Q050" . :LSQVER-050)) :test #'equal))

(defun str->quic-version (str)
  (let ((key (gethash str version-map)))
    (if key
        (foreign-enum-value 'version key)
        (error 'invalid-quic-version :requested str :available version-map))))

(defclass pipe ()
  ((input :accessor input)
   (output :accessor output)
   (ctx :initarg :ctx :accessor ctx)))

(defmethod initialize-instance :after ((pipe pipe) &key)
  (multiple-value-bind (i o) (sb-posix:pipe)
    (setf (input pipe) i)
    (setf (output pipe) o)))

(defmethod close-pipe ((pipe pipe))
  (sb-posix:close (input pipe))
  (sb-posix:close (output pipe)))

(defcallback write-pipe :void ((ctx :pointer) (buf :pointer) (len :int) (fin :int))
  (sb-posix:write (output (weird-pointers:restore ctx)) buf len))

(defclass client (socket)
  (;; This lock protects the request-queue
   (rq-lock         :initform (bt:make-lock) :accessor rq-lock)
   (request-queue   :initform '() :accessor request-queue)
   (host            :initarg :host :initform (error "You must supply a host to connect to"))
   (port            :initarg :port :initform 443)
   (engine-version  :initarg :quic-version :initform (error "You must supply a QUIC protocol version string")
                    :accessor engine-version)
   (engine-settings :initform (cffi-helpers:safe-foreign-alloc '(:struct engine-settings))
                    :accessor engine-settings)
   (engine-api      :initform (cffi-helpers:safe-foreign-alloc '(:struct engine-api))
                    :accessor engine-api)
   (engine          :accessor engine)
   ;; This lock protects the quic-conn
   (lock            :initform (bt:make-lock) :accessor lock)
   (quic-conn       :accessor quic-conn)
   (peer-ctx        :accessor peer-ctx)
   (conn-ctx        :accessor conn-ctx)
   (stream-if-ctx   :accessor packets-if-ctx)
   (packets-out-ctx :accessor packets-out-ctx)
   (log-level       :initarg :log-level :initform "info")))

(defmethod initialize-instance :after ((client client) &key)
  (unless already-lsquic-global-init
    (setf already-lsquic-global-init t)
    (global-init global-client))
  (with-slots (engine-settings engine-version engine-api log-level wp-self) client
    (check-null-p engine-settings)
    (check-null-p engine-api)
    (engine-init-settings engine-settings lseng-http)
    ;; TODO: write a with-foreign-alloc macro, if can't borrow one
    ;; from somewhere, instead of this foreign-alloc that we don't
    ;; free.
    (let ((errbuf (foreign-alloc :char :count 256)))
      (with-foreign-slots ((es-versions es-ql-bits) engine-settings (:struct engine-settings))
        (cffi-helpers:with-pointer-to-int (bufsize 256)
          (setf es-ql-bits 0)
          (setf es-versions (logior es-versions (ash 1 (str->quic-version engine-version))))
          (when (not (eq 0 (engine-check-settings engine-settings lseng-http errbuf bufsize)))
            (error 'invalid-settings))
          (logger-initialize log-level)
          (with-foreign-slots ((ea-settings
                                ea-packets-out
                                ea-packets-out-ctx
                                ea-stream-if
                                ea-stream-if-ctx) engine-api (:struct engine-api))
            (setf ea-settings engine-settings)
            (setf ea-stream-if client-callbacks)
            (setf ea-packets-out (callback cb-packets-out))))))))

(defmethod set-context ((client client))
  (format t "~A~%" client)
  (with-slots (peer-ctx conn-ctx stream-if-ctx packets-out-ctx engine-api) client
    (let ((wp (weird-pointers:save client)))
      (setf peer-ctx wp)
      (setf conn-ctx wp)
      (setf stream-if-ctx wp)
      (setf packets-out-ctx wp)
      (with-foreign-slots ((ea-packets-out-ctx ea-stream-if-ctx) engine-api (:struct engine-api))
        (setf ea-stream-if-ctx wp)
        (setf ea-packets-out-ctx wp)))))

(defmethod quic-connect ((client client))
  (with-slots (host port engine socket engine-version quic-conn peer-ctx conn-ctx) client
    (with-pointer-to-int (zero 0)
      (let* ((version (str->quic-version engine-version))
             (udp-socket (create-udp-socket host :port port)))
        (setf socket udp-socket)
        (let ((conn (engine-connect
                     engine
                     version
                     (local-sockaddr udp-socket)
                     (peer-sockaddr udp-socket)
                     peer-ctx
                     conn-ctx
                     host
                     0
                     (cffi:null-pointer)
                     zero
                     (cffi:null-pointer)
                     zero)))
          (check-null-p conn)
          (setf quic-conn conn)))))
  (sb-ext:schedule-timer
   (sb-ext:make-timer (lambda () (process-conns client)) :thread t)
   0.1)
  client)

(defmethod new-engine ((client client))
  (with-slots (engine-api) client
    (let ((engine (engine-new lseng-http engine-api)))
      (check-null-p engine)
      (setf (engine client) engine))))

(defmethod close-client ((client client))
  (with-slots (engine) client
    (engine-destroy engine)))

(defmethod process-conns ((client client))
  (bt:with-lock-held ((lock client))
    (with-slots (engine) client
      (lsquic:engine-process-conns engine)
      (with-pointer-to-int (diff 0)
        (when (> (lsquic:engine-earliest-adv-tick engine diff) 0)
          (sb-ext:schedule-timer
           (sb-ext:make-timer (lambda () (process-conns client)) :thread t)
           (/ (mem-aref diff :int) 1000000)))))))

(defmethod packets-in ((client client))
  (bt:with-lock-held ((lock client))
    (with-slots (socket engine peer-ctx) client
      (let ((read (udp:recv-packets-in engine (local-sockaddr socket) (sb-bsd-sockets:socket-file-descriptor (socket socket)) peer-ctx)))
        (when (>= read 0)
          (lsquic:engine-process-conns engine)
          (sb-ext:schedule-timer
           (sb-ext:make-timer (lambda () (packets-in client)) :thread t)
           0.3))))))

(defmethod push-stream-ctx ((client client) ctx)
  (bt:with-lock-held ((rq-lock client))
    (push ctx (request-queue client))))

(defmethod pop-stream-ctx ((client client))
  (bt:with-lock-held ((rq-lock client))
    (pop (request-queue client))))

(defmethod new-stream ((client client) ctx)
  (let ((pipe (make-instance 'pipe :ctx ctx)))
    (push-stream-ctx client pipe)
    (conn-make-stream (quic-conn client))
    pipe))

(defmethod send-request ((client client) request)
  (let ((pipe (new-stream client request)))
     pipe))

