(in-package :lsquic)

;; The stream interface in lsquic requires callbacks for certain parts
;; of the connection.
;;
;; I don't want to make users of this library need to know that it is
;; written with CFFI.
;;
;; Therefore, we have a little indirection. We use the weird-pointers
;; library which wraps a funcallable Lisp object into a native pointer
;; and this is what we pass to lsquic as the context.
;;
;; Each callback then pulls that back out and tries to use it.

(defparameter already-lsquic-global-init nil)

(defcallback cb-on-new-conn :pointer ((stream-if-ctx :pointer) (lsquic-conn :pointer))
  (format t "cb-on-new-conn: ~D  / ~A~%" (conn-n-avail-streams lsquic-conn) (callback cb-on-new-conn))
  stream-if-ctx)

(defcallback cb-ongoaway-received :pointer ((lsquic-conn :pointer))
  (format t "cb-ongoaway-received~%"))

(defcallback cb-on-conn-closed :void ((lsquic-conn :pointer))
  (format t "cb-on-conn-closed~%")
  (force-output))

(defcallback cb-on-new-stream :pointer ((stream-if-ctx :pointer) (lsquic-stream :pointer))
  (format t "cb-on-new-stream~%")
  (force-output)
  (let ((stream-ctx (conn-get-ctx (stream-conn lsquic-stream))))
    (when (not (eq (stream-is-pushed lsquic-stream) 0))
      (stream-wantwrite lsquic-stream 1))
    stream-if-ctx))

(defcallback cb-on-read :void ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (format t "cb-on-read~%")
  (force-output))

(defcallback cb-on-write :void ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (format t "cb-on-write~%")
  (force-output)
  (let ((ctx (weird-pointers:restore lsquic-stream-ctx)))
    (on-write ctx)))

(defcallback cb-on-close :void ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (format t "cb-on-close~%") (force-output))

(defcallback cb-on-hsk-done :void ((lsquic-conn :pointer (lsquic-hsk-status lsquic-hsk-status)))
  (format t "cb-on-hsk-done~%") (force-output))

(defcallback cb-on-new-token :void ((lsquic-conn :pointer) (token :pointer) (token_size :int))
  (format t "cb-on-new-token~%") (force-output))

(defcallback cb-on-sess-resume-info :void ((lsquic-conn :pointer) (token :pointer) (token_size :int))
  (format t "cb-on-sess-resume~%") (force-output))

(defcallback cb-packets-out :int ((packets-out-ctx :pointer) (specs :pointer) (count :int))
  (declare (ignore packets-out-ctx))
  (let ((n 0))
    (loop while (< n count)
          do
             (progn
               (with-foreign-slots ((iov iovlen dest-sa peer-ctx) specs (:struct out-spec))
                 (let ((client (weird-pointers:restore peer-ctx)))
                   (let ((count (udp:send-packets-out dest-sa iov iovlen (sb-bsd-sockets:socket-file-descriptor (socket (socket client))))))
                     (when (< count 0)
                       (error)))
                   (incf n)
                   (cffi:incf-pointer specs)))))
    (if (> n 0)
        n
        -1)))

(defparameter client-callbacks
  (progn
    (let ((lsif (cffi-helpers:safe-foreign-alloc '(:struct stream-if))))
      (with-foreign-slots ((on-new-conn
                            on-goaway-received
                            on-conn-closed
                            on-new-stream
                            on-read
                            on-write
                            on-close
                            on-hsk-done
                            on-new-token
                            on-sess-resume-info) lsif (:struct stream-if))
        (setf on-new-conn (callback cb-on-new-conn))
        (setf on-goaway-received (callback cb-ongoaway-received))
        (setf on-conn-closed (callback cb-on-conn-closed))
        (setf on-new-stream (callback cb-on-new-stream))
        (setf on-read (callback cb-on-read))
        (setf on-write (callback cb-on-write))
        (setf on-close (callback cb-on-close))
        (setf on-hsk-done (callback cb-on-hsk-done))
        (setf on-new-token (callback cb-on-new-token))
        (setf on-sess-resume-info (callback cb-on-sess-resume-info))
        lsif))))

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

(defclass client (socket)
  ((lock            :initform (bt:make-lock) :accessor lock)
   (host            :initarg :host :initform (error "You must supply a host to connect to"))
   (port            :initarg :port :initform 443)
   (engine-version  :initarg :quic-version :initform (error "You are required to supply a QUIC protocol version string")
                    :accessor engine-version)
   (engine-settings :initform (cffi-helpers:safe-foreign-alloc '(:struct engine-settings))
                    :accessor engine-settings)
   (engine-api      :initform (cffi-helpers:safe-foreign-alloc '(:struct engine-api))
                    :accessor engine-api)
   (engine          :accessor engine)
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
        (let ((conn (lsquic:engine-connect engine
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
  (with-slots (engine-settings engine-api) client
    (foreign-free engine-settings)
    (foreign-free engine-api)))

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

(defmethod on-write ((client client))
  (write-headers (headers client) client)
  (write-body (body client) client))
