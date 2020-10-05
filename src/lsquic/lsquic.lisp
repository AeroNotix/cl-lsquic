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

(defcallback cb-on-new-conn :pointer ((stream-if-ctx :pointer) (conn :pointer))
  stream-if-ctx)

(defcallback cb-ongoaway-received :pointer ((conn :pointer)))

(defcallback cb-on-conn-closed :void ((conn :pointer)))

(defcallback cb-on-new-stream :pointer ((stream-if-ctx :pointer) (stream :pointer))
  (let ((stream-ctx (conn-get-ctx (stream-conn stream))))
    (when (eq (stream-is-pushed stream) 0)
      (stream-wantwrite stream 1)
      (weird-pointers:save (pop-stream-ctx (weird-pointers:restore stream-ctx))))))

(defcallback stream-readf :unsigned-int ((stream-ctx :pointer) (buf :pointer) (buf-len :unsigned-int) (fin :int))
  (format t "in stream-readf: ~A~%" (cffi:foreign-string-to-lisp buf))
  (force-output)
  buf-len)

(defcallback cb-on-read :void ((stream :pointer) (stream-ctx :pointer))
  (let ((ctx (weird-pointers:restore stream-ctx))
        (bytes-read (stream-readf stream (callback stream-readf) stream-ctx)))
    (when (eq (stream-is-rejected stream) 1)
      (stream-close stream))
    (when (eq bytes-read 0)
      (stream-shutdown stream 0)
      (stream-wantread stream 0))))

(defparameter send-headers nil)

(defcallback cb-on-write :void ((stream :pointer) (stream-ctx :pointer))
  (let* ((ctx (weird-pointers:restore stream-ctx))
         (packed-headers (lsxpack-headers (request ctx)))
         (eos 1))
    (unless send-headers
      (stream-send-headers stream packed-headers eos)
      (stream-shutdown stream 1)
      (stream-wantwrite stream 0)
      (stream-wantread stream 1)
      (setf send-headers t))))

(defcallback cb-on-close :void ((stream :pointer) (stream-ctx :pointer)))

(defcallback cb-on-hsk-done :void ((conn :pointer (hsk-status lsquic-hsk-status))))

(defcallback cb-on-new-token :void ((conn :pointer) (token :pointer) (token_size :int)))

(defcallback cb-on-sess-resume-info :void ((conn :pointer) (token :pointer) (token_size :int)))

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
                       (error "short write")))
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
