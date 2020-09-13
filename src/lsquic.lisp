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

(defcallback cb-on-new-conn :pointer ((stream-if-ctx :pointer) (lsquic-conn :pointer))
  (format t "cb-on-new-conn~%")
  (let* ((client (weird-pointers:restore stream-if-ctx)))
    (new-stream-ctx client lsquic-conn)))

(defcallback cb-on-goaway-received :pointer ((lsquic-conn :pointer))
  (format t "cb-on-goaway-received~%"))

(defcallback cb-on-conn-closed :void ((lsquic-conn :pointer))
  (format t "cb-on-conn-closed~%")
  (force-output))

(defcallback cb-on-new-stream :pointer ((lsquic-conn :pointer) (lsquic-stream :pointer))
  (let ((ctx (weird-pointers:restore (lsquic-conn-get-ctx lsquic-conn))))
    (with-pointer-to-int (one 1)
      (lsquic-stream-wantwrite lsquic-stream 1)))
  lsquic-conn)

(defcallback cb-on-read :pointer ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (format t "cb-on-read~%")
  (force-output)
  ;; (let ((ctx (weird-pointers:restore (lsquic-conn-get-ctx lsquic-conn)))))
  )

(defcallback cb-on-write :pointer ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (format t "cb-on-write~%")
  (force-output)
  ;; (let ((ctx (weird-pointers:restore lsquic-stream-ctx)))
  ;;   )
  )

(defcallback cb-on-close :pointer ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (format t "cb-on-close~%") (force-output))
(defcallback cb-on-hsk-done :pointer ((lsquic-conn :pointer (lsquic-hsk-status lsquic-hsk-status)))
  (format t "cb-on-hsk-done~%") (force-output))
(defcallback cb-on-new-token :pointer ((lsquic-conn :pointer) (token :pointer) (token_size :int))
  (format t "cb-on-new-token~%") (force-output))
(defcallback cb-on-sess-resume :pointer ((lsquic-conn :pointer) (token :pointer) (token_size :int))
  (format t "cb-on-sess-resume~%") (force-output))
(defcallback cb-packets-out :int ((packets-out-ctx :pointer) (specs :pointer) (count :int))
  (declare (ignore packets-out-ctx))
  (let ((n 0))
    (loop while (< n count)
          do
             (progn
               (with-foreign-slots ((iov iovlen dest-sa peer-ctx) specs (:struct lsquic-out-spec))
                 (let ((client (weird-pointers:restore peer-ctx)))
                   (let ((count (send-packets-out dest-sa iov iovlen (sb-bsd-sockets:socket-file-descriptor (socket (socket client))))))
                     (when (< count 0)
                       (error)))
                   (incf n)
                   (cffi:incf-pointer specs)))))
    (if (> n 0)
        n
        -1)))

(defparameter client-callbacks
  (progn
    (let ((lsif (safe-foreign-alloc '(:struct lsquic-stream-if))))
      (with-foreign-slots ((on-new-conn
	                        on-goaway-received
	                        on-conn-closed
	                        on-new-stream
	                        on-read
	                        on-write
	                        on-close
	                        on-hsk-done
	                        on-new-token
                            on-sess-resume-info) lsif (:struct lsquic-stream-if))
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
