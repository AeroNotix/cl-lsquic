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
  (let* ((fn (weird-pointers:restore stream-if-ctx))
         (new-stream-ctx (funcall fn lsquic-conn)))
    (weird-pointers:save new-stream-ctx)))

(defcallback cb-on-goaway-received :pointer ((lsquic-conn :pointer))
  (let ((ctx (weird-pointers:restore (lsquic-conn-get-ctx lsquic-conn))))))

(defcallback cb-on-conn-closed :pointer ((lsquic-conn :pointer))
  (let ((ctx (weird-pointers:restore (lsquic-conn-get-ctx lsquic-conn))))))

(defcallback cb-on-new-stream :pointer ((lsquic-conn :pointer) (lsquic-stream :pointer))
  (let ((ctx (weird-pointers:restore (lsquic-conn-get-ctx lsquic-conn))))
    (with-pointer-to-int (one 1)
      (lsquic-stream-wantwrite lsquic-stream 1))))

(defcallback cb-on-read :pointer ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (let ((ctx (weird-pointers:restore (lsquic-conn-get-ctx lsquic-conn))))))

(defcallback cb-on-write :pointer ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer))
  (let ((ctx (weird-pointers:restore lsquic-stream-ctx)))
    ))

(defcallback cb-on-close :pointer ((lsquic-stream :pointer) (lsquic-stream-ctx :pointer)))
(defcallback cb-on-hsk-done :pointer ((lsquic-conn :pointer (lsquic-hsk-status lsquic-hsk-status))))
(defcallback cb-on-new-token :pointer ((lsquic-conn :pointer) (token :pointer) (token_size :int)))
(defcallback cb-on-sess-resume :pointer ((lsquic-conn :pointer) (token :pointer) (token_size :int)))
(defcallback cb-packets-out :int ((packets-out-ctx :pointer) (specs :pointer) (count :int))
  -1)

(defparameter client-callbacks
  (progn
    (let ((lsif (foreign-alloc '(:struct lsquic-stream-if))))
      (with-foreign-slots ((on-new-conn on-conn-closed on-new-stream on-read on-write on-close) lsif (:struct lsquic-stream-if))
        (setf on-new-conn (callback cb-on-new-conn))
        (setf on-conn-closed (callback cb-on-conn-closed))
        (setf on-new-stream (callback cb-on-new-stream))
        (setf on-read (callback cb-on-read))
        (setf on-write (callback cb-on-write))
        (setf on-close (callback cb-on-close))))))
