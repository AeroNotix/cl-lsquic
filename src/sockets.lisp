(in-package :lsquic)

(defclass socket ()
  ((socket :initarg :socket :accessor socket)
   (local-sockaddr :initarg :local-sockaddr :accessor local-sockaddr)
   (peer-sockaddr :initarg :peer-sockaddr :accessor peer-sockaddr)))

(defun get-peer-address-for-host (host)
  (let ((addrs (multiple-value-list (sb-bsd-sockets:get-host-by-name host))))
    (loop for addr in addrs
          when (sb-bsd-sockets:host-ent-address addr)
            collect addr)))

(defun parse-ip-from-string (ip delimiter byte-spec &optional (radix 10))
  (map 'vector (lambda (octet)
                 (ldb byte-spec (parse-integer octet :radix radix)))
       (split-sequence:split-sequence delimiter ip)))

(defun ip-to-byte-array (ip)
  (parse-ip-from-string ip #\. (byte 8 0)))

(defun ip-vec-to-string (ip)
  "Pretty devious. If I do say so myself."
  (format nil "~{~D~^.~}" (coerce ip 'list)))

(defun as-sockaddr (socket-family ip-port)
  (let* ((ip (nth 0 ip-port))
         (port (nth 1 ip-port))
         (ip-str (ip-vec-to-string ip)))
    (with-pointer-to-int (p port)
      (gen-sockaddr socket-family ip-str p))))

(defun create-udp-socket (host &key (port 443))
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
         (addr (nth 0 (get-peer-address-for-host host)))
         (socket-family (sb-bsd-sockets:socket-family socket)))
    (sb-bsd-sockets:socket-connect socket (sb-bsd-sockets:host-ent-address addr) port)
    (let ((local-sockaddr (as-sockaddr socket-family (multiple-value-list (sb-bsd-sockets:socket-name socket))))
          (peer-sockaddr (as-sockaddr socket-family (multiple-value-list (sb-bsd-sockets:socket-peername socket)))))
      (make-instance 'socket :socket socket :local-sockaddr local-sockaddr :peer-sockaddr peer-sockaddr))))

(defmethod close-udp-socket ((socket socket))
  (with-slots (local-sockaddr peer-sockaddr) socket
    (foreign-free local-sockaddr)
    (foreign-free peer-sockaddr)))

