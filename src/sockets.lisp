(in-package :lsquic)


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


(defun create-udp-socket (host &key (port 443))
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
        (addr (nth 0 (get-peer-address-for-host host))))
    (sb-bsd-sockets:socket-connect socket (sb-bsd-sockets:host-ent-address addr) port))

