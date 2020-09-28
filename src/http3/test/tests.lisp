(defpackage #:http3/test
  (:use :cl :http3 :fiveam))

(in-package #:http3/test)
(def-suite :http3)
(in-suite :http3)

;; (test can-create-request
;;   (let ((request (make-instance 'lsquic:request :authority "google.com")))
;;     (is (lsquic:lsxpack-headers request))))

(test can-make-network-request
  (let ((client (make-instance 'lsquic:client
                               :quic-version "Q050"
                               :log-level "info"
                               :host "google.com")))
    (http3:connect client)
    (http3:make-get-request client :path "/")))


;; (test header-packer
;;   (let* ((request (make-instance 'lsquic:request :authority "google.com"))
;;          (hdrs (sb-sys:without-gcing (lsquic:lsxpack-headers request))))
;;     (is (eq 1 0))))
