## cl-lsquic

Common Lisp bindings to the https://github.com/litespeedtech/lsquic library.

Intended to be able to provide an HTTP<>HTTP3 reverse proxy. We are not there
yet.

```lisp

;; Load library.
(ql:quickload :lsquic)

;; Make an HTTP3 client to google.
(let ((http3-client (make-instance 'lsquic:http3-client :quic-version "Q050" :host "google.com")))
 ;; Watch that things don't completely explode!
 (lsquic:quic-connect http3-client))
```
