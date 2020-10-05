## cl-lsquic

Common Lisp bindings to the https://github.com/litespeedtech/lsquic library.

Intended to be able to provide an HTTP<>HTTP3 reverse proxy. We are not there
yet.

```lisp
;; Load library.
(ql:quickload :http3)

;; Make an HTTP3 client to google.
(let ((client (make-instance 'lsquic:client :quic-version "Q050" :host "google.com")))
 (http3:connect client)
 (http3:make-get-request client :path "/"))
```
