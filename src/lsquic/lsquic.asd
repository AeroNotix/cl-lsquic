(defsystem :lsquic
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "FFI Bindings to litespeed QUIC"
  :components ((:file "package")
               (:file "ffi")
               (:file "ffi-helpers")
               (:file "logging")
               (:file "sockets")
               (:file "lsquic")
               (:file "request")
               (:file "io")
               (:file "client"))
  :depends-on (:bordeaux-threads
               :cffi
               :dns
               :cffi-helpers
               :split-sequence
               :weird-pointers
               :udp))
