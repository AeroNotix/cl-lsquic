(defsystem :lsquic
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "FFI Bindings to litespeed QUIC"
  :components ((:file "src/lsquic-package")
               (:file "src/ffi")
               (:file "src/logging")
               (:file "src/sockets")
               (:file "src/lsquic"))
  :depends-on (:bordeaux-threads
               :cffi
               :dns
               :cffi-helpers
               :split-sequence
               :weird-pointers
               :udp))
