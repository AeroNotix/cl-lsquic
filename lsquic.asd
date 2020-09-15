(defsystem :lsquic
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "FFI Bindings to litespeed QUIC"
  :components ((:file "src/lsquic/package")
               (:file "src/lsquic/ffi")
               (:file "src/lsquic/logging")
               (:file "src/lsquic/sockets")
               (:file "src/lsquic/lsquic"))
  :depends-on (:bordeaux-threads
               :cffi
               :dns
               :cffi-helpers
               :split-sequence
               :weird-pointers
               :udp))
