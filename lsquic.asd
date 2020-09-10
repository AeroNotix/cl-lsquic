(defsystem :lsquic
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "FFI Bindings to litespeed QUIC"
  :components ((:file "src/package")
               (:file "src/ffi")
               (:file "src/ffi-dns")
               (:file "src/ffi-udp")
               (:file "src/tyranny")
               (:file "src/logging")
               (:file "src/lsquic")
               (:file "src/sockets")
               (:file "src/http3"))
  :depends-on (:cffi
               :bordeaux-threads
               :static-vectors
               :weird-pointers
               :usocket))
