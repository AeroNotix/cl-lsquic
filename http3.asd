(defsystem :http3
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "FFI Bindings to litespeed QUIC"
  :components ((:file "src/package")
               (:file "src/ffi-dns")
               (:file "src/ffi-udp")
               (:file "src/http3"))
  :depends-on (:bordeaux-threads
               :cffi
               :cffi-helpers
               :lsquic
               :static-vectors
               :weird-pointers
               :usocket))
