(defsystem :http3
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "High-level HTTP3 Client"
  :components ((:file "package")
               (:file "http3"))
  :depends-on (:bordeaux-threads
               :cffi
               :cffi-helpers
               :lsquic
               :static-vectors
               :weird-pointers
               :usocket))
