(defsystem :udp
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "Little CFFI functions"
  :components ((:file "src/ffi-udp"))
  :depends-on (:cffi))
