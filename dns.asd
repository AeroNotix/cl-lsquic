(defsystem :dns
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "Little CFFI functions"
  :components ((:file "src/dns/ffi"))
  :depends-on (:cffi))
