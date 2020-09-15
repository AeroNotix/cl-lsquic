(defsystem :dns
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "Helpful DNS functions where Lisp doesn't efficiently do the right things"
  :components ((:file "src/dns/ffi"))
  :depends-on (:cffi))
