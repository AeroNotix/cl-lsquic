(defsystem :cffi-helpers
  :author "Aaron France"
  :version "0.0.1"
  :licence "WTFPL"
  :serial t
  :description "Little CFFI functions"
  :components ((:file "tyranny"))
  :depends-on (:cffi
               :static-vectors))
