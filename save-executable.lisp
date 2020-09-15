(progn
  (ql:quickload :http3)
  (sb-ext:save-lisp-and-die
   "main"
   :executable t
   :purify t
   :compression 9))
