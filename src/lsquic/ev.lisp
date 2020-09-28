(in-package :lsquic)

(defcallback process-conns-cb :void ((evloop :pointer) (timer :pointer) (revents :int))
  (declare (ignore revents))
  (with-foreign-slots ((lev::data) timer (:struct lev:ev-timer))
    (let ((client (weird-pointers:restore lev::data)))
      (with-slots (engine) client
        (lev:ev-timer-stop evloop timer)
        (lsquic:engine-process-conns engine)
        (with-pointer-to-int (diff 0)
          (when (> (lsquic:engine-earliest-adv-tick engine diff) 0)
            (lev:ev-timer-init evloop (callback process-conns-cb) diff 0)))))))

(defcallback packets-in-cb :void ((evloop :pointer) (io :pointer) (revents :int))
  (declare (ignore revents evloop))
  ;; TODO: Switch to libev
  (with-foreign-slots ((lev::data) io (:struct lev:ev-io))
    (let ((client (weird-pointers:restore lev::data)))
      (with-slots (socket engine peer-ctx) client
        (udp:recv-packets-in
         engine
         (local-sockaddr socket)
         (get-fd client)
         peer-ctx)))))
