(in-package :lsquic)

(defun calculate-next-tick (diff)
  (coerce (if (>= diff DF-CLOCK-GRANULARITY)
              (/ diff 1000000)
              (if (<= diff 0)
                  5d0
                  (/ DF-CLOCK-GRANULARITY 1000000)))
          'double-float))

(defcallback process-conns-cb :void ((evloop :pointer) (timer :pointer) (revents :int))
  (declare (ignore revents))
  (format t "process-conns-cb~%")
  (with-foreign-slots ((lev::data) timer (:struct lev:ev-timer))
    (let ((client (weird-pointers:restore lev::data)))
      (with-slots (engine) client
        (lev:ev-timer-stop evloop timer)
        (lsquic:engine-process-conns engine)
        (with-pointer-to-int (diff 0)
          (when (> (lsquic:engine-earliest-adv-tick engine diff) 0)
            (format t "diff ~D~%" (calculate-next-tick (cffi:mem-ref diff :int)))
            (lev:ev-timer-init timer 'process-conns-cb (calculate-next-tick (cffi:mem-ref diff :int)) 0d0)
            (lev:ev-timer-start evloop timer)))))))

(defcallback packets-in-cb :void ((evloop :pointer) (io :pointer) (revents :int))
  (declare (ignore revents evloop))
  (with-foreign-slots ((lev::data) io (:struct lev:ev-io))
    (let ((client (weird-pointers:restore lev::data)))
      (with-slots (socket engine peer-ctx) client
        (udp:recv-packets-in
         engine
         (local-sockaddr socket)
         (get-fd client)
         peer-ctx)))))
