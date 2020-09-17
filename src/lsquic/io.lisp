(in-package :lsquic)

(defclass pipe ()
  ((input :accessor input)
   (output :accessor output)
   (request :initarg :request :accessor request)))

(defmethod initialize-instance :after ((pipe pipe) &key)
  (multiple-value-bind (i o) (sb-posix:pipe)
    (setf (input pipe) i)
    (setf (output pipe) o)))

(defmethod close-pipe ((pipe pipe))
  (sb-posix:close (input pipe))
  (sb-posix:close (output pipe)))

(defcallback write-pipe :void ((ctx :pointer) (buf :pointer) (len :int) (fin :int))
  (sb-posix:write (output (weird-pointers:restore ctx)) buf len))