(in-package :http3)

(defgeneric stream-read (stream buf count)
  (:documentation "An implementation of a stream of data, writes count bytes to buf"))

(defgeneric stream-remaining (stream)
  (:documentation "Returns the amount of bytes remaining on a stream"))

(defclass streamer () ())
