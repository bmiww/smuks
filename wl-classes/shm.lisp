(in-package :smuks)

(defclass shm (wl-shm:dispatch)
  ())

(defmethod wl-shm:create-pool ((shm shm) id fd size)
  (print "create-pool"))
