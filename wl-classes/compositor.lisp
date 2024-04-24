(in-package :smuks)

(defclass compositor (wl-compositor:dispatch)
  ())

(defmethod wl-compositor:create-surface ((compositor compositor) id)
  (print "CALLED CREATE SURFACE"))
(defmethod wl-compositor:create-region ((compositor compositor) id)
  (print "CALLED CREATE REGION"))
