(in-package :smuks)

(defclass surface (wl-surface:dispatch)
  ())

(defmethod wl-surface:commit ((surface surface))
  )
