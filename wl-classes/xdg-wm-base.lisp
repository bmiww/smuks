(in-package :smuks)


(defclass xdg-wm-base (xdg-wm-base:dispatch)
  ())

(defmethod xdg-wm-base:get-xdg-surface ((xdg xdg-wm-base) id surface)
  ())
