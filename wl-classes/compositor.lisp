(in-package :smuks)

(defclass compositor (wl-compositor:dispatch)
  ((surfaces :initform (make-hash-table :test 'equal) :accessor surfaces)))


(defmethod wl-compositor:create-surface ((compositor compositor) id)
  (let ((surface (wl:mk-if 'surface compositor id)))
    (setf (gethash id (surfaces compositor)) surface)
    surface))


(defmethod wl-compositor:create-region ((compositor compositor) id)
  )
