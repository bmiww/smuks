(in-package :smuks)

(defclass compositor (wl-compositor:dispatch)
  ((surfaces :initform (make-hash-table :test 'equal) :accessor surfaces)))

(defmethod all-surfaces ((compositor compositor)) (alexandria:hash-table-values (surfaces compositor)))
(defmethod all-ready-surfaces ((compositor compositor))
  (let* ((all (all-surfaces compositor))
	 ;; TODO: Once you implement proper damage and don't clear on every frame
	 ;; You could try to filter based on needs-redraw
	 ;; (ready (remove-if-not #'needs-redraw all)))
	 (ready (remove-if-not #'texture all)))
    ready))


(defmethod wl-compositor:create-surface ((compositor compositor) id)
  (let ((surface (wl:mk-if 'surface compositor id)))
    (setf (gethash id (surfaces compositor)) surface)
    surface))


(defmethod wl-compositor:create-region ((compositor compositor) id)
  )
