
;;  ██████╗ ██████╗ ███╗   ███╗██████╗  ██████╗ ███████╗██╗████████╗ ██████╗ ██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔═══██╗██╔════╝██║╚══██╔══╝██╔═══██╗██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║   ██║███████╗██║   ██║   ██║   ██║██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║   ██║╚════██║██║   ██║   ██║   ██║██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ╚██████╔╝███████║██║   ██║   ╚██████╔╝██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝
(in-package :smuks)

(defclass compositor (wl-compositor:dispatch)
  ((surfaces :initform (make-hash-table :test 'equal) :accessor surfaces)))

(defmethod all-surfaces ((compositor compositor)) (reverse (alexandria:hash-table-values (surfaces compositor))))
(defmethod all-ready-surfaces ((compositor compositor))
  (let* ((all (all-surfaces compositor))
	 ;; TODO: Once you implement proper damage and don't clear on every frame
	 ;; You could try to filter based on needs-redraw
	 ;; (ready (remove-if-not #'needs-redraw all)))
	 (ready (remove-if-not #'texture all)))
    (let ((layers nil)
	  (toplevels nil)
	  (popups nil)
	  (cursors nil)
	  (surfaces nil))
      (loop for surface in ready
	    do (typecase surface
		 (layer-surface (push surface layers))
		 (toplevel (push surface toplevels))
		 (popup (push surface popups))
		 (cursor (push surface cursors))
		 (t (push surface surfaces))))
      (list layers toplevels popups cursors surfaces))))

(defmethod all-popups ((compositor compositor))
  (remove-if-not (lambda (surf) (and (typep surf 'popup) (texture surf))) (all-surfaces compositor)))

(defmethod wl-compositor:create-surface ((compositor compositor) id)
  (let ((surface (wl:mk-if 'surface compositor id :compositor compositor)))
    (setf (gethash id (surfaces compositor)) surface)
    surface))

(defmethod wl-compositor:create-region ((compositor compositor) id)
  (wl:mk-if 'region compositor id))

(defmethod toplevel-surface ((compositor compositor))
  (loop for value being the hash-values of (surfaces compositor)
	when (typep value 'toplevel)
	return value))

(defmethod rem-surface ((compositor compositor) surface)
  (remhash (wl-surface::wl_surface-id surface) (surfaces compositor)))

;; ┬─┐┌─┐┌─┐┬┌─┐┌┐┌
;; ├┬┘├┤ │ ┬││ ││││
;; ┴└─└─┘└─┘┴└─┘┘└┘
(defclass region (wl-region:dispatch)
  ((rectangles :initform nil :accessor rectangles)))

(defmethod wl-region:add ((region region) x y width height)
  (push (list x y width height) (rectangles region)))

(defmethod wl-region:subtract ((region region) x y width height)
  (setf (rectangles region) (remove (list x y width height) (rectangles region))))


;; ███████╗██╗   ██╗██████╗  ██████╗ ██████╗ ███╗   ███╗██████╗  ██████╗ ███████╗██╗████████╗ ██████╗ ██████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔═══██╗██╔════╝██║╚══██╔══╝██╔═══██╗██╔══██╗
;; ███████╗██║   ██║██████╔╝██║     ██║   ██║██╔████╔██║██████╔╝██║   ██║███████╗██║   ██║   ██║   ██║██████╔╝
;; ╚════██║██║   ██║██╔══██╗██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║   ██║╚════██║██║   ██║   ██║   ██║██╔══██╗
;; ███████║╚██████╔╝██████╔╝╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ╚██████╔╝███████║██║   ██║   ╚██████╔╝██║  ██║
;; ╚══════╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝
(defclass subcompositor (wl-subcompositor:dispatch)
  ())

(defmethod wl-subcompositor:get-subsurface ((subcompositor subcompositor) id surface parent)
  (wl:up-if 'subsurface surface id :parent parent))
