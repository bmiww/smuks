
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
  (let* ((all (all-surfaces compositor)))
    (remove-if-not #'texture all)))

(defmethod all- ((compositor compositor) class)
  (remove-if-not (lambda (surf) (typep surf class)) (all-ready-surfaces compositor)))

(defmethod all-popups ((compositor compositor)) (all- compositor 'popup))
(defmethod all-layers ((compositor compositor)) (all- compositor 'layer-surface))
(defmethod all-toplevels ((compositor compositor)) (all- compositor 'toplevel))
(defmethod all-subsurfaces ((compositor compositor)) (all- compositor 'subsurface))
(defmethod all-cursors ((compositor compositor)) (all- compositor 'cursor))

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



;; ██████╗ ███████╗███████╗██╗  ██╗████████╗ ██████╗ ██████╗ ███████╗
;; ██╔══██╗██╔════╝██╔════╝██║ ██╔╝╚══██╔══╝██╔═══██╗██╔══██╗██╔════╝
;; ██║  ██║█████╗  ███████╗█████╔╝    ██║   ██║   ██║██████╔╝███████╗
;; ██║  ██║██╔══╝  ╚════██║██╔═██╗    ██║   ██║   ██║██╔═══╝ ╚════██║
;; ██████╔╝███████╗███████║██║  ██╗   ██║   ╚██████╔╝██║     ███████║
;; ╚═════╝ ╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝     ╚══════╝
;; TODO: Because of desktop - drag & drop icon surfaces aren't rendering in a desktop other than the originating desktop
(defclass desktop ()
  ((output :initform nil :initarg :output :accessor output)
   (windows :initform nil :accessor windows)
   (fullscreen-window :initform nil :accessor fullscreen-window)))

(defmethod has-output ((desktop desktop) output) (eq output (output desktop)))

(defmethod width ((desktop desktop)) (output-width (output desktop)))
(defmethod height ((desktop desktop)) (output-height (output desktop)))

(defun recalculate-toplevel (toplevel width height new-x new-y)
  (with-accessors ((x x) (y y) (compo-max-width compo-max-width) (compo-max-height compo-max-height)
		   (pending-buffer pending-buffer)) toplevel

    (setf compo-max-width width compo-max-height height)
    (setf x new-x y new-y)

    (configure-toplevel-default toplevel)

    ;; NOTE If the dimensions of the toplevel are less than what is allocated
    ;; This should only be done after the client notifies their dimensions
    (after xdg-surface:set-window-geometry toplevel
	   (lambda (xdg x y width height)
	     (declare (ignore x y))
	     (with-accessors ((x x) (y y)) xdg
	       (when (< 0 width compo-max-width)   (setf x (+ x (/ (- compo-max-width width) 2))))
	       (when (< 0 height compo-max-height) (setf y (+ y (/ (- compo-max-height height) 2)))))))))

(defmethod recalculate-layout ((desktop desktop))
  (when (and (output desktop) (windows desktop))
    (let* ((output (output desktop))
	   (d-width (output-width output)) (d-height (output-height output))
	   (amount (length (windows desktop)))
	   (width-per (floor (/ d-width amount))))
      (loop
	for toplevel in (remove-if (lambda (toplevel) (not (typep toplevel 'toplevel))) (windows desktop))
	for i from 0
	do (recalculate-toplevel
	    toplevel
	    width-per
	    d-height
	    (+ (* i width-per) (screen-x output))
	    (screen-y output))))))
