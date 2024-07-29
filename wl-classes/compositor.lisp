
;;  ██████╗ ██████╗ ███╗   ███╗██████╗  ██████╗ ███████╗██╗████████╗ ██████╗ ██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔═══██╗██╔════╝██║╚══██╔══╝██╔═══██╗██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║   ██║███████╗██║   ██║   ██║   ██║██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║   ██║╚════██║██║   ██║   ██║   ██║██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ╚██████╔╝███████║██║   ██║   ╚██████╔╝██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝
(in-package :smuks)

(defclass compositor-global (wl-compositor:global)
  ((desktops :initform nil :accessor desktops)
   (surfaces :initform (make-hash-table) :accessor surfaces)))

(defmethod initialize-instance :after ((compositor compositor-global) &key)
  (setf (desktops compositor) (loop for i from 0 below 10 collect (make-instance 'desktop))))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐  ┌─┐┬┌┬┐┌─┐
;; │  │  │├┤ │││ │───└─┐│ ││├┤
;; └─┘┴─┘┴└─┘┘└┘ ┴   └─┘┴─┴┘└─┘
(defclass compositor (wl-compositor:dispatch)
  ((surfaces :initform (make-hash-table :test 'equal) :accessor surfaces)))


;; ┌┬┐┌─┐┌┬┐┬ ┬
;; │││├┤  │ ├─┤
;; ┴ ┴└─┘ ┴ ┴ ┴
(defmethod all-surfaces ((compositor compositor)) (reverse (alexandria:hash-table-values (surfaces compositor))))
(defmethod all-ready-surfaces ((compositor compositor))
  (let* ((all (all-surfaces compositor)))
    (remove-if-not #'texture all)))

(defmethod all- ((compositor compositor) class)
  (remove-if-not (lambda (surf) (typep surf class)) (all-ready-surfaces compositor)))

(defmethod all-layers ((compositor compositor)) (all- compositor 'layer-surface))
(defmethod all-cursors ((compositor compositor)) (all- compositor 'cursor))
(defmethod all-popups ((compositor compositor))
  (remove-if-not (lambda (surf) (and (typep surf 'popup) (texture surf))) (all-surfaces compositor)))

(defmethod toplevel-surface ((compositor compositor))
  (loop for value being the hash-values of (surfaces compositor)
	when (typep value 'toplevel)
	return value))

(defmethod rem-surface ((compositor compositor) surface)
  (remhash (wl-surface::wl_surface-id surface) (surfaces compositor)))


;; ┬ ┬┬   ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ││││───├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; └┴┘┴─┘ ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod wl-compositor:create-region ((compositor compositor) id) (wl:mk-if 'region compositor id))
(defmethod wl-compositor:create-surface ((compositor compositor) id)
  (setf (gethash id (surfaces compositor)) (wl:mk-if 'surface compositor id :compositor compositor)))


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

(defmethod add-window ((desktop desktop) window)
  (pushnew window (windows desktop))
  (before cl-wl:destroy window (lambda (toplevel) (rm-window desktop toplevel))))

(defmethod rm-window ((desktop desktop) window)
  (setf (windows desktop) (remove window (windows desktop))))

(defun recalculate-toplevel (toplevel width height new-x new-y)
  (with-accessors ((x x) (y y) (compo-max-width compo-max-width) (compo-max-height compo-max-height)
		   (pending-buffer pending-buffer)) toplevel

    (setf compo-max-width width compo-max-height height)
    (setf x new-x y new-y)

    ;; NOTE If the dimensions of the toplevel are less than what is allocated
    ;; We center the window in the middle of their allocated space
    (let ((serial (configure-toplevel-default toplevel)))
      (after
       wl-surface:commit toplevel
       (lambda (surface)
	 (when (eq (last-serial surface) serial)
	   (with-accessors ((x x) (y y) (width width) (height height)
			    (compo-max-height compo-max-height) (compo-max-width compo-max-width))
	       surface
	     (when (< 0 width compo-max-width)   (setf x (+ x (/ (- compo-max-width width) 2))))
	     (when (< 0 height compo-max-height) (setf y (+ y (/ (- compo-max-height height) 2)))))))))))

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
