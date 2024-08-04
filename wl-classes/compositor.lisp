
;;  ██████╗ ██████╗ ███╗   ███╗██████╗  ██████╗ ███████╗██╗████████╗ ██████╗ ██████╗
;; ██╔════╝██╔═══██╗████╗ ████║██╔══██╗██╔═══██╗██╔════╝██║╚══██╔══╝██╔═══██╗██╔══██╗
;; ██║     ██║   ██║██╔████╔██║██████╔╝██║   ██║███████╗██║   ██║   ██║   ██║██████╔╝
;; ██║     ██║   ██║██║╚██╔╝██║██╔═══╝ ██║   ██║╚════██║██║   ██║   ██║   ██║██╔══██╗
;; ╚██████╗╚██████╔╝██║ ╚═╝ ██║██║     ╚██████╔╝███████║██║   ██║   ╚██████╔╝██║  ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝
(in-package :smuks)

(defclass compo (wl-compositor:global)
  ((desktops :initform (loop for i from 0 below 10 collect (make-instance 'desktop)) :accessor desktops)
   (surfaces :initform nil :accessor surfaces)
   (toplevels :initform nil)
   (cursors :initform nil)
   (popups :initform nil)
   (layers :initform nil)
   (subsurfaces :initform nil)))

(defun type-slot (object)
  (case object
    (toplevel 'toplevels)
    (popup 'popups)
    (layer-surface 'layers)
    (cursor 'cursors)
    (subsurface 'subsurfaces)
    (surface 'surfaces)
    (xdg-surface 'surfaces)))

(defun has-texture (surface) (texture surface))

(defmethod layers ((compo compo)) (remove-if-not #'has-texture (slot-value compo 'layers)))
(defmethod popups ((compo compo)) (remove-if-not #'has-texture (slot-value compo 'popups)))
(defmethod cursors ((compo compo)) (remove-if-not #'has-texture (slot-value compo 'cursors)))
(defmethod subsurfaces ((compo compo)) (remove-if-not #'has-texture (slot-value compo 'subsurfaces)))
(defmethod toplevels ((compo compo))
  (remove-if-not
   (lambda (toplevel) (and (has-texture toplevel) (not (closed toplevel))))
   (slot-value compo 'toplevels)))

;; ┌┬┐┌─┐┌┬┐┬ ┬
;; │││├┤  │ ├─┤
;; ┴ ┴└─┘ ┴ ┴ ┴
(defmethod new-surface ((global compo) surface)
  (pushnew surface (surfaces global))

  ;; If a surface changes type - we put it in the correct list
  (wl:before*
   wl:change-if surface
   (lambda (class surface &rest rest)
     (declare (ignore rest))
     (let ((slot (type-slot (class-name (class-of surface)))) (new-slot (type-slot class)))
       (unless (eq new-slot slot)
	 (setf (slot-value global slot) (remove surface (slot-value global slot)))
	 (pushnew surface (slot-value global (type-slot class))))))))

(defmethod rem-global-surface ((global compo) surface) (setf (surfaces global) (remove surface (surfaces global))))


;;  ██████╗██╗     ██╗███████╗███╗   ██╗████████╗    ██████╗ ██╗███████╗██████╗  █████╗ ████████╗ ██████╗██╗  ██╗
;; ██╔════╝██║     ██║██╔════╝████╗  ██║╚══██╔══╝    ██╔══██╗██║██╔════╝██╔══██╗██╔══██╗╚══██╔══╝██╔════╝██║  ██║
;; ██║     ██║     ██║█████╗  ██╔██╗ ██║   ██║       ██║  ██║██║███████╗██████╔╝███████║   ██║   ██║     ███████║
;; ██║     ██║     ██║██╔══╝  ██║╚██╗██║   ██║       ██║  ██║██║╚════██║██╔═══╝ ██╔══██║   ██║   ██║     ██╔══██║
;; ╚██████╗███████╗██║███████╗██║ ╚████║   ██║       ██████╔╝██║███████║██║     ██║  ██║   ██║   ╚██████╗██║  ██║
;;  ╚═════╝╚══════╝╚═╝╚══════╝╚═╝  ╚═══╝   ╚═╝       ╚═════╝ ╚═╝╚══════╝╚═╝     ╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝
(defclass compositor (wl-compositor:dispatch)
  ((surfaces :initform (make-hash-table :test 'equal) :accessor surfaces)))


;; ┌┬┐┌─┐┌┬┐┬ ┬
;; │││├┤  │ ├─┤
;; ┴ ┴└─┘ ┴ ┴ ┴
(defmethod all-cursors ((compositor compositor))
  (remove-if-not (lambda (surf) (typep surf 'cursor)) (alexandria:hash-table-values (surfaces compositor))))

(defmethod toplevel-surface ((compositor compositor))
  (loop for value being the hash-values of (surfaces compositor)
	when (typep value 'toplevel)
	  return value))

(defmethod rem-surface ((compositor compositor) surface)
  (rem-global-surface (wl:global compositor) surface)
  (remhash (wl-surface::wl_surface-id surface) (surfaces compositor)))


;; ┬ ┬┬   ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ││││───├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; └┴┘┴─┘ ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod wl-compositor:create-region ((compositor compositor) id) (wl:mk-if 'region compositor id))
(defmethod wl-compositor:create-surface ((compositor compositor) id)
  (new-surface
   (wl:global compositor)
   (setf (gethash id (surfaces compositor)) (wl:mk-if 'surface compositor id :compositor compositor))))


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

(defun recalculate-toplevel (output toplevel width height new-x new-y)
  (with-accessors ((x x) (y y) (compo-max-width compo-max-width) (compo-max-height compo-max-height)
		   (pending-buffer pending-buffer)
		   (width-want width) (height-want height)) toplevel

    (setf compo-max-width width compo-max-height height)
    (setf x (- new-x (screen-x output)))
    (setf y (- new-y (screen-y output)))

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
	    output
	    toplevel
	    width-per
	    d-height
	    (+ (* i width-per) (screen-x output))
	    (screen-y output))))))
