
;; ███████╗ ██████╗██████╗ ███████╗███████╗███╗   ██╗███████╗
;; ██╔════╝██╔════╝██╔══██╗██╔════╝██╔════╝████╗  ██║██╔════╝
;; ███████╗██║     ██████╔╝█████╗  █████╗  ██╔██╗ ██║███████╗
;; ╚════██║██║     ██╔══██╗██╔══╝  ██╔══╝  ██║╚██╗██║╚════██║
;; ███████║╚██████╗██║  ██║███████╗███████╗██║ ╚████║███████║
;; ╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═══╝╚══════╝
(in-package :smuks)

(defvar *screen-tracker* nil)

;; ┌─┐┌─┐┬─┐┌─┐┌─┐┌┐┌
;; └─┐│  ├┬┘├┤ ├┤ │││
;; └─┘└─┘┴└─└─┘└─┘┘└┘
(defclass screen ()
  ((tracker :initarg :tracker :accessor tracker)
   (buffer :initarg :buffer :accessor buffer)
   (fb :initarg :fb :accessor fb)
   (connector :initarg :connector :accessor connector)
   (egl-image :initform nil :accessor egl-image)
   (drm :initarg :drm :accessor drm)
   (gl-framebuffer :initform nil :accessor gl-framebuffer)
   (shaders :initform nil :accessor shaders)
   (frame-counter :initform (make-instance 'frame-counter) :accessor frame-counter)
   (scene :initarg :scene :initform nil :accessor scene)
   (configuring-neighbors :initform nil :accessor configuring-neighbors)
   (orientation :initform :landscape :initarg :orientation :reader orientation)
   (screen-x :initarg :screen-x :initform 0 :accessor screen-x)
   (screen-y :initarg :screen-y :initform 0 :accessor screen-y)))

(defmethod (setf orientation) (orientation (screen screen))
  (setf (slot-value screen 'orientation) orientation)
  (recalculate-dimensions (tracker screen))
  (prep-shaders screen))

(defmethod shader-rot-val ((screen screen))
  "Returns the screen shader rotation based on orientation and screen 'physical' proportions"
  (if (> (height screen) (width screen))
      (case (orientation screen) (:landscape -90) (:portrait 0) (:landscape-i 90) (:portrait-i 180))
      ;; TODO: These are very likely wrong
      (case (orientation screen) (:landscape 0) (:portrait 90) (:landscape-i 180) (:portrait-i 270))))

(defmethod screen-width ((screen screen))
  "Returns the screen width based on orientation and screen 'physical' proportions"
  (if (> (height screen) (width screen))
      (case (orientation screen) ((:landscape :landscape-i) (height screen)) ((:portrait :portrait-i) (width screen)))
      (case (orientation screen) ((:landscape :landscape-i) (width screen)) ((:portrait :portrait-i) (height screen)))))

(defmethod screen-height ((screen screen))
  "Returns the screen height based on orientation and screen 'physical' proportions"
  (if (> (height screen) (width screen))
      (case (orientation screen) ((:landscape :landscape-i) (width screen)) ((:portrait :portrait-i) (height screen)))
      (case (orientation screen) ((:landscape :landscape-i) (height screen)) ((:portrait :portrait-i) (width screen)))))

(defmethod width ((screen screen)) (hdisplay (connector screen)))
(defmethod height ((screen screen)) (vdisplay (connector screen)))
(defmethod vrefresh ((screen screen)) (vrefresh (connector screen)))
(defmethod connector-type ((screen screen)) (connector-type (connector screen)))
(defmethod start-monitor ((screen screen))
  (setf (egl-image screen) (create-egl-image *egl* (buffer screen) (width screen) (height screen)))
  (setf (gl-framebuffer screen) (create-gl-framebuffer (egl-image screen)))

  (set-crtc! (fd (drm screen))
	     (fb screen)
	     (connector screen)))

(defmethod shader ((screen screen) (type (eql :rect))) (car (shaders screen)))
(defmethod shader ((screen screen) (type (eql :texture))) (cadr (shaders screen)))
(defmethod shader ((screen screen) (type (eql :capsule))) (nth 2 (shaders screen)))

(defmethod prep-shaders ((screen screen))
  (let ((width (screen-width screen)) (height (screen-height screen)) (rot (shader-rot-val screen)))
    (prep-gl-implementation (fb screen) width height)
    (setf (shaders screen) `(,(shader-init:create-rect-shader width height rot)
			     ,(restart-case (shader-init:create-texture-shader width height rot)
				(ignore () (nth 1 (shaders screen))))
			     ,(restart-case (shader-init:create-capsule-shader width height rot)
				(ignore () (nth 2 (shaders screen))))))))

(defmethod update-projections ((screen screen) projection)
  (let ((projection (sglutil:make-projection-matrix (screen-width screen) (screen-height screen) (shader-rot-val screen))))
    (loop for shader in (shaders screen)
	  do (shaders:update-projection shader projection))))

(defmethod render-scene ((screen screen)) (funcall (scene screen) screen))
(defmethod set-scene ((screen screen) scene) (setf (scene screen) scene))


(defmethod cleanup-screen ((screen screen))
  ;; TODO: Didn't clean up gl framebuffer before - might still be worthwhile to check
  ;; (when (gl-framebuffer screen)
    ;; (delete-gl-framebuffer (gl-framebuffer screen))
    ;; (setf (gl-framebuffer screen) nil))
  (when (egl-image screen)
    (seglutil:destroy-image *egl* (egl-image screen))
    (setf (egl-image screen) nil))
  (when (fb screen)
    (sdrm:rm-framebuffer! (drm screen) (fb screen) (buffer screen))
    (setf (fb screen) nil)))



;; ████████╗██████╗  █████╗  ██████╗██╗  ██╗███████╗██████╗
;; ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██║ ██╔╝██╔════╝██╔══██╗
;;    ██║   ██████╔╝███████║██║     █████╔╝ █████╗  ██████╔╝
;;    ██║   ██╔══██╗██╔══██║██║     ██╔═██╗ ██╔══╝  ██╔══██╗
;;    ██║   ██║  ██║██║  ██║╚██████╗██║  ██╗███████╗██║  ██║
;;    ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝
;; TODO: Maybe this can also have egl in it?
;; TODO: Not sure if the screen-tracker should hold the desktop logic
(defclass screen-tracker ()
  ((drm :initarg :drm :accessor drm)
   (screens :initform nil :accessor screens)
   (max-width :initform 0 :accessor max-width)
   (max-height :initform 0 :accessor max-height)))

(defmethod initialize-instance :after ((tracker screen-tracker) &key drm)
  (let ((connectors (connectors drm)))
    ;; TODO: Don't think i really need to sort anything any more here
    (setf connectors (sort connectors (lambda (a b)
					(declare (ignore b))
					(if (eq (connector-type a) :dsi) t nil))))

    ;; TODO: For now stacking screens vertically only
    (let ((screen-y 0))
      (setf (screens tracker)
	    (loop for connector in connectors
		  for index from 0
		  for fb-obj = (create-connector-framebuffer drm connector)
		  when fb-obj
		    collect (let ((height (vdisplay connector))
				  (width (hdisplay connector)))
			      (prog1
				  (make-instance 'screen
				     :connector connector
				     :tracker tracker
				     :orientation (guess-orientation width height)
				     :buffer (framebuffer-buffer fb-obj)
				     :fb (framebuffer-id fb-obj)
				     :scene (nth index *scenes*)
				     :screen-y screen-y
				     :drm drm)
				(incf screen-y height))))))
    ))

(defmethod stop-measuring-fps ((tracker screen-tracker))
  (loop for screen in (screens tracker) do (stop (frame-counter screen))))

(defmethod measure-fps ((tracker screen-tracker))
  (loop for screen in (screens tracker)
	do (run (frame-counter screen) (format nil "FPS:ScreenFB:~a: " (fb screen)))))

(defmethod start-monitors ((tracker screen-tracker))
  (loop for screen in (screens tracker)
	do (start-monitor screen)))

(defmethod screen-by-crtc ((tracker screen-tracker) crtc-id)
  (find-if (lambda (screen) (eq (crtc-id (connector screen)) crtc-id)) (screens tracker)))

(defmethod prep-shaders ((tracker screen-tracker))
  (loop for screen in (screens tracker)
	do (prep-shaders screen)))

(defmethod cleanup-screen-tracker ((tracker screen-tracker))
  (stop-measuring-fps tracker)
  (loop for screen in (screens tracker)
	do (cleanup-screen screen)
	finally (setf (screens tracker) nil)))

(defmethod update-projections ((tracker screen-tracker) projection)
  (mapcar (lambda (screen) (update-projections screen projection)) (screens tracker)))

(defmethod handle-drm-change ((tracker screen-tracker))
  (let ((connectors (sdrm::recheck-resources (drm tracker))))
    (loop for connector in connectors
	  for existing-screen = (find-if (lambda (screen) (eq (id (connector screen)) (id connector))) (screens tracker))
	  do
	     (progn
	       (if existing-screen
		   (unless (connected (connector existing-screen))
		     (cleanup-screen existing-screen)
		     (setf (screens tracker) (remove existing-screen (screens tracker))))
		   (when (connected connector)
		     (let ((fb-obj (create-connector-framebuffer (drm tracker) connector)))
		       (when fb-obj
			 (let ((screen (make-instance 'screen
					  :connector connector
					  :buffer (framebuffer-buffer fb-obj)
					  :fb (framebuffer-id fb-obj)
					  :scene (nth (length (screens tracker)) *scenes*)
					  :drm (drm tracker))))
			   (prep-shaders screen)
			   (start-monitor screen)

			   ;; TODO: For now disabled the fancy - Set screen scene
			   ;; while i figure out multi screen a bit more
			   ;; (when (> (length (screens tracker)) 0)
			     ;; ;; TODO: This shouldn't be the first screen, there can be more than two screens overall
			     ;; (let ((first (nth 0 (screens tracker))))
			       ;; (set-scene screen 'scene-nothing-yet)
			       ;; (set-scene first 'scene-select-screen-pos)
			       ;; (setf (configuring-neighbors first) t)))

			   (push screen (screens tracker))
			   (render-frame screen))))))))
    (recalculate-dimensions tracker)))


;; TODO: This also needs to take into account screen positions
;; And overall bounds when screens are skewed from each other
(defmethod recalculate-dimensions ((tracker screen-tracker))
  (let ((screen-y 0))
    (loop for screen in (screens tracker)
	  do (setf (screen-y screen) screen-y)
	     (incf screen-y (screen-height screen)))))

;; TODO: Suboptimal. Since it has to go through the whole list on every pointer motion event
(defmethod bounds-check ((tracker screen-tracker) x y)
  (let* ((likely-screen (car (screens tracker))))
    (loop for screen in (screens tracker)
	  ;; TODO: Weird skip of the first item
	  unless (eq likely-screen screen)
	    when (or (> x (screen-x screen) (screen-x likely-screen))
		     (> y (screen-y screen) (screen-y likely-screen)))
	      do (setf likely-screen screen))

    (let ((width (screen-width likely-screen))
	  (height (screen-height likely-screen)))
      (values (min (max x (screen-x likely-screen)) (+ (screen-x likely-screen) width))
	      (min (max y (screen-y likely-screen)) (+ (screen-y likely-screen) height))
	      likely-screen))))


;; TODO: Suboptimal. Since this is used to check if inputs should be handled differently,
;; This is a nasty amount of extra work that needs to be done
(defmethod configuring-neighbors? ((tracker screen-tracker))
  (some (lambda (screen) (configuring-neighbors screen)) (screens tracker)))

(defmethod kickstart-frame-render-for-all ((tracker screen-tracker))
  (loop for screen in (screens tracker)
	do (render-frame screen)))

;; NOTE: I'll maybe use this to identify my tablet screen for the sake of associating touch or accelerometer events with it.
(defmethod dsi-screen ((tracker screen-tracker)) (find-if (lambda (screen) (eq (connector-type screen) :dsi)) (screens tracker)))

(defmethod is-in-click-location? ((tracker screen-tracker) x y)
  ;; TOOD: Do not use the first screen, actually refer to the screen-tracker width/height stuff
  (let ((locations (click-locations (car (screens tracker)) *stupid-size*))
	(result nil))
    (dolist (location locations)
      (let ((x-rect (first location)) (y-rect (second location)))
	(when (and (<= x-rect x (+ x-rect *stupid-size*))
		   (<= y-rect y (+ y-rect *stupid-size*)))
	  (setf result (third location)))))
    result))




;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun guess-orientation (width height)
  (if (>= width height) :landscape :portrait))



;; ┌┬┐┌─┐┌─┐┌┬┐┬┌┐┌┌─┐  ┌─┐┌┬┐┬ ┬┌─┐┌─┐
;;  │ ├┤ └─┐ │ │││││ ┬  └─┐ │ │ │├┤ ├┤
;;  ┴ └─┘└─┘ ┴ ┴┘└┘└─┘  └─┘ ┴ └─┘└  └
(defvar *y-pos* 220.0)
(defvar y-up t)
(defun next-y-pos ()
  (when (> *y-pos* 300.0)
    (setf y-up nil))
  (when (< *y-pos* 150.0)
    (setf y-up t))
  (incf *y-pos* (if y-up 1 -1)))

(defvar *red-x* 50.0)
(defvar *red-y* 100.0)

(defun scene-1 (screen)
  (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
						    :x 10.0 :y (next-y-pos) :w 50.0 :h 60.0
						    :color '(0.2 0.2 0.2 1.0))))

  (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
						    :x *red-x* :y *red-y* :w 200.0 :h 50.0
						    :color '(1.0 0.0 0.0 0.6))))
  (shaders.capsule:draw (shader screen :capsule) `(,(shaders.capsule::make-rect
						    :x 400.0 :y 600.0 :w 200.0 :h 200.0
						    :color '(0.2 0.2 0.2 1.0)))))

(defvar *y-2-pos* 220.0)
(defvar y-2-up t)
(defun next-y-2-pos ()
  (when (> *y-2-pos* 500.0)
    (setf y-2-up nil))
  (when (< *y-2-pos* 30.0)
    (setf y-2-up t))
  (incf *y-2-pos* (if y-2-up 1 -1)))


(defun scene-2 (screen)
  (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
						    :x 500.0 :y (next-y-2-pos) :w 200.0 :h 150.0
						    :color '(0.2 0.2 0.2 1.0))))
  (shaders.capsule:draw (shader screen :capsule) `(,(shaders.capsule::make-rect
						    :x 800.0 :y 600.0 :w 200.0 :h 150.0
						    :color '(0.2 0.2 0.2 1.0)))))

;; NOTE: Accidentally managed to draw like a laying down exclamation mark with this
(defun scene-nothing-yet (screen)
  (let* ((size 100.0) (width (flo (width screen))) (height (flo (height screen))) (half (/ size 2)))
    (flet ((draw-rect (x y)
	     (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
							       :x x :y y :w size :h size
							       :color '(0.2 0.2 0.2 1.0))))))
      (draw-rect (- width size (/ width 9) 0.0) (- (/ height 2) half))
      (draw-rect (- width size (/ width 6) 50.0) (- (/ height 2) half))
      (draw-rect (- width size (/ width 3) 50.0) (- (/ height 2) half)))))


(defvar *scenes* (list 'scene-1 'scene-2))
(defvar *stupid-size* 150.0)

(defun click-locations (screen size)
  (let ((width (flo (width screen))) (height (flo (height screen))))
    (list `(0.0 0.0 :top-left)
	  `(,(- (/ width 2) (/ size 2)) 0.0 :top-center)
	  `(,(- width size) 0.0 :top-right)
	  `(0.0 ,(- (/ height 2) (/ size 2)) :center-left)
	  `(,(- (/ width 2) (/ size 2)) ,(- (/ height 2) (/ size 2)) :center-center)
	  `(,(- width size) ,(- (/ height 2) (/ size 2)) :center-right)
	  `(0.0 ,(- height size) :bottom-left)
	  `(,(- (/ width 2) (/ size 2)) ,(- height size) :bottom-center)
	  `(,(- width size) ,(- height size) :bottom-right))))


(defun scene-select-screen-pos (screen)
  (let ((size *stupid-size*))
    (flet ((draw-capsule (x y)
	     (shaders.capsule:draw (shader screen :capsule) `(,(shaders.capsule::make-rect
								:x x :y y :w size :h size
								:color '(0.2 0.2 0.2 1.0))))))
      (dolist (pos (click-locations screen size))
	(draw-capsule (first pos) (second pos))))))
