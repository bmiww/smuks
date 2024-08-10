
;;  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
;; ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
;; ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
;; ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
;; ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
;;  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝
;; Announces clients of the outputs (monitors/modes)
(in-package :smuks)


;;  ██████╗ ██╗      ██████╗ ██████╗  █████╗ ██╗
;; ██╔════╝ ██║     ██╔═══██╗██╔══██╗██╔══██╗██║
;; ██║  ███╗██║     ██║   ██║██████╔╝███████║██║
;; ██║   ██║██║     ██║   ██║██╔══██╗██╔══██║██║
;; ╚██████╔╝███████╗╚██████╔╝██████╔╝██║  ██║███████╗
;;  ╚═════╝ ╚══════╝ ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; TODO: This is where you probably want to fake a virtual output for use cases such as:
;; Video call video sharing - virtual output with fake details
(defclass output (wl-output:global)
  ;; TODO: Thse x/y most likely mean the same as screen-x and screen-y
  ((x :initarg :x :accessor output-x)
   (y :initarg :y :accessor output-y)
   (refresh-rate :initarg :refresh-rate :accessor output-refresh-rate)
   (subpixel-orientation :initarg :subpixel-orientation :initform :unknown :accessor output-subpixel-orientation)
   (make :initarg :make :accessor output-make)
   (model :initarg :model :accessor output-model)
   ;; TODO: This transform is most likely related to orientation and could be get rid of
   (transform :initarg :transform :initform :normal :accessor output-transform)
   ;; NOTE: A lot of these details were migrated from the screen class
   (connector :initarg :connector :accessor connector)
   (drm :initarg :drm :accessor drm)
   (shaders :initform nil :accessor shaders)
   (framebuffers :initarg :framebuffers :initform nil :accessor framebuffers)
   (active-framebuffer :initform nil :accessor active-framebuffer)
   (frame-counter :initform (make-instance 'frame-counter) :accessor frame-counter)
   (scene :initarg :scene :initform nil :accessor scene)
   (configuring-neighbors :initform nil :accessor configuring-neighbors)
   (orientation :initform :landscape :initarg :orientation :reader orientation)
   (paused :initform t :accessor paused)
   (screen-x :initarg :screen-x :initform 0 :accessor screen-x)
   (screen-y :initarg :screen-y :initform 0 :accessor screen-y)
   (client-cursor-drawn :initform nil :accessor client-cursor-drawn)
   (accelerometer :initform nil :accessor accelerometer))
  (:documentation "Defines a lot of details regarding a physical output.
A physical output will mostly be a monitor/screen.
Most slots should be self-explanatory, so i'll keep it short:

real-height, real-width - actual physical dimensions of the output

subpixel-orientation - red green blue - or - blue green red - maybe vertical.
Unknown is also a possibility.

make - the manufacturer of the output
model - the model name/number

transform - is the output rotated? is the output flipped?
"))


(defmethod initialize-instance :after ((output output) &key)
  ;; NOTE: We try to find an accelerometer if the connector is a builtin one
  ;; For now i don't have a smarter way to determine drm dev and iio dev relationships
  (when (eq (connector-type output) :dsi)
    (setf (accelerometer output) (iio-accelerometer:find-accelerometer-dev))
    (when (accelerometer output)
      (pollr "accelerometer"
	     (iio-accelerometer::fd (accelerometer output))
	     (cb (determine-orientation output
					(wl:get-display output)
					(iio-accelerometer::read-accelerometer (accelerometer output))))))))



(defmethod render-scene ((output output)) (when (scene output) (funcall (scene output) output)))
(defmethod prep-shaders ((output output))
  (with-accessors ((width output-width) (height output-height) (rot shader-rot-val)) output
    (let ((gl-version (gl-version (wl:get-display output))))

      (loop for framebuffer in (framebuffers output)
	    do (prep-gl-implementation (framebuffer-id framebuffer) width height))

      (setf (shaders output) `(,(shader-init:create-rect-shader width height rot gl-version)
			       ,(restart-case (shader-init:create-texture-shader width height rot gl-version)
				  (ignore () (nth 1 (shaders output))))
			       ,(shader-init:create-xrgb8888-shader width height rot gl-version)
			       ,(shader-init:create-surface-shader width height rot gl-version)
			       ,(shader-init:create-surface-xrgb8888-shader width height rot gl-version))))))

(defmethod update-shaders ((output output))
  (with-accessors ((width output-width) (height output-height) (rot shader-rot-val)) output
      (let ((projection (sglutil:projection-matrix width height rot)))
	(mapcar (lambda (shader) (shaders:update-projection shader projection)) (shaders output)))))


(defmethod start-monitor ((output output))
  (loop for framebuffer in (framebuffers output)
	do (progn
	     (setf (framebuffer-egl-image framebuffer)
		   (create-egl-image (egl (wl:get-display output)) (framebuffer-buffer framebuffer) (width output) (height output)))
	     (setf (framebuffer-gl-buffer framebuffer)
		   (create-gl-framebuffer (framebuffer-egl-image framebuffer)))))

  ;; For the first frame - we only need the first buffer
  (resume-output output))

(defmethod pause-output ((output output))
  (unless (paused output)
    (setf (paused output) t)))

(defmethod resume-output ((output output))
  (when (paused output)
    (setf (paused output) nil)
    (let ((first (first (framebuffers output))))
      (set-crtc! (fd (drm output))
		 (framebuffer-id first)
		 (connector output)))))


(defmethod next-framebuffer ((output output))
  (let ((framebuffers (framebuffers output))
	(active (active-framebuffer output)))
    (setf (active-framebuffer output) (or (cadr (member active framebuffers)) (first framebuffers)))))


;; ┌─┐┌─┐┌┬┐┌┬┐┌─┐┬─┐┌─┐
;; └─┐├┤  │  │ ├┤ ├┬┘└─┐
;; └─┘└─┘ ┴  ┴ └─┘┴└─└─┘
(defmethod (setf orientation) (orientation (output output))
  (unless orientation (error "Provided orientation cannot be nil."))
  (setf (slot-value output 'orientation) orientation)
  (recalculate-dimensions (wl:get-display output))
  (update-shaders output)
  (let ((related-desktop (find-output-desktop (wl:get-display output) output)))
    (recalculate-layout related-desktop)))

(defmethod update-projections ((output output) projection)
  (let ((projection (sglutil:projection-matrix (output-width output) (output-height output) (shader-rot-val output))))
    (loop for shader in (shaders output)
	  do (shaders:update-projection shader projection))))

(defmethod set-scene ((output output) scene) (setf (scene output) scene))

;; ┌─┐┌─┐┬  ┌─┐┌─┐┌┬┐┌─┐┬─┐┌─┐
;; └─┐├┤ │  ├┤ │   │ │ │├┬┘└─┐
;; └─┘└─┘┴─┘└─┘└─┘ ┴ └─┘┴└─└─┘
;; NOTE: Reads nested or dependant values of an output
(defmethod shader-rot-val ((output output))
  "Returns the output shader rotation based on orientation and output 'physical' proportions"
  (if (> (height output) (width output))
      (case (orientation output) (:landscape -90) (:portrait 0) (:landscape-i 90) (:portrait-i 180))
      ;; TODO: These are very likely wrong
      (case (orientation output) (:landscape 0) (:portrait 90) (:landscape-i 180) (:portrait-i 270))))

(defmethod output-width ((output output))
  "Returns the output width based on orientation and output 'physical' proportions"
  (if (> (height output) (width output))
      (case (orientation output) ((:landscape :landscape-i) (height output)) ((:portrait :portrait-i) (width output)))
      (case (orientation output) ((:landscape :landscape-i) (width output)) ((:portrait :portrait-i) (height output)))))

(defmethod output-height ((output output))
  "Returns the output height based on orientation and output 'physical' proportions"
  (if (> (height output) (width output))
      (case (orientation output) ((:landscape :landscape-i) (width output)) ((:portrait :portrait-i) (height output)))
      (case (orientation output) ((:landscape :landscape-i) (height output)) ((:portrait :portrait-i) (width output)))))

(defmethod screen-x-max ((output output)) (+ (screen-x output) (output-width output)))
(defmethod screen-y-max ((output output)) (+ (screen-y output) (output-height output)))
(defmethod width ((output output)) (hdisplay (connector output)))
(defmethod height ((output output)) (vdisplay (connector output)))
(defmethod vrefresh ((output output)) (vrefresh (connector output)))
(defmethod connector-type ((output output)) (connector-type (connector output)))
(defmethod rect-shader ((output output)) (car (shaders output)))
(defmethod surface-shader ((output output) texture)
  (ecase (tex-format texture)
    (:argb8888 (nth 3 (shaders output)))
    (:xrgb8888 (nth 4 (shaders output)))))

(defmethod texture-shader ((output output) texture)
  (ecase (tex-format texture)
    (:argb8888 (nth 1 (shaders output)))
    (:xrgb8888 (nth 2 (shaders output)))))

(defmethod determine-orientation ((output output) display accel)
  (with-accessors ((orientation orientation) (accelerometer accelerometer)) output
    (unless accelerometer (error "No accelerometer connected to output."))
    (let* ((current-orient orientation))
      (destructuring-bind (x z y) accel
	(declare (ignore z))
	(let* ((y-neg (<= y 0)) (x-neg (<= x 0))
	       (x (abs x)) (y (abs y))
	       (new-orient
		 (cond
		   ((and y-neg (>= y x)) :portrait)
		   ((>= y x) :portrait-i)
		   ((and x-neg (>= x y)) :landscape)
		   ((>= x y) :landscape-i))))
	  (unless (eq current-orient new-orient) (setf orientation new-orient)))))))

;; ┌─┐┬  ┌─┐┌─┐┌┐┌┬ ┬┌─┐
;; │  │  ├┤ ├─┤││││ │├─┘
;; └─┘┴─┘└─┘┴ ┴┘└┘└─┘┴
(defmethod cleanup-output ((output output))
  (with-slots (framebuffers accelerometer drm) output
    (loop for framebuffer in framebuffers
	  do (let ((egl-image (framebuffer-egl-image framebuffer))
		   (gl-buffer (framebuffer-gl-buffer framebuffer)))
	       (when gl-buffer (gl:delete-framebuffer gl-buffer))
	       (when egl-image
		 (seglutil:destroy-image (egl (wl:get-display output)) egl-image)
		 (setf (framebuffer-egl-image framebuffer) nil))
	       (sdrm:rm-framebuffer! drm framebuffer)))

    (when accelerometer (iio-accelerometer::close-dev accelerometer))

    (setfnil framebuffers accelerometer)))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun guess-orientation (width height)
  "Used to determine initial orientation based on the width/height of an output"
  (if (>= width height) :landscape :portrait))


;; ██████╗ ██╗███████╗██████╗  █████╗ ████████╗ ██████╗██╗  ██╗
;; ██╔══██╗██║██╔════╝██╔══██╗██╔══██╗╚══██╔══╝██╔════╝██║  ██║
;; ██║  ██║██║███████╗██████╔╝███████║   ██║   ██║     ███████║
;; ██║  ██║██║╚════██║██╔═══╝ ██╔══██║   ██║   ██║     ██╔══██║
;; ██████╔╝██║███████║██║     ██║  ██║   ██║   ╚██████╗██║  ██║
;; ╚═════╝ ╚═╝╚══════╝╚═╝     ╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝
;; TODO: Implement the release request handler
(defclass output-dispatch (wl-output:dispatch)
  ())

(defmethod output-width ((output output-dispatch)) (output-width (wl:global output)))
(defmethod output-height ((output output-dispatch)) (output-height (wl:global output)))

;; TODO: Add posibility to name an output - send via name event
;; TODO: Add posibility to give outputs a description - send via description event
(defmethod initialize-instance :after ((output output-dispatch) &key)
  (let ((global (wl:global output)))
    (wl-output:send-geometry output
			     (output-x global)
			     (output-y global)
			     ;; TODO: The next two should be the real physical dimensions
			     ;; Not just screen size
			     (output-width global)
			     (output-height global)
			     (output-subpixel-orientation global)
			     (output-make global)
			     (output-model global)
			     (output-transform global))

    ;; The "1" should identify that this mode is the current output mode
    ;; Sending non-current modes (separate event) is possible but deprecated.
    (wl-output:send-mode output
			 '(:current)
			 (output-width global)
			 (output-height global)
			 (output-refresh-rate global))


    ;; Scale can also be changed and sent later. Clients assume 1, but we're being explicit.
    (wl-output:send-scale output 1)

    (when (>= (wl:version-want output) 4)
      (wl-output:send-name output "Smuks output")
      (wl-output:send-description output "Smuks output - the best output in the world. But currently just the one. No support for more."))

    (wl-output:send-done output)))
