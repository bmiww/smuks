
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
   (screen-x :initarg :screen-x :initform 0 :accessor screen-x)
   (screen-y :initarg :screen-y :initform 0 :accessor screen-y)
   (client-cursor-drawn :initform nil :accessor client-cursor-drawn))
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


(defmethod render-scene ((output output)) (funcall (scene output) output))
(defmethod prep-shaders ((output output))
  (let ((width (output-width output)) (height (output-height output)) (rot (shader-rot-val output))
	(gl-version (gl-version (wl:get-display output))))
    ;; NOTE: Binds the first framebuffer to generate the shaders. Don't think that in itself is necessary.
    ;; But regardless, both buffer dimensions should be identical here.
    (loop for framebuffer in (framebuffers output)
	  do (prep-gl-implementation (framebuffer-id framebuffer) width height))

    (let ((rect (shader output :rect))
	  (texture (shader output :texture)))
      (if (and rect texture)
	  (progn
	    (shaders:update-projection rect (sglutil:projection-matrix width height rot))
	    (shaders:update-projection texture (sglutil:projection-matrix width height rot)))
	  (setf (shaders output) `(,(shader-init:create-rect-shader width height rot gl-version)
				   ,(restart-case (shader-init:create-texture-shader width height rot gl-version)
				      (ignore () (nth 1 (shaders output))))))))))


(defmethod start-monitor ((output output))
  (loop for framebuffer in (framebuffers output)
	do (progn
	     (setf (framebuffer-egl-image framebuffer)
		   (create-egl-image (egl (wl:get-display output)) (framebuffer-buffer framebuffer) (width output) (height output)))
	     (setf (framebuffer-gl-buffer framebuffer)
		   (create-gl-framebuffer (framebuffer-egl-image framebuffer)))))

  ;; For the first frame - we only need the first buffer
  (let ((first (first (framebuffers output))))
    (set-crtc! (fd (drm output))
	       (framebuffer-id first)
	       (connector output))))

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
  (prep-shaders output))

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

(defmethod width ((output output)) (hdisplay (connector output)))
(defmethod height ((output output)) (vdisplay (connector output)))
(defmethod vrefresh ((output output)) (vrefresh (connector output)))
(defmethod connector-type ((output output)) (connector-type (connector output)))
(defmethod shader ((output output) (type (eql :rect))) (car (shaders output)))
(defmethod shader ((output output) (type (eql :texture))) (cadr (shaders output)))


;; ┌─┐┬  ┌─┐┌─┐┌┐┌┬ ┬┌─┐
;; │  │  ├┤ ├─┤││││ │├─┘
;; └─┘┴─┘└─┘┴ ┴┘└┘└─┘┴
(defmethod cleanup-output ((output output))
  (loop for framebuffer in (framebuffers output)
	do (let ((egl-image (framebuffer-egl-image framebuffer))
		 (framebuffer-id (framebuffer-id framebuffer))
		 (framebuffer-buffer (framebuffer-buffer framebuffer)))
	     (when egl-image
	       (seglutil:destroy-image (egl (wl:get-display output)) egl-image)
	       (setf (framebuffer-egl-image framebuffer) nil))
	     (when (and framebuffer-id framebuffer-buffer)
	       (sdrm:rm-framebuffer! (drm output) framebuffer-id framebuffer-buffer)
	       (setf (framebuffer-id framebuffer) nil
		     (framebuffer-buffer framebuffer) nil)))))


;; ██████╗ ██╗███████╗██████╗  █████╗ ████████╗ ██████╗██╗  ██╗
;; ██╔══██╗██║██╔════╝██╔══██╗██╔══██╗╚══██╔══╝██╔════╝██║  ██║
;; ██║  ██║██║███████╗██████╔╝███████║   ██║   ██║     ███████║
;; ██║  ██║██║╚════██║██╔═══╝ ██╔══██║   ██║   ██║     ██╔══██║
;; ██████╔╝██║███████║██║     ██║  ██║   ██║   ╚██████╗██║  ██║
;; ╚═════╝ ╚═╝╚══════╝╚═╝     ╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝
;; TODO: Implement the release request handler
(defclass output-dispatch (wl-output:dispatch)
  ())

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

    ;; NOTE: For example Weston actually doesn't understand these - since these are for a later version of the protocol
    ;; TODO: Maybe add a version check.
    ;; (wl-output:send-name output "Smuks output")
    ;; (wl-output:send-description output "Smuks output - the best output in the world. But currently just the one. No support for more.")

    (wl-output:send-done output)))
