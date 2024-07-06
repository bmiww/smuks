
;; ██████╗ ██╗███████╗██████╗ ██╗      █████╗ ██╗   ██╗
;; ██╔══██╗██║██╔════╝██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝
;; ██║  ██║██║███████╗██████╔╝██║     ███████║ ╚████╔╝
;; ██║  ██║██║╚════██║██╔═══╝ ██║     ██╔══██║  ╚██╔╝
;; ██████╔╝██║███████║██║     ███████╗██║  ██║   ██║
;; ╚═════╝ ╚═╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝
(in-package :smuks)
(defclass display (wl:display)
  ;; Not sure we need 32. That's a lot of fingers.
  ((touch-slot-interesses :initform (make-array 32 :initial-element nil) :reader touch-slot-interesses)
   (drm :initarg :drm :accessor drm)
   (egl :initarg :egl :accessor egl)
   (libseat :initarg :libseat :accessor libseat)
   (gl-version :initarg :gl-version :accessor gl-version)

   ;; TODO: I would like to turn this into a coord struct - but naming is weird.
   ;; coord assumes desktop. This uses output.
   (cursor-x :initform 0 :accessor cursor-x)
   (cursor-y :initform 0 :accessor cursor-y)
   (cursor-screen :initform nil :accessor cursor-screen)

   (display-serial :initform 0 :accessor display-serial)
   (keyboard-focus :initform nil)
   (pointer-focus :initform nil :accessor pointer-focus)
   (pending-drag :initform nil :accessor pending-drag)
   (desktops :initform nil :accessor desktops)
   (active-desktop :initform nil :accessor active-desktop)
   (outputs :initform nil :accessor outputs)

   (k-alt? :initform nil :accessor k-alt?)
   (k-ctrl? :initform nil :accessor k-ctrl?)
   (k-shift? :initform nil :accessor k-shift?)
   (k-super? :initform nil :accessor k-super?)))

(defvar *framebuffer-count* 2)

(defmethod initialize-instance :after ((display display) &key)
  ;; NOTE: For now creating 10 desktops each for one number key
  (setf (desktops display)
	(loop for i from 0 below 10
	      collect (make-instance 'desktop)))

  (setf (active-desktop display) (first (desktops display))))


(defgeneric input (display type event))
(defgeneric process (display type usecase event))

(defmethod wl:rem-client :before ((display display) client)
  (with-slots (keyboard-focus pointer-focus pending-drag) display
    (when (and pointer-focus (eq (wl:client pointer-focus) client)) (setf (pointer-focus display) nil))
    (when (and keyboard-focus (eq (wl:client keyboard-focus) client)) (setf (keyboard-focus display) nil))
    (when (and pending-drag (eq (wl:client pending-drag) client)) (setf (pending-drag display) nil))))

(defmethod wl:rem-client :after ((display display) client)
  (handle-surface-change display))


(defmethod (setf active-desktop) :before (new (display display))
  (when (active-desktop display)
    (unless (eq new (active-desktop display))
      (setf (output new) (output (active-desktop display)))
      (setf (output (active-desktop display)) nil)
      (handle-surface-change display))))

;; ┌─┐┌─┐┌┬┐┬ ┬┌─┐
;; └─┐├┤  │ │ │├─┘
;; └─┘└─┘ ┴ └─┘┴

(defmethod init-globals ((display display))
  ;; TODO: When you recompile the compiled classes - these globals aren't updated, needing a rerun
  (make-instance 'wl-compositor:global :display display :dispatch-impl 'compositor)
  (make-instance 'wl-subcompositor:global :display display :dispatch-impl 'subcompositor)
  (make-instance 'shm-global :display display :dispatch-impl 'shm)
  (make-instance 'seat-global :display display :dispatch-impl 'seat)
  (make-instance 'wl-data-device-manager:global :display display :dispatch-impl 'dd-manager)
  (make-instance 'xdg-wm-base:global :display display :dispatch-impl 'wm-base)
  (make-instance 'dmabuf-global :display display :dispatch-impl 'dmabuf)
  (make-instance 'layer-shell-global :display display :dispatch-impl 'layer-shell)
  (make-instance 'zxdg-decoration-manager-v1:global :display display :dispatch-impl 'decoration-manager)
  (make-instance 'zwp-text-input-manager-v3:global :display display :dispatch-impl 'text-input-manager)
  (make-instance 'zwp-input-method-manager-v2:global :display display :dispatch-impl 'input-method-manager)
  (make-instance 'zwp-virtual-keyboard-manager-v1:global :display display :dispatch-impl 'virtual-keyboard-manager)
  (make-instance 'xwayland-shell-v1:global :display display :dispatch-impl 'xwayland)

  (init-outputs display)

  ;; TODO: Needs outputs at this point. Could be moved if i rewrite initialization
  ;; NOTE: For each screen we have available attach it to a different desktop
  (loop for output in (outputs display)
	for desktop in (desktops display)
	do (setf (output desktop) output))

  ;; TODO: Setting initial cursor pos. Should be part of init init. But don't have outputs at that point yet
  (multiple-value-bind (x y output) (bounds-check display (cursor-x display) (cursor-y display))
    (declare (ignore x y))
    (setf (cursor-screen display) output)))

(defmethod init-outputs ((display display))
  (let ((connectors (connectors (drm display))))
    ;; NOTE: Sort connectors so that dsi (builtin) is first
    (setf connectors (sort connectors (lambda (a b)
					(declare (ignore b))
					(if (eq (connector-type a) :dsi) t nil))))
    (let ((screen-y 0))
      (setf (outputs display)
	    (loop for connector in connectors
		  for index from 0
		  for fb-objs = (create-connector-framebuffer (drm display) connector *framebuffer-count*)
		  when fb-objs
		    collect (let ((height (vdisplay connector))
				  (width (hdisplay connector)))
			      (prog1
				  (make-instance 'output
				     :connector connector
				     :orientation (guess-orientation width height)
				     :framebuffers fb-objs
				     :scene (nth index *scenes*)
				     :screen-y screen-y
				     :drm (drm display)
				     ;; NOTE: Moved from old output thing
				     :display display :dispatch-impl 'output-dispatch
				     ;; TODO: These could be determined differently\
				     :x 0 :y 0
				     :refresh-rate (sdrm:vrefresh connector)
				     :make "TODO: Fill out make" :model "TODO: Fill out model")
				(incf screen-y height))))))))


;; TODO: Make it possible to remove outputs from display
;; TODO: When output is added notify each clients registry object
;; TODO: When output is removed notify each clients registry object
(defmethod add-output ((display display) output)
  (push (outputs display) output))

(defmethod pause-outputs ((display display))
  (loop for output in (outputs display)
	do (pause-output output)))

(defmethod resume-outputs ((display display))
  (loop for output in (outputs display)
	do (resume-output output))
  (kickstart-frame-render-for-all display))


;; ┌┬┐┌─┐┌─┐┬  ┌─┐
;;  │ │ ││ ││  └─┐
;;  ┴ └─┘└─┘┴─┘└─┘
;; TODO: Suboptimal. Since it has to go through the whole list on every pointer motion event
(defmethod bounds-check ((display display) x y)
  (let* ((likely-output (car (outputs display))))
    (loop for output in (outputs display)
	  ;; TODO: Weird skip of the first item
	  unless (eq likely-output output)
	    when (or (> x (screen-x output) (screen-x likely-output))
		     (> y (screen-y output) (screen-y likely-output)))
	      do (setf likely-output output))

    (let ((width (output-width likely-output))
	  (height (output-height likely-output)))
      (values (min (max x (screen-x likely-output)) (+ (screen-x likely-output) width))
	      (min (max y (screen-y likely-output)) (+ (screen-y likely-output) height))
	      likely-output))))

(defmethod next-serial ((display display)) (incf (display-serial display)))

(defmethod (setf keyboard-focus) (focus-surface (display display))
  (let ((current (keyboard-focus display)))
    (if focus-surface
	(let* ((client (wl:client focus-surface)) (seat (seat client)))
	  (when seat
	    (setf (slot-value display 'keyboard-focus) focus-surface)
	    (keyboard-destroy-callback seat (lambda (keyboard) (declare (ignore keyboard)) (setf (slot-value display 'keyboard-focus) nil)))

	    (when current (keyboard-leave (seat current) current))
	    ;; TODO: You're supposed to send the actual pressed keys as last arg
	    ;; But currently don't have a keypress manager/tracker
	    (keyboard-enter seat focus-surface '())))
	(progn
	  (when current (keyboard-leave (seat current) current))
	  (setf (slot-value display 'keyboard-focus) nil)))))

(defmethod keyboard-focus ((display display)) (slot-value display 'keyboard-focus))


;; ┬┌┐┌┌─┐┬ ┬┌┬┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; ││││├─┘│ │ │   ├─┤├─┤│││ │││  │││││ ┬
;; ┴┘└┘┴  └─┘ ┴   ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod input ((display display) type event)
  (cond
    ((configuring-neighbors? display) (process display type :screen-setup event))
    (t (process display type :passthrough event))))

(defmethod input ((display display) (type (eql :pointer-axis)) event)
  "This is deprecated in libinput >1.19. Therefore ignorable.")


;; ┬ ┬┬┌┐┌┌┬┐┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; │││││││ │││ ││││  ├─┤├─┤│││ │││  │││││ ┬
;; └┴┘┴┘└┘─┴┘└─┘└┴┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod new-toplevel ((display display) surface)
  (let* ((desktop (active-desktop display)))
    (setf (desktop surface) desktop)
    (setf (windows desktop) (pushnew surface (windows desktop)))
    (wl:add-destroy-callback
     surface
     (lambda (surf)
       (setf (windows desktop) (remove surf (windows desktop)))
       (recalculate-layout desktop)))

    (recalculate-layout desktop)))

;; TODO: This is in essence what focus-pointer-surface2 does - and could just be combined
(defmethod handle-surface-change ((display display) &optional surface)
  "If a surface has had a change - it is no longer visible, has a different surface on top,
or has a all required parameters initiated to be focusable,
then this can be called to determine the new focus surfaces."
  (focus-pointer-surface2 display surface))

(defmethod focus-pointer-surface2 ((display display) &optional surface)
  (with-accessors ((x cursor-x) (y cursor-y) (focus pointer-focus)) display
    (let ((surface (or surface (surface-at-coords display x y))))
      (if surface
	  (let ((seat (seat surface)) (surf-x (- x (x surface))) (surf-y (- y (y surface))))
	    (when seat
	      (if (eq focus surface)
		  ;; NOTE: If already pointer focus - return it as is
		  surface
		  ;; NOTE: Otherwise - switch focus to the new window
		  (progn
		    (when focus
		      (pointer-leave (seat focus)))
		    (setf (keyboard-focus display) surface)
		    (pointer-enter seat surface surf-x surf-y)
		    (setf focus surface)))))
	  ;; NOTE: If no surface at coordinates - remove display pointer focus
	  (when focus
	    (pointer-leave (seat focus))
	    (setf (keyboard-focus display) nil)
	    (setf focus nil))))))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defmethod surface-at-coords ((display display) x y)
  "Iterate all clients and their surfaces to find one that intersects with the given coordinates"
  (let* ((clients (wl:all-clients display))
	 (output (output-at-coords display x y))
	 (desktop (find-output-desktop display output)))
    (when desktop
      (loop for client in clients
	    do (let* ((compositor (compositor client))
		      (toplevels (and compositor (all-surfaces compositor)))
		      (popups (and compositor (all-popups compositor))))
		 (when popups
		   (let ((popup (find-bounder popups x y desktop)))
		     (when popup (return-from surface-at-coords popup))))

		 (when toplevels
		   (let ((toplevel (find-bounder toplevels x y desktop)))
		     (when toplevel (return-from surface-at-coords toplevel)))))))))

(defmethod output-at-coords ((display display) x y)
  (loop for output in (outputs display)
	when (and (<= (screen-x output) x (screen-x-max output))
		  (<= (screen-y output) y (screen-y-max output)))
	  return output))

(defun find-bounder (surfaces x y desktop)
  (loop for surface in surfaces
	when (in-bounds surface x y desktop) return surface))

(defmethod update-cursor ((display display) dx dy)
  (let ((new-x (+ (cursor-x display) dx))
	(new-y (+ (cursor-y display) dy)))
    (setf (values (cursor-x display) (cursor-y display) (cursor-screen display))
	  (bounds-check display new-x new-y))))

(defmethod orient-point ((display display) x y)
  (let ((touch-screen (dsi-output display)))
    (case (orientation touch-screen)
      (:landscape (values y (+ (- (output-height touch-screen) x) (screen-x touch-screen))))
      (:portrait (- (output-width touch-screen) x)))))

(defmethod find-output-desktop ((display display) output)
  (find output (desktops display) :key #'output))


;; ┌─┐┬ ┬┌┬┐┌─┐┬ ┬┌┬┐  ┬ ┬┌┬┐┬┬
;; │ ││ │ │ ├─┘│ │ │   │ │ │ ││
;; └─┘└─┘ ┴ ┴  └─┘ ┴   └─┘ ┴ ┴┴─┘
;; NOTE: I'll maybe use this to identify my tablet screen for the sake of associating touch or accelerometer events with it.
(defmethod dsi-output ((display display)) (find-if (lambda (output) (eq (connector-type output) :dsi)) (outputs display)))

(defmethod start-monitors ((display display))
  (loop for output in (outputs display)
	do (start-monitor output)))

(defmethod output-by-crtc ((display display) crtc-id)
  (find-if (lambda (output) (eq (crtc-id (connector output)) crtc-id)) (outputs display)))

(defmethod update-projections ((display display) projection)
  (mapcar (lambda (output) (update-projections output projection)) (outputs display)))


(defmethod prep-shaders2 ((display display) &key gl-version)
  (when gl-version (setf (gl-version display) gl-version))
  (loop for output in (outputs display)
	do (prep-shaders output)))

;; TODO: Suboptimal. Since this is used to check if inputs should be handled differently,
;; This is a nasty amount of extra work that needs to be done
(defmethod configuring-neighbors? ((display display))
  (some (lambda (output) (configuring-neighbors output)) (outputs display)))

;; TODO: This also needs to take into account screen positions
;; And overall bounds when outputs are skewed from each other
(defmethod recalculate-dimensions ((display display))
  (let ((screen-y 0))
    (loop for output in (outputs display)
	  do (setf (screen-y output) screen-y)
	     (incf screen-y (output-height output)))))

(defun guess-orientation (width height)
  "Used to determine initial orientation based on the width/height of an output"
  (if (>= width height) :landscape :portrait))


;; TODO: Lots of duplication befween this and initialize-instance
(defmethod handle-drm-change ((display display))
  (let ((connectors (sdrm::recheck-resources (drm display))))
    (loop for connector in connectors
	  for existing-output = (find-if (lambda (output) (eq (id (connector output)) (id connector))) (outputs display))
	  do
	     (progn
	       (if existing-output
		   (unless (connected (connector existing-output))
		     (cleanup-output existing-output)
		     (setf (outputs display) (remove existing-output (outputs display))))
		   (when (connected connector)
		     (let ((fb-objs (create-connector-framebuffer (drm display) connector *framebuffer-count*)))
		       (when fb-objs
			 (let ((output (make-instance 'output
					  :connector connector
					  :display display
					  :framebuffers fb-objs
					  :scene (nth (length (outputs display)) *scenes*)
					  :drm (drm display))))
			   (prep-shaders output)
			   (add-output display output)
			   (start-monitor output)
			   (render-frame output))))))))
    (recalculate-dimensions display)))

;; TODO: If multiple toplevels for one client - this should probably first kill off all toplevels and then the client?
;; Unless we expect the client to die off itself once toplevels go away?
(defmethod kill-focus-client ((display display))
  (when (keyboard-focus display)
    (wl:destroy-client (wl:client (keyboard-focus display)))))

;; ┌─┐┬ ┬┌┬┐┌─┐┬ ┬┌┬┐  ┌┬┐┌─┐┌┐ ┬ ┬┌─┐
;; │ ││ │ │ ├─┘│ │ │    ││├┤ ├┴┐│ ││ ┬
;; └─┘└─┘ ┴ ┴  └─┘ ┴   ─┴┘└─┘└─┘└─┘└─┘
(defmethod stop-measuring-fps ((display display))
  (loop for output in (outputs display) do (stop (frame-counter output))))

(defmethod measure-fps ((display display))
  (loop for output in (outputs display)
	do (run (frame-counter output) (format nil "FPS:Output:~a: " output))))

(defmethod kickstart-frame-render-for-all ((display display))
  (loop for output in (outputs display)
	do (render-frame output)))


;; ┌─┐┬  ┌─┐┌─┐┌┐┌┬ ┬┌─┐
;; │  │  ├┤ ├─┤││││ │├─┘
;; └─┘┴─┘└─┘┴ ┴┘└┘└─┘┴
(defmethod cleanup-display ((display display))

  ;; Run cleanup for all outputs
  (stop-measuring-fps display)
  (loop for output in (outputs display)
	do (cleanup-output output)
	finally (setf (outputs display) nil))

  ;; Close the libwayland processes and the globals from lisp end
  (wl:destroy display))
