
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
   (gl-version :initarg :gl-version :accessor gl-version)
   (cursor-x :initform 0 :accessor cursor-x)
   (cursor-y :initform 0 :accessor cursor-y)
   (cursor-screen :initform nil :accessor cursor-screen)
   ;; TODO: Both of these are dumb - these should be per CRTC/monitor/whatever
   (dev-t :initarg :dev-t :accessor dev-t)
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

  (init-outputs)

  ;; TODO: Needs outputs at this point. Could be moved if i rewrite initialization
  ;; NOTE: For each screen we have available attach it to a different desktop
  (loop for output in (outputs display)
	for desktop in (desktops display)
	do (setf (screen desktop) output))

  ;; TODO: Setting initial cursor pos. Should be part of init init. But don't have outputs at that point yet
  (multiple-value-bind (x y screen) (bounds-check display (cursor-x display) (cursor-y display))
    (declare (ignore x y))
    (setf (cursor-screen display) screen)))

(defmethod init-outputs ((display display))
  (let ((connectors (connectors (drm display))))
    ;; TODO: Don't think i really need to sort anything any more here
    (setf connectors (sort connectors (lambda (a b)
					(declare (ignore b))
					(if (eq (connector-type a) :dsi) t nil))))

    ;; TODO: For now stacking screens vertically only
    (let ((screen-y 0))
      (setf (outputs display)
	    (loop for connector in connectors
		  for index from 0
		  for fb-objs = (create-connector-framebuffer (drm display) connector *framebuffer-count*)
		  when fb-objs
		    collect (let ((height (vdisplay connector))
				  (width (hdisplay connector)))
			      (prog1
				  (make-instance 'output-global
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
				     :width (width screen) :height (height screen)
				     :real-width (width screen) :real-height (height screen)
				     :refresh-rate (sdrm:vrefresh screen)
				     :make "TODO: Fill out make" :model "TODO: Fill out model")
				(incf screen-y height))))))))


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

    (let ((width (screen-width likely-output))
	  (height (screen-height likely-output)))
      (values (min (max x (screen-x likely-output)) (+ (screen-x likely-output) width))
	      (min (max y (screen-y likely-output)) (+ (screen-y likely-output) height))
	      likely-output))))

(defmethod next-serial ((display display)) (incf (display-serial display)))

(defmethod (setf keyboard-focus) (focus-surface (display display))
  (if focus-surface
      (let* ((client (wl:client focus-surface)) (seat (seat client)))
	(when seat
	  (setf (slot-value display 'keyboard-focus) focus-surface)
	  (keyboard-destroy-callback seat (lambda (keyboard) (declare (ignore keyboard)) (setf (slot-value display 'keyboard-focus) nil)))

	  ;; TODO: You're supposed to send the actual pressed keys as last arg
	  ;; But currently don't have a keypress manager/tracker
	  (keyboard-enter seat focus-surface '())))
      (setf (slot-value display 'keyboard-focus) nil)))

(defmethod keyboard-focus ((display display)) (slot-value display 'keyboard-focus))


;; ┬┌┐┌┌─┐┬ ┬┌┬┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; ││││├─┘│ │ │   ├─┤├─┤│││ │││  │││││ ┬
;; ┴┘└┘┴  └─┘ ┴   ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod input ((display display) type event)
  (cond
    ((configuring-neighbors? (outputs display)) (process display type :screen-setup event))
    (t (process display type :passthrough event))))

(defmethod input ((display display) (type (eql :pointer-axis)) event)
  "This is deprecated in libinput >1.19. Therefore ignorable.")


;; ┬ ┬┬┌┐┌┌┬┐┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; │││││││ │││ ││││  ├─┤├─┤│││ │││  │││││ ┬
;; └┴┘┴┘└┘─┴┘└─┘└┴┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod new-toplevel ((display display) surface)
  (let* ((desktop (active-desktop display)))
    (setf (windows desktop) (pushnew surface (windows desktop)))
    (wl:add-destroy-callback
     surface
     (lambda (surf)
       (setf (windows desktop) (remove surf (windows desktop)))
       (recalculate-layout desktop)))

    (recalculate-layout desktop)))

(defmethod finalize-toplevel ((display display) surface)
  (with-slots (x y width height) surface
    (when (and (<= x (cursor-x display) (+ x width)) (<= y (cursor-y display) (+ y height)))
      ;; (setf (pointer-focus display) surface)
      ;; (setf (keyboard-focus display) surface)
      )))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defmethod surface-at-coords ((display display) x y)
  "Iterate all clients and their surfaces to find one that intersects with the given coordinates"
  (let* ((clients (wl:all-clients display)))
    (loop for client in clients
	  do (let* ((compositor (compositor client))
		    (toplevels (all-surfaces compositor))
		    (popups (all-popups compositor)))
	       (let ((popup (find-bounder popups x y)))
		 (when popup (return-from surface-at-coords popup)))

	       (let ((toplevel (find-bounder toplevels x y)))
		 (when toplevel (return-from surface-at-coords toplevel)))))))

(defun find-bounder (surfaces x y)
  (loop for surface in surfaces
	when (in-bounds surface x y) return surface))

(defmethod update-cursor ((display display) dx dy)
  (let ((new-x (+ (cursor-x display) dx))
	(new-y (+ (cursor-y display) dy)))
    (setf (values (cursor-x display) (cursor-y display) (cursor-screen display))
	  (bounds-check (outputs display) new-x new-y))))

(defmethod orient-point ((display display) x y)
  (let ((touch-screen (dsi-screen display)))
    (case (orientation touch-screen)
      (:landscape (values y (+ (- (screen-height touch-screen) x) (screen-x touch-screen))))
      (:portrait (- (screen-width touch-screen) x)))))


(defmethod find-output-desktop ((display display) output)
  (let ((screen (output-screen (wl:global output))))
    (find-screen-desktop display screen)))

(defmethod find-screen-desktop ((display display) screen)
  (find screen (desktops display) :key #'screen))


;; ┌─┐┬ ┬┌┬┐┌─┐┬ ┬┌┬┐  ┬ ┬┌┬┐┬┬
;; │ ││ │ │ ├─┘│ │ │   │ │ │ ││
;; └─┘└─┘ ┴ ┴  └─┘ ┴   └─┘ ┴ ┴┴─┘
;; NOTE: I'll maybe use this to identify my tablet screen for the sake of associating touch or accelerometer events with it.
(defmethod dsi-screen ((display display)) (find-if (lambda (screen) (eq (connector-type screen) :dsi)) (outputs display)))

(defmethod start-monitors ((display display))
  (loop for screen in (outputs display)
	do (start-monitor screen)))

(defmethod screen-by-crtc ((display display) crtc-id)
  (find-if (lambda (screen) (eq (crtc-id (connector screen)) crtc-id)) (outputs display)))

(defmethod update-projections ((display display) projection)
  (mapcar (lambda (screen) (update-projections screen projection)) (outputs display)))


(defmethod prep-shaders2 ((display display) &key gl-version)
  (when gl-version (setf (gl-version display) gl-version))
  (loop for output in (outputs display)
	do (prep-shaders output)))

;; TODO: Suboptimal. Since this is used to check if inputs should be handled differently,
;; This is a nasty amount of extra work that needs to be done
(defmethod configuring-neighbors? ((display display))
  (some (lambda (screen) (configuring-neighbors screen)) (outputs display)))

;; TODO: This also needs to take into account screen positions
;; And overall bounds when outputs are skewed from each other
(defmethod recalculate-dimensions ((display display))
  (let ((screen-y 0))
    (loop for screen in (outputs display)
	  do (setf (screen-y screen) screen-y)
	     (incf screen-y (screen-height screen)))))

(defun guess-orientation (width height)
  "Used to determine initial orientation based on the width/height of an output"
  (if (>= width height) :landscape :portrait))


;; TODO: Lots of duplication befween this and initialize-instance
(defmethod handle-drm-change ((display display))
  (let ((connectors (sdrm::recheck-resources (drm display))))
    (loop for connector in connectors
	  for existing-screen = (find-if (lambda (screen) (eq (id (connector screen)) (id connector))) (outputs display))
	  do
	     (progn
	       (if existing-screen
		   (unless (connected (connector existing-screen))
		     (cleanup-screen existing-screen)
		     (setf (outputs display) (remove existing-screen (outputs display))))
		   (when (connected connector)
		     (let ((fb-objs (create-connector-framebuffer (drm display) connector *framebuffer-count*)))
		       (when fb-objs
			 (let ((screen (make-instance 'screen
					  :connector connector
					  :display display
					  :framebuffers fb-objs
					  :scene (nth (length (outputs display)) *scenes*)
					  :drm (drm display))))
			   (prep-shaders screen)
			   (start-monitor screen)

			   ;; TODO: For now disabled the fancy - Set screen scene
			   ;; while i figure out multi screen a bit more
			   ;; (when (> (length (outputs display)) 0)
			     ;; ;; TODO: This shouldn't be the first screen, there can be more than two outputs overall
			     ;; (let ((first (nth 0 (outputs display))))
			       ;; (set-scene screen 'scene-nothing-yet)
			       ;; (set-scene first 'scene-select-screen-pos)
			       ;; (setf (configuring-neighbors first) t)))

			   (push screen (outputs display))
			   (render-frame screen))))))))
    (recalculate-dimensions display)))


;; ┌─┐┬ ┬┌┬┐┌─┐┬ ┬┌┬┐  ┌┬┐┌─┐┌┐ ┬ ┬┌─┐
;; │ ││ │ │ ├─┘│ │ │    ││├┤ ├┴┐│ ││ ┬
;; └─┘└─┘ ┴ ┴  └─┘ ┴   ─┴┘└─┘└─┘└─┘└─┘
(defmethod stop-measuring-fps ((display display))
  (loop for output in (outputs display) do (stop (frame-counter output))))

(defmethod measure-fps ((display display))
  (loop for output in (outputs display)
	do (run (frame-counter output) (format nil "FPS:Output:~a: " output))))

(defmethod kickstart-frame-render-for-all ((display display))
  (loop for screen in (outputs display)
	do (render-frame screen)))




;; ┌─┐┬  ┌─┐┌─┐┌┐┌┬ ┬┌─┐
;; │  │  ├┤ ├─┤││││ │├─┘
;; └─┘┴─┘└─┘┴ ┴┘└┘└─┘┴
(defmethod cleanup-display ((display display))

  ;; Run cleanup for all outputs
  (stop-measuring-fps display)
  (loop for screen in (outputs display)
	do (cleanup-screen screen)
	finally (setf (outputs display) nil))

  ;; Close the libwayland processes and the globals from lisp end
  (wl:destroy display))


;; ┌┬┐┌─┐┌─┐┬┌─┌┬┐┌─┐┌─┐┌─┐
;;  ││├┤ └─┐├┴┐ │ │ │├─┘└─┐
;; ─┴┘└─┘└─┘┴ ┴ ┴ └─┘┴  └─┘
;; TODO: Move this
;; TODO: Rename to workspace or something???
;; TODO: Because of desktop - drag & drop icon surfaces aren't rendering in a desktop other than the originating desktop
(defclass desktop ()
  ((output :initform nil :initarg :output :accessor output)
   (windows :initform nil :accessor windows)
   (fullscreen-window :initform nil :accessor fullscreen-window)))

(defmethod has-output ((desktop desktop) output) (eq output (output desktop)))

(defmethod width ((desktop desktop)) (screen-width (output desktop)))
(defmethod height ((desktop desktop)) (screen-height (output desktop)))

(defmethod recalculate-layout ((desktop desktop))
  (when (windows desktop)
    (let* ((output (output desktop))
	   (d-width (screen-width output)) (d-height (screen-height output))
	   (amount (length (windows desktop)))
	   (width-per (floor (/ d-width amount))))
      (loop
	for window in (windows desktop)
	for i from 0
	do (progn
	     (setf
	      (x window) (+ (* i width-per) (screen-x output))
	      (y window) (screen-y output)
	      (compo-max-width window) width-per
	      (compo-max-height window) d-height)

	     (configure-toplevel-default window))))))
