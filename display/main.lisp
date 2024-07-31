
;; ██████╗ ██╗███████╗██████╗ ██╗      █████╗ ██╗   ██╗
;; ██╔══██╗██║██╔════╝██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝
;; ██║  ██║██║███████╗██████╔╝██║     ███████║ ╚████╔╝
;; ██║  ██║██║╚════██║██╔═══╝ ██║     ██╔══██║  ╚██╔╝
;; ██████╔╝██║███████║██║     ███████╗██║  ██║   ██║
;; ╚═════╝ ╚═╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝
(in-package :smuks)

(defvar *framebuffer-count* 2)

(defclass display (wl:display)
  ;; Not sure we need 32. That's a lot of fingers.
  ((touch-slot-interesses :initform (make-array 32 :initial-element nil) :reader touch-slot-interesses)
   (drm :initform nil :initarg :drm :accessor drm)
   (egl :initarg :egl :accessor egl)
   (libseat :initarg :libseat :accessor libseat)
   (gl-version :initarg :gl-version :accessor gl-version)

   (compositor :initform nil :accessor compositor)

   ;; TODO: I would like to turn this into a coord struct - but naming is weird.
   ;; coord assumes desktop. This uses output.
   (cursor-x :initform 0 :accessor cursor-x)
   (cursor-y :initform 0 :accessor cursor-y)
   (cursor-screen :initform nil :accessor cursor-screen)

   (display-serial :initform 0 :accessor display-serial)

   (keyboard-focus :initform nil)
   (exclusive-keyboard-focus :initform nil :accessor exclusive-keyboard-focus)

   (pointer-focus :initform nil :accessor pointer-focus)
   (pending-drag :initform nil :accessor pending-drag)

   (active-desktop :initform nil :accessor active-desktop)
   (outputs :initform nil :accessor outputs)

   (k-alt? :initform nil :accessor k-alt?)
   (k-ctrl? :initform nil :accessor k-ctrl?)
   (k-shift? :initform nil :accessor k-shift?)
   (k-super? :initform nil :accessor k-super?)))


(defmethod initialize-instance :after ((display display) &key)
  )


(defgeneric input (display type event))
(defgeneric process (display type usecase event))

(defmethod desktops ((display display)) (desktops (compositor display)))

(defmethod wl:rem-client :before ((display display) client)
  (with-slots (keyboard-focus pointer-focus pending-drag) display
    (when (and pointer-focus (eq (wl:client pointer-focus) client)) (setf (pointer-focus display) nil))
    (when (and keyboard-focus (eq (wl:client keyboard-focus) client)) (setf (keyboard-focus display) nil))
    (when (and pending-drag (eq (wl:client pending-drag) client)) (setf (pending-drag display) nil))))

(defmethod wl:rem-client :after ((display display) client)
  (handle-surface-change display))


(defmethod (setf active-desktop) :before (new-desktop (display display))
  (when (active-desktop display)
    (unless (eq new-desktop (active-desktop display))
      (setf (output new-desktop) (output (active-desktop display)))
      (recalculate-layout new-desktop)

      (setf (output (active-desktop display)) nil)
      (handle-surface-change display))))

(defmethod set-active-desktop-nr ((display display) nr)
  (setf (active-desktop display) (nth nr (desktops display))))

;; ┌─┐┌─┐┌┬┐┬ ┬┌─┐
;; └─┐├┤  │ │ │├─┘
;; └─┘└─┘ ┴ └─┘┴

(defmethod init-globals ((display display))
  ;; TODO: When you recompile the compiled classes - these globals aren't updated, needing a rerun
  (setf (compositor display) (make-instance 'compo :display display :dispatch-impl 'compositor))
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

  (setf (active-desktop display) (first (desktops display)))

  ;; TODO: Needs outputs at this point. Could be moved if i rewrite initialization
  ;; NOTE: For each screen we have available attach it to a different desktop
  (loop for output in (outputs display)
	for desktop in (desktops display)
	do (setf (output desktop) output))

  ;; TODO: Setting initial cursor pos. Should be part of init init. But don't have outputs at that point yet
  (multiple-value-bind (x y output) (bounds-check display (cursor-x display) (cursor-y display))
    (declare (ignore x y))
    (setf (cursor-screen display) output)))

(defun update-existing-output (output connector)
  (declare (ignore connector))
  (log! "Updating existing output (TODO: Implement update): ~a" output))

(defmethod get-highest-screen-y ((display display))
  (let ((highest 0))
    (loop for output in (outputs display)
	  when (> (screen-y-max output) highest)
	    do (setf highest (screen-y-max output)))
    highest))

(defmethod init-outputs ((display display) &optional refresh)
  (let ((connectors (if refresh (sdrm::recheck-resources (drm display)) (connectors (drm display)))))
    ;; NOTE: Sort connectors so that dsi (builtin) is first
    (setf connectors (sort connectors (lambda (a b)
					(declare (ignore b))
					(if (eq (connector-type a) :dsi) t nil))))
    (let ((screen-y (get-highest-screen-y display)) (outputs nil))
      (loop for connector in connectors
	    for index from 0
	    for existing-output = (find-if (lambda (output) (eq (id (connector output)) (id connector))) (outputs display))
	    do (if existing-output
		   ;; TODO: When an output details are updated - notify clients
		   (if (connected (connector existing-output))
		       (progn (update-existing-output existing-output connector) (push existing-output outputs))
		       (remove-output display existing-output))

		   ;; TODO: When output is added notify each clients registry object
		   (when (connected connector)
		     (let ((fb-objs (create-connector-framebuffer (drm display) connector *framebuffer-count*))
			   (height (vdisplay connector)) (width (hdisplay connector)))
		       (when fb-objs
			 (let ((output (make-instance 'output
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
					  :make "TODO: Fill out make" :model "TODO: Fill out model")))
			   (pushnew output outputs)
			   (incf screen-y height)
			   (start-output display output)))))))
      (setf (outputs display) outputs))))


(defmethod start-output ((display display) output)
  (prep-shaders output)
  (loop for desktop in (desktops display)
	when (null (output desktop))
	  return (setf (output desktop) output))
  (start-monitor output)
  (render-frame display output))

;; TODO: When output is removed notify each clients registry object
(defmethod remove-output ((display display) output)
  (log! "Removing output ~a" output)
  (remove-output-from-desktops display output)
  (cleanup-output output)
  (setf (outputs display) (remove output (outputs display))))

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
  (let* ((likely-output (cursor-screen display)))
    (loop for output in (remove likely-output (outputs display))
	  when (and
		(<= (screen-x output) x (screen-x-max output))
		(<= (screen-y output) y (screen-y-max output)))
	    do (setf likely-output output))

    (let ((width (output-width likely-output))
	  (height (output-height likely-output)))
      (values (min (max x (screen-x likely-output)) (+ (screen-x likely-output) width))
	      (min (max y (screen-y likely-output)) (+ (screen-y likely-output) height))
	      likely-output))))

(defmethod next-serial ((display display)) (incf (display-serial display)))

;; ┬┌┐┌┌─┐┬ ┬┌┬┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; ││││├─┘│ │ │   ├─┤├─┤│││ │││  │││││ ┬
;; ┴┘└┘┴  └─┘ ┴   ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod input ((display display) type event)
  (cond
    ((configuring-neighbors? display) (process display type :screen-setup event))
    (t (process display type :passthrough event))))

(defmethod input ((display display) (type (eql :pointer-axis)) event)
  "This is deprecated in libinput >1.19. Therefore ignorable.")

(defmethod input ((display display) (type (eql :device-added)) event)
  "We ignore the device-added event, since we are using libinput path based contexts")

;; ┬ ┬┬┌┐┌┌┬┐┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; │││││││ │││ ││││  ├─┤├─┤│││ │││  │││││ ┬
;; └┴┘┴┘└┘─┴┘└─┘└┴┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod new-toplevel ((display display) surface)
  (let* ((desktop (active-desktop display)))
    (setf (desktop surface) desktop)
    (setf (windows desktop) (pushnew surface (windows desktop)))
    (before wl:destroy surface
	    (lambda (surf)
	      (setf (windows desktop) (remove surf (windows desktop)))
	      (when (eq surface (keyboard-focus display))
		(setf (keyboard-focus display) (car (windows desktop))))
	      (recalculate-layout desktop)))

    (recalculate-layout desktop)

    ;; NOTE: This makes it so that if the surface becomes a child of another
    ;; It shouldn't be tracked as a desktop toplevel anymore
    ;; TODO: This doesn't however cover the case when a surface was before moved to another desktop
    (after xdg-toplevel:set-parent surface
	   (lambda (toplevel parent)
	     (when parent
	       (setf (windows desktop) (remove toplevel (windows desktop)))
	       (recalculate-layout desktop))))

    ;; NOTE: client confirms the window dimensions. We set it as ready for render.
    (let ((surface-configure-serial (awaiting-ack surface)))
      (after wl-surface:commit surface
	     (lambda (toplevel)
	       (if (or
		    (not (awaiting-ack toplevel))
		    (< surface-configure-serial (awaiting-ack toplevel)))
		   (setf (initial-config-ackd toplevel) t)
		   :keep))))))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defmethod surface-at-coords ((display display) x y)
  "Iterate all clients and their surfaces to find one that intersects with the given coordinates"
  (let* ((output (output-at-coords display x y))
	 (desktop (find-output-desktop display output))
	 (compositor (compositor display)))
    (when desktop
      (let ((popups (all-popups compositor)) (toplevels (all-surfaces compositor)))
	(when popups
	  (let ((popup (find-bounder popups x y)))
	    (when popup (return-from surface-at-coords popup))))

	(when toplevels
	  (let ((toplevel (find-desktop-bounder toplevels x y desktop)))
	    (when toplevel (return-from surface-at-coords toplevel))))))))

(defmethod output-at-coords ((display display) x y)
  (loop for output in (outputs display)
	when (and (<= (screen-x output) x (screen-x-max output))
		  (<= (screen-y output) y (screen-y-max output)))
	  return output))

(defun find-desktop-bounder (surfaces x y desktop)
  (loop for surface in surfaces
	when (in-desktop-bounds surface x y desktop)
	  return surface))

(defun find-bounder (surfaces x y)
  (loop for surface in surfaces
	when (in-bounds surface x y)
	  return surface))

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

;; TODO: Suboptimal. Since this is used to check if inputs should be handled differently,
;; This is a nasty amount of extra work that needs to be done
(defmethod configuring-neighbors? ((display display)) (some (lambda (output) (configuring-neighbors output)) (outputs display)))

(defmethod output-by-crtc ((display display) crtc-id) (find-if (lambda (output) (eq (crtc-id (connector output)) crtc-id)) (outputs display)))
(defmethod update-projections ((display display) projection) (mapcar (lambda (output) (update-projections output projection)) (outputs display)))
(defmethod start-monitors ((display display)) (mapcar (lambda (out) (start-monitor out)) (outputs display)))
(defmethod first-desktop-with-output ((display display)) (find-if (lambda (desktop) (output desktop)) (desktops display)))

;; TODO: This also needs to take into account screen positions
;; And overall bounds when outputs are skewed from each other
(defmethod recalculate-dimensions ((display display))
  (let ((screen-y 0))
    (loop for output in (outputs display)
	  do (setf (screen-y output) screen-y)
	     (incf screen-y (output-height output)))))

(defmethod remove-output-from-desktops ((display display) output)
  (loop for desktop in (desktops display)
	when (eq output (output desktop))
	  do (setf (output desktop) nil)
	     (when (eq desktop (active-desktop display))
	       (setf (active-desktop display) (first-desktop-with-output display)))))

(defmethod perform-send-to-desktop ((display display) window desktop)
  (let ((old-desktop (desktop window)))
    (unless (eq old-desktop desktop)
      (setf (desktop window) desktop)

      (add-window desktop window)
      (rm-window old-desktop window)

      (recalculate-layout desktop)
      (recalculate-layout old-desktop))))

(defmethod send-to-desktop ((display display) desktop)
  (let ((window (keyboard-focus display)))
    (when window (perform-send-to-desktop display window desktop))))

(defmethod send-to-output ((display display) output-nr)
  (let ((window (keyboard-focus display)) (output (nth output-nr (outputs display))))
    (when (and window output)
      (let ((desktop (find-output-desktop display output)))
	(perform-send-to-desktop display window desktop)))))


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
	do (render-frame display output)))


;; ┌─┐┬  ┌─┐┌─┐┌┐┌┬ ┬┌─┐
;; │  │  ├┤ ├─┤││││ │├─┘
;; └─┘┴─┘└─┘┴ ┴┘└┘└─┘┴
(defmethod cleanup-display ((display display))
  (with-slots (drm libseat outputs) display
    ;; NOTE: Run cleanup for all outputs
    (stop-measuring-fps display)
    (loop for output in (outputs display)
	  do (cleanup-output output)
	  finally (setf (outputs display) nil))

    ;; Close the libwayland processes and the globals from lisp end
    (wl:destroy display)

    (when drm     (sdrm:close-drm drm))
    (when libseat (libseat:close-seat libseat))

    (setfnil drm libseat)))
