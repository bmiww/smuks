
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
   (egl :accessor egl)
   (cursor-x :initform 0 :accessor cursor-x)
   (cursor-y :initform 0 :accessor cursor-y)
   ;; TODO: Both of these are dumb - these should be per CRTC/monitor/whatever
   (display-width :initarg :display-width)
   (display-height :initarg :display-height)
   (dev-t :initarg :dev-t :accessor dev-t)
   (display-serial :initform 0 :accessor display-serial)
   (keyboard-focus :initform nil)
   (pointer-focus :initform nil :accessor pointer-focus)
   (orientation :initarg :orientation :initform :landscape :accessor orientation)
   (windows :initform nil :accessor windows)))

(defmethod wl:rem-client :before ((display display) client)
  (with-slots (keyboard-focus pointer-focus) display
    (when (and pointer-focus (eq (wl:client pointer-focus) client)) (setf (pointer-focus display) nil))
    (when (and keyboard-focus (eq (wl:client keyboard-focus) client)) (setf (keyboard-focus display) nil))))

;; ┌─┐┌─┐┌┬┐┬ ┬┌─┐
;; └─┐├┤  │ │ │├─┘
;; └─┘└─┘ ┴ └─┘┴

;; TODO: I don't much like the drm arg here
(defmethod init-globals ((display display) screens)
  ;; TODO: When you recompile the compiled classes - these globals aren't updated, needing a rerun
  (make-instance 'wl-compositor:global :display display :dispatch-impl 'compositor)
  (make-instance 'wl-subcompositor:global :display display :dispatch-impl 'subcompositor)
  (make-instance 'shm-global :display display :dispatch-impl 'shm)
  (make-instance 'seat-global :display display :dispatch-impl 'seat)
  (make-instance 'wl-data-device-manager:global :display display :dispatch-impl 'dd-manager)
  (make-instance 'xdg-wm-base:global :display display :dispatch-impl 'wm-base)
  (make-instance 'dmabuf-global :display display :dispatch-impl 'dmabuf)
  (loop for screen in screens
	do (init-output display screen)))


;; TODO: This is very incomplete.
;; Lots of fake stuff here
;; Real width/height are just width/height - should be mm of real screen size
;; X/Y are just 0,0 - since i'm only handling one screen
(defmethod init-output ((display display) screen)
  (make-instance 'output-global :display display :dispatch-impl 'output
		    :x 0 :y 0
		    :width (width screen) :height (height screen)
		    :real-width (width screen) :real-height (height screen)
		    :refresh-rate (sdrm:vrefresh screen)
		    :make "TODO: Fill out make" :model "TODO: Fill out model"))


;; ┬─┐┌─┐┌─┐┌┬┐┌─┐┬─┐┌─┐
;; ├┬┘├┤ ├─┤ ││├┤ ├┬┘└─┐
;; ┴└─└─┘┴ ┴─┴┘└─┘┴└─└─┘
(defmethod display-width ((display display))
  (case (orientation display)
    ((:landscape :landscape-i) (slot-value display 'display-height))
    ((:portrait :portrait-i) (slot-value display 'display-width))))

(defmethod display-height ((display display))
  (case (orientation display)
    ((:landscape :landscape-i) (slot-value display 'display-width))
    ((:portrait :portrait-i) (slot-value display 'display-height))))

;; ┌┬┐┌─┐┌─┐┬  ┌─┐
;;  │ │ ││ ││  └─┐
;;  ┴ └─┘└─┘┴─┘└─┘
(defmethod next-serial ((display display)) (incf (display-serial display)))

(defmethod (setf keyboard-focus) (focus-surface (display display))
  (let* ((client (wl:client focus-surface))
	 (seat (seat client)))
    (setf (slot-value display 'keyboard-focus) focus-surface)
    (keyboard-destroy-callback seat (lambda (keyboard) (declare (ignore keyboard)) (setf (slot-value display 'keyboard-focus) nil)))

    ;; TODO: You're supposed to send the actual pressed keys as last arg
    ;; But currently don't have a keypress manager/tracker
    (keyboard-enter seat (next-serial display) focus-surface '())
    ;; TODO: We are supposed to send the active modifiers after an enter event.
    ;; For now lazy
    (keyboard-modifiers seat (next-serial display) 0 0 0 0)))

(defmethod keyboard-focus ((display display)) (slot-value display 'keyboard-focus))

(defmethod input ((display display) type event)
  (log! "No handler for input event: ~a" (event-type event)))

;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;;  │ │ ││ ││  ├─┤  ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;;  ┴ └─┘└─┘└─┘┴ ┴  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod orient-point ((display display) x y)
  (case (orientation display)
    (:landscape (values y (- (display-height display) x)))
    (:portrait (- (display-width display) x))))


(defmethod input ((display display) (type (eql :touch-down)) event)
  "Notify a client that a touch event has occured.
Save the client as interested in the slot for later reference."
  (let* ((x (flo (touch@-x event)))
	 (y (flo (touch@-y event)))
	 (slot (touch@-seat-slot event)))

    (setf (values x y) (orient-point display x y))
    (let ((surface (surface-at-coords display x y)))
      (when surface
	(let* ((client (wl:client surface))
	       (seat (seat client)))
	  (when (seat-touch seat)
	    (pushnew client (aref (touch-slot-interesses display) slot))
	    (touch-down seat surface slot x y)))))))

(defmethod input ((display display) (type (eql :touch-up)) event)
  "Notify all clients interested in the specific touch slot
and then clean the list out"
  (let* ((slot (touch-up@-seat-slot event))
	 (clients (aref (touch-slot-interesses display) slot)))
    (dolist (client clients)
      (when (seat-touch (seat client))
	(touch-up (seat client) slot)))
    (setf (aref (touch-slot-interesses display) slot) nil)))

(defmethod input ((display display) (type (eql :touch-motion)) event)
  "Notify all clients interested in the specific touch slot of the motion event"
  (let* ((x (flo (touch@-x event)))
	 (y (flo (touch@-y event)))
	 (slot (touch@-seat-slot event))
	 (clients (aref (touch-slot-interesses display) slot))
	 (motiond? nil))

    (setf (values x y) (orient-point display x y))
    (dolist (client clients)
      (when (seat-touch (seat client))
	(setf motiond? t)
	(touch-motion (seat client) slot x y)))
    motiond?))

;; TODO: Not sure if it would be possible or even make sense to keep a list
;; of interested clients instead of broadcasting to all
(defmethod input ((display display) (type (eql :touch-frame)) event)
  "Notify all clients that a touch frame event has occured"
  (declare (ignore event))
  (dolist (client (wl:all-clients display))
    (when (seat-touch (seat client))
      (touch-frame (seat client)))))


;; ┌─┐┌─┐┬┌┐┌┌┬┐┌─┐┬─┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ├─┘│ │││││ │ ├┤ ├┬┘  ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; ┴  └─┘┴┘└┘ ┴ └─┘┴└─  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod add-dx ((display display) dx)
  (let ((width (display-width display))
	(height (display-height display)))
    (case (orientation display)
      (:landscape-i (setf dx (- dx)))
      (:portrait-i (setf dx (- dx))))
    (case (orientation display)
      ((:landscape :landscape-i)
       (incf (cursor-x display) dx)
       (when (>= (cursor-x display) width) (setf (cursor-x display) width))
       (when (< (cursor-x display) 0) (setf (cursor-x display) 0))
       (cursor-x display))
      ((:portrait :portrait-i)
       (incf (cursor-y display) dx)
       (when (>= (cursor-y display) height) (setf (cursor-y display) height))
       (when (< (cursor-y display) 0) (setf (cursor-y display) 0))
       (cursor-y display)))))

(defmethod add-dy ((display display) dy)
  (let ((width (display-width display))
	(height (display-height display)))
    (case (orientation display)
      (:portrait (setf dy (- dy)))
      (:landscape-i (setf dy (- dy))))
    (case (orientation display)
      ((:landscape :landscape-i)
       (incf (cursor-y display) dy)
       (when (>= (cursor-y display) height) (setf (cursor-y display) height))
       (when (< (cursor-y display) 0) (setf (cursor-y display) 0))
       (cursor-y display))
      ((:portrait :portrait-i)
       (incf (cursor-x display) dy)
       (when (>= (cursor-x display) width) (setf (cursor-x display) width))
       (when (< (cursor-x display) 0) (setf (cursor-x display) 0))
       (cursor-x display)))))


;; TODO: This is a bit shit in case if 2 windows are overlapping or next to each other
;; If a pointer focus switches from one surface to another - then hell breaks loose
(defmethod input ((display display) (type (eql :pointer-motion)) event)
  (let* ((new-x (add-dx display (flo (pointer-motion@-dx event))))
	 (new-y (add-dy display (flo (pointer-motion@-dy event))))
	 (surface (surface-at-coords display new-x new-y)))
    (if surface
	(let* ((client (wl:client surface)) (seat (seat client)) (seat-mouse (seat-mouse seat)))
	  (if (and seat-mouse (active-surface seat))
	      (pointer-motion seat new-x new-y)
	      (when seat-mouse
		(pointer-enter seat surface new-x new-y)
		;; TODO: perfrom pointer-laeve on the old (pointer-focus display)
		(setf (pointer-focus display) surface))))
	(when (pointer-focus display)
	  (let* ((client (wl:client (pointer-focus display))) (seat (seat client)))
	    (pointer-leave seat)
	    (setf (pointer-focus display) nil))))))


;; NOTE: Additionally - sets display keyboard focus to the surface
(defmethod input ((display display) (type (eql :pointer-button)) event)
  (let* ((button (pointer-button@-button event))
	 (state (pointer-button@-state event))
	 (surface (surface-at-coords display (cursor-x display) (cursor-y display))))
    (when surface
      (let* ((client (wl:client surface)) (seat (seat client)))
	(setf (keyboard-focus display) surface)
	(pointer-button seat button state)))))


(defmethod input ((display display) (type (eql :pointer-axis)) event)
  "This is deprecated in libinput >1.19. Therefore ignorable.")

(defmethod input ((display display) (type (eql :pointer-scroll-finger)) event)
  (let* ((surface (surface-at-coords display (cursor-x display) (cursor-y display))))
    (when surface
      (let* ((client (wl:client surface)) (seat (seat client)))
	(pointer-scroll-finger seat (pointer-scroll-finger@-dy event) (pointer-scroll-finger@-dx event))))))


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘
;; TODO: Very annoyed by the nil checks here
;; TODO: This does not clean up the keyboard-focus property when a client is gone.
(defmethod input ((display display) (type (eql :keyboard-key)) event)
  (let* ((key (keyboard@-key event))
	 (state (keyboard@-state event))
	 (surface (keyboard-focus display))
	 (client (and surface (wl:client surface)))
	 (seat (and client (seat client)))
	 (seat-keyboard (and seat (seat-keyboard seat))))
    (when seat-keyboard
      ;; tODO: Key needs to be translated to the XKB keycode
      (wl-keyboard:send-key seat-keyboard (next-serial display) (get-ms) key
			    (case state (:pressed 1) (:released 0))))
    ;; Probably F12
    (when (and (eq state :pressed) (eq key 88))
      (shutdown))))


;; ┬ ┬┬┌┐┌┌┬┐┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; │││││││ │││ ││││  ├─┤├─┤│││ │││  │││││ ┬
;; └┴┘┴┘└┘─┴┘└─┘└┴┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod recalculate-layout ((display display))
  (when (windows display)
    (let* ((d-width (display-width display)) (d-height (display-height display))
	   (amount (length (windows display)))
	   (width-per (floor (/ d-width amount))))
      (loop
	for window in (windows display)
	for i from 0
	do (with-slots (x y width height) window
	     (setf x (* i width-per)
		   y 0
		     width width-per
		     height d-height)
	     (xdg-toplevel:send-configure window width height '(1)))))))

(defmethod new-toplevel ((display display) surface)
  (setf (windows display) (pushnew surface (windows display)))
  (wl:add-destroy-callback
   surface
   (lambda (surf)
     (setf (windows display) (remove surf (windows display)))
     (recalculate-layout display)))

  (recalculate-layout display))

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
  (let ((clients (wl:all-clients display)))
    (loop for client in clients
	  for compositor = (compositor client)
	  for surfaces = (all-surfaces compositor)

	  for candidate = (loop for surface in surfaces
				when (in-bounds surface x y)
				  return surface)
	  when candidate
	  return candidate)))
