
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
   (dev-t :initarg :dev-t :accessor dev-t)
   (display-serial :initform 0 :accessor display-serial)
   (keyboard-focus :initform nil)
   (pointer-focus :initform nil :accessor pointer-focus)
   (pending-drag :initform nil :accessor pending-drag)
   (windows :initform nil :accessor windows)
   (screens :initarg :screen-tracker :initform nil :accessor screens)))

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


;; ┬┌┐┌┌─┐┬ ┬┌┬┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; ││││├─┘│ │ │   ├─┤├─┤│││ │││  │││││ ┬
;; ┴┘└┘┴  └─┘ ┴   ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod input ((display display) type event)
  (cond
    ((configuring-neighbors? (screens display)) (process display type :screen-setup event))
    (t (process display type :passthrough event))))

(defmethod input ((display display) (type (eql :pointer-axis)) event)
  "This is deprecated in libinput >1.19. Therefore ignorable.")


;; ┬ ┬┬┌┐┌┌┬┐┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┬┌┐┌┌─┐
;; │││││││ │││ ││││  ├─┤├─┤│││ │││  │││││ ┬
;; └┴┘┴┘└┘─┴┘└─┘└┴┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘┴┘└┘└─┘
(defmethod recalculate-layout ((display display))
  (when (windows display)
    ;; TODO: Replace the car of screens by an actual iteration through screens
    (let* ((first (car (screens (screens display))))
	   (d-width (screen-width first)) (d-height (screen-height first))
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

(defmethod update-cursor ((display display) dx dy)
  (let ((new-x (+ (cursor-x display) dx))
	(new-y (+ (cursor-y display) dy)))
    (setf (values (cursor-x display) (cursor-y display)) (bounds-check (screens display) new-x new-y))))

(defmethod orient-point ((display display) x y)
  (error "While working on multi-screen support, you broke most of the touchscreen stuff")
  ;; (case (orientation *first*)
    ;; (:landscape (values y (- (screen-height *first*) x)))
    ;; (:portrait (- (screen-width *first*) x)))
  )
