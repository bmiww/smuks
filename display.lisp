
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
   (egl :initarg :egl :accessor egl)
   (cursor-x :initform 0 :accessor cursor-x)
   (cursor-y :initform 0 :accessor cursor-y)
   (cursor-screen :initform nil :accessor cursor-screen)
   ;; TODO: Both of these are dumb - these should be per CRTC/monitor/whatever
   (dev-t :initarg :dev-t :accessor dev-t)
   (display-serial :initform 0 :accessor display-serial)
   (keyboard-focus :initform nil)
   (pointer-focus :initform nil :accessor pointer-focus)
   (pending-drag :initform nil :accessor pending-drag)
   (screens :initarg :screen-tracker :initform nil :accessor screens)
   (desktops :initform nil :accessor desktops)
   (active-desktop :initform nil :accessor active-desktop)
   (outputs :initform nil :accessor outputs)

   (k-alt? :initform nil :accessor k-alt?)
   (k-ctrl? :initform nil :accessor k-ctrl?)
   (k-shift? :initform nil :accessor k-shift?)
   (k-super? :initform nil :accessor k-super?)))


(defmethod initialize-instance :after ((display display) &key screen-tracker)
  (multiple-value-bind (x y screen) (bounds-check screen-tracker (cursor-x display) (cursor-y display))
    (declare (ignore x y))
    (setf (cursor-screen display) screen))

  ;; NOTE: For now creating 10 desktops each for one number key
  (setf (desktops display)
	(loop for i from 0 below 10
	      collect (make-instance 'desktop)))

  (setf (active-desktop display) (first (desktops display)))

  ;; NOTE: For each screen we have available attach it to a different desktop
  (loop for screen in (screens (screens display))
	for desktop in (desktops display)
	do (setf (screen desktop) screen)))


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
  (make-instance 'layer-shell-global :display display :dispatch-impl 'layer-shell)
  (make-instance 'zxdg-decoration-manager-v1:global :display display :dispatch-impl 'decoration-manager)
  (setf (outputs display)
	(loop for screen in screens
	      collect (init-output display screen))))


;; TODO: This is very incomplete.
;; Lots of fake stuff here
;; Real width/height are just width/height - should be mm of real screen size
;; X/Y are just 0,0 - since i'm only handling one screen
(defmethod init-output ((display display) screen)
  (make-instance 'output-global :display display :dispatch-impl 'output
		    :x 0 :y 0
		    :width (width screen) :height (height screen)
		    :screen screen
		    :real-width (width screen) :real-height (height screen)
		    :refresh-rate (sdrm:vrefresh screen)
		    :make "TODO: Fill out make" :model "TODO: Fill out model"))


;; ┌┬┐┌─┐┌─┐┬  ┌─┐
;;  │ │ ││ ││  └─┐
;;  ┴ └─┘└─┘┴─┘└─┘
(defmethod next-serial ((display display)) (incf (display-serial display)))

(defmethod (setf keyboard-focus) (focus-surface (display display))
  (if focus-surface
      (let* ((client (wl:client focus-surface))
	     (seat (seat client)))
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
    ((configuring-neighbors? (screens display)) (process display type :screen-setup event))
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
    (setf (values (cursor-x display) (cursor-y display) (cursor-screen display))
	  (bounds-check (screens display) new-x new-y))))

(defmethod orient-point ((display display) x y)
  (error "While working on multi-screen support, you broke most of the touchscreen stuff")
  ;; (case (orientation *first*)
    ;; (:landscape (values y (- (screen-height *first*) x)))
    ;; (:portrait (- (screen-width *first*) x)))
  )


(defmethod find-output-desktop ((display display) output)
  (let ((screen (output-screen (wl:global output))))
    (find-screen-desktop display screen)))

(defmethod find-screen-desktop ((display display) screen)
  (find screen (desktops display) :key #'screen))


;; ┌┬┐┌─┐┌─┐┬┌─┌┬┐┌─┐┌─┐┌─┐
;;  ││├┤ └─┐├┴┐ │ │ │├─┘└─┐
;; ─┴┘└─┘└─┘┴ ┴ ┴ └─┘┴  └─┘
;; TODO: Move this
;; TODO: Rename to workspace or something???
;; TODO: Because of desktop - drag & drop icon surfaces aren't rendering in a desktop other than the originating desktop
(defclass desktop ()
  ((screen :initform nil :initarg :screen :accessor screen)
   (windows :initform nil :accessor windows)
   (fullscreen-window :initform nil :accessor fullscreen-window)))

(defmethod has-screen ((desktop desktop) screen) (eq screen (screen desktop)))

(defmethod width ((desktop desktop)) (screen-width (screen desktop)))
(defmethod height ((desktop desktop)) (screen-height (screen desktop)))

(defmethod recalculate-layout ((desktop desktop))
  (when (windows desktop)
    ;; TODO: Replace the car of screens by an actual iteration through screens
    (let* ((screen (screen desktop))
	   (d-width (screen-width screen)) (d-height (screen-height screen))
	   (amount (length (windows desktop)))
	   (width-per (floor (/ d-width amount))))
      (loop
	for window in (windows desktop)
	for i from 0
	do (with-slots (x y width height) window
	     (setf x (+ (* i width-per) (screen-x screen))
		   y (+ 0 (screen-y screen))
		   width width-per
		   height d-height)
	     (configure-toplevel window))))))
