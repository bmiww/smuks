
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
   (cursor-x :initform 0 :accessor cursor-x)
   (cursor-y :initform 0 :accessor cursor-y)
   ;; TODO: Both of these are dumb - these should be per CRTC/monitor/whatever
   (display-width :initarg :display-width :accessor display-width)
   (display-height :initarg :display-height :accessor display-height)))

(defmethod input ((display display) type event)
  (log! "No handler for input event: ~a~%" (event-type event)))


;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;;  │ │ ││ ││  ├─┤  ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;;  ┴ └─┘└─┘└─┘┴ ┴  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod input ((display display) (type (eql :touch-down)) event)
  "Notify a client that a touch event has occured.
Save the client as interested in the slot for later reference."
  (let* ((x (flo (touch@-x event)))
	 (y (flo (touch@-y event)))
	 (slot (touch@-seat-slot event))
	 (surface (surface-at-coords *wayland* x y)))
    (when surface
      (let* ((client (wl:client surface))
	     (seat (seat client)))
	(when (seat-touch seat)
	  (pushnew client (aref (touch-slot-interesses display) slot))
	  (touch-down seat surface slot x y))))))

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
(defmethod input ((display display) (type (eql :pointer-motion)) event)
  (let* ((new-x (+ (cursor-x display) (flo (pointer-motion@-dx event))))
	 (new-y (+ (cursor-y display) (flo (pointer-motion@-dy event))))
	 (new-x (max 0 (min (display-width display) new-x)))
	 (new-y (max 0 (min (display-height display) new-y)))
	 (surface (surface-at-coords *wayland* new-x new-y)))

    (when surface
      (let* ((client (wl:client surface))
	     (seat (seat client))
	     (seat-pointer (seat-pointer seat)))
	(log! "Client: ~a~%" client)
	(log! "Seat: ~a~%" seat)
	(log! "Seat-pointer: ~a~%" seat-pointer)
	(if (and seat-pointer (active-surface seat-pointer))
	    (pointer-motion seat new-x new-y)
	    (when seat-pointer (pointer-enter seat surface new-x new-y)))))
    (setf (cursor-x display) new-x)
    (setf (cursor-y display) new-y)))

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
