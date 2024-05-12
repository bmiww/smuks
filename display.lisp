
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
   (display-width :initarg :display-width :accessor display-width)
   (display-height :initarg :display-height :accessor display-height)
   (dev-t :initarg :dev-t :accessor dev-t)
   (display-serial :initform 0 :accessor display-serial)
   (keyboard-focus :initform nil)))

(defmethod next-serial ((display display)) (incf (display-serial display)))

(defmethod (setf keyboard-focus) (focus-surface (display display))
  (let* ((client (wl:client focus-surface))
	 (seat (seat client))
	 (seat-keyboard (seat-keyboard seat)))
    (setf (slot-value display 'keyboard-focus) focus-surface)

    ;; TODO: You're supposed to send the actual pressed keys as last arg
    ;; But currently don't have a keypress manager/tracker
    (wl-keyboard:send-enter seat-keyboard (next-serial display)
			    focus-surface '())
    ;; TODO: We are supposed to send the active modifiers after an enter event.
    ;; For now lazy
    (wl-keyboard:send-modifiers seat-keyboard (next-serial display) 0 0 0 0)))

(defmethod keyboard-focus ((display display)) (slot-value display 'keyboard-focus))

(defmethod input ((display display) type event)
  (log! "No handler for input event: ~a" (event-type event)))

;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;;  │ │ ││ ││  ├─┤  ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;;  ┴ └─┘└─┘└─┘┴ ┴  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod input ((display display) (type (eql :touch-down)) event)
  "Notify a client that a touch event has occured.
Save the client as interested in the slot for later reference."
  (let* ((x (flo (touch@-x event)))
	 (y (flo (touch@-y event)))
	 (slot (touch@-seat-slot event))
	 (surface (surface-at-coords display x y)))
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
	 (surface (surface-at-coords display new-x new-y)))

    (when surface
      (let* ((client (wl:client surface))
	     (seat (seat client))
	     (seat-pointer (seat-pointer seat)))
	(log! "Client: ~a" client)
	(log! "Seat: ~a" seat)
	(log! "Seat-pointer: ~a" seat-pointer)
	(if (and seat-pointer (active-surface seat-pointer))
	    (pointer-motion seat new-x new-y)
	    (when seat-pointer (pointer-enter seat surface new-x new-y)))))
    (setf (cursor-x display) new-x)
    (setf (cursor-y display) new-y)))


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘
;; TODO: Very annoyed by the nil checks here
(defmethod input ((display display) (type (eql :keyboard-key)) event)
  (let* ((key (keyboard@-key event))
	 (state (keyboard@-state event))
	 (surface (keyboard-focus display))
	 (client (or surface (wl:client surface)))
	 (seat (or client (seat client)))
	 (seat-keyboard (or seat (seat-keyboard seat))))
    (when seat-keyboard
      ;; tODO: Key needs to be translated to the XKB keycode
      (wl-keyboard:send-key seat-keyboard (next-serial display) (get-ms) key
			    (case state (:pressed 1) (:released 0))))
    ;; Probably F12
    (when (and (eq state :pressed) (eq key 88))
      (shutdown))))


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
