
;; ████████╗ ██████╗ ██╗   ██╗ ██████╗██╗  ██╗
;; ╚══██╔══╝██╔═══██╗██║   ██║██╔════╝██║  ██║
;;    ██║   ██║   ██║██║   ██║██║     ███████║
;;    ██║   ██║   ██║██║   ██║██║     ██╔══██║
;;    ██║   ╚██████╔╝╚██████╔╝╚██████╗██║  ██║
;;    ╚═╝    ╚═════╝  ╚═════╝  ╚═════╝╚═╝  ╚═╝
(in-package :smuks)

(defmethod process ((display display) (type (eql :touch-down)) (usecase (eql :passthrough)) event)
  "Notify a client that a touch event has occured.
Save the client as interested in the slot for later reference."
  (declare (ignore usecase))
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

(defmethod process ((display display) (type (eql :touch-up)) (usecase (eql :passthrough)) event)
  "Notify all clients interested in the specific touch slot
and then clean the list out"
  (declare (ignore usecase))
  (let* ((slot (touch-up@-seat-slot event))
	 (clients (aref (touch-slot-interesses display) slot)))
    (dolist (client clients)
      (when (seat-touch (seat client))
	(touch-up (seat client) slot)))
    (setf (aref (touch-slot-interesses display) slot) nil)))

(defmethod process ((display display) (type (eql :touch-motion)) (usecase (eql :passthrough)) event)
  "Notify all clients interested in the specific touch slot of the motion event"
  (declare (ignore usecase))
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
(defmethod process ((display display) (type (eql :touch-frame)) (usecase (eql :passthrough)) event)
  "Notify all clients that a touch frame event has occured"
  (declare (ignore usecase))
  (dolist (client (wl:all-clients display))
    (when (seat-touch (seat client))
      (touch-frame (seat client)))))
