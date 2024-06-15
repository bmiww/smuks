
;; ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗███████╗
;; ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝██╔════╝
;; ██║██╔██╗ ██║██████╔╝██║   ██║   ██║   ███████╗
;; ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║   ╚════██║
;; ██║██║ ╚████║██║     ╚██████╔╝   ██║   ███████║
;; ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝   ╚══════╝
(in-package :smuks)

(defmethod process ((display display) type (usecase (eql :passthrough)) event)
  (log! "No :passthrough handler for input event: ~a" (event-type event)))

;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;;  │ │ ││ ││  ├─┤  ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;;  ┴ └─┘└─┘└─┘┴ ┴  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
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


;; ┌─┐┌─┐┬┌┐┌┌┬┐┌─┐┬─┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ├─┘│ │││││ │ ├┤ ├┬┘  ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; ┴  └─┘┴┘└┘ ┴ └─┘┴└─  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
;; TODO: This is a bit shit in case if 2 windows are overlapping or next to each other
;; If a pointer focus switches from one surface to another - then hell breaks loose
;; TODO: Might be that leave should be called before enter
(defmethod process ((display display) (type (eql :pointer-motion)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (update-cursor display (flo (pointer-motion@-dx event)) (flo (pointer-motion@-dy event)))
  (let* ((new-x (cursor-x display)) (new-y (cursor-y display))
	 (surface (surface-at-coords display new-x new-y)))
    (if surface
	(let* ((client (wl:client surface))
	       (seat (seat client))
	       (surf-x (- new-x (x surface))) (surf-y (- new-y (y surface))))
	  (when seat
	    (if (and (active-surface seat) (eq (pointer-focus display) surface))
		(progn
		  (pointer-motion seat surf-x surf-y))
		(progn
		  (when (pointer-focus display)
		    (pointer-leave (seat (wl:client (pointer-focus display)))))
		  (pointer-enter seat surface surf-x surf-y)
		  (setf (pointer-focus display) surface)))))
	(when (pointer-focus display)
	  (pointer-leave (seat (wl:client (pointer-focus display))))
	  (setf (pointer-focus display) nil)))))

;; NOTE: Additionally - sets display keyboard focus to the surface
(defmethod process ((display display) (type (eql :pointer-button)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (let* ((button (pointer-button@-button event))
	 (state (pointer-button@-state event))
	 (surface (surface-at-coords display (cursor-x display) (cursor-y display)))
	 (client (and surface (wl:client surface))))

    (when surface
      (let* ((seat (seat client)))
	(when seat
	  (setf (keyboard-focus display) surface)
	  (pointer-button seat button state))))))


(defmethod process ((display display) (type (eql :pointer-scroll-finger)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (let* ((surface (surface-at-coords display (cursor-x display) (cursor-y display))))
    (when surface
      (let* ((client (wl:client surface)) (seat (seat client)))
	(pointer-scroll-finger seat (pointer-scroll-finger@-dy event) (pointer-scroll-finger@-dx event))))))


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘
;; TODO: Very annoyed by the nil checks here
;; TODO: This does not clean up the keyboard-focus property when a client is gone.
(defmethod process ((display display) (type (eql :keyboard-key)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (let* ((key (keyboard@-key event))
	 (state (keyboard@-state event))
	 (surface (keyboard-focus display))
	 (client (and surface (wl:client surface)))
	 (seat (and client (seat client)))
	 (seat-keyboard (and seat (seat-keyboard seat)))
	 (mods-changed? nil))
    (setf mods-changed? (case key
      ;; LEFT ALT and RIGHT ALT
      ((56 100) (setf (k-alt? display) (press? state)) t)
      ;; SUPER/WINDOWS
      (125 (setf (k-super? display) (press? state)) t)
      ;; LEFT SHIFT AND RIGHT SHITF
      ((42 54) (setf (k-shift? display) (press? state)) t)
      ;; LEFT CTRL AND RIGHT CTRL
      ((29 97) (setf (k-ctrl? display) (press? state)) t)
      (t nil)))

    (when seat-keyboard
      (when mods-changed? (notify-kb-modifiers seat))
      ;; tODO: Key needs to be translated to the XKB keycode
      ;; NOTE: Although - i don't know - this seems to be working perfectly fine
      (wl-keyboard:send-key seat-keyboard (next-serial display) (get-ms) key state))
    ;; Probably F12
    (when (and (eq state :pressed) (eq key 88))
      (shutdown))
    (when (and (eq state :pressed) (k-super? display))
      (case key
	;; Key p
	(25 (uiop:launch-program "anyrun"))

	;; Numeric keys - switching desktops
	(2 (setf (active-desktop display) (nth 0 (desktops display))))
	(3 (setf (active-desktop display) (nth 1 (desktops display))))))))

(defun press? (state) (eq state :pressed))
