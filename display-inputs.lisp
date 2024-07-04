
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
(defmethod process ((display display) (type (eql :pointer-motion)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (update-cursor display (flo (pointer-motion@-dx event)) (flo (pointer-motion@-dy event)))
  (let ((surface (focus-pointer-surface display)))
    (when surface (pointer-motion (seat surface) (- (cursor-x display) (x surface)) (- (cursor-y display) (y surface))))))

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
	  (unless (eq (keyboard-focus display) surface) (setf (keyboard-focus display) surface))
	  (pointer-button seat button state))))))


(defmethod process ((display display) (type (eql :pointer-scroll-finger)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (let* ((surface (surface-at-coords display (cursor-x display) (cursor-y display))))
    (when surface
      (let* ((client (wl:client surface)) (seat (seat client)))
	(pointer-scroll-finger seat (pointer-scroll-finger@-dy event) (pointer-scroll-finger@-dx event))))))


;; TODO: I do not know what to emulate via gesture holds so for now this is empty
(defmethod process ((display display) (type (eql :gesture-hold-begin)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  )

;; TODO: I do not know what to emulate via gesture holds so for now this is empty
(defmethod process ((display display) (type (eql :gesture-hold-end)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  )


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘
(defmethod process ((display display) (type (eql :keyboard-key)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (keyboard-key display (keyboard@-key event) (keyboard@-state event)))

;; TODO: Very annoyed by the nil checks here
(defmethod keyboard-key ((display display) key state)
  (let* ((surface (keyboard-focus display))
	 (client (and surface (wl:client surface)))
	 (seat (and client (seat client)))
	 (seat-keyboard (and seat (seat-keyboard seat)))
	 (mods-changed? nil)
	 (press? (eq state :pressed)))
    (setf mods-changed? (case key
      ;; LEFT ALT and RIGHT ALT
      ((56 100) (setf (k-alt? display) press?) t)
      ;; SUPER/WINDOWS
      (125 (setf (k-super? display) press?) t)
      ;; LEFT SHIFT AND RIGHT SHITF
      ((42 54) (setf (k-shift? display) press?) t)
      ;; LEFT CTRL AND RIGHT CTRL
      ((29 97) (setf (k-ctrl? display) press?) t)
      (t nil)))

    (when seat-keyboard
      (when mods-changed? (notify-kb-modifiers seat))
      ;; tODO: Key needs to be translated to the XKB keycode
      ;; NOTE: Although - i don't know - this seems to be working perfectly fine
      (wl-keyboard:send-key seat-keyboard (next-serial display) (get-ms) key state))
    ;; super-shift-*
    (when (and press? (k-super? display) (k-shift? display))
      (case key
	;; enter - Launch a terminal
	(28 (uiop:launch-program "kitty"))
	;; Key c - kill the currently selected window
	(46 (kill-focus-client display))))
    ;; Probably F12
    (when (and press? (eq key 88))
      (shutdown))
    (when (and press? (k-super? display))
      (case key
	;; Key p
	(25 (uiop:launch-program "onagre"))

	;; TODO: Here you wanted to add the "next"/"previous" window focus stuff
	;; Key j
	(36 ())
	;; Key k
	(37 ())

	;; Numeric keys - switching desktops
	(2 (setf (active-desktop display) (nth 0 (desktops display))))
	(3 (setf (active-desktop display) (nth 1 (desktops display))))))))
