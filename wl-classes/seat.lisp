
;; ███████╗███████╗ █████╗ ████████╗
;; ██╔════╝██╔════╝██╔══██╗╚══██╔══╝
;; ███████╗█████╗  ███████║   ██║
;; ╚════██║██╔══╝  ██╔══██║   ██║
;; ███████║███████╗██║  ██║   ██║
;; ╚══════╝╚══════╝╚═╝  ╚═╝   ╚═╝
;; ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; │ ┬│  │ │├┴┐├─┤│
;; └─┘┴─┘└─┘└─┘┴ ┴┴─┘
(in-package :smuks)
(defclass seat-global (wl-seat:global)
  ((keymap-mmap :initform nil :accessor keymap-mmap)))

;; TODO: For now leaving as one static. But since you'd like to create manipulateable multiseats
;; Then make that possible by getting rid of this static thing
(defvar *static-one-seat-name* "seat0")

(defmethod initialize-instance :after ((seat seat-global) &key)
  (let* ((xkb-context (xkb:xkb-context-new :no-flags))
	 (xkb-keymap (xkb:new-keymap-from-names xkb-context "" "" "" "" ""))
	 (keymap-file (with-xdg-mem-file (stream "keymap" :element-type 'character)
			(format stream (xkb:keymap-get-as-string xkb-keymap :text-v1)))))
    (setf (keymap-mmap seat) keymap-file)))

(defmethod wl-seat:dispatch-bind :after ((global seat-global) client data version id)
  (declare (ignore data version))
  (let* ((interface (wl:iface client id))
	 (capabilities '(:pointer :keyboard :touch)))
    ;; TODO: Somehow some of the weston examples don't like the seat name event
    ;; Since it's not of high importance - for now disabling.
    ;; Might be a version mismatch i guess
    ;; (wl-seat:send-name interface *static-one-seat-name*)
    (wl-seat:send-capabilities interface capabilities)))


;; ┌─┐┌─┐┌─┐┌┬┐
;; └─┐├┤ ├─┤ │
;; └─┘└─┘┴ ┴ ┴
(defclass seat (wl-seat:dispatch)
  ((name :initarg :name :initform nil)
   (capabilities :initarg :capabilities :initform nil)
   (event-serial :initarg :event-serial :initform 0)
   ;; TODO: Rename active surface to identify it as a mouse pointer active surface
   (active-surface :initform nil :accessor active-surface)
   (pointer :initform nil :accessor seat-mouse)
   (pointer-x :initform 0 :accessor pointer-x)
   (pointer-y :initform 0 :accessor pointer-y)
   (keyboard :initform nil :accessor seat-keyboard)
   (touch :initform nil :accessor seat-touch)))

(defmethod cursor-hidden ((seat seat)) (cursor-hidden (seat-mouse seat)))
(defmethod keymap-mmap ((seat seat)) (keymap-mmap (wl:global seat)))

(defmethod next-serial ((seat seat))
  (incf (slot-value seat 'event-serial)))

(defmethod wl-seat:get-keyboard ((seat seat) id)
  (setf (seat-keyboard seat) (wl:mk-if 'keyboard seat id :seat seat))
  (finalize-toplevel (wl:get-display seat)))

(defmethod keyboard-enter ((seat seat) surface keys)
  (let* ((keyboard (seat-keyboard seat)) (display (wl:get-display seat)))
    (wl-keyboard:send-enter keyboard (next-serial display) surface keys)
    (notify-kb-modifiers seat)))

(defun stupid-xkb-modifier-bitfield (display)
  (let ((mods 0))
    (when (k-shift? display) (setf mods (logior mods 1)))
    (when (k-ctrl? display) (setf mods (logior mods 4)))
    (when (k-alt? display) (setf mods (logior mods 8)))
    (when (k-super? display) (setf mods (logior mods 64)))
    mods))

;; TODO: For now ignoring the latched and locked modifiers
;; TODO: For now ignoring the group thing - supposedly it indicates a layout change?
(defmethod notify-kb-modifiers ((seat seat))
  (let* ((display (wl:get-display seat)) (keyboard (seat-keyboard seat)))
    (wl-keyboard:send-modifiers keyboard
				(next-serial display)
				(stupid-xkb-modifier-bitfield display)
				0 0 0)))

(defmethod keyboard-destroy-callback ((seat seat) callback)
  (let* ((keyboard (seat-keyboard seat)))
    (wl:add-destroy-callback keyboard callback)))


;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬
;;  │ │ ││ ││  ├─┤
;;  ┴ └─┘└─┘└─┘┴ ┴
(defmethod wl-seat:get-touch ((seat seat) id)
  (setf (seat-touch seat) (wl:mk-if 'touch seat id :seat seat)))

(defmethod touch-down ((seat seat) surface slot x y)
  (let ((seat-touch (seat-touch seat)))
    (setf (active-surface seat) surface)
    (setf (aref (slot-surfaces seat-touch) slot) surface)
    ;; TODO: Might use the time from libinput - not sure if it was ms though
    (wl-touch:send-down seat-touch (next-serial seat)
			(get-ms) surface slot
			(- x (x surface)) (- y (y surface)))))

(defmethod touch-up ((seat seat) slot)
  (let ((seat-touch (seat-touch seat)))
    (setf (aref (slot-surfaces seat-touch) slot) nil)
    (wl-touch:send-up (seat-touch seat) (next-serial seat) (get-ms) slot)))

(defmethod touch-motion ((seat seat) slot x y)
  (let* ((seat-touch (seat-touch seat))
	 (surface (aref (slot-surfaces seat-touch) slot)))
    ;; TODO: Not sure if this would be a thing - events should come in order
    ;; If not - this probably shouldn't error but just ignore the event
    (unless surface (error "No surface for touch slot. Motion after UP?"))
    (wl-touch:send-motion seat-touch (get-ms) slot
			  (- x (x surface)) (- y (y surface)))))

(defmethod touch-frame ((seat seat)) (wl-touch:send-frame (seat-touch seat)))

;; ┌─┐┌─┐┬┌┐┌┌┬┐┌─┐┬─┐
;; ├─┘│ │││││ │ ├┤ ├┬┘
;; ┴  └─┘┴┘└┘ ┴ └─┘┴└─
(defmethod wl-seat:get-pointer ((seat seat) id)
  (setf (seat-mouse seat) (wl:mk-if 'pointer seat id :seat seat))
  (finalize-toplevel (wl:get-display seat)))

;; TODO: Should the x and y coordinates also set the seat pointer coords
(defmethod pointer-enter ((seat seat) surface x y)
  (let ((seat-mouse (seat-mouse seat)))
    (when seat-mouse
      (setf (active-surface seat) surface)
      (wl-pointer:send-enter
       seat-mouse (next-serial seat) surface
       (surface-x surface x)
       (surface-y surface y))
      (pointer-frame seat))))

(defmethod pointer-leave ((seat seat))
  (let ((seat-mouse (seat-mouse seat)))
    (when seat-mouse
      (if (active-surface seat)
	  (progn
	    (wl-pointer:send-leave seat-mouse (next-serial seat) (active-surface seat))
	    (setf (active-surface seat) nil)
	    (pointer-frame seat))
	  (log! "No active surface for pointer leave")))))

(defmethod pointer-motion ((seat seat) x y)
  (let* ((seat-mouse (seat-mouse seat))
	 (surface (active-surface seat)))
    (when seat-mouse
      (unless surface (error "No active surface for pointer motion"))
      (wl-pointer:send-motion
       seat-mouse (get-ms)
       (surface-x surface (setf (pointer-x seat) x))
       (surface-y surface (setf (pointer-y seat) y)))
      (pointer-frame seat))))

;; TODO: Instead of ignoring button if no surface is active
;; The caller should make sure that an active surface has been provided
(defmethod pointer-button ((seat seat) button state)
  (let* ((seat-mouse (seat-mouse seat))
	 (surface (active-surface seat)))
    (when (and seat-mouse surface)
      (wl-pointer:send-button seat-mouse (next-serial seat) (get-ms) button state)
      (pointer-frame seat))))

(defmethod pointer-scroll-stop ((seat seat) axis)
  (let* ((seat-mouse (seat-mouse seat))
	 (surface (active-surface seat)))
    (when seat-mouse
      (unless surface (error "No active surface for pointer scroll stop"))
      (pointer-axis-stop seat-mouse axis)
      (pointer-frame seat))))

(defmethod pointer-scroll-finger ((seat seat) dx dy)
  (let* ((seat-mouse (seat-mouse seat))
	 (surface (active-surface seat)))
    (when seat-mouse
      (when surface
	(if dx
	    (progn
	      (pointer-axis-source seat-mouse :finger)
	      (wl-pointer:send-axis seat-mouse (get-ms) :vertical-scroll dx))
	    (progn
	      (pointer-axis-source seat-mouse :finger)
	      (pointer-scroll-stop seat :vertical-scroll)))

	(if dy
	    (progn
	      (pointer-axis-source seat-mouse :finger)
	      (wl-pointer:send-axis seat-mouse (get-ms) :horizontal-scroll dy))
	    (progn
	      (pointer-axis-source seat-mouse :finger)
	      (pointer-scroll-stop seat :horizontal-scroll)))

	(pointer-frame seat)))))

(defmethod pointer-frame ((seat seat))
  (let ((mouse (seat-mouse seat)))
    (when (and mouse (>= (wl:version-want seat) 5))
      (wl-pointer:send-frame (seat-mouse seat)))))

;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬  ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;;  │ │ ││ ││  ├─┤   │││└─┐├─┘├─┤ │ │  ├─┤
;;  ┴ └─┘└─┘└─┘┴ ┴  ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
(defclass touch (wl-touch:dispatch)
  ((seat :initarg :seat :initform nil)
   (slot-surfaces :initform (make-array 32 :initial-element nil) :accessor slot-surfaces)))


;; ┌─┐┌─┐┬┌┐┌┌┬┐┌─┐┬─┐  ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;; ├─┘│ │││││ │ ├┤ ├┬┘   │││└─┐├─┘├─┤ │ │  ├─┤
;; ┴  └─┘┴┘└┘ ┴ └─┘┴└─  ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
(defclass pointer (wl-pointer:dispatch)
  ((seat :initarg :seat :initform nil)
   (cursor-hidden :initform nil :accessor cursor-hidden)))

(defmethod pointer-axis-stop ((pointer pointer) axis)
  (when (>= (wl:version-want pointer) 5) (wl-pointer:send-axis-stop pointer (get-ms) axis)))

(defmethod pointer-axis-source ((pointer pointer) axis)
  (when (>= (wl:version-want pointer) 5) (wl-pointer:send-axis-source pointer axis)))

;; TODO: PROTOCOL: If surface is nil - the pointer image should be hidden
(defmethod wl-pointer:set-cursor ((pointer pointer) serial surface hotspot-x hotspot-y)
  (if surface
      (progn
	(change-class surface 'cursor)
	(setf (x surface) hotspot-x (y surface) hotspot-y))
      (setf (cursor-hidden pointer) t)))


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐  ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││   │││└─┐├─┘├─┤ │ │  ├─┤
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘  ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
(defclass keyboard (wl-keyboard:dispatch)
  ((seat :initarg :seat :initform nil :accessor seat)))

(defvar *key-repeat-rate* 4)
(defvar *key-repeat-delay* 500)

(defmethod initialize-instance :after ((keyboard keyboard) &key)
  (let* ((seat (seat keyboard))
	 (keymap-mmap (keymap-mmap seat)))
    ;; TODO: Explore how many clients actually WANT xkb - i would prefer to roll with no_keymap
    (wl-keyboard:send-keymap keyboard :xkb-v1 (mmap-pool-fd keymap-mmap) (mmap-pool-size keymap-mmap))
    ;; TODO: Not a big fan of key repeats in general - but not going to block clients on this
    ;; Adjust on feel when trying out clients
    ;; Values of 0 could be used to disable repeat behaviour also
    ;; TODO: Since version 4 - until you figure out how to handle versions - disabling
    ;; (wl-keyboard:send-repeat-info keyboard *key-repeat-rate* *key-repeat-delay*)
    ))
