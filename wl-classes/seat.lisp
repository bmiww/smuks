
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

(defvar *pointer* 1)
(defvar *keyboard* 2)
(defvar *touch* 4)

(defmethod initialize-instance :after ((seat seat-global) &key)
  (let* ((xkb-context (xkb:xkb-context-new :no-flags))
	 (xkb-keymap (xkb:new-keymap-from-names xkb-context "" "" "" "" ""))
	 (keymap-file (with-xdg-mem-file (stream "keymap" :element-type 'character)
			(format stream (xkb:keymap-get-as-string xkb-keymap :text-v1)))))
    (setf (keymap-mmap seat) keymap-file)))

(defmethod wl-seat:dispatch-bind :after ((global seat-global) client data version id)
  (declare (ignore data version))
  (let* ((interface (wl:iface client id))
	 ;; TODO: you would first check libinput for device capabilities
	 (capabilities (logior *touch* *pointer* *keyboard*)))
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
   (pointer :initform nil :accessor seat-mouse)
   (keyboard :initform nil :accessor seat-keyboard)
   (touch :initform nil :accessor seat-touch)))

(defmethod keymap-mmap ((seat seat)) (keymap-mmap (wl:global seat)))

(defmethod next-serial ((seat seat))
  (incf (slot-value seat 'event-serial)))

(defmethod wl-seat:get-keyboard ((seat seat) id)
  (let* ((client (wl:client seat))
	 (surface (toplevel-surface (compositor client))))
    (setf (seat-keyboard seat) (wl:mk-if 'keyboard seat id :seat seat))
    ;; TODO: Might not be the best idea. Basically - a new client - regardless of what they have - will get a
    ;; Keyboard focus event. And are assumed to be the main keyboard focus.
    ;; But all new clients would get in this method if they request keyboard capabilities - so meh?
    ;; AAAAAAAAH - THIS THING IS HORRIBLE - different clients might initiate resources differently.
    ;; This wont work for all clients.
    ;; I should perform keyboard focus when BOTH TOPLEVEL and KEYBOARD are created.
    (when surface (setf (keyboard-focus (wl:get-display surface)) surface))))

(defmethod keyboard-enter ((seat seat) serial surface keys)
  (let* ((keyboard (seat-keyboard seat)))
    (wl-keyboard:send-enter keyboard serial surface keys)))

(defmethod keyboard-modifiers ((seat seat) serial depressed latched locked group)
  (let* ((keyboard (seat-keyboard seat)))
    (wl-keyboard:send-modifiers keyboard serial depressed latched locked group)))

(defmethod keyboard-destroy-callback ((seat seat) callback)
  (let* ((keyboard (seat-keyboard seat)))
    (setf (wl::destroy-callback keyboard) callback)))


;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬
;;  │ │ ││ ││  ├─┤
;;  ┴ └─┘└─┘└─┘┴ ┴
(defmethod wl-seat:get-touch ((seat seat) id)
  (setf (seat-touch seat) (wl:mk-if 'touch seat id :seat seat)))

(defmethod touch-down ((seat seat) surface slot x y)
  (let ((seat-touch (seat-touch seat)))
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
  (setf (seat-mouse seat) (wl:mk-if 'pointer seat id :seat seat)))

(defmethod pointer-enter ((seat seat) surface x y)
  (let ((seat-mouse (seat-mouse seat)))
    (setf (active-surface seat-mouse) surface)
    (wl-pointer:send-enter seat-mouse (next-serial seat) surface
			   (- x (x surface)) (- y (y surface)))))

(defmethod pointer-leave ((seat seat))
  (let ((seat-mouse (seat-mouse seat)))
    (unless (active-surface seat-mouse) (error "No active surface for pointer leave"))
    (wl-pointer:send-leave seat-mouse (next-serial seat) (active-surface seat-mouse))
    (setf (active-surface seat-mouse) nil)))

(defmethod pointer-motion ((seat seat) x y)
  (let* ((seat-mouse (seat-mouse seat))
	 (surface (active-surface seat-mouse)))
    (unless surface (error "No active surface for pointer motion"))
    (wl-pointer:send-motion seat-mouse (get-ms) (- x (x surface)) (- y (y surface)))))

(defmethod pointer-button ((seat seat) button state)
  (let* ((seat-mouse (seat-mouse seat))
	 (surface (active-surface seat-mouse)))
    (unless surface (error "No active surface for pointer button"))
    (wl-pointer:send-button seat-mouse (next-serial seat) (get-ms) button
			    (case state (:pressed 1) (:released 0)))))

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
   (active-surface :initform nil :accessor active-surface)))

;; TODO: PROTOCOL: If surface is nil - the pointer image should be hidden
(defmethod wl-pointer:set-cursor ((pointer pointer) serial surface hotspot-x hotspot-y)
  (when surface
    (setf (role surface) pointer)
    (setf (x surface) hotspot-x)
    (setf (y surface) hotspot-y)))


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐  ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││   │││└─┐├─┘├─┤ │ │  ├─┤
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘  ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
(defclass keyboard (wl-keyboard:dispatch)
  ((seat :initarg :seat :initform nil :accessor seat)))

(defvar *key-repeat-rate* 4)
(defvar *key-repeat-delay* 500)

(defmethod initialize-instance :after ((keyboard keyboard) &key)
  (let* ((seat (seat keyboard))
	 (keymap-mmap (keymap-mmap seat))
	 (client (wl:client keyboard))
	 (surface (toplevel-surface (compositor client))))
    ;; TODO: 1 stands for xkb-keymap. Enumerate this properly
    ;; TODO: Explore how many clients actually WANT xkb - i would prefer to roll with no_keymap
    (wl-keyboard:send-keymap keyboard 1 (mmap-pool-fd keymap-mmap) (mmap-pool-size keymap-mmap))
    ;; TODO: Not a big fan of key repeats in general - but not going to block clients on this
    ;; Adjust on feel when trying out clients
    ;; Values of 0 could be used to disable repeat behaviour also
    (wl-keyboard:send-repeat-info keyboard *key-repeat-rate* *key-repeat-delay*)))
