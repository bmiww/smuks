
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
  ())

;; TODO: For now leaving as one static. But since you'd like to create manipulateable multiseats
;; Then make that possible by getting rid of this static thing
(defvar *static-one-seat-name* "seat0")

(defvar *pointer* 1)
(defvar *keyboard* 2)
(defvar *touch* 4)

(defmethod wl-seat:dispatch-bind :after ((global seat-global) client data version id)
  (declare (ignore data version))
  (let* ((interface (wl:iface client id))
	 ;; TODO: For now making touch a capability statically.
	 ;; In ideal - you would first check libinput for device capabilities
	 (capabilities *touch*))
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
   (pointer :initform nil :accessor seat-pointer)
   (keyboard :initform nil :accessor seat-keyboard)
   (touch :initform nil :accessor seat-touch)))

(defmethod next-serial ((seat seat))
  (incf (slot-value seat 'event-serial)))

(defmethod wl-seat:get-keyboard ((seat seat) id)
  (error "Keyboard asked - none was there"))

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
  (setf (seat-pointer seat) (wl:mk-if 'pointer seat id :seat seat)))

(defmethod pointer-enter ((seat seat) surface x y)
  (let ((seat-pointer (seat-pointer seat)))
    (setf (active-surface seat-pointer) surface)
    (wl-pointer:send-enter seat-pointer (next-serial seat) surface
			   (- x (x surface)) (- y (y surface)))))

(defmethod pointer-motion ((seat seat) x y)
  (let* ((seat-pointer (seat-pointer seat))
	 (surface (active-surface seat-pointer)))
    (unless surface (error "No active surface for pointer motion"))
    (wl-pointer:send-motion seat-pointer (get-ms) (- x (x surface)) (- y (y surface)))))


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
