
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
   (pointer :initarg :pointer :initform nil)
   (keyboard :initarg :keyboard :initform nil)
   (touch :initform nil :accessor seat-touch)))

(defmethod next-serial ((seat seat))
  (incf (slot-value seat 'event-serial)))

(defmethod wl-seat:get-pointer ((seat seat) id)
  (error "Pointer asked - none was there"))

(defmethod wl-seat:get-keyboard ((seat seat) id)
  (error "Keyboard asked - none was there"))

(defmethod wl-seat:get-touch ((seat seat) id)
  (setf (seat-touch seat) (wl:mk-if 'touch seat id :seat seat)))

(defmethod touch-down ((seat seat) surface slot x y)
  ;; TODO: Might use the time from libinput - not sure if it was ms though
  (wl-touch:send-down (seat-touch seat) (next-serial seat) (get-ms) surface
		      slot (- x (x surface)) (- y (y surface))))

(defmethod touch-up ((seat seat) slot)
  (wl-touch:send-up (seat-touch seat) (next-serial seat) (get-ms) slot))

(defmethod touch-frame ((seat seat)) (wl-touch:send-frame (seat-touch seat)))

;; ┌┬┐┌─┐┬ ┬┌─┐┬ ┬
;;  │ │ ││ ││  ├─┤
;;  ┴ └─┘└─┘└─┘┴ ┴
(defclass touch (wl-touch:dispatch)
  ((seat :initarg :seat :initform nil)))
