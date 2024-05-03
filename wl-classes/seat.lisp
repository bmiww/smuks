
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
    (wl-seat:send-name interface *static-one-seat-name*)
    (wl-seat:send-capabilities interface capabilities)
    (setf (gethash id (seats client)) global)))


;; ┌─┐┌─┐┌─┐┌┬┐
;; └─┐├┤ ├─┤ │
;; └─┘└─┘┴ ┴ ┴
(defclass seat (wl-seat:dispatch)
  ((name :initarg :name :initform nil)
   (capabilities :initarg :capabilities :initform nil)
   (pointer :initarg :pointer :initform nil)
   (keyboard :initarg :keyboard :initform nil)
   (touch :initarg :touch :initform nil)))

(defmethod wl-seat:get-pointer ((seat seat) id)
  (error "Pointer asked - none was there"))

(defmethod wl-seat:get-keyboard ((seat seat) id)
  (error "Keyboard asked - none was there"))

(defmethod wl-seat:get-touch ((seat seat) id)
  (error "Touch asked - none was there"))
