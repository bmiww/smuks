
;; ██╗  ██╗██████╗  ██████╗       ██████╗  ██████╗ ██████╗ ██╗   ██╗██████╗
;; ╚██╗██╔╝██╔══██╗██╔════╝       ██╔══██╗██╔═══██╗██╔══██╗██║   ██║██╔══██╗
;;  ╚███╔╝ ██║  ██║██║  ███╗█████╗██████╔╝██║   ██║██████╔╝██║   ██║██████╔╝
;;  ██╔██╗ ██║  ██║██║   ██║╚════╝██╔═══╝ ██║   ██║██╔═══╝ ██║   ██║██╔═══╝
;; ██╔╝ ██╗██████╔╝╚██████╔╝      ██║     ╚██████╔╝██║     ╚██████╔╝██║
;; ╚═╝  ╚═╝╚═════╝  ╚═════╝       ╚═╝      ╚═════╝ ╚═╝      ╚═════╝ ╚═╝
(in-package :smuks)
(defclass popup (xdg-popup:dispatch xdg-surface)
  ())

(defmethod shared-initialize :after ((popup popup) slot-names &key positioner)
  (declare (ignore slot-names))
  (setup-from-positioner popup positioner))

(defmethod setup-from-positioner ((popup popup) positioner)
  (with-slots (relative-x relative-y) popup
    (with-slots (x y off-x off-y a-width a-height anchor) positioner
      (setf relative-x (+ x off-x))
      (setf relative-y (+ y off-y))
      (case anchor
	(:none nil)
	(:top-left nil)
	(:top (incf relative-x (floor (/ a-width 2))))
	(:bottom (incf relative-x (floor (/ a-width 2))))
	(:left (incf relative-y (floor (/ a-height 2))))
	(:right (incf relative-y (floor (/ a-height 2))))
	(:bottom-left (incf relative-y a-height))
	(:bottom-right (incf relative-x a-width) (incf relative-y a-height))
	(:top-right (incf relative-x a-width)))

      (setf (x popup) relative-x (y popup) relative-y))))

(defmethod xdg-popup:grab ((popup popup) seat serial)
  "Grab here implies keyboard focus. If keyboard focus is lost - TODO"
  (let ((active-surface (active-surface seat)))
    (unless (or active-surface
		(typep active-surface 'toplevel)
		(typep active-surface 'popup))
      (error "No active surface to grab from"))

    ;; TODO: The keys here are supposed to be the currently pressed keys
    (keyboard-enter seat popup '())))

;; TODO: Should 'send-repositioned with the token
(defmethod xdg-popup:reposition ((popup popup) positioner token)
  (setup-from-positioner popup positioner))


;; ██████╗  ██████╗ ███████╗██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗██████╗
;; ██╔══██╗██╔═══██╗██╔════╝██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝██╔══██╗
;; ██████╔╝██║   ██║███████╗██║   ██║   ██║██║   ██║██╔██╗ ██║█████╗  ██████╔╝
;; ██╔═══╝ ██║   ██║╚════██║██║   ██║   ██║██║   ██║██║╚██╗██║██╔══╝  ██╔══██╗
;; ██║     ╚██████╔╝███████║██║   ██║   ██║╚██████╔╝██║ ╚████║███████╗██║  ██║
;; ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝
;; A structure the client fills out.
;; Used to position a popup relative to a parent surface.
(defclass positioner (xdg-positioner:dispatch)
  ((x :initform 0 :accessor x)
   (y :initform 0 :accessor y)
   (off-x :initform 0 :accessor off-x)
   (off-y :initform 0 :accessor off-y)
   (width :initform 0 :accessor width)
   (height :initform 0 :accessor height)
   (a-width :initform 0 :accessor a-width)
   (a-height :initform 0 :accessor a-height)
   (anchor :initform :none :accessor anchor)
   (gravity :initform :top :accessor gravity)
   (constraint :initform '() :accessor constraint)))

;; TODO: Not really doing anything with this yet.
;; Not sure it is that important for me
(defmethod xdg-positioner:set-reactive ((positioner positioner))
  (log! "Setting reactive"))


(defmethod xdg-positioner:set-size ((positioner positioner) width height)
  (setf (width positioner) width
	(height positioner) height))

(defmethod xdg-positioner:set-anchor-rect ((positioner positioner) x! y! width height)
  (with-slots (x y a-width a-height) positioner
    (setf x x!
	  y y!
	  a-width width
	  a-height height)))

(defmethod xdg-positioner:set-anchor ((positioner positioner) anchor)
  (setf (anchor positioner) anchor))

;; TODO: Still have no clue what gravity does in this situation
(defmethod xdg-positioner:set-gravity ((positioner positioner) gravity)
  (log! "Gravity??? ~a" gravity)
  (setf (gravity positioner) gravity))

(defmethod xdg-positioner:set-offset ((positioner positioner) x y)
  (setf (off-x positioner) x
	(off-y positioner) y))

;; TODO: Implement constraint handling
(defmethod xdg-positioner:set-constraint-adjustment ((positioner positioner) constraint)
  "This indicates what to do with the popup surface if it's width/height/x/y is outside the bounds of the parent surface."
  (log! "Setting constraint adjustment: ~a" constraint)
  (setf (constraint positioner) constraint))

;; TODO: Implement set parent size - I'm not yet entirely sure what its supposed to achieve.
;; Says that this should assume the parent size for popup positioning - but not really resize the parent?
(defmethod xdg-positioner:set-parent-size ((positioner positioner) width height)
  (log! "Setting parent size? ~a ~a" width height))

;; TODO: Also not implemented. Should be used in conjuction with set-parent-size
(defmethod xdg-positioner:set-parent-configure ((positioner positioner) serial)
  (log! "Setting parent configure? ~a" serial))
