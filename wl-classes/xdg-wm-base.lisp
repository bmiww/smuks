
;; ██╗  ██╗██████╗  ██████╗
;; ╚██╗██╔╝██╔══██╗██╔════╝
;;  ╚███╔╝ ██║  ██║██║  ███╗
;;  ██╔██╗ ██║  ██║██║   ██║
;; ██╔╝ ██╗██████╔╝╚██████╔╝
;; ╚═╝  ╚═╝╚═════╝  ╚═════╝
(in-package :smuks)

;; ┬ ┬┌┬┐  ┌┐ ┌─┐┌─┐┌─┐
;; ││││││  ├┴┐├─┤└─┐├┤
;; └┴┘┴ ┴  └─┘┴ ┴└─┘└─┘
(defclass wm-base (xdg-wm-base:dispatch)
  ((xdg-surfaces :initform (make-hash-table :test 'eq) :accessor xdg-surfaces)))

(defmethod xdg-wm-base:get-xdg-surface ((xdg wm-base) id surface)
  (let ((xdg-surface (wl:up-if 'xdg-surface surface id)))
    (setf (gethash id (xdg-surfaces xdg)) xdg-surface)))

(defmethod xdg-wm-base:create-positioner ((xdg wm-base) id)
  (wl:mk-if 'positioner xdg id))


;; ┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐┌─┐
;; └─┐│ │├┬┘├┤ ├─┤│  ├┤
;; └─┘└─┘┴└─└  ┴ ┴└─┘└─┘
(defclass xdg-surface (xdg-surface:dispatch surface)
  ())

(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((display (wl:get-display xdg)))
    (wl:up-if 'toplevel xdg id)
    (new-toplevel display xdg)

    ;; TODO: the last argument - the state - is actually not an enum
    ;; It's an array. So i can't really use the enum logic here
    ;; The xml also doesn't define that the array here would be filled with enum values
    (xdg-toplevel:send-configure xdg (width xdg) (height xdg) '(1))
    (xdg-surface:send-configure xdg (incf (configure-serial xdg)))))

(defmethod xdg-surface:get-popup ((xdg xdg-surface) id parent positioner)
  (wl:up-if 'popup xdg id :positioner positioner :grab-parent parent)
  (setf (grab-child parent) xdg)
  (xdg-popup:send-configure xdg (x positioner) (y positioner) (width positioner) (height positioner))
  (xdg-surface:send-configure xdg (incf (configure-serial xdg))))

(defmethod xdg-surface:set-window-geometry ((xdg xdg-surface) x y width height)
  (setf (width xdg) width)
  (setf (height xdg) height)
  (setf (new-dimensions? xdg) t))

;; NOTE: For now leaving empty - but could be used in some way to finalize
;; The configuration sequence. Applying pending state or whatnot. Not sure
(defmethod xdg-surface:ack-configure ((xdg xdg-surface) serial)
  ())


;; ┌─┐┬─┐┌─┐┌┐ ┌┐ ┌─┐┌┐ ┬  ┌─┐
;; │ ┬├┬┘├─┤├┴┐├┴┐├─┤├┴┐│  ├┤
;; └─┘┴└─┴ ┴└─┘└─┘┴ ┴└─┘┴─┘└─┘
;; TODO: This could also just be part of xdg-surface probably
(defclass grabbable ()
  ((grab-child :initform nil :accessor grab-child)
   (grab-parent :initarg :grab-parent :initform nil :accessor grab-parent))
  (:documentation "A grabbable object is an object that can have a grab child
Destroying the grabbable object will also destroy the grab child"))


;; ┌┬┐┌─┐┌─┐┬  ┌─┐┬  ┬┌─┐┬
;;  │ │ │├─┘│  ├┤ └┐┌┘├┤ │
;;  ┴ └─┘┴  ┴─┘└─┘ └┘ └─┘┴─┘
(defclass toplevel (xdg-toplevel:dispatch xdg-surface grabbable)
  ((title :initform nil :accessor title)
   (app-id :initform nil :accessor app-id)
   (parent :initform nil :accessor parent)
   (min-width :initform 0 :accessor min-width)
   (min-height :initform 0 :accessor min-height)
   (max-width :initform 0 :accessor max-width)
   (max-height :initform 0 :accessor max-height)
   (desktop :initform nil :accessor desktop)))

(defmethod xdg-toplevel:set-title ((toplevel toplevel) title)
  (setf (title toplevel) title))

(defmethod xdg-toplevel:set-app-id ((toplevel toplevel) app-id)
  (setf (app-id toplevel) app-id))

;; TODO: For now keeping the move request empty
;; Since you probably want tiling - this will mostly be ignored
;; But - could introduce specific states/flags
(defmethod xdg-toplevel:move ((toplevel toplevel) seat serial)
  (log! "xdg-toplevel:move: Not implemented"))

(defmethod xdg-toplevel:set-parent ((toplevel toplevel) parent)
  (log! "xdg-toplevel:set-parent: Not implemented fully")
  (when parent (setf (parent toplevel) parent)))


;; TODO: xdg-toplevel:set-min-size: size limitations still ignored
(defmethod xdg-toplevel:set-min-size ((toplevel toplevel) width height)
  (setf (min-width toplevel) width)
  (setf (min-height toplevel) height))

;; TODO: xdg-toplevel:set-max-size: size limitations still ignored
(defmethod xdg-toplevel:set-max-size ((toplevel toplevel) width height)
  (setf (max-width toplevel) width)
  (setf (max-height toplevel) height))

;; TODO: Integrate better with the sizing/positioning once you figure that out
(defmethod xdg-toplevel:set-maximized ((toplevel toplevel))
  "A client wants to maximize their window to maximum size.
For tiling managers - i think i'll just resend the original configure event.
Supposed to answer with a configure event showing the new size."
  (log! "xdg-toplevel:set-maximized: Not considered in great detail")
  (xdg-toplevel:send-configure toplevel (width toplevel) (height toplevel) '(1))
  (xdg-surface:send-configure toplevel (incf (configure-serial toplevel))))

(defmethod xdg-toplevel:unset-maximized ((toplevel toplevel))
  "A client wants to unset maximized state.
For tiling managers - i think i'll just resend the original configure event.
Supposed to answer with a configure event showing the new size."
  (log! "xdg-toplevel:unset-maximized: Not considered in great detail")
  (xdg-toplevel:send-configure toplevel (width toplevel) (height toplevel) '(1))
  (xdg-surface:send-configure toplevel (incf (configure-serial toplevel))))

(defmethod xdg-toplevel:resize ((toplevel toplevel) seat serial edges)
  "A client wants to resize their window."
  (log! "xdg-toplevel:resize: Not implemented"))


(defmethod xdg-toplevel:set-fullscreen ((toplevel toplevel) output)
  (let* ((display (wl:get-display toplevel))
	 (desktop (find-output-desktop display output))
	 (screen (screen desktop)))
    ;; TODO: None of the stupid cases where the states are sent as an array without a specified enum. So we use 2 as :fullscreen
    (xdg-toplevel:send-configure toplevel (screen-width screen) (screen-height screen) '(2))
    (xdg-surface:send-configure toplevel (incf (configure-serial toplevel)))))


;; TODO: The configure sizes as 0 might be ridiculous
;; For now assuming that this will tell the client that it should offer a resize
(defmethod xdg-toplevel:unset-fullscreen ((toplevel toplevel))
  "A client wants to unset fullscreen state."
  (xdg-toplevel:send-configure toplevel 0 0 '(1))
  (xdg-surface:send-configure toplevel (incf (configure-serial toplevel))))


;; ┌─┐┌─┐┌─┐┬ ┬┌─┐
;; ├─┘│ │├─┘│ │├─┘
;; ┴  └─┘┴  └─┘┴
;; TODO: As far as i can understand the positioner is transient and used only for the duration of the popup creation
;; Will have dangling references here possibly
(defclass popup (xdg-popup:dispatch xdg-surface grabbable)
  ((positioner :initarg :positioner :accessor positioner)))

;; TODO: Check if the shared initialize here is dangerous.
;; Might be fine - worst case scenario - scope it to run only when an ACTUAL popup is being created
(defmethod shared-initialize :after ((popup popup) slot-names &key positioner)
  (declare (ignore slot-names))
  (with-slots (x y off-x off-y a-width a-height anchor) positioner
    (incf x off-x) (incf y off-y)
    (case anchor
      (:bottom-left (incf y a-height)))

    (setf (x popup) x (y popup) y)))

(defmethod wl-surface:commit ((popup popup))
  (commit-toplevel popup))


;; TODO: Seat ignored - global seat used instead.
;; TODO: This is supposed to check which nearest toplevel or popup has a keyboard focus on seat level - i think.
;; Since for now i don't do explicit keyboard focus on seat level (i should) - we can try using the active-surface instead
(defmethod xdg-popup:grab ((popup popup) seat serial)
  "Grab here implies keyboard focus. If keyboard focus is lost - TODO"
  (let ((active-surface (active-surface seat)))
    (unless (or active-surface
		(typep active-surface 'toplevel)
		(typep active-surface 'popup))
      (error "No active surface to grab from"))

    ;; TODO: The keys here are supposed to be the currently pressed keys
    (keyboard-enter seat popup '())))

;; ┌─┐┌─┐┌─┐┬┌┬┐┬┌─┐┌┐┌┌─┐┬─┐
;; ├─┘│ │└─┐│ │ ││ ││││├┤ ├┬┘
;; ┴  └─┘└─┘┴ ┴ ┴└─┘┘└┘└─┘┴└─
;; TODO: The initforms for some of these things should probably be keywords rather than symbols
(defclass positioner (xdg-positioner:dispatch)
  ((x :initform 0 :accessor x)
   (y :initform 0 :accessor y)
   (off-x :initform 0 :accessor off-x)
   (off-y :initform 0 :accessor off-y)
   (width :initform 0 :accessor width)
   (height :initform 0 :accessor height)
   (a-width :initform 0 :accessor a-width)
   (a-height :initform 0 :accessor a-height)
   (anchor :initform :bottom-left :accessor anchor)
   (gravity :initform 'top :accessor gravity)
   (constraint :initform 'none :accessor constraint)))

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

(defmethod xdg-positioner:set-gravity ((positioner positioner) gravity)
  (setf (gravity positioner) gravity))

(defmethod xdg-positioner:set-offset ((positioner positioner) x y)
  (setf (off-x positioner) x
	(off-y positioner) y))

(defmethod xdg-positioner:set-constraint-adjustment ((positioner positioner) constraint)
  (setf (constraint positioner) constraint))
