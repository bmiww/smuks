
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
  ((xdg-x-offset :initform 0 :accessor xdg-x-offset)
   (xdg-y-offset :initform 0 :accessor xdg-y-offset)
   (grab-child :initform nil :accessor grab-child)
   (grab-parent :initarg :grab-parent :initform nil :accessor grab-parent)

   ;; These are so far only used for toplevels
   (new-x? :initform nil :accessor new-x?)
   (new-y? :initform nil :accessor new-y?))
  (:documentation
   "An xdg surface identifies a toplevel or a popup surface.
The main purpose here is to define that child/parent relationships between the former."))


(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((display (wl:get-display xdg)))
    (wl:up-if 'toplevel xdg id)
    (new-toplevel display xdg)
    (add-state xdg :maximized)
    (configure-toplevel-default xdg)))

(defmethod xdg-surface:get-popup ((xdg xdg-surface) id parent positioner)
  (wl:up-if 'popup xdg id :positioner positioner :grab-parent parent)
  (setf (grab-child parent) xdg)
  (xdg-popup:send-configure xdg (x positioner) (y positioner) (width positioner) (height positioner))
  (xdg-surface:send-configure xdg (incf (configure-serial xdg))))

(defmethod xdg-surface:set-window-geometry ((xdg xdg-surface) x y width height)
  (with-accessors ((new-x? new-x?) (new-y? new-y?) (x x) (y y)) xdg
    (when new-x? (setf x (+ x (/ (- compo-max-width width) 2))))
    (when new-y? (setf y (+ y (/ (- compo-max-height height) 2)))))

  (unless (and (eq width (width xdg)) (eq height (height xdg)))
    (setf (xdg-x-offset xdg) x
	  (xdg-y-offset xdg) y
	  (width xdg) width
	  (height xdg) height
	  (new-dimensions? xdg) t)))

;; NOTE: For now leaving empty - but could be used in some way to finalize
;; The configuration sequence. Applying pending state or whatnot. Not sure
(defmethod xdg-surface:ack-configure ((xdg xdg-surface) serial)
  )


;; ┌┬┐┌─┐┌─┐┬  ┌─┐┬  ┬┌─┐┬
;;  │ │ │├─┘│  ├┤ └┐┌┘├┤ │
;;  ┴ └─┘┴  ┴─┘└─┘ └┘ └─┘┴─┘
(defclass toplevel (xdg-toplevel:dispatch xdg-surface)
  ((title :initform nil :accessor title)
   (app-id :initform nil :accessor app-id)
   (parent :initform nil :accessor parent)
   (min-width :initform 0 :accessor min-width)
   (min-height :initform 0 :accessor min-height)
   (max-width :initform 0 :accessor max-width)
   (max-height :initform 0 :accessor max-height)
   (compo-max-width :initform 0 :accessor compo-max-width)
   (compo-max-height :initform 0 :accessor compo-max-height)
   (desktop :initform nil :accessor desktop)
   (states :initform nil :accessor states)
   (first-commit :initform t :accessor first-commit)))

(defmethod surface-x ((toplevel toplevel) x) (+ x (xdg-x-offset toplevel)))
(defmethod surface-y ((toplevel toplevel) y) (+ y (xdg-y-offset toplevel)))

(defmethod wl-surface:commit :after ((surface toplevel))
  (when (first-commit surface)
    (setf (first-commit surface) nil)
    (handle-surface-change (wl:get-display surface) surface)))

(defmethod xdg-toplevel:set-title ((toplevel toplevel) title)
  (setf (title toplevel) title))

(defmethod xdg-toplevel:set-app-id ((toplevel toplevel) app-id)
  (setf (app-id toplevel) app-id))

;; NOTE: For now keeping the move request empty
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

(defmethod xdg-toplevel:set-maximized ((toplevel toplevel))
  "A client wants to maximize their window to maximum size.
For tiling managers - i think i'll just resend the original configure event.
Supposed to answer with a configure event showing the new size."
  (let ((width (width toplevel))
	(height (height toplevel)))

    (when (< (compo-max-width toplevel) width) (setf width (compo-max-width toplevel)))
    (when (< (compo-max-height toplevel) height) (setf height (compo-max-height toplevel)))

    (add-state toplevel :maximized)
    (do-window-configure toplevel width height)))

(defmethod xdg-toplevel:unset-maximized ((toplevel toplevel))
  "A client wants to unset maximized state.
For tiling managers - i think i'll just resend the original configure event.
Supposed to answer with a configure event showing the new size."
  (let ((width (width toplevel))
	(height (height toplevel)))

    (when (or (< width 0) (< (compo-max-width toplevel) width)) (setf width (compo-max-width toplevel)))
    (when (or (< height 0) (< (compo-max-height toplevel) height)) (setf height (compo-max-height toplevel)))

    (do-window-configure toplevel width height)))

(defmethod xdg-toplevel:set-minimized ((toplevel toplevel))
  "A client wants to minimize their window.
For my purposes - i'm just ignoring this and giving the client the current state as configure"
  (do-window-configure toplevel (width toplevel) (height toplevel)))

(defmethod xdg-toplevel:resize ((toplevel toplevel) seat serial edges)
  "A client wants to resize their window."
  (log! "xdg-toplevel:resize: Not implemented")
  )


(defmethod xdg-toplevel:set-fullscreen ((toplevel toplevel) output)
  (let* ((output (wl:global output))
	 (height (compo-max-height toplevel))
	 (width (compo-max-width toplevel)))
    (when (and (eq (output-height output) height)
	       (eq (output-width output) width))
      (setf height (output-height output))
      (setf width (output-width output)))

    (add-state toplevel :fullscreen)

    (do-window-configure toplevel width height)))

(defmethod xdg-toplevel:unset-fullscreen ((toplevel toplevel))
  "A client wants to unset fullscreen state."
  (rem-state toplevel :fullscreen)
  (do-window-configure toplevel (compo-max-width toplevel) (compo-max-height toplevel)))

(defmethod configure-toplevel-default ((toplevel toplevel))
  (do-window-configure toplevel (compo-max-width toplevel) (compo-max-height toplevel)))


(defmethod do-window-configure ((toplevel toplevel) width height)
  (xdg-toplevel:send-configure toplevel width height (apply 'configure-states (states toplevel)))
  (xdg-surface:send-configure toplevel (incf (configure-serial toplevel))))


(defmethod add-state ((toplevel toplevel) state)
  (pushnew state (states toplevel)))

(defmethod rem-state ((toplevel toplevel) state)
  (setf (states toplevel) (remove state (states toplevel))))

;; ┌─┐┌─┐┌─┐┬ ┬┌─┐
;; ├─┘│ │├─┘│ │├─┘
;; ┴  └─┘┴  └─┘┴
(defclass popup (xdg-popup:dispatch xdg-surface)
  ())

(defmethod shared-initialize :after ((popup popup) slot-names &key positioner)
  (declare (ignore slot-names))
  (setup-from-positioner popup positioner))

(defmethod setup-from-positioner ((popup popup) positioner)
  (with-slots (x y off-x off-y a-width a-height anchor) positioner
    (incf x off-x) (incf y off-y)
    (case anchor
      (:none ())
      (:top (incf x (floor (/ a-width 2))))
      (:bottom (incf x (floor (/ a-width 2))))
      (:left (incf y (floor (/ a-height 2))))
      (:right (incf y (floor (/ a-height 2))))
      (:bottom-left (incf y a-height))
      (:bottom-right (incf x a-width) (incf y a-height))
      (:top-right (incf x a-width))
      (:top-left nil))

    (setf (x popup) x (y popup) y)))

(defmethod xdg-popup:grab ((popup popup) seat serial)
  "Grab here implies keyboard focus. If keyboard focus is lost - TODO"
  (let ((active-surface (active-surface seat)))
    (unless (or active-surface
		(typep active-surface 'toplevel)
		(typep active-surface 'popup))
      (error "No active surface to grab from"))

    ;; TODO: The keys here are supposed to be the currently pressed keys
    (keyboard-enter seat popup '())))

(defmethod xdg-popup:reposition ((popup popup) positioner token)
  (setup-from-positioner popup positioner))

;; ┌─┐┌─┐┌─┐┬┌┬┐┬┌─┐┌┐┌┌─┐┬─┐
;; ├─┘│ │└─┐│ │ ││ ││││├┤ ├┬┘
;; ┴  └─┘└─┘┴ ┴ ┴└─┘┘└┘└─┘┴└─
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
   (gravity :initform :top :accessor gravity)
   (constraint :initform '() :accessor constraint)))

;; TODO: Not really doing anything with this yet.
;; Not sure it is that important for me
(defmethod xdg-positioner:set-reactive ((positioner positioner))
  )


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
  (setf (gravity positioner) gravity))

(defmethod xdg-positioner:set-offset ((positioner positioner) x y)
  (setf (off-x positioner) x
	(off-y positioner) y))

;; TODO: Implement constraint handling
(defmethod xdg-positioner:set-constraint-adjustment ((positioner positioner) constraint)
  "This indicates what to do with the popup surface if it's width/height/x/y is outside the bounds of the parent surface."
  (setf (constraint positioner) constraint))

;; TODO: Implement set parent size - I'm not yet entirely sure what its supposed to achieve.
;; Says that this should assume the parent size for popup positioning - but not really resize the parent?
(defmethod xdg-positioner:set-parent-size ((positioner positioner) width height)
  )

;; TODO: Also not implemented. Should be used in conjuction with set-parent-size
(defmethod xdg-positioner:set-parent-configure ((positioner positioner) serial)
  )

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun configure-states (&rest states)
  (loop for state in states
	collect (case state
		  (:maximized 1)
		  (:fullscreen 2)
		  (:resizing 3)
		  (:activated 4)
		  (:tiled-left 5)
		  (:tiled-right 6)
		  (:tiled-top 7)
		  (:tiled-bottom 8)
		  (:suspended 9))))
