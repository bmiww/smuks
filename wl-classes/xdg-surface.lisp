
;; ██╗  ██╗██████╗  ██████╗       ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ╚██╗██╔╝██╔══██╗██╔════╝       ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;;  ╚███╔╝ ██║  ██║██║  ███╗█████╗███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;;  ██╔██╗ ██║  ██║██║   ██║╚════╝╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ██╔╝ ██╗██████╔╝╚██████╔╝      ███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚═╝  ╚═╝╚═════╝  ╚═════╝       ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
(in-package :smuks)

(defclass xdg-surface (xdg-surface:dispatch surface surface-configure)
  ((xdg-x-offset :initform 0 :accessor xdg-x-offset)
   (xdg-y-offset :initform 0 :accessor xdg-y-offset)
   (grab-child :initform nil :reader grab-child)
   (grab-parent :initarg :grab-parent :initform nil :reader grab-parent))
  (:documentation
   "An xdg surface identifies a toplevel or a popup surface.
The main purpose here is to define that child/parent relationships between the former."))

(defmethod (setf grab-child) (child (xdg xdg-surface))
  (setf (slot-value xdg 'grab-child) child)
  (when child (setf (slot-value child 'grab-parent) xdg)))

(defmethod (setf grab-parent) (parent (xdg xdg-surface))
  (setf (slot-value xdg 'grab-parent) parent)
  (when parent (setf (slot-value parent 'grab-child) xdg)))

;; ┌┬┐┌─┐┌┬┐┬ ┬
;; │││├┤  │ ├─┤
;; ┴ ┴└─┘ ┴ ┴ ┴
(defmethod reposition-children ((xdg xdg-surface))
  (let ((child (grab-child xdg)))
    (when (and child (typep child 'xdg-surface))
      (setf (x child) (+ (x xdg) (if (typep child 'popup) (relative-x child) 0)))
      (setf (y child) (+ (y xdg) (if (typep child 'popup) (relative-y child) 0)))
      (reposition-children child))))


;; ┬ ┬┬    ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ││││    ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; └┴┘┴─┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod xdg-surface:set-window-geometry ((xdg xdg-surface) x y width height)
  (unless (and (eq width (width xdg)) (eq height (height xdg)))
    (setf (xdg-x-offset xdg) x
	  (xdg-y-offset xdg) y
	  (width xdg) width
	  (height xdg) height
	  (new-dimensions? xdg) t)
    (reposition-children xdg)))

(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((display (wl:get-display xdg)))
    (wl:up-if 'toplevel xdg id)
    (add-state xdg :maximized)

    (new-toplevel display xdg)))

;; TODO: Unify this configure with the one that toplevel uses to some extent?
;; At lest the surface:send-configure should be the same.
(defmethod xdg-surface:get-popup ((xdg xdg-surface) id parent positioner)
  (wl:up-if 'popup xdg id :positioner positioner :grab-parent parent)
  (when parent (setf (grab-child parent) xdg))
  (xdg-popup:send-configure xdg (x positioner) (y positioner) (width positioner) (height positioner))
  (xdg-surface:send-configure xdg (configure-serial xdg)))

;; NOTE: For now leaving empty - but could be used in some way to finalize
;; The configuration sequence. Applying pending state or whatnot. Not sure
(defcontinue xdg-surface:ack-configure ((xdg xdg-surface) serial)
  (if (eq serial (awaiting-ack xdg))
      (setf (awaiting-ack xdg) nil)
      (wrn! "Configure serial out of sync. Expected ~a, got ~a" (awaiting-ack xdg) serial)))
