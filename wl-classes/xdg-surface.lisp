
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
   (grab-child :initform nil :accessor grab-child)
   (grab-parent :initarg :grab-parent :initform nil :accessor grab-parent))
  (:documentation
   "An xdg surface identifies a toplevel or a popup surface.
The main purpose here is to define that child/parent relationships between the former."))


;; ┌┬┐┌─┐┌┬┐┬ ┬
;; │││├┤  │ ├─┤
;; ┴ ┴└─┘ ┴ ┴ ┴
;; TODO: Seemingly should perform itself
;; once parent changes width/height/x/y
(defmethod reposition-child-toplevel ((xdg xdg-surface))
  (let* ((parent (grab-parent xdg))
	 (parent-width (min (compo-max-width parent) (width parent)))
	 (parent-height (min (compo-max-height parent) (height parent))))
    (setf (x xdg) (+ (x parent) (/ parent-width 2) (- (/ (width xdg) 2)))
	  (y xdg) (+ (y parent) (/ parent-height 2) (- (/ (height xdg) 2))))))


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
    (when (grab-child xdg) (reposition-child-toplevel (grab-child xdg)))))

(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((display (wl:get-display xdg)))
    (wl:up-if 'toplevel xdg id)
    (add-state xdg :maximized)

    (new-toplevel display xdg)))

;; TODO: Unify this configure with the one that toplevel uses to some extent?
;; At lest the surface:send-configure should be the same.
(defmethod xdg-surface:get-popup ((xdg xdg-surface) id parent positioner)
  (wl:up-if 'popup xdg id :positioner positioner :grab-parent parent)
  (setf (grab-child parent) xdg)
  (xdg-popup:send-configure xdg (x positioner) (y positioner) (width positioner) (height positioner))
  (xdg-surface:send-configure xdg (configure-serial xdg)))


;; NOTE: For now leaving empty - but could be used in some way to finalize
;; The configuration sequence. Applying pending state or whatnot. Not sure
(defcontinue xdg-surface:ack-configure ((xdg xdg-surface) serial)
  (if (eq serial (awaiting-ack xdg))
      (setf (awaiting-ack xdg) nil)
      (wrn! "Configure serial out of sync. Expected ~a, got ~a" (awaiting-ack xdg) serial)))
