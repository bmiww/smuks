
;; ███████╗ ██████╗██████╗ ███████╗███████╗███╗   ██╗    ███████╗███████╗████████╗██╗   ██╗██████╗
;; ██╔════╝██╔════╝██╔══██╗██╔════╝██╔════╝████╗  ██║    ██╔════╝██╔════╝╚══██╔══╝██║   ██║██╔══██╗
;; ███████╗██║     ██████╔╝█████╗  █████╗  ██╔██╗ ██║    ███████╗█████╗     ██║   ██║   ██║██████╔╝
;; ╚════██║██║     ██╔══██╗██╔══╝  ██╔══╝  ██║╚██╗██║    ╚════██║██╔══╝     ██║   ██║   ██║██╔═══╝
;; ███████║╚██████╗██║  ██║███████╗███████╗██║ ╚████║    ███████║███████╗   ██║   ╚██████╔╝██║
;; ╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═══╝    ╚══════╝╚══════╝   ╚═╝    ╚═════╝ ╚═╝
;; ┬┌┐┌┌─┐┬ ┬┌┬┐┌─┐
;; ││││├─┘│ │ │ └─┐
;; ┴┘└┘┴  └─┘ ┴ └─┘
(in-package :smuks)

(defmethod process ((display display) type (usecase (eql :screen-setup)) event)
  (declare (ignore display type usecase))
  (log! "No :screen-setup handler for input event: ~a" (event-type event)))


(defmethod process ((display display) (type (eql :pointer-motion)) (usecase (eql :screen-setup)) event)
  (declare (ignore usecase))
  (let* ((new-x (add-dx display (flo (pointer-motion@-dx event))))
	 (new-y (add-dy display (flo (pointer-motion@-dy event)))))
    (is-in-click-location? (screens display) new-x new-y)))



;;  █████╗ ██╗     ██╗    ██╗ █████╗ ██╗   ██╗███████╗
;; ██╔══██╗██║     ██║    ██║██╔══██╗╚██╗ ██╔╝██╔════╝
;; ███████║██║     ██║ █╗ ██║███████║ ╚████╔╝ ███████╗
;; ██╔══██║██║     ██║███╗██║██╔══██║  ╚██╔╝  ╚════██║
;; ██║  ██║███████╗╚███╔███╔╝██║  ██║   ██║   ███████║
;; ╚═╝  ╚═╝╚══════╝ ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝
;; These are parts of the input processing which should happen always.
;; TODO: Move these to their own file?
;; TODO: Weirdly enough - (add-dx) and (add-dy) already set the new values to the display. Should we do it differently?

;; (defmethod process :after ((display display) (type (eql :pointer-motion)) usecase event)
  ;; (declare (ignore usecase))
  ;; )
