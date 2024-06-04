
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
  (update-cursor display (flo (pointer-motion@-dx event)) (flo (pointer-motion@-dy event)))
  (let* ((new-x (cursor-x display)) (new-y (cursor-y display)))
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
