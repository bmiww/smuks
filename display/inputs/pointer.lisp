
;; ██████╗  ██████╗ ██╗███╗   ██╗████████╗███████╗██████╗
;; ██╔══██╗██╔═══██╗██║████╗  ██║╚══██╔══╝██╔════╝██╔══██╗
;; ██████╔╝██║   ██║██║██╔██╗ ██║   ██║   █████╗  ██████╔╝
;; ██╔═══╝ ██║   ██║██║██║╚██╗██║   ██║   ██╔══╝  ██╔══██╗
;; ██║     ╚██████╔╝██║██║ ╚████║   ██║   ███████╗██║  ██║
;; ╚═╝      ╚═════╝ ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
(in-package :smuks)

(defmethod process ((display display) (type (eql :pointer-motion)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (update-cursor display (flo (pointer-motion@-dx event)) (flo (pointer-motion@-dy event)))
  (let ((surface (handle-surface-change display)))
    (when surface (pointer-motion (seat surface) (- (cursor-x display) (x surface)) (- (cursor-y display) (y surface))))))

;; NOTE: Additionally - sets display keyboard focus to the surface
(defmethod process ((display display) (type (eql :pointer-button)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (let* ((button (pointer-button@-button event))
	 (state (pointer-button@-state event))
	 (surface (surface-at-coords display (cursor-x display) (cursor-y display)))
	 (client (and surface (wl:client surface))))

    (when surface
      (let* ((seat (seat client)))
	(when seat
	  (unless (eq (keyboard-focus display) surface) (setf (keyboard-focus display) surface))
	  (pointer-button seat button state))))))


(defmethod process ((display display) (type (eql :pointer-scroll-finger)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (let* ((surface (surface-at-coords display (cursor-x display) (cursor-y display))))
    (when surface
      (let* ((client (wl:client surface)) (seat (seat client)))
	(pointer-scroll-finger seat (pointer-scroll-finger@-dy event) (pointer-scroll-finger@-dx event))))))


;; TODO: I do not know what to emulate via gesture holds so for now this is empty
(defmethod process ((display display) (type (eql :gesture-hold-begin)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  )

;; TODO: I do not know what to emulate via gesture holds so for now this is empty
(defmethod process ((display display) (type (eql :gesture-hold-end)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  )
