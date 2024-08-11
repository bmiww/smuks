
;; ██╗      █████╗ ██╗   ██╗███████╗██████╗       ███████╗██╗  ██╗███████╗██╗     ██╗
;; ██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗      ██╔════╝██║  ██║██╔════╝██║     ██║
;; ██║     ███████║ ╚████╔╝ █████╗  ██████╔╝█████╗███████╗███████║█████╗  ██║     ██║
;; ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗╚════╝╚════██║██╔══██║██╔══╝  ██║     ██║
;; ███████╗██║  ██║   ██║   ███████╗██║  ██║      ███████║██║  ██║███████╗███████╗███████╗
;; ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝      ╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝
;; NOTE: zwlr layer shell objects
;; https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_shell_v1:enum:layer
(in-package :smuks)
(defclass layer-shell-global (zwlr-layer-shell-v1:global)
  ())


;; ┬  ┌─┐┬ ┬┌─┐┬─┐  ┌─┐┬ ┬┌─┐┬  ┬
;; │  ├─┤└┬┘├┤ ├┬┘  └─┐├─┤├┤ │  │
;; ┴─┘┴ ┴ ┴ └─┘┴└─  └─┘┴ ┴└─┘┴─┘┴─┘
(defclass layer-shell (zwlr-layer-shell-v1:dispatch)
  ())

(defmethod zwlr-layer-shell-v1:get-layer-surface ((shell layer-shell) layer-surface surface output layer namespace)
  (wl:up-if 'layer-surface surface layer-surface
	    :output (if output (wl:global output) (output (active-desktop (wl:get-display surface))))
	    :layer layer :namespace namespace))


;; ┬  ┌─┐┬ ┬┌─┐┬─┐  ┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐┌─┐
;; │  ├─┤└┬┘├┤ ├┬┘  └─┐│ │├┬┘├┤ ├─┤│  ├┤
;; ┴─┘┴ ┴ ┴ └─┘┴└─  └─┘└─┘┴└─└  ┴ ┴└─┘└─┘
;; TODO: The xdg-surface here could be reduced down to just hierarchical-surface or something
;; Since we only care about the parent/child relation here
;; In which case it might be possible to pull back in surface?
;; Or perhaps xdg-surface should consume surface-configure since now the use cases are all here
(defclass layer-surface (zwlr-layer-surface-v1:dispatch xdg-surface)
  ((output :initarg :output :reader output)
   (layer :initarg :layer :reader layer)
   (namespace :initarg :namespace :reader namespace)
   (keyboard-interactivity :initform nil :accessor keyboard-interactivity)
   (anchor :initform nil :accessor anchor)
   (exclusive-zone :initform nil :accessor exclusive-zone)
   (new-size? :initform nil :accessor new-size?)
   ;; TODO: For now just a quadruple - might make sense to introduce a struct for this
   (margins :initform '(0 0 0 0) :accessor margins)))


;; ┬ ┬┬    ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ││││    ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; └┴┘┴─┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod wl-surface:commit :before ((surface layer-surface))
  (when (new-size? surface)
    (let* ((output (output surface)) (serial (configure-serial surface)))

      (with-accessors ((width width) (height height) (x x) (y y)) surface
	(when (or (not width) (zerop width)) (setf width (output-width output)))
	(when (or (not height) (zerop height)) (setf height (output-height output)))

	;; TODO: These should probably go into the anchor - or some kind of finalizer function
	(unless x (setf x (- (/ (output-width output) 2) (/ (width surface) 2))))
	(unless y (setf y (- (/ (output-height output) 2) (/ (height surface) 2))))

	(setf (awaiting-ack surface) serial)
	(zwlr-layer-surface-v1:send-configure surface serial (width surface) (height surface))
	(setf (new-size? surface) nil)))))

(defmethod zwlr-layer-surface-v1:set-keyboard-interactivity ((surface layer-surface) keyboard-interactivity)
  (setf (slot-value surface 'keyboard-interactivity) keyboard-interactivity)
  (when (eq keyboard-interactivity :exclusive)
    (grab-keyboard-focus (wl:get-display surface) surface)))

(defmethod zwlr-layer-surface-v1:destroy :before ((surface layer-surface))
  (ungrab-keyboard-focus (wl:get-display surface) surface))

(defmethod zwlr-layer-surface-v1:set-margin ((surface layer-surface) top right bottom left)
  (setf (margins surface) (list top right bottom left)))


;; TODO: This size should be double-buffered
;; TODO: Setting these might conflict with whatever i have going on in the surface methods.
;; Therefore maybe i could ultimately just try calling the surface method
(defmethod zwlr-layer-surface-v1:set-size ((surface layer-surface) width height)
  (unless (and (eq width (width surface)) (eq height (height surface)))
    (setf (width surface) width)
    (setf (height surface) height)
    (setf (new-dimensions? surface) t)
    (setf (new-size? surface) t)))

;; TODO: This anchor should be double-buffered
(defmethod zwlr-layer-surface-v1:set-anchor ((surface layer-surface) anchor)
  (setf (anchor surface) (or anchor '(:center)))

  ;; NOTE: This transforms the bitfield anchor into a singular value similar to popup anchors
  (flet ((has? (anchors) (when (listp (anchor surface)) (has-anchors? (anchor surface) anchors))))
    (when (has? '(:top :bottom :left :right)) (setf (anchor surface) :center))
    (when (has? '(:left :right)) (setf (anchor surface) (rm-anchors (anchor surface) :left :right)))
    (when (has? '(:top :bottom)) (setf (anchor surface) (rm-anchors (anchor surface) :top :bottom)))
    (unless (anchor surface) (setf (anchor surface) :center))
    (when (listp (anchor surface))
      (setf (anchor surface)
	    (cond
	      ((has? '(:top :left)) :top-left)
	      ((has? '(:top :right)) :top-right)
	      ((has? '(:bottom :left)) :bottom-left)
	      ((has? '(:bottom :right)) :bottom-right)
	      ((has? '(:top)) :top)
	      ((has? '(:bottom)) :bottom)
	      ((has? '(:left)) :left)
	      ((has? '(:right)) :right)))))
  (resolve-position surface))


;; TODO: This has some very implementation specific details
;; Check the protocol for more information on how to implement it.
;; For now - leaving it dumb.
(defmethod zwlr-layer-surface-v1:set-exclusive-zone ((surface layer-surface) zone)
  (setf (exclusive-zone surface) zone))

(defmethod zwlr-layer-surface-v1:ack-configure ((surface layer-surface) serial)
  (if (eq serial (awaiting-ack surface))
      (progn
	(setf (awaiting-ack surface) nil)
	(resolve-position surface))
      (wrn! "Configure serial out of sync. Expected ~a, got ~a" (awaiting-ack surface) serial)))

(defmethod zwlr-layer-surface-v1:get-popup ((surface layer-surface) popup-surface)
  (setf (grab-child surface) popup-surface)
  (reposition-children surface))

(defmethod resolve-position ((surface layer-surface))
  (with-accessors ((ow output-width) (oh output-height)) (output surface)
    (with-accessors ((x x) (y y) (w width) (h height)) surface
      (setf (values x y)
	    (case (anchor surface)
	      (:top-left     (values 0                      0))
	      (:top-right    (values (- ow w)               0))
	      (:bottom-left  (values 0                      (- oh h)))
	      (:bottom-right (values (- ow w)               (- oh h)))
	      (:top          (values (- (/ ow 2) (/ w 2))   0))
	      (:bottom       (values (- (/ ow 2) (/ w 2))   (- oh h)))
	      (:left         (values 0                      (- (/ oh 2) (/ h 2))))
	      (:right        (values (- ow w)               (- (/ oh 2) (/ h 2))))
	      (:center       (values (- (/ ow 2) (/ w 2))   (- (/ oh 2) (/ h 2))))))))

  (reposition-children surface))

(defmethod (setf x) :after (value (surface layer-surface)) (reposition-children surface))
(defmethod (setf y) :after (value (surface layer-surface)) (reposition-children surface))
(defmethod (setf desktop) :after (value (surface layer-surface)) (reposition-children surface))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun rm-anchors (anchor &rest to-remove)
  (let ((new-anchor anchor))
    (dolist (remove to-remove)
      (setf new-anchor (remove remove new-anchor)))
    new-anchor))

(defun has-anchors? (anchor-list find-anchors)
  (let ((has-all t))
    (when (zerop (length find-anchors)) (setf has-all nil))
    (dolist (anchor find-anchors)
      (when (not (member anchor anchor-list)) (setf has-all nil)))
    has-all))
