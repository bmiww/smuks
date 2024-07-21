
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
  (wl:up-if 'layer-surface surface layer-surface :output output :layer layer :namespace namespace))

;; ┬  ┌─┐┬ ┬┌─┐┬─┐  ┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐┌─┐
;; │  ├─┤└┬┘├┤ ├┬┘  └─┐│ │├┬┘├┤ ├─┤│  ├┤
;; ┴─┘┴ ┴ ┴ └─┘┴└─  └─┘└─┘┴└─└  ┴ ┴└─┘└─┘
(defclass layer-surface (zwlr-layer-surface-v1:dispatch surface surface-configure)
  ((output :initarg :output :reader output)
   (layer :initarg :layer :reader layer)
   (namespace :initarg :namespace :reader namespace)
   (keyboard-interactivity :initform nil :accessor keyboard-interactivity)
   (anchor :initform nil :accessor anchor)
   (exclusive-zone :initform nil :accessor exclusive-zone)
   (new-size? :initform nil :accessor new-size?)
   ;; TODO: For now just a quadruple - might make sense to introduce a struct for this
   (margins :initform '(0 0 0 0) :accessor margins)))

(defmethod wl-surface:commit :before ((surface layer-surface))
  ;; TODO: Maybe possible to use the surface level "new-dimensions?" flag
  (when (new-size? surface)
    (let* ((display (wl:get-display surface))
	   (desktop (active-desktop display)))
      (when (eq (width surface) 0) (setf (width surface) (width desktop)))
      (when (eq (height surface) 0) (setf (height surface) (height desktop)))
      (zwlr-layer-surface-v1:send-configure surface (configure-serial surface) (width surface) (height surface))
      (setf (new-size? surface) nil))))

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

;; TODO: Seemingly can be null - which means - centered. Thanks protocol for being specific
;; TODO: This anchor should be double-buffered
;; TODO: Cover more than just the center case
;; TODO: The coordinates should most likely be based off of the current active desktop screen size
(defmethod zwlr-layer-surface-v1:set-anchor ((surface layer-surface) anchor)
  (setf (anchor surface) (if anchor anchor :center))
  (case (anchor surface)
    (:center (setf (x surface) 50 (y surface) 50))))

;; TODO: This has some very implementation specific details
;; Check the protocol for more information on how to implement it.
;; For now - leaving it dumb.
(defmethod zwlr-layer-surface-v1:set-exclusive-zone ((surface layer-surface) zone)
  (setf (exclusive-zone surface) zone))

(defmethod zwlr-layer-surface-v1:ack-configure ((surface layer-surface) serial)
  (if (eq serial (awaiting-ack surface))
      (setf (awaiting-ack surface) nil)
      (wrn! "Configure serial out of sync. Expected ~a, got ~a" (awaiting-ack surface) serial)))
