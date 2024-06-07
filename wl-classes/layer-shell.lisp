
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
(defclass layer-surface (zwlr-layer-surface-v1:dispatch surface)
  ((output :initarg :output :reader output)
   (layer :initarg :layer :reader layer)
   (namespace :initarg :namespace :reader namespace)
   (keyboard-interactivity :initform nil :accessor keyboard-interactivity)
   (anchor :initform nil :accessor anchor)
   (exclusive-zone :initform nil :accessor exclusive-zone)))

(defmethod zwlr-layer-surface-v1:set-keyboard-interactivity ((surface layer-surface) keyboard-interactivity)
  (setf (slot-value surface 'keyboard-interactivity) keyboard-interactivity))

;; TODO: This size should be double-buffered
;; TODO: Setting these might conflict with whatever i have going on in the surface methods.
;; Therefore maybe i could ultimately just try calling the surface method
(defmethod zwlr-layer-surface-v1:set-size ((surface layer-surface) width height)
  (setf (width surface) width)
  (setf (height surface) height)
  (setf (new-dimensions? surface) t))

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

;; TODO: I'm still not handling ack-configure events.
;; For now - don't care - leaving empty
;; TODO: Also - not really sending the configure event out
(defmethod zwlr-layer-surface-v1:ack-configure ((surface layer-surface) serial))
