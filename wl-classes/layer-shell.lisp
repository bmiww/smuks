
;; ██╗      █████╗ ██╗   ██╗███████╗██████╗       ███████╗██╗  ██╗███████╗██╗     ██╗
;; ██║     ██╔══██╗╚██╗ ██╔╝██╔════╝██╔══██╗      ██╔════╝██║  ██║██╔════╝██║     ██║
;; ██║     ███████║ ╚████╔╝ █████╗  ██████╔╝█████╗███████╗███████║█████╗  ██║     ██║
;; ██║     ██╔══██║  ╚██╔╝  ██╔══╝  ██╔══██╗╚════╝╚════██║██╔══██║██╔══╝  ██║     ██║
;; ███████╗██║  ██║   ██║   ███████╗██║  ██║      ███████║██║  ██║███████╗███████╗███████╗
;; ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝      ╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝
;; NOTE: zwlr layer shell objects
;; https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_shell_v1:enum:layer
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
   (anchor :initform nil :accessor anchor)))

(defmethod zwlr-layer-surface-v1:set-keyboard-interactivity ((surface layer-surface) keyboard-interactivity)
  (setf (slot-value surface 'keyboard-interactivity) keyboard-interactivity))

;; TODO: This size should be double-buffered
;; TODO: Setting these might conflict with whatever i have going on in the surface methods.
;; Therefore maybe i could ultimately just try calling the surface method
(defmethod zwlr-layer-surface-v1:set-size ((surface layer-surface) width height)
  (setf (width surface) width)
  (setf (height surface) height))

;; TODO: Seemingly can be null - which means - centered. Thanks protocol for being specific
;; TODO: This anchor should be double-buffered
(defmethod zwlr-layer-surface-v1:set-anchor ((surface layer-surface) anchor)
  (setf (anchor surface) (if anchor anchor :center)))
