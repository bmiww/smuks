
;; ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗    ███╗   ███╗███████╗████████╗██╗  ██╗ ██████╗ ██████╗
;; ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝    ████╗ ████║██╔════╝╚══██╔══╝██║  ██║██╔═══██╗██╔══██╗
;; ██║██╔██╗ ██║██████╔╝██║   ██║   ██║       ██╔████╔██║█████╗     ██║   ███████║██║   ██║██║  ██║
;; ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║       ██║╚██╔╝██║██╔══╝     ██║   ██╔══██║██║   ██║██║  ██║
;; ██║██║ ╚████║██║     ╚██████╔╝   ██║       ██║ ╚═╝ ██║███████╗   ██║   ██║  ██║╚██████╔╝██████╔╝
;; ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝       ╚═╝     ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═════╝
;; https://wayland.app/protocols/input-method-unstable-v2
(in-package :smuks)


;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass input-method-manager (zwp-input-method-manager-v2:dispatch)
  ())

(defmethod zwp-input-method-manager-v2:get-input-method ((manager input-method-manager) seat id)
  (wl:mk-if 'input-method manager id :seat seat))


;; ┬┌┐┌┌─┐┬ ┬┌┬┐  ┌┬┐┌─┐┌┬┐┬ ┬┌─┐┌┬┐
;; ││││├─┘│ │ │   │││├┤  │ ├─┤│ │ ││
;; ┴┘└┘┴  └─┘ ┴   ┴ ┴└─┘ ┴ ┴ ┴└─┘─┴┘
(defclass input-method (zwp-input-method-v2:dispatch)
  ((seat :initarg :seat :accessor seat)))
