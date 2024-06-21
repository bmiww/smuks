
;; ████████╗███████╗██╗  ██╗████████╗    ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗
;; ╚══██╔══╝██╔════╝╚██╗██╔╝╚══██╔══╝    ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝
;;    ██║   █████╗   ╚███╔╝    ██║       ██║██╔██╗ ██║██████╔╝██║   ██║   ██║
;;    ██║   ██╔══╝   ██╔██╗    ██║       ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║
;;    ██║   ███████╗██╔╝ ██╗   ██║       ██║██║ ╚████║██║     ╚██████╔╝   ██║
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝       ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝
;; https://wayland.app/protocols/text-input-unstable-v3
(in-package :smuks)


;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass text-input-manager (zwp-text-input-manager-v3:dispatch)
  ())

(defmethod zwp-text-input-manager-v3:get-text-input ((manager text-input-manager) id seat)
  (wl:mk-if 'text-input manager id :seat seat))


;; ┬┌┐┌┌─┐┬ ┬┌┬┐
;; ││││├─┘│ │ │
;; ┴┘└┘┴  └─┘ ┴
(defclass text-input (zwp-text-input-v3:dispatch)
  ((seat :initarg :seat :accessor seat)))
