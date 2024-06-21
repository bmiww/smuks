
;; ██████╗ ███████╗ ██████╗ ██████╗ ██████╗     ███╗   ███╗ █████╗ ███╗   ██╗ █████╗  ██████╗ ███████╗██████╗
;; ██╔══██╗██╔════╝██╔════╝██╔═══██╗██╔══██╗    ████╗ ████║██╔══██╗████╗  ██║██╔══██╗██╔════╝ ██╔════╝██╔══██╗
;; ██║  ██║█████╗  ██║     ██║   ██║██████╔╝    ██╔████╔██║███████║██╔██╗ ██║███████║██║  ███╗█████╗  ██████╔╝
;; ██║  ██║██╔══╝  ██║     ██║   ██║██╔══██╗    ██║╚██╔╝██║██╔══██║██║╚██╗██║██╔══██║██║   ██║██╔══╝  ██╔══██╗
;; ██████╔╝███████╗╚██████╗╚██████╔╝██║  ██║    ██║ ╚═╝ ██║██║  ██║██║ ╚████║██║  ██║╚██████╔╝███████╗██║  ██║
;; ╚═════╝ ╚══════╝ ╚═════╝ ╚═════╝ ╚═╝  ╚═╝    ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝
(in-package :smuks)

;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass decoration-manager (zxdg-decoration-manager-v1:dispatch)
  ())

(defmethod zxdg-decoration-manager-v1:get-toplevel-decoration ((self decoration-manager) id toplevel)
  (wl:mk-if 'decoration self id :toplevel toplevel))


;; ┌┬┐┌─┐┌─┐┌─┐┬─┐
;;  ││├┤ │  │ │├┬┘
;; ─┴┘└─┘└─┘└─┘┴└─
;; NOTE: For my current use case - i do not want clients to do their own decorations ever.
(defclass decoration (zxdg-toplevel-decoration-v1:dispatch)
  ((toplevel :initarg :toplevel :reader toplevel)))

(defmethod zxdg-toplevel-decoration-v1:set-mode ((self decoration) mode)
  (declare (ignore mode))
  (zxdg-toplevel-decoration-v1:send-configure self :server-side))

(defmethod zxdg-toplevel-decoration-v1:unset-mode ((self decoration)) (declare (ignore self)))
