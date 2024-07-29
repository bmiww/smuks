
;; ██████╗ ███████╗ ██████╗ ██╗ ██████╗ ███╗   ██╗
;; ██╔══██╗██╔════╝██╔════╝ ██║██╔═══██╗████╗  ██║
;; ██████╔╝█████╗  ██║  ███╗██║██║   ██║██╔██╗ ██║
;; ██╔══██╗██╔══╝  ██║   ██║██║██║   ██║██║╚██╗██║
;; ██║  ██║███████╗╚██████╔╝██║╚██████╔╝██║ ╚████║
;; ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
(in-package :smuks)

(defclass region (wl-region:dispatch)
  ((rectangles :initform nil :accessor rectangles)))

(defmethod wl-region:add ((region region) x y width height)
  (push (list x y width height) (rectangles region)))

(defmethod wl-region:subtract ((region region) x y width height)
  (setf (rectangles region) (remove (list x y width height) (rectangles region))))
