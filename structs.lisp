
;; ███████╗████████╗██████╗ ██╗   ██╗ ██████╗████████╗███████╗
;; ██╔════╝╚══██╔══╝██╔══██╗██║   ██║██╔════╝╚══██╔══╝██╔════╝
;; ███████╗   ██║   ██████╔╝██║   ██║██║        ██║   ███████╗
;; ╚════██║   ██║   ██╔══██╗██║   ██║██║        ██║   ╚════██║
;; ███████║   ██║   ██║  ██║╚██████╔╝╚██████╗   ██║   ███████║
;; ╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝  ╚═════╝   ╚═╝   ╚══════╝
(in-package :smuks)

;; ┌─┐┌─┐┌─┐┬─┐┌┬┐
;; │  │ ││ │├┬┘ ││
;; └─┘└─┘└─┘┴└──┴┘
(defstruct coord
  (x nil) (y nil) (desktop nil))

(defmethod x ((coord coord)) (coord-x coord))
(defmethod (setf x) (val (coord coord)) (setf (coord-x coord) val))
(defmethod y ((coord coord)) (coord-y coord))
(defmethod (setf y) (val (coord coord)) (setf (coord-y coord) val))
(defmethod desktop ((coord coord)) (coord-desktop coord))
(defmethod (setf desktop) (val (coord coord)) (setf (coord-desktop coord) val))
