
;; ██████╗ ███████╗ ██████╗ ██╗ ██████╗ ███╗   ██╗
;; ██╔══██╗██╔════╝██╔════╝ ██║██╔═══██╗████╗  ██║
;; ██████╔╝█████╗  ██║  ███╗██║██║   ██║██╔██╗ ██║
;; ██╔══██╗██╔══╝  ██║   ██║██║██║   ██║██║╚██╗██║
;; ██║  ██║███████╗╚██████╔╝██║╚██████╔╝██║ ╚████║
;; ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚═╝ ╚═════╝ ╚═╝  ╚═══╝
(in-package :smuks)

(defstruct dimloc x y width height)
(defstruct (add (:include dimloc)))
(defstruct (sub (:include dimloc)))

(defclass region (wl-region:dispatch)
  ((adds :initform nil :accessor adds)
   (subs :initform nil :accessor subs)))


;; ┬ ┬┬    ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐
;; ││││    ├─┤├─┤│││ │││  ├┤
;; └┴┘┴─┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘
(defmethod wl-region:add ((region region) x y width height)
  (push (make-add x y width height) (rectangles region)))

(defmethod wl-region:subtract ((region region) x y width height)
  (push (make-sub x y width height) (rectangles region)))


;; ┌┬┐┌─┐┌┬┐┬ ┬┌─┐┌┬┐┌─┐
;; │││├┤  │ ├─┤│ │ ││└─┐
;; ┴ ┴└─┘ ┴ ┴ ┴└─┘─┴┘└─┘
(defmethod in-region (region x y)
  (dolist (sub (subs region))
    (when (and (<= (sub-x sub) x (+ (sub-x sub) (sub-width sub)))
	       (<= (sub-y sub) y (+ (sub-y sub) (sub-height sub))))
      (return-from in-region nil)))
  (dolist (add (adds region))
    (when (and (<= (add-x add) x (+ (add-x add) (add-width add)))
	       (<= (add-y add) y (+ (add-y add) (add-height add))))
      (return-from in-region t)))
  nil)
