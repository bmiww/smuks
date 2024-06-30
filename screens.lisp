
;; ███████╗ ██████╗███████╗███╗   ██╗███████╗    ███████╗████████╗██╗   ██╗███████╗███████╗
;; ██╔════╝██╔════╝██╔════╝████╗  ██║██╔════╝    ██╔════╝╚══██╔══╝██║   ██║██╔════╝██╔════╝
;; ███████╗██║     █████╗  ██╔██╗ ██║█████╗█████╗███████╗   ██║   ██║   ██║█████╗  █████╗
;; ╚════██║██║     ██╔══╝  ██║╚██╗██║██╔══╝╚════╝╚════██║   ██║   ██║   ██║██╔══╝  ██╔══╝
;; ███████║╚██████╗███████╗██║ ╚████║███████╗    ███████║   ██║   ╚██████╔╝██║     ██║
;; ╚══════╝ ╚═════╝╚══════╝╚═╝  ╚═══╝╚══════╝    ╚══════╝   ╚═╝    ╚═════╝ ╚═╝     ╚═╝
;; TODO File could be renamed/repurposed now that screens odnt liove here any more
(in-package :smuks)

(defvar *scenes* (list 'scene-1 'scene-2))
(defvar *stupid-size* 150.0)

;; ┌┬┐┌─┐┌─┐┌┬┐┬┌┐┌┌─┐  ┌─┐┌┬┐┬ ┬┌─┐┌─┐
;;  │ ├┤ └─┐ │ │││││ ┬  └─┐ │ │ │├┤ ├┤
;;  ┴ └─┘└─┘ ┴ ┴┘└┘└─┘  └─┘ ┴ └─┘└  └
(defvar *y-pos* 220.0)
(defvar y-up t)
(defun next-y-pos ()
  (when (> *y-pos* 300.0)
    (setf y-up nil))
  (when (< *y-pos* 150.0)
    (setf y-up t))
  (incf *y-pos* (if y-up 1 -1)))

(defvar *red-x* 50.0)
(defvar *red-y* 100.0)

(defun scene-1 (screen)
  (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
						    :x 10.0 :y (next-y-pos) :w 50.0 :h 60.0
						    :color '(0.2 0.2 0.2 1.0))))

  (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
						    :x *red-x* :y *red-y* :w 200.0 :h 50.0
						    :color '(1.0 0.0 0.0 0.6)))))

(defvar *y-2-pos* 220.0)
(defvar y-2-up t)
(defun next-y-2-pos ()
  (when (> *y-2-pos* 500.0)
    (setf y-2-up nil))
  (when (< *y-2-pos* 30.0)
    (setf y-2-up t))
  (incf *y-2-pos* (if y-2-up 1 -1)))


(defun scene-2 (screen)
  (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
						    :x 500.0 :y (next-y-2-pos) :w 200.0 :h 150.0
						    :color '(0.2 0.2 0.2 1.0))))
  )

;; NOTE: Accidentally managed to draw like a laying down exclamation mark with this
(defun scene-nothing-yet (screen)
  (let* ((size 100.0) (width (flo (width screen))) (height (flo (height screen))) (half (/ size 2)))
    (flet ((draw-rect (x y)
	     (shaders.rectangle:draw (shader screen :rect) `(,(shaders.rectangle::make-rect
							       :x x :y y :w size :h size
							       :color '(0.2 0.2 0.2 1.0))))))
      (draw-rect (- width size (/ width 9) 0.0) (- (/ height 2) half))
      (draw-rect (- width size (/ width 6) 50.0) (- (/ height 2) half))
      (draw-rect (- width size (/ width 3) 50.0) (- (/ height 2) half)))))


(defun click-locations (screen size)
  (let ((width (flo (width screen))) (height (flo (height screen))))
    (list `(0.0 0.0 :top-left)
	  `(,(- (/ width 2) (/ size 2)) 0.0 :top-center)
	  `(,(- width size) 0.0 :top-right)
	  `(0.0 ,(- (/ height 2) (/ size 2)) :center-left)
	  `(,(- (/ width 2) (/ size 2)) ,(- (/ height 2) (/ size 2)) :center-center)
	  `(,(- width size) ,(- (/ height 2) (/ size 2)) :center-right)
	  `(0.0 ,(- height size) :bottom-left)
	  `(,(- (/ width 2) (/ size 2)) ,(- height size) :bottom-center)
	  `(,(- width size) ,(- height size) :bottom-right))))


(defun scene-select-screen-pos (screen)
  (let ((size *stupid-size*))
    (flet ((draw-rect (x y)
	     (shaders.rectangle:draw (shader screen :capsule) `(,(shaders.rectangle::make-rect
								:x x :y y :w size :h size
								:color '(0.2 0.2 0.2 1.0))))))
      (dolist (pos (click-locations screen size))
	(draw-rect (first pos) (second pos))))))
