
(defpackage :shader-init
  (:use :cl :sdrm :smuks-util :sglutil)
  (:export create-rect-shader create-texture-shader))
(in-package :shader-init)

(defun create-rect-shader (device)
  (let* ((projection (make-projection-matrix (width device) (height device)))
	 (rect-shader (make-instance 'shaders.rectangle:shader :projection projection)))
    rect-shader))

(defun create-texture-shader (device)
  (let* ((projection (make-projection-matrix (width device) (height device)))
	 (texture-shader (make-instance 'shaders.texture:shader :projection projection)))
    texture-shader))
