
(defpackage :shader-init
  (:use :cl :sdrm :smuks-util :sglutil)
  (:export create-rect-shader create-texture-shader))
(in-package :shader-init)

(defun create-rect-shader (width height)
  (let* ((projection (sglutil:make-projection-matrix width height))
	 (rect-shader (make-instance 'shaders.rectangle:shader :projection projection)))
    rect-shader))

(defun create-texture-shader (width height)
  (let* ((projection (sglutil:make-projection-matrix width height))
	 (texture-shader (make-instance 'shaders.texture:shader :projection projection)))
    texture-shader))
