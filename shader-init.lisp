
(defpackage :shader-init
  (:use :cl :sdrm :smuks-util :sglutil)
  (:export
   create-rect-shader create-texture-shader
   create-capsule-shader))
(in-package :shader-init)

(defun create-rect-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation))
	 (rect-shader (make-instance 'shaders.rectangle:shader :projection projection :gl-version gl-version)))
    rect-shader))

(defun create-texture-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation))
	 (texture-shader (make-instance 'shaders.texture:shader :projection projection :gl-version gl-version)))
    texture-shader))

(defun create-capsule-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation))
	 (capsule-shader (make-instance 'shaders.capsule:shader :projection projection :gl-version gl-version)))
    capsule-shader))
