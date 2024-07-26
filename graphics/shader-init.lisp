
(defpackage :shader-init
  (:use :cl :sdrm :smuks-util :sglutil)
  (:export
   create-rect-shader create-texture-shader create-xrgb8888-shader
   create-surface-shader create-surface-xrgb8888-shader))
(in-package :shader-init)

(defun create-rect-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation)))
    (make-instance 'shaders.rectangle:shader :projection projection :gl-version gl-version)))

;; TODO: Rename to argb8888
(defun create-texture-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation)))
    (make-instance 'shaders.texture:shader
       :projection projection :gl-version gl-version
       :format :argb8888)))

(defun create-xrgb8888-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation)))
    (make-instance 'shaders.texture:shader
       :projection projection :gl-version gl-version
       :format :xrgb8888)))

(defun create-surface-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation)))
    (make-instance 'shaders.surface:shader
       :projection projection :gl-version gl-version
       :format :argb8888)))

(defun create-surface-xrgb8888-shader (width height &optional rotation gl-version)
  (let* ((projection (sglutil:projection-matrix width height rotation)))
    (make-instance 'shaders.surface:shader
       :projection projection :gl-version gl-version
       :format :xrgb8888)))
