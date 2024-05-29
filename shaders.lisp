
;; ███████╗██╗  ██╗ █████╗ ██████╗ ███████╗██████╗ ███████╗
;; ██╔════╝██║  ██║██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
;; ███████╗███████║███████║██║  ██║█████╗  ██████╔╝███████╗
;; ╚════██║██╔══██║██╔══██║██║  ██║██╔══╝  ██╔══██╗╚════██║
;; ███████║██║  ██║██║  ██║██████╔╝███████╗██║  ██║███████║
;; ╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝
(defpackage :shaders
  (:use :cl :smuks-util)
  (:export
   ;; Funcs
   create-shader array-buffer-data allocate-gl-array fill-buffer
   ;; Params
   *instanced-vert* *instanced-texture-vert*
   ;; Shader base
   shader-base update-projection pointer projection uni-projection))

(in-package :shaders)

(defclass shader-base ()
  ((pointer :accessor pointer)
   (projection :accessor projection)
   (uni-projection :accessor uni-projection)))

(defgeneric update-projection (shader projection))
(defmethod update-projection ((shader shader-base) new-projection)
  (with-slots (pointer projection uni-projection) shader
    (setf projection new-projection)
    (gl:use-program pointer)
    (gl:uniform-matrix-3fv uni-projection projection nil)))

;; ┌─┐┬ ┬┌┐┌┌─┐┌─┐
;; ├┤ │ │││││  └─┐
;; └  └─┘┘└┘└─┘└─┘

(defun create-shader (vertex fragment)
  (let ((vertex-shader (gl:create-shader :vertex-shader))
	(fragment-shader (gl:create-shader :fragment-shader))
	(shader-program (gl:create-program)))
    (gl:shader-source vertex-shader vertex)
    (check-gl-error "Vertex shader sourcing")
    (gl:shader-source fragment-shader fragment)
    (check-gl-error "Fragment shader sourcing")
    (gl:compile-shader vertex-shader)
    (check-gl-error "Vertex shader compilation")
    (gl:compile-shader fragment-shader)
    (check-gl-error "Fragment shader compilation")

    (when (not (gl:get-shader vertex-shader :compile-status))
      (format t "Vertex shader info: ~a~%" (gl:get-shader-info-log vertex-shader))
      (error "failed to compile vertex shader"))
    (when (not (gl:get-shader fragment-shader :compile-status))
      (format t "Fragment shader info: ~a~%" (gl:get-shader-info-log fragment-shader))
      (error "failed to compile fragment shader"))

    (gl:attach-shader shader-program vertex-shader)
    (gl:attach-shader shader-program fragment-shader)
    (gl:link-program shader-program)

    (when (not (gl:get-program shader-program :link-status))
      (format t "Shader program info: ~a~%" (gl:get-program-info-log shader-program))
      (error "failed to link shader program"))

    shader-program))

(defun allocate-gl-array (length) (gl:alloc-gl-array :float length))

;; TODO: Stupid name. This just fills a buffer with the fancy 4 corner coords
;; TODO: This one recreates the array every time, and is not very efficient
;; Prefer fill-buffer with a preallocated array
;; TODO: Extra considerations for instanced rendering where the length of the array might be variable
;; And require different handling
(defun array-buffer-data (vbo verts)
  (gl:bind-buffer :array-buffer vbo)
  (let* ((length (length verts))
	 (arr (gl:alloc-gl-array :float length)))
    (dotimes (i length)
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)))

(defun fill-buffer (vbo verts arr)
  (gl:bind-buffer :array-buffer vbo)
  (dotimes (i (length verts))
    (setf (gl:glaref arr i) (aref verts i)))
  (gl:buffer-data :array-buffer :static-draw arr))

;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┌─┐
;; │  │ ││││└─┐ │ └─┐
;; └─┘└─┘┘└┘└─┘ ┴ └─┘

(defparameter *instanced-vert* #(1.0 0.0  0.0 0.0  1.0 1.0  0.0 1.0))
(defparameter *instanced-texture-vert* #(1.0 0.0  0.0 0.0  1.0 1.0  0.0 1.0))
