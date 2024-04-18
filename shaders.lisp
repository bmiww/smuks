
;; ███████╗██╗  ██╗ █████╗ ██████╗ ███████╗██████╗ ███████╗
;; ██╔════╝██║  ██║██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔════╝
;; ███████╗███████║███████║██║  ██║█████╗  ██████╔╝███████╗
;; ╚════██║██╔══██║██╔══██║██║  ██║██╔══╝  ██╔══██╗╚════██║
;; ███████║██║  ██║██║  ██║██████╔╝███████╗██║  ██║███████║
;; ╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚══════╝╚═╝  ╚═╝╚══════╝
(declaim (optimize (speed 0) (safety 0) (debug 3) (compilation-speed 0)))

(defpackage :shaders
  (:use :cl :smuks-util)
  (:export
   ;; Funcs
   create-shader array-buffer-data
   ;; Params
   *instanced-vert* *instanced-texture-vert*))

(in-package :shaders)

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

(defun array-buffer-data (vbo verts)
  (gl:bind-buffer :array-buffer vbo)
  (let* ((length (length verts))
	 (arr (gl:alloc-gl-array :float length)))
    (dotimes (i length)
      (setf (gl:glaref arr i) (aref verts i)))
    (gl:buffer-data :array-buffer :static-draw arr)))


(defun flatten (lst &aux (result '()))
  (labels ((rflatten (lst1)
             (dolist (el lst1 result)
               (if (listp el)
                 (rflatten el)
                 (push el result)))))
      (nreverse (rflatten lst))))

;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┌─┐
;; │  │ ││││└─┐ │ └─┐
;; └─┘└─┘┘└┘└─┘ ┴ └─┘

(defparameter *instanced-vert* #(1.0 0.0  0.0 0.0  1.0 1.0  0.0 1.0))
(defparameter *instanced-texture-vert* #(1.0 0.0  0.0 0.0  1.0 1.0  0.0 1.0))
