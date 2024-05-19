
;; ████████╗███████╗██╗  ██╗████████╗██╗   ██╗██████╗ ███████╗
;; ╚══██╔══╝██╔════╝╚██╗██╔╝╚══██╔══╝██║   ██║██╔══██╗██╔════╝
;;    ██║   █████╗   ╚███╔╝    ██║   ██║   ██║██████╔╝█████╗
;;    ██║   ██╔══╝   ██╔██╗    ██║   ██║   ██║██╔══██╗██╔══╝
;;    ██║   ███████╗██╔╝ ██╗   ██║   ╚██████╔╝██║  ██║███████╗
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
(declaim (optimize (speed 0) (safety 0) (debug 3) (compilation-speed 0)))

(defpackage :shaders.texture
  (:use :cl :sglutil :shaders)
  (:export shader update-matrix draw))
(in-package :shaders.texture)

(defclass shader (shaders:shader-base)
  ((uni-matrix :accessor uni-matrix)
   (uni-texture :accessor uni-texture)
   (uni-sampler :accessor uni-sampler)

   (instanced-vbo :accessor instanced-vbo)
   (runtime-vbo :accessor runtime-vbo)
   (attr-vert)
   (attr-position)
   (vao)))

(defparameter vertex-shader-texture "
#version 310 es
uniform mat3 matrix;
uniform mat3 tex_matrix;
uniform mat3 projection;

in vec2 vert;
in vec4 vert_position;

out vec2 v_tex_coords;

mat2 scale(vec2 scale_vec){
    return mat2(
        scale_vec.x, 0.0,
        0.0, scale_vec.y
    );
}

void main() {
    vec2 vert_transform_translation = vert_position.xy;
    vec2 vert_transform_scale = vert_position.zw;
    vec3 position = vec3(vert * scale(vert_transform_scale) + vert_transform_translation, 1.0);
    v_tex_coords = (tex_matrix * position).xy;
    gl_Position = vec4(projection * matrix * position, 1.0);
}")


(defparameter fragment-shader-abgr "
#version 310 es

precision mediump float;
uniform sampler2D sampler;
in vec2 v_tex_coords;
out vec4 color;

void main() {
    color = texture2D(sampler, v_tex_coords);
}")


(defmethod shaders:update-projection ((program shader) new-projection)
  (with-slots (pointer projection uni-projection) program
    (setf projection new-projection)
    (gl:use-program pointer)
    (gl:uniform-matrix-3fv uni-projection projection nil)))

(defmethod initialize-instance :before ((program shader) &key projection)
  (with-slots (pointer vao uni-projection instanced-vbo runtime-vbo attr-vert attr-position
	       uni-matrix uni-texture uni-sampler) program
    (setf pointer (shaders:create-shader vertex-shader-texture fragment-shader-abgr))

    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))

    (setf uni-sampler (gl:get-uniform-location pointer "sampler"))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf uni-matrix (gl:get-uniform-location pointer "matrix"))
    (setf uni-texture (gl:get-uniform-location pointer "tex_matrix"))

    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-position (gl:get-attrib-location pointer "vert_position"))

    (gl:use-program pointer)
    (gl:uniformi uni-sampler 0)
    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)

    (when projection (shaders:update-projection program projection))))


;; TODO: This could be improved by sending in a list of surfaces to draw at once.
;; For now just setting draw-instances to 1 and calling draw for each surface
;; See the rect-shader for an example.
(defvar *draw-instances* 1)

;; TODO: TRANSFORM???
(defmethod draw ((program shader) texture position orientation)
  (destructuring-bind (x y width height) position
    (let ((pos-matrix (make-position-matrix (coerce x 'double-float) (coerce y 'double-float)))
	  (tex-matrix (texture-matrix width height orientation)))
      (with-slots (vao pointer instanced-vbo runtime-vbo attr-vert attr-position
		   uni-matrix uni-texture) program
	(gl:use-program pointer)
	(gl:bind-vertex-array vao)

	(gl:active-texture :texture0)
	(gl:bind-texture :texture-2d texture)

	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)

	(gl:bind-buffer :array-buffer instanced-vbo)
	(gl:enable-vertex-attrib-array attr-vert)
	(gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

	(gl:uniform-matrix-3fv uni-matrix pos-matrix nil)
	(gl:uniform-matrix-3fv uni-texture tex-matrix nil)

	(gl:bind-buffer :array-buffer runtime-vbo)
	(shaders:array-buffer-data
	 runtime-vbo
	 (concatenate
	  'simple-vector
	  (util:flatten (loop for rect in `(,(make-rect :x 0.0 :y 0.0 :w width :h height)) collect (space-tuple rect)))))
	(gl:enable-vertex-attrib-array attr-position)
	(gl:vertex-attrib-pointer attr-position 4 :float nil (* 4 4) (cffi:null-pointer))

	;; NOTE This has some info on how everything gets collected together with the attrib pointers
	;; https://www.khronos.org/opengl/wiki/Generic_Vertex_Attribute_-_examples

	(%gl:vertex-attrib-divisor attr-vert 0)
	(%gl:vertex-attrib-divisor attr-position 1)

	(gl:draw-arrays-instanced :triangle-strip 0 4 *draw-instances*)))))


(defun from-nonuniform-scale (x y)
  (let ((matrix (clem:identity-matrix 3)))
    (setf (clem:mref matrix 0 0) x)
    (setf (clem:mref matrix 1 1) y)
    matrix))

(defun texture-matrix (width height orientation)
  (matrix->array
   (clem:m* (clem:identity-matrix 3)
	    (from-nonuniform-scale (coerce (/ 1.0 width) 'double-float) (coerce (/ 1.0 height) 'double-float)))))

(defstruct rect
  x y
  w h
  color)

(defmethod space-tuple ((rect rect))
  (list (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))
