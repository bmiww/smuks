
;; ████████╗███████╗██╗  ██╗████████╗██╗   ██╗██████╗ ███████╗
;; ╚══██╔══╝██╔════╝╚██╗██╔╝╚══██╔══╝██║   ██║██╔══██╗██╔════╝
;;    ██║   █████╗   ╚███╔╝    ██║   ██║   ██║██████╔╝█████╗
;;    ██║   ██╔══╝   ██╔██╗    ██║   ██║   ██║██╔══██╗██╔══╝
;;    ██║   ███████╗██╔╝ ██╗   ██║   ╚██████╔╝██║  ██║███████╗
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
(defpackage :shaders.texture
  (:use :cl :sglutil)
  (:export shader update-projection update-matrix draw))
(in-package :shaders.texture)

(defclass shader ()
  ((pointer :accessor pointer)
   (projection :accessor projection)
   (matrix :accessor matrix)
   (uni-projection :accessor uni-projection)
   (uni-matrix :accessor uni-matrix)
   (uni-texture :accessor uni-texture)

   (instanced-vbo :accessor instanced-vbo)
   (runtime-vbo :accessor runtime-vbo)
   (attr-vert)
   (attr-position)
   (attr-color)
   (vao)))

(defmethod update-projection ((program shader) new-projection)
  (with-slots (pointer projection uni-projection) program
    (setf projection new-projection)
    (gl:use-program pointer)
    (gl:uniform-matrix-3fv uni-projection projection nil)))

(defmethod initialize-instance :before ((program shader) &key projection)
  (with-slots (pointer vao uni-projection instanced-vbo runtime-vbo attr-vert attr-position attr-color) program
    (setf pointer (shaders:create-shader vertex-shader-texture fragment-shader-abgr))
    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))
    ;; (setf matrix (gl:get-uniform-location pointer "matrix"))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf uni-matrix (gl:get-uniform-location pointer "matrix"))
    (setf uni-texture (gl:get-uniform-location pointer "tex_matrix"))

    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-position (gl:get-attrib-location pointer "vert_position"))
    (setf attr-color (gl:get-attrib-location pointer "color"))

    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)

    (when projection (update-projection program projection))))

;; TODO: TRANSFORM???
(defmethod draw ((program shader) texture position)
  (destructuring-bind (x y width height) position
    (let ((pos-matrix (make-position-matrix x y)))
      (with-slots (vao pointer instanced-vbo runtime-vbo attr-vert attr-position attr-color
		   uni-matrix) program
	(gl:use-program pointer)
	(gl:bind-vertex-array vao)

	(gl:bind-buffer :array-buffer instanced-vbo)
	(gl:active-texture :texture0)
	(gl:bind-texture :texture-2d texture)

	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)

	(gl:enable-vertex-attrib-array attr-vert)
	(gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

	(gl:uniform-matrix-3fv uni-matrix pos-matrix nil)

	(shaders:array-buffer-data
	 runtime-vbo
	 (concatenate
	  'simple-vector
	  (shaders::flatten (loop for rect in position collect (space-tuple rect)))))

	(gl:bind-buffer :array-buffer runtime-vbo)
	(gl:enable-vertex-attrib-array attr-position)
	(gl:vertex-attrib-pointer attr-position 4 :float nil (* 4 4) (cffi:null-pointer))

	;; NOTE This has some info on how everything gets collected together with the attrib pointers
	;; https://www.khronos.org/opengl/wiki/Generic_Vertex_Attribute_-_examples
	(gl:enable-vertex-attrib-array attr-color)
	(gl:vertex-attrib-pointer attr-color 4 :float nil (* 4 4) (cffi:inc-pointer (cffi:null-pointer) (* 4 4 (length rects))))

	(%gl:vertex-attrib-divisor attr-vert 0)
	(%gl:vertex-attrib-divisor attr-position 1)
	(%gl:vertex-attrib-divisor attr-color 1)

	(gl:draw-arrays-instanced :triangle-strip 0 4 (length rects))))))


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
uniform sampler2D tex;
uniform vec4 shade;
in vec2 v_tex_coords;
out vec4 color;

void main() {
    color = texture2D(tex, v_tex_coords) * shade;
}")

(defstruct rect
  x y
  w h
  color)

(defmethod space-tuple ((rect rect))
  (list (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))
