
;; ████████╗███████╗██╗  ██╗████████╗██╗   ██╗██████╗ ███████╗
;; ╚══██╔══╝██╔════╝╚██╗██╔╝╚══██╔══╝██║   ██║██╔══██╗██╔════╝
;;    ██║   █████╗   ╚███╔╝    ██║   ██║   ██║██████╔╝█████╗
;;    ██║   ██╔══╝   ██╔██╗    ██║   ██║   ██║██╔══██╗██╔══╝
;;    ██║   ███████╗██╔╝ ██╗   ██║   ╚██████╔╝██║  ██║███████╗
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
(defpackage :shaders.texture
  (:use :cl :sglutil :shaders)
  (:export shader update-matrix draw))
(in-package :shaders.texture)

(defclass shader (shaders:shader-base)
  ((uni-translation :accessor uni-translation)
   (uni-texture-scaling :accessor uni-texture-scaling)
   (uni-sampler :accessor uni-sampler)

   (instanced-vbo :accessor instanced-vbo)
   (runtime-vbo :accessor runtime-vbo)
   (gl-buffer-array :accessor gl-buffer-array)
   (attr-vert)
   (attr-position)
   (vao)))

(defparameter vertex-shader-texture "
#version 310 es
uniform mat3 translation;
uniform mat3 tex_scaling_matrix;
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
    v_tex_coords = (tex_scaling_matrix * position).xy;
    gl_Position = vec4(projection * translation * position, 1.0);
}")


(defparameter fragment-shader-abgr "
#version 310 es

precision mediump float;
uniform sampler2D sampler;
in vec2 v_tex_coords;
out vec4 color;

void main() {
    vec4 tex_color = texture2D(sampler, v_tex_coords);
    if (tex_color.a < 0.1) {
       tex_color = vec4(0.0, 0.0, 0.0, 0.2);
    }
    color = tex_color;
}")

(defmethod initialize-instance :before ((program shader) &key projection)
  (with-slots (pointer vao uni-projection instanced-vbo runtime-vbo attr-vert attr-position
	       uni-translation uni-texture-scaling uni-sampler gl-buffer-array) program
    (setf pointer (shaders:create-shader vertex-shader-texture fragment-shader-abgr))

    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))

    (setf uni-sampler (gl:get-uniform-location pointer "sampler"))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf uni-translation (gl:get-uniform-location pointer "translation"))
    (setf uni-texture-scaling (gl:get-uniform-location pointer "tex_scaling_matrix"))

    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-position (gl:get-attrib-location pointer "vert_position"))
    ;; TODO: This 4 is horrible.
    ;; Especially since i might at some point allocate more than one vertice
    (setf gl-buffer-array (shaders:allocate-gl-array 4))

    (gl:use-program pointer)
    (gl:uniformi uni-sampler 0)
    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)

    (when projection (shaders:update-projection program projection))))


;; TODO: This could be improved by sending in a list of surfaces to draw at once.
;; For now just setting draw-instances to 1 and calling draw for each surface
;; See the rect-shader for an example.
(defvar *draw-instances* 1)

;; TODO: You should be able to turn shaders into render passes
;; So that a texture shader wouldn't need to be reenabled for each texture
(defmethod draw ((program shader) texture position)
  (destructuring-bind (x y width height) position
    (let ((translation-matrix (translation-matrix x y))
	  (tex-scaling-matrix (scaling-matrix width height)))
      (with-slots (vao pointer instanced-vbo runtime-vbo attr-vert attr-position
		   uni-translation uni-texture-scaling gl-buffer-array) program
	(gl:use-program pointer)
	(gl:bind-vertex-array vao)

	(gl:active-texture :texture0)
	(gl:bind-texture :texture-2d (tex-id texture))

	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)

	(gl:bind-buffer :array-buffer instanced-vbo)
	(gl:enable-vertex-attrib-array attr-vert)
	(gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

	(gl:uniform-matrix-3fv uni-translation translation-matrix t)
	(gl:uniform-matrix-3fv uni-texture-scaling tex-scaling-matrix t)

	(gl:bind-buffer :array-buffer runtime-vbo)

	(shaders:fill-buffer
	 runtime-vbo
	 (concatenate
	  'simple-vector
	  (util:flatten (loop for rect in `(,(make-rect :x 0.0 :y 0.0 :w width :h height)) collect (space-tuple rect))))
	 gl-buffer-array)
	(gl:enable-vertex-attrib-array attr-position)
	(gl:vertex-attrib-pointer attr-position 4 :float nil (* 4 4) (cffi:null-pointer))

	(%gl:vertex-attrib-divisor attr-vert 0)
	(%gl:vertex-attrib-divisor attr-position 1)

	(gl:draw-arrays :triangle-strip 0 4)))))

(defstruct rect
  x y
  w h
  color)

(defmethod space-tuple ((rect rect))
  (list (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))
