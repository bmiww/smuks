
;; ████████╗███████╗██╗  ██╗████████╗██╗   ██╗██████╗ ███████╗
;; ╚══██╔══╝██╔════╝╚██╗██╔╝╚══██╔══╝██║   ██║██╔══██╗██╔════╝
;;    ██║   █████╗   ╚███╔╝    ██║   ██║   ██║██████╔╝█████╗
;;    ██║   ██╔══╝   ██╔██╗    ██║   ██║   ██║██╔══██╗██╔══╝
;;    ██║   ███████╗██╔╝ ██╗   ██║   ╚██████╔╝██║  ██║███████╗
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; TODO: I'd like to extract the 100 version shaders a bit further out, right now the whole thing is messy
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
   (attr-transform-scale)
   (format)
   (vao)))


;; ┌─┐┬  ┌─┐┬
;; │ ┬│  └─┐│
;; └─┘┴─┘└─┘┴─┘
(defparameter vertex-shader-texture-310-es "
#version 310 es
uniform mat3 translation;
uniform mat3 tex_scaling_matrix;
uniform mat3 projection;

in vec2 vert;
in vec2 vert_transform_scale;

out vec2 v_tex_coords;

mat2 scale(vec2 scale_vec){
    return mat2(
        scale_vec.x, 0.0,
        0.0, scale_vec.y
    );
}

void main() {
    vec3 position = vec3(vert * scale(vert_transform_scale), 1.0);
    v_tex_coords = (tex_scaling_matrix * position).xy;
    gl_Position = vec4(projection * translation * position, 1.0);
}")


(defparameter fragment-shader-abgr-310-es "
#version 310 es

precision mediump float;
uniform sampler2D sampler;
in vec2 v_tex_coords;
out vec4 color;

void main() {
    vec4 tex_color = texture2D(sampler, v_tex_coords);
    color = tex_color;
}")

(defparameter fragment-shader-xbgr-310-es "
#version 310 es

precision mediump float;
uniform sampler2D sampler;
in vec2 v_tex_coords;
out vec4 color;

void main() {
    vec4 tex_color = vec4(texture2D(sampler, v_tex_coords).rgb, 1.0);
    color = tex_color;
}")

(defparameter vertex-shader-texture-100 "
#version 100

uniform mat3 translation;
uniform mat3 tex_scaling_matrix;
uniform mat3 projection;

attribute vec2 vert;
attribute vec2 vert_transform_scale;

varying vec2 v_tex_coords;

mat2 scale(vec2 scale_vec){
    return mat2(
        scale_vec.x, 0.0,
        0.0, scale_vec.y
    );
}

void main() {
    vec3 position = vec3(vert * scale(vert_transform_scale), 1.0);
    v_tex_coords = (tex_scaling_matrix * position).xy;
    gl_Position = vec4(projection * translation * position, 1.0);
}")


(defparameter fragment-shader-abgr-100 "
#version 100

precision mediump float;
uniform sampler2D sampler;
varying vec2 v_tex_coords;

void main() {
    vec4 tex_color = texture2D(sampler, v_tex_coords);
    gl_FragColor = tex_color;
}")

;; ┌─┐┌─┐┌┬┐┌─┐
;; │  │ │ ││├┤
;; └─┘└─┘─┴┘└─┘
(defmethod initialize-instance :before ((program shader) &key projection gl-version (format :argb8888))
  (setf (gl-version program) gl-version)
  (with-slots (pointer vao uni-projection instanced-vbo runtime-vbo attr-vert attr-transform-scale
	       uni-translation uni-texture-scaling uni-sampler gl-buffer-array) program

    (let ((fragment-shader
	    (ecase gl-version
	      (:GL-2-0
	       (ecase format
		 (:argb8888 fragment-shader-abgr-100)))
	      (:GL-3-1
	       (ecase format
		 (:argb8888 fragment-shader-abgr-310-es)
		 (:xrgb8888 fragment-shader-xbgr-310-es)))))

	  (vertex-shader (case gl-version
			   (:GL-2-0 vertex-shader-texture-100)
			   (:GL-3-1 vertex-shader-texture-310-es))))
      (setf pointer (shaders:create-shader vertex-shader fragment-shader)))

    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))

    (setf uni-sampler (gl:get-uniform-location pointer "sampler"))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf uni-translation (gl:get-uniform-location pointer "translation"))
    (setf uni-texture-scaling (gl:get-uniform-location pointer "tex_scaling_matrix"))

    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-transform-scale (gl:get-attrib-location pointer "vert_transform_scale"))

    ;; TODO: This 2 is horrible.
    ;; Especially since i might at some point allocate more than one vertice
    (setf gl-buffer-array (shaders:allocate-gl-array 2))

    (gl:use-program pointer)
    (gl:bind-vertex-array vao)

    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)
    (gl:uniformi uni-sampler 0)

    (gl:bind-buffer :array-buffer instanced-vbo)
    (gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

    (gl:bind-buffer :array-buffer runtime-vbo)
    (gl:vertex-attrib-pointer attr-transform-scale 2 :float nil (* 2 4) (cffi:null-pointer))

    (%gl:vertex-attrib-divisor attr-vert 0)
    (%gl:vertex-attrib-divisor attr-transform-scale 1)

    (when projection (shaders:update-projection program projection))))


;; TODO: You should be able to turn shaders into render passes
;; So that a texture shader wouldn't need to be reenabled for each texture
(defmethod draw ((program shader) texture position)
  (destructuring-bind (x y width height) position
    (let ((translation-matrix (translation-matrix x y))
	  (tex-scaling-matrix (scaling-matrix width height)))
      (with-slots (vao pointer instanced-vbo runtime-vbo attr-vert attr-transform-scale
		   uni-translation uni-texture-scaling gl-buffer-array) program
	(gl:use-program pointer)
	(gl:bind-vertex-array vao)

	(gl:active-texture :texture0)
	(gl:bind-texture :texture-2d (tex-id texture))

	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)

	(uniform-matrix-3fv program uni-translation translation-matrix)
	(uniform-matrix-3fv program uni-texture-scaling tex-scaling-matrix)

	(shaders:fill-buffer
	 runtime-vbo
	 (list width height)
	 gl-buffer-array)

	(gl:enable-vertex-attrib-array attr-vert)
	(gl:enable-vertex-attrib-array attr-transform-scale)

	(gl:draw-arrays :triangle-strip 0 4)

	(gl:disable-vertex-attrib-array attr-vert)
	(gl:disable-vertex-attrib-array attr-transform-scale)
	(gl:bind-buffer :array-buffer 0)
	(gl:bind-vertex-array 0)
	(gl:bind-texture :texture-2d 0)))))
