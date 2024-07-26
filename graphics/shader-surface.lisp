
;; ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
;; TODO: I'd like to extract the 100 version shaders a bit further out, right now the whole thing is messy
(defpackage :shaders.surface
  (:use :cl :sglutil :shaders)
  (:export shader update-matrix draw-surface))
(in-package :shaders.surface)

(defclass shader (shaders:shader-base)
  ((uni-translation :accessor uni-translation)
   (uni-texture-scaling :accessor uni-texture-scaling)
   (uni-sampler :accessor uni-sampler)
   (uni-active :accessor uni-active)

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
(defparameter vertex-shader-surface-310-es "
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
uniform int active_surface;

in vec2 v_tex_coords;
out vec4 color;

void main() {
    vec4 tex_color = texture2D(sampler, v_tex_coords);
    if (active_surface != 1) {
      tex_color = tex_color * vec4(0.5, 0.5, 0.5, 1.0);
    }

    color = tex_color;
}")

(defparameter fragment-shader-xbgr-310-es "
#version 310 es

precision mediump float;
uniform sampler2D sampler;
uniform int active_surface;

in vec2 v_tex_coords;
out vec4 color;

void main() {
    vec4 tex_color = vec4(texture2D(sampler, v_tex_coords).rgb, 1.0);
    if (active_surface != 1) {
      tex_color = tex_color * vec4(0.5, 0.5, 0.5, 1.0);
    }

    color = tex_color;
}")

;; ┌─┐┌─┐┌┬┐┌─┐
;; │  │ │ ││├┤
;; └─┘└─┘─┴┘└─┘
(defmethod initialize-instance :before ((program shader) &key projection gl-version (format :argb8888))
  (setf (gl-version program) gl-version)
  (with-slots (pointer vao uni-projection instanced-vbo runtime-vbo attr-vert attr-transform-scale
	       uni-translation uni-texture-scaling uni-sampler gl-buffer-array
	       uni-active) program

    (let ((fragment-shader
	    (ecase format
	      (:argb8888 fragment-shader-abgr-310-es)
	      (:xrgb8888 fragment-shader-xbgr-310-es)))
	  (vertex-shader vertex-shader-surface-310-es))
      (setf pointer (shaders:create-shader vertex-shader fragment-shader)))

    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))

    (setf uni-sampler (gl:get-uniform-location pointer "sampler"))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf uni-translation (gl:get-uniform-location pointer "translation"))
    (setf uni-texture-scaling (gl:get-uniform-location pointer "tex_scaling_matrix"))
    (setf uni-active (gl:get-uniform-location pointer "active_surface"))

    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-transform-scale (gl:get-attrib-location pointer "vert_transform_scale"))

    ;; TODO: This 4 is horrible.
    ;; Especially since i might at some point allocate more than one vertice
    (setf gl-buffer-array (shaders:allocate-gl-array 4))

    (gl:use-program pointer)
    (gl:uniformi uni-sampler 0)
    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)

    (when projection (shaders:update-projection program projection))))


(defmethod draw-surface ((program shader) texture position &key active)
  (destructuring-bind (x y width height) position
    (let ((translation-matrix (translation-matrix x y))
	  (tex-scaling-matrix (scaling-matrix width height)))
      (with-slots (vao pointer instanced-vbo runtime-vbo attr-vert attr-transform-scale
		   uni-translation uni-texture-scaling gl-buffer-array
		   uni-active) program
	(gl:use-program pointer)
	(gl:bind-vertex-array vao)

	(gl:active-texture :texture0)
	(gl:bind-texture :texture-2d (tex-id texture))

	(gl:tex-parameter :texture-2d :texture-min-filter :linear)
	(gl:tex-parameter :texture-2d :texture-mag-filter :linear)

	(gl:bind-buffer :array-buffer instanced-vbo)
	(gl:enable-vertex-attrib-array attr-vert)
	(gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

	(gl:uniformi uni-active (if active 1.0 0.0))

	(uniform-matrix-3fv program uni-translation translation-matrix)
	(uniform-matrix-3fv program uni-texture-scaling tex-scaling-matrix)

	(gl:bind-buffer :array-buffer runtime-vbo)

	(shaders:fill-buffer
	 runtime-vbo
	 (concatenate 'simple-vector (util:flatten (list (list width height))))
	 gl-buffer-array)
	(gl:enable-vertex-attrib-array attr-transform-scale)
	(gl:vertex-attrib-pointer attr-transform-scale 4 :float nil (* 4 4) (cffi:null-pointer))


	(when (eq (gl-version program) :GL-3-1)
	  (%gl:vertex-attrib-divisor attr-vert 0)
	  (%gl:vertex-attrib-divisor attr-transform-scale 1))

	(gl:draw-arrays :triangle-strip 0 4)))))
