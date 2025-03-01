
;; ██████╗ ███████╗ ██████╗████████╗ █████╗ ███╗   ██╗ ██████╗ ██╗     ███████╗
;; ██╔══██╗██╔════╝██╔════╝╚══██╔══╝██╔══██╗████╗  ██║██╔════╝ ██║     ██╔════╝
;; ██████╔╝█████╗  ██║        ██║   ███████║██╔██╗ ██║██║  ███╗██║     █████╗
;; ██╔══██╗██╔══╝  ██║        ██║   ██╔══██║██║╚██╗██║██║   ██║██║     ██╔══╝
;; ██║  ██║███████╗╚██████╗   ██║   ██║  ██║██║ ╚████║╚██████╔╝███████╗███████╗
;; ╚═╝  ╚═╝╚══════╝ ╚═════╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚══════╝╚══════╝
(defpackage :shaders.rectangle
  (:use :cl :shaders)
  (:export shader update-matrix draw))
(in-package :shaders.rectangle)

;; TODO Rect - move/remove/improve
(defstruct rect x y w h color)
(defmethod space-tuple ((rect rect))
  (list (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))


(defclass shader (shaders:shader-base)
  ((instanced-vbo :accessor instanced-vbo)
   (runtime-vbo :accessor runtime-vbo)
   (gl-buffer-array :accessor gl-buffer-array)
   (attr-vert)
   (attr-position)
   (attr-color)
   (vao)))

;; ┌─┐┬  ┌─┐┬
;; │ ┬│  └─┐│
;; └─┘┴─┘└─┘┴─┘
(defparameter vertex-shader-rectangle-310-es "
#version 310 es

uniform mat3 projection;

in vec2 vert;
in vec4 position;
in vec4 color;
out vec4 incolor;

mat2 scale(vec2 scale_vec){
    return mat2(
        scale_vec.x, 0.0,
        0.0, scale_vec.y
    );
}

void main() {
    vec2 transform_translation = position.xy;
    vec2 transform_scale = position.zw;
    vec3 position = vec3(vert * scale(transform_scale) + transform_translation, 1.0);

    incolor = color;
    gl_Position = vec4(projection * position, 1.0);
}
")

(defparameter fragment-shader-rectangle-310-es "
#version 310 es

precision mediump float;
in vec4 incolor;
out vec4 color;

void main() {
    color = incolor;
}
")

(defparameter vertex-shader-rectangle-100 "
#version 100

uniform mat3 projection;

attribute vec2 vert;
attribute vec4 position;
attribute vec4 color;
varying vec4 incolor;

mat2 scale(vec2 scale_vec){
    return mat2(
        scale_vec.x, 0.0,
        0.0, scale_vec.y
    );
}

void main() {
    vec2 transform_translation = position.xy;
    vec2 transform_scale = position.zw;
    vec3 position = vec3(vert * scale(transform_scale) + transform_translation, 1.0);

    incolor = color;
    gl_Position = vec4(projection * position, 1.0);
}
")

(defparameter fragment-shader-rectangle-100 "
#version 100

precision mediump float;
varying vec4 incolor;

void main() {
    gl_FragColor = incolor;
}
")


;; ┌─┐┌─┐┌┬┐┌─┐
;; │  │ │ ││├┤
;; └─┘└─┘─┴┘└─┘
(defmethod initialize-instance :before ((program shader) &key projection gl-version)
  (setf (gl-version program) gl-version)
  (with-slots (pointer vao uni-projection instanced-vbo runtime-vbo attr-vert attr-position attr-color gl-buffer-array) program
    (let ((fragment-shader (case gl-version
			     (:GL-2-0 fragment-shader-rectangle-100)
			     (:GL-3-1 fragment-shader-rectangle-310-es)))
	  (vertex-shader (case gl-version
			   (:GL-2-0 vertex-shader-rectangle-100)
			   (:GL-3-1 vertex-shader-rectangle-310-es))))
      (setf pointer (shaders:create-shader vertex-shader fragment-shader)))

    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-position (gl:get-attrib-location pointer "position"))
    (setf attr-color (gl:get-attrib-location pointer "color"))
    (setf gl-buffer-array (shaders:allocate-gl-array 8))

    (gl:use-program pointer)
    (gl:bind-vertex-array vao)
    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)

    (gl:bind-buffer :array-buffer instanced-vbo)
    (gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

    (gl:bind-buffer :array-buffer runtime-vbo)
    (gl:vertex-attrib-pointer attr-position 4 :float nil (* 4 4) (cffi:null-pointer))
    (gl:vertex-attrib-pointer attr-color 4 :float nil (* 4 4) (cffi:inc-pointer (cffi:null-pointer) (* 4 4)))

    (ecase (gl-version program)
      (:GL-3-1
       (progn
	 (%gl:vertex-attrib-divisor attr-vert 0)
	 (%gl:vertex-attrib-divisor attr-position 1)
	 (%gl:vertex-attrib-divisor attr-color 1))))

    (when projection (shaders:update-projection program projection))))

(defmethod draw ((program shader) rects)
  (with-slots (vao pointer instanced-vbo runtime-vbo
	       attr-vert attr-position attr-color
	       gl-buffer-array) program
    (gl:use-program pointer)
    (gl:bind-vertex-array vao)

    (shaders:fill-buffer
     runtime-vbo
     `(,@(space-tuple (car rects))
       ,@(rect-color (car rects)))
     gl-buffer-array)

    (gl:enable-vertex-attrib-array attr-vert)
    (gl:enable-vertex-attrib-array attr-position)
    (gl:enable-vertex-attrib-array attr-color)

    (ecase (gl-version program)
      (:GL-2-0
       (progn
	 ;; (gl:draw-arrays :triangle-strip 0 9)
	 ))
      (:GL-3-1
       (progn
	 (%gl:vertex-attrib-divisor attr-vert 0)
	 (%gl:vertex-attrib-divisor attr-position 1)
	 (%gl:vertex-attrib-divisor attr-color 1)

	 (gl:draw-arrays-instanced :triangle-strip 0 4 (length rects)))))

    (gl:disable-vertex-attrib-array attr-vert)
    (gl:disable-vertex-attrib-array attr-position)
    (gl:disable-vertex-attrib-array attr-color)
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)))
