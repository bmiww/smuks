;; ┬─┐┌─┐┌─┐┌┬┐┌─┐┌┐┌┌─┐┬  ┌─┐
;; ├┬┘├┤ │   │ ├─┤││││ ┬│  ├┤
;; ┴└─└─┘└─┘ ┴ ┴ ┴┘└┘└─┘┴─┘└─┘
(declaim (optimize (speed 0) (safety 0) (debug 3) (compilation-speed 0)))

(defpackage :shaders.rectangle
  (:use :cl)
  (:export shader update-projection update-matrix draw))
(in-package :shaders.rectangle)

(defclass shader ()
  ((pointer :accessor pointer)
   (projection :accessor projection)
   (matrix :accessor matrix)
   (uni-projection :accessor uni-projection)
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
    (setf pointer (shaders:create-shader vertex-shader-rectangle fragment-shader-rectangle))
    (setf instanced-vbo (gl:gen-buffer))
    (setf runtime-vbo (gl:gen-buffer))
    (setf vao (gl:gen-vertex-array))
    ;; (setf matrix (gl:get-uniform-location pointer "matrix"))
    (setf uni-projection (gl:get-uniform-location pointer "projection"))
    (setf attr-vert (gl:get-attrib-location pointer "vert"))
    (setf attr-position (gl:get-attrib-location pointer "position"))
    (setf attr-color (gl:get-attrib-location pointer "color"))

    (shaders:array-buffer-data instanced-vbo shaders:*instanced-vert*)

    (when projection (update-projection program projection))))

(defmethod draw ((program shader) rects)
  (with-slots (vao pointer instanced-vbo runtime-vbo attr-vert attr-position attr-color) program
    (gl:use-program pointer)
    (gl:bind-vertex-array vao)

    (gl:bind-buffer :array-buffer instanced-vbo)
    (gl:enable-vertex-attrib-array attr-vert)
    (gl:vertex-attrib-pointer attr-vert 2 :float nil (* 2 4) (cffi:null-pointer))

    (shaders:array-buffer-data
     runtime-vbo
     (concatenate
      'simple-vector
      (shaders::flatten (loop for rect in rects collect (space-tuple rect)))
      (shaders::flatten (loop for rect in rects collect (rect-color rect)))))

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

    (gl:draw-arrays-instanced :triangle-strip 0 4 (length rects))))


(defparameter vertex-shader-rectangle "
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

(defparameter fragment-shader-rectangle "
#version 310 es

precision mediump float;
in vec4 incolor;
out vec4 color;

void main() {
    color = incolor;
}
")

(defstruct rect
  x y
  w h
  color)

(defmethod space-tuple ((rect rect))
  (list (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))
