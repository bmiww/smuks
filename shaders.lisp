
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
   shader-base update-projection pointer projection uni-projection

   defshader update-shaders-of-name

   gl-version
   uniform-matrix-3fv))

(in-package :shaders)

;; TODO: Make previous optional for debugging
;; NOTE: Previous is meant to track previous versions of the shader (pointers)
(defclass shader-base ()
  ((previous :initform nil :accessor previous)
   (pointer :accessor pointer)
   (projection :accessor projection)
   (uni-projection :accessor uni-projection)
   (gl-version :initarg :gl-version :accessor gl-version)))

(defgeneric update-projection (shader projection))
(defmethod update-projection ((shader shader-base) new-projection)
  (with-slots (pointer projection uni-projection) shader
    (setf projection new-projection)
    (gl:use-program pointer)

    (uniform-matrix-3fv shader uni-projection projection)
    (check-gl-error "buffer-data uniform-matrix-3fv")))

(defmethod uniform-matrix-3fv ((shader shader-base) uniform matrix)
  "A utility wrapper for older GL versions.
GL es does not like the transpose flag."
  (gl:uniform-matrix-3fv uniform
			 (sglutil:mat->arr
			  (case (gl-version shader)
			    (:gl-2-0 (sglutil:transpose-mat matrix))
			    (:gl-3-0 matrix)))
			 (case (gl-version shader)
			   (:gl-2-0 nil)
			   (:gl-3-0 t))))


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
  (gl:buffer-data :array-buffer :static-draw arr)
  ;; TODO: Create and reenable the buffer-sub-data flow
  ;; TODO: The width/height actually doesn't change that often
  ;; A lot of this buffer prep could be skipped
  ;; (buffer-sub-data :array-buffer arr)
  )

;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┌─┐
;; │  │ ││││└─┐ │ └─┐
;; └─┘└─┘┘└┘└─┘ ┴ └─┘

(defparameter *instanced-vert* #(1.0 0.0  0.0 0.0  1.0 1.0  0.0 1.0))
(defparameter *instanced-texture-vert* #(1.0 0.0  0.0 0.0  1.0 1.0  0.0 1.0))


;; ┌─┐┬ ┬┌─┐┌┬┐┌─┐┬─┐  ┬  ┬┌─┐┬─┐┌─┐┬┌─┐┌┐┌┌─┐
;; └─┐├─┤├─┤ ││├┤ ├┬┘  └┐┌┘├┤ ├┬┘└─┐││ ││││└─┐
;; └─┘┴ ┴┴ ┴─┴┘└─┘┴└─   └┘ └─┘┴└─└─┘┴└─┘┘└┘└─┘
;; TODO: Do not use - eval does not run on the GL thread
;; Would either:
;; Need to attach to swank somehow
;; Or have a poller that picks up the please compile message
;; For now got lazy and abandoned the idea

(defvar *shader-tracker* (make-hash-table :test 'equalp))
;; NOTE: this variable should be transient and mostly stay nil.
;; Any calling code should set it to nil after using it.
(defvar *enable-shader-versioning* nil)

;; TODO: Maybe check if the calling package already has this var
;; In order to save the form from the first time this was invoked
(defmacro defshader (class name &body body)
  (let ((form `(defvar ,name ,@body))
	(existing (gethash class *shader-tracker*)))
    (unless existing (setf (gethash class *shader-tracker*) (list)))
    ;; (when (and *enable-shader-versioning* existing) (rebuild-shader class form))
    form))

(defun rebuild-shader (class form)
  (eval form)
  (print "Rebuild shader is broken, please don't call it")
  (let* ((versions (gethash class *shader-tracker*)))
    (dolist (shader versions)
      (with-slots (projection) shader
	(make-instance class :projection projection)))))

(defmethod initialize-instance :after ((shader shader-base) &key)
  (let ((list (or (gethash (type-of shader) *shader-tracker*)
		  (setf (gethash (type-of shader) *shader-tracker*) (list)))))
    (declare (ignore list))
    ;; (setf (gethash (type-of shader) *shader-tracker*) (cons shader list))))
    ))
