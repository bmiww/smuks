
;;  ██████╗ ██╗      ██╗   ██╗████████╗██╗██╗
;; ██╔════╝ ██║      ██║   ██║╚══██╔══╝██║██║
;; ██║  ███╗██║█████╗██║   ██║   ██║   ██║██║
;; ██║   ██║██║╚════╝██║   ██║   ██║   ██║██║
;; ╚██████╔╝███████╗ ╚██████╔╝   ██║   ██║███████╗
;;  ╚═════╝ ╚══════╝  ╚═════╝    ╚═╝   ╚═╝╚══════╝
(defpackage :smuks-gl-util
  (:use :cl :sdrm :smuks-util)
  (:nicknames :sglutil)
  (:export
   create-rect-shader
   check-gl-error
   check-gl-fb-status
   prep-gl-implementation
   create-gl-framebuffer))
(in-package :smuks-gl-util)

;; ┌─┐┬    ┌─┐┬─┐┌─┐┌─┐
;; │ ┬│    ├─┘├┬┘├┤ ├─┘
;; └─┘┴─┘  ┴  ┴└─└─┘┴
(defvar *instanced-verts* '(1.0 0.0   0.0 0.0   1.0 1.0   0.0 1.0))

(defun init-instanced-verts ()
  (let* ((vbo (gl:gen-buffer))
	 (arr (gl:alloc-gl-array :float (length *instanced-verts*))))
    (dotimes (i (length *instanced-verts*))
      (setf (gl:glaref arr i) (nth i *instanced-verts*)))

    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw arr)
    (check-gl-error "Init instanced verts")
    (gl:bind-buffer :array-buffer 0)))

(defun create-gl-framebuffer (image)
  (let* ((texture (gl:gen-texture))
	 (framebuffer (gl:gen-framebuffer)))
    (check-gl-error "Gen texture/framebuffer")
    (gl:bind-texture :texture-2d texture)
    (%gl:egl-image-target-texture-2d-oes :texture-2d image)
    (check-gl-error "egl-image-target-texture-2d-oes")
    (gl:bind-framebuffer :framebuffer framebuffer)

    (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)
    (check-gl-fb-status "After attaching texture")

    (values framebuffer texture)))

(defun create-rect-shader (device)
  (let* ((projection (make-projection-matrix (width device) (height device)))
	 (rect-shader (make-instance 'shaders.rectangle:shader :projection projection)))
    rect-shader))

(defun prep-gl-implementation (device framebuffer)
  (gl:bind-framebuffer :framebuffer framebuffer)
  (let* ((main-vbo (init-instanced-verts)))
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:viewport 0 0 (width device) (height device))
    main-vbo))


;; ┌┬┐┌─┐┌┬┐┬─┐┬┌─┐┌─┐┌─┐
;; │││├─┤ │ ├┬┘││  ├┤ └─┐
;; ┴ ┴┴ ┴ ┴ ┴└─┴└─┘└─┘└─┘
(defun make-projection-matrix (width height)
  (let* ((projection (clem:identity-matrix 3))
	 (x (/ 2.0 width))
	 (y (/ 2.0 height)))

    (setf (clem:mref projection 2 0)
	  (coerce (* -1.0 (copysign (+ (multf (clem:mref projection 0 0) x)
				       (multf (clem:mref projection 1 0) x)))) 'double-float))

    (setf (clem:mref projection 2 1)
	  (coerce (* -1.0 (copysign (+ (multf (clem:mref projection 0 1) y)
				       (multf (clem:mref projection 1 1) y)))) 'double-float))

    (let ((projection (clem::matrix->list projection)))
      (make-array (list (length projection)) :initial-contents projection))))

(defun copysign (val) (if (>= val 0) 1 -1))
