
(defpackage :smuks-gl-util
  (:use :cl)
  (:nicknames :sglutil)
  (:export
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
    ;; (log! "First check~%")
    ;; (check-gl-fb-status)

    (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)
    (check-gl-fb-status "After attaching texture")

    ;; (gl:bind-texture :texture-2d 0)
    ;; (gl:bind-framebuffer :framebuffer 0)

    (values framebuffer texture)))

(defun prep-gl-implementation (drm-device)
  (gl:bind-framebuffer :framebuffer *frame-buffer*)
  (let* ((main-vbo (init-instanced-verts))
	 (shaders "NOT IMPLEMENTED"))
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:viewport 0 0 (width drm-device) (height drm-device))

    (values main-vbo shaders)))


;; ┌─┐┬─┐┬─┐┌─┐┬─┐  ┌─┐┬ ┬┌─┐┌─┐┬┌─┌─┐
;; ├┤ ├┬┘├┬┘│ │├┬┘  │  ├─┤├┤ │  ├┴┐└─┐
;; └─┘┴└─┴└─└─┘┴└─  └─┘┴ ┴└─┘└─┘┴ ┴└─┘
(defun check-gl-error (&optional (prefix "GL Error"))
  (let ((msg (case (gl:get-error)
	       (:zero nil)
	       (:invalid-enum "Invalid enum")
	       (:invalid-value "Invalid value")
	       (:invalid-operation "Invalid operation")
	       (:stack-overflow "Stack overflow")
	       (:stack-underflow "Stack underflow")
	       (:out-of-memory "Out of memory")
	       (t "Unknown error"))))
    (when msg (error (format nil "~a: ~a" prefix msg)))))

(defun check-gl-fb-status (&optional (prefix "FB status"))
  (let ((msg (case (gl:check-framebuffer-status :framebuffer)
	       (:framebuffer-complete-oes nil)
	       (:framebuffer-complete nil)
	       (:zero (check-gl-error))
	       (:framebuffer-incomplete-attachment "Framebuffer incomplete attachment")
	       (:framebuffer-incomplete-missing-attachment "Framebuffer incomplete missing attachment")
	       (:framebuffer-unsupported "Framebuffer unsupported")
	       (:framebuffer-incomplete-multisample "Framebuffer incomplete multisample")
	       (:framebuffer-undefined "Framebuffer undefined")
	       (t (error "Uncovered GL framebuffer error code")))))
    (when msg (error (format nil "~a: ~a~%" prefix msg)))))
