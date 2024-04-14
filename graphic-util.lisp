
(defpackage :smuks-gl-util
  (:use :cl)
  (:nicknames :sglutil)
  (:export
   check-egl-error
   check-gl-error
   check-gl-fb-status))
(in-package :smuks-gl-util)

(defun check-egl-error (&optional (prefix "EGL Error"))
  (let ((msg (case (egl:get-error)
	       (:success nil)
	       (:bad-alloc "EGL_BAD_ALLOC")
	       (:bad-config "EGL_BAD_CONFIG")
	       (:bad-context "EGL_BAD_CONTEXT")
	       (:bad-current-surface "EGL_BAD_CURRENT_SURFACE")
	       (:bad-display "EGL_BAD_DISPLAY")
	       (:bad-match "EGL_BAD_MATCH")
	       (:bad-native-pixmap "EGL_BAD_NATIVE_PIXMAP")
	       (:bad-native-window "EGL_BAD_NATIVE_WINDOW")
	       (:bad-parameter "EGL_BAD_PARAMETER")
	       (:bad-surface "EGL_BAD_SURFACE")
	       (t "TRAP: Unknown EGL error"))))
    (when msg (error (format nil "~a: ~a" prefix msg)))))


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
