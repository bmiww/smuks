
;; ███████╗ ██████╗ ██╗
;; ██╔════╝██╔════╝ ██║
;; █████╗  ██║  ███╗██║
;; ██╔══╝  ██║   ██║██║
;; ███████╗╚██████╔╝███████╗
;; ╚══════╝ ╚═════╝ ╚══════╝
;; NOTE: libwayland egl code
;; https://gitlab.freedesktop.org/wayland/wayland/-/tree/main/egl?ref_type=heads
;; NOTE: Nvidia eglstream code for binding egl to wayland
;; https://github.com/NVIDIA/egl-wayland/blob/master/src/wayland-egldisplay.c#L82

(defpackage :smuks-egl-util
  (:use :cl)
  (:nicknames :seglutil)
  (:export
   create-egl-image destroy-image
   create-egl-image-from-buffer
   init-egl cleanup-egl
   check-egl-error))
(in-package :smuks-egl-util)

(defvar context-attribs
  (list
   :context-major-version 3
   :context-minor-version 1
   :none))

;; ┌─┐┌─┐┌┐┌┌┬┐┌─┐─┐ ┬┌┬┐
;; │  │ ││││ │ ├┤ ┌┴┬┘ │
;; └─┘└─┘┘└┘ ┴ └─┘┴ └─ ┴
(defun init-egl (gbm wl-display)
  (egl:load-egl-extensions) (check-egl-error "Binding extensions")

  (let* ((display (egl:get-display gbm)))
    (multiple-value-bind (major minor) (egl:initialize display)
      (util:log! "EGL version: ~a.~a" major minor)
      (check-egl-error "Initializing display"))
    (egl:bind-wl-display display wl-display) (check-egl-error "Binding wayland")
    (egl:bind-api :opengl-es-api)            (check-egl-error "Binding api")

    (let* ((context (apply 'egl:create-context
			   `(,display ,(cffi:null-pointer) ,(cffi:null-pointer) ,@context-attribs))))
      (check-egl-error "Initializing egl context")

      (when (cffi:null-pointer-p context) (error "Failed to create context (got null pointer)"))
      (egl:make-current display (cffi:null-pointer) (cffi:null-pointer) context)
      (when (cffi:null-pointer-p (egl:get-current-context)) (error "Context not CURRENT (got null pointer)"))

      (values display context))))

;; TODO: For some reason make-current and terminate here are hanging
;; Don't know when i introduced this bug
;; And don't know why it's happening
;; Doesn't happen always though. Maybe multiscreen stuff?
(defun cleanup-egl (egl wl context)
  (egl:unbind-wl-display egl wl)
  (egl:make-current egl (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))
  (egl:destroy-context egl context)
  (egl:terminate egl))


;; ┬┌┬┐┌─┐┌─┐┌─┐
;; ││││├─┤│ ┬├┤
;; ┴┴ ┴┴ ┴└─┘└─┘
;; NOTE: Stride is pitch. Eh.
(defun create-egl-image (egl buffer-object width height)
  (let ((stride (gbm:bo-get-stride buffer-object)) (offset 0))
    ;; TODO: Maybe the gl lib already has this extension defined. That lib seems a bit more polished
    (egl:create-image-khr egl (cffi:null-pointer) egl::LINUX_DMA_BUF_EXT (cffi:null-pointer)
			  :width width :height height
			  :linux-drm-fourcc-ext gbm::FORMAT_XRGB8888
			  :dma-buf-plane0-fd-ext (gbm:bo-get-fd buffer-object)
			  :dma-buf-plane0-pitch-ext stride
			  :dma-buf-plane0-offset-ext offset
			  :none)))

;; TODO: Make this multi-planar if you decide to support other pixel formats
;; TODO: This is also practically identical to the above function. Might want to merge them.
;; TODO: EGL lib - automatically append :none
(defun create-egl-image-from-buffer (egl width height format fd offset stride)
  (prog1
      (egl:create-image-khr egl (cffi:null-pointer) egl::LINUX_DMA_BUF_EXT (cffi:null-pointer)
			    :width width :height height
			    :linux-drm-fourcc-ext format
			    :dma-buf-plane0-fd-ext fd
			    :dma-buf-plane0-pitch-ext stride
			    :dma-buf-plane0-offset-ext offset
			    :none)
    (check-egl-error "Creating egl image")))


(defun destroy-image (egl image) (egl:destroy-image egl image))


;; ┌─┐┬─┐┬─┐┌─┐┬─┐  ┌─┐┬ ┬┌─┐┌─┐┬┌─┌─┐
;; ├┤ ├┬┘├┬┘│ │├┬┘  │  ├─┤├┤ │  ├┴┐└─┐
;; └─┘┴└─┴└─└─┘┴└─  └─┘┴ ┴└─┘└─┘┴ ┴└─┘
(defun check-egl-error (&optional (prefix "EGL Error"))
  (let* ((err (egl:get-error))
	 (msg (case err
		(:success             nil)
		(:bad-alloc           "EGL_BAD_ALLOC")
		(:bad-config          "EGL_BAD_CONFIG")
		(:bad-context         "EGL_BAD_CONTEXT")
		(:bad-display         "EGL_BAD_DISPLAY")
		(:bad-match           "EGL_BAD_MATCH")
		(:bad-native-pixmap   "EGL_BAD_NATIVE_PIXMAP")
		(:bad-native-window   "EGL_BAD_NATIVE_WINDOW")
		(:bad-parameter       "EGL_BAD_PARAMETER")
		(:bad-surface         "EGL_BAD_SURFACE")
		(:bad-current-surface "EGL_BAD_CURRENT_SURFACE")
		(:not-initialized     "Not initialized")
		(t (format nil "TRAP: Unknown EGL error: ~a" err)))))
    (when msg (error (format nil "~a: ~a" prefix msg)))))
