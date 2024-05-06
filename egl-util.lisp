
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
    (egl:initialize display)                 (check-egl-error "Initializing display")
    (egl:bind-wl-display display wl-display) (check-egl-error "Binding wayland")
    (egl:bind-api :opengl-es-api)            (check-egl-error "Binding api")

    (let* ((context (apply 'egl:create-context
			   `(,display ,(cffi:null-pointer) ,(cffi:null-pointer) ,@context-attribs))))
      (check-egl-error "Initializing egl context")

      (when (cffi:null-pointer-p context) (error "Failed to create context (got null pointer)"))
      (egl:make-current display (cffi:null-pointer) (cffi:null-pointer) context)
      (when (cffi:null-pointer-p (egl:get-current-context)) (error "Context not CURRENT (got null pointer)"))

      (values display context))))

(defun cleanup-egl (egl wl context)
  (egl:unbind-wl-display egl wl)
  (egl:make-current egl (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))
  (egl:destroy-context egl context)
  (egl:terminate egl))


;; ┬┌┬┐┌─┐┌─┐┌─┐
;; ││││├─┤│ ┬├┤
;; ┴┴ ┴┴ ┴└─┘└─┘
;; NOTE: Stride is pitch. Eh.
(defun create-egl-image (egl buffer-object)
  (let* ((stride (gbm:bo-get-stride buffer-object))
	 (offset 0) (bpp 32) (depth 24))
	 ;; TODO: Maybe the gl lib already has this extension defined. That lib seems a bit more polished
    (egl:create-image-khr egl (cffi:null-pointer) egl::LINUX_DMA_BUF_EXT (cffi:null-pointer)
			  ;; TODO: In the rust thing this was an FD not a pointer
			  :dma-buf-plane-fd-ext (gbm:bo-get-fd buffer-object)
			  :width width :height height
			  :linux-drm-fourcc-ext gbm::FORMAT_XRGB8888
			  :dma-buf-plane0-pitch-ext stride
			  :dma-buf-plane0-offset-ext offset
			  :none)))

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
