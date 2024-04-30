
;; ███████╗ ██████╗ ██╗
;; ██╔════╝██╔════╝ ██║
;; █████╗  ██║  ███╗██║
;; ██╔══╝  ██║   ██║██║
;; ███████╗╚██████╔╝███████╗
;; ╚══════╝ ╚═════╝ ╚══════╝
(defpackage :smuks-egl-util
  (:use :cl :sdrm)
  (:nicknames :seglutil)
  (:export
   check-egl-error
   create-framebuffer
   destroy-image
   init-egl
   cleanup-egl))
(in-package :smuks-egl-util)

(defvar context-attribs
  (list
   :context-major-version 3
   :context-minor-version 1
   :none))

;; NOTE: Stride is pitch. Eh.
;; TODO: Might make more sense to pass in a buffer-object rather than create it here
;; TODO: Framebuffer also doesn't make much sense here. It has nothing to do with egl at this point
(defun create-framebuffer (egl device)
  (let* ((width (width device))
	 (height (height device))
	 (buffer-object (sdrm:create-bo device))
	 (handle (gbm:bo-get-handle buffer-object))
	 (stride (gbm:bo-get-stride buffer-object))
	 (offset 0) (bpp 32) (depth 24)
	 (framebuffer (add-framebuffer (fd device) width height depth bpp stride handle))
	 ;; TODO: It's possible that the gl lib already has this extension defined. And that lib seems a bit more stable
	 (egl-image (egl:create-image-khr egl (cffi:null-pointer) egl::LINUX_DMA_BUF_EXT (cffi:null-pointer)
					  ;; TODO: In the rust thing this was an FD not a pointer
					  :dma-buf-plane-fd-ext (gbm:bo-get-fd buffer-object)
					  :width width :height height
					  :linux-drm-fourcc-ext gbm::FORMAT_XRGB8888
					  :dma-buf-plane0-pitch-ext stride
					  :dma-buf-plane0-offset-ext offset
					  :none)))
    (values framebuffer egl-image buffer-object)))



;; NOTE: libwayland egl code
;; https://gitlab.freedesktop.org/wayland/wayland/-/tree/main/egl?ref_type=heads
;; NOTE: Nvidia eglstream code for binding egl to wayland
;; https://github.com/NVIDIA/egl-wayland/blob/master/src/wayland-egldisplay.c#L82

;; NOTE: Mesa egl code for binding egl to wayland
;; https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/egl/main/eglapi.c#L2311
;; And what seems to be main function for it:
;; https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/egl/drivers/dri2/egl_dri2.c#L3156
;; Here the display gets used further inside of more wayland specific code:
;; https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/egl/wayland/wayland-drm/wayland-drm.c#L251

;; Global create (wl_global_create)
;; https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-server.c#L1299

;; One example where DRM calls wl_drm_authenticate which could be found in wayland-drm.c
;; https://gitlab.freedesktop.org/mesa/mesa/-/blob/main/src/egl/drivers/dri2/platform_wayland.c#L1662

;; NOTE: Libwayland display create code:
;; https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-server.c#L1132
;; And the wl_display struct:
;; https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-server.c#L92
;; TODO: If this fails - it is very likely that it is because i do not have a wayland-display-ptr

;; TODO: Check if you can just malloc the display and pass it to eglGetDisplay
;; So far from the mesa code - i don't see any of the struct fields being directly accessed
(defun init-egl (drm-dev wl-display)
  (egl:load-egl-extensions)
  (check-egl-error "Binding extensions")
  (let* ((display (egl:get-display (gbm-pointer drm-dev)))
	 ;; TODO: Possibly i did not need to find a config for the fancy gbm buffers
	 ;; Check as you go along.
	 (config (cffi:null-pointer)))
    (egl:initialize display)
    (check-egl-error "Initializing display")
    (egl:bind-wl-display display wl-display)
    (check-egl-error "Binding wayland")
    (egl:bind-api :opengl-es-api)
    (check-egl-error "Binding api")
    (let* ((context (apply 'egl:create-context `(,display ,config ,(cffi:null-pointer) ,@context-attribs))))
      (check-egl-error "Initializing egl context")
      (when (cffi:null-pointer-p context) (error "Failed to create context (was null pointer)"))
      (egl:make-current display (cffi:null-pointer) (cffi:null-pointer) context)
      (when (cffi:null-pointer-p (egl:get-current-context)) (error "Context not CURRENT (was null pointer)"))
      (values display context))))

(defun destroy-image (egl image) (egl:destroy-image egl image))

(defun cleanup-egl (egl wl context)
  (egl:unbind-wl-display egl wl)
  (egl:make-current egl (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer))
  (egl:destroy-context egl context)
  (egl:terminate egl))

;; ┌─┐┬─┐┬─┐┌─┐┬─┐  ┌─┐┬ ┬┌─┐┌─┐┬┌─┌─┐
;; ├┤ ├┬┘├┬┘│ │├┬┘  │  ├─┤├┤ │  ├┴┐└─┐
;; └─┘┴└─┴└─└─┘┴└─  └─┘┴ ┴└─┘└─┘┴ ┴└─┘
(defun check-egl-error (&optional (prefix "EGL Error"))
  (let* ((err (egl:get-error))
	 (msg (case err
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
	       (:not-initialized "Not initialized")
	       (t (format nil "TRAP: Unknown EGL error: ~a" err)))))
    (when msg (error (format nil "~a: ~a" prefix msg)))))
