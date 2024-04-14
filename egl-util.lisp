
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
   init-egl))
(in-package :smuks-egl-util)

(defvar context-attribs
  (list
   :context-major-version 3
   :context-minor-version 1
   :none))

;; NOTE: Stride is pitch. Eh.
(defun create-framebuffer (egl device)
  (let* ((width (width device))
	 (height (height device))
	 (buffer-object (gbm:bo-create (gbm-pointer device)
				       width height gbm::FORMAT_XRGB8888
				       (logior gbm::BO_USE_SCANOUT gbm::BO_USE_RENDERING)))
	 (handle (gbm:bo-get-handle buffer-object))
	 (stride (gbm:bo-get-stride buffer-object))
	 (offset 0) (bpp 32) (depth 24)
	 (frame-buffer (add-framebuffer (fd device) width height depth bpp stride handle))
	 ;; TODO: It's possible that the gl lib already has this extension defined. And that lib seems a bit more stable
	 (egl-image (egl:create-image-khr egl (cffi:null-pointer) egl::LINUX_DMA_BUF_EXT (cffi:null-pointer)
					  ;; TODO: In the rust thing this was an FD not a pointer
					  :dma-buf-plane-fd-ext (gbm:bo-get-fd buffer-object)
					  :width width :height height
					  :linux-drm-fourcc-ext gbm::FORMAT_XRGB8888
					  :dma-buf-plane0-pitch-ext stride
					  :dma-buf-plane0-offset-ext offset
					  :none)))
    (values frame-buffer egl-image)))


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
(defun init-egl (drm-dev)
  (egl:init-egl-wayland)
  (let* ((wayland-display-ptr (cffi:null-pointer))
	 (display (egl:get-display (gbm-pointer drm-dev)))
	 ;; TODO: Possibly i did not need to find a config for the fancy gbm buffers
	 ;; Check as you go along. Worst case revert a bit
	 (config (cffi:null-pointer)))
    ;; TODO: This one is problematic - since i don't exactly have the wl_display struct around here.
    ;; https://elixir.bootlin.com/mesa/mesa-19.0.6/source/docs/specs/WL_bind_wayland_display.spec
    (egl:bind-wayland-display display wayland-display-ptr)
    (egl:initialize display)
    (egl:bind-api :opengl-es-api)
    (let* ((context (apply 'egl:create-context `(,display ,config ,(cffi:null-pointer) ,@context-attribs))))
      (check-egl-error "Initializing egl context")
      (when (cffi:null-pointer-p context) (error "Failed to create context (was null pointer)"))
      (egl:make-current display (cffi:null-pointer) (cffi:null-pointer) context))
    (when (cffi:null-pointer-p (egl:get-current-context)) (error "Context not CURRENT (was null pointer)"))
    display))


;; ┌─┐┬─┐┬─┐┌─┐┬─┐  ┌─┐┬ ┬┌─┐┌─┐┬┌─┌─┐
;; ├┤ ├┬┘├┬┘│ │├┬┘  │  ├─┤├┤ │  ├┴┐└─┐
;; └─┘┴└─┴└─└─┘┴└─  └─┘┴ ┴└─┘└─┘┴ ┴└─┘
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
