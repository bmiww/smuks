
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")
(defvar *socket* nil)
(defvar *wayland* nil)
(defvar *smuks-exit* nil)
(defvar *drm-dev* nil)
(defvar *drm-thread* nil)
(defvar *egl* nil)
(defvar *main-vbo* nil)
(defvar *egl-image* nil)
(defvar *frame-buffer* nil)
(defvar *gl-frame-buffer* nil)
(defvar *texture* nil)
(defvar *shaders* nil)
(defvar *active-crtc* nil)

(defvar *client-thread* nil)

(defun kill-all-threads ()
  (mapcar (lambda (thread) (thread:destroy-thread thread)) (thread:all-threads)))

(defvar *test-program* "weston-terminal")
;; (defvar *test-program* "weston-flower")
;; (defvar *test-program* "kitty")

;; NOTE: Some very simple WL client you found here
;; https://github.com/emersion/hello-wayland/blob/master/main.c
;; (defvar *test-program* "hello-wayland")

(defun heading ()
  (format t "~%")
  (format t "███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗~%")
  (format t "██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝~%")
  (format t "███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗~%")
  (format t "╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║~%")
  (format t "███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║~%")
  (format t "╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝~%")
  (format t "~%"))

(defun shutdown () (setf *smuks-exit* t) (cleanup))
(defun cleanup ()
  ;; TODO: This kills off the client listener rather ungracefully
  (when *client-thread* (bt:destroy-thread *client-thread*) (setf *client-thread* nil))
  (when (and *drm-thread* (bt:thread-alive-p *drm-thread*)) (bt:destroy-thread *drm-thread*) (setf *drm-thread* nil))
  (when *active-crtc* (free-crtc *drm-dev*) (setf *active-crtc* nil))
  (when *drm-dev* (close-drm *drm-dev*) (setf *drm-dev* nil)))

  ;; TODO: Add cleanup/restarts for crtc grabs
(defun main ()
  (setf *log-output* *standard-output*)
  (setf *smuks-exit* nil)
  (heading)
  (cleanup)

  ;; NOTE: Maybe setup kill signals for the process
  ;; TODO: Maybe add a "restart" to set *smuks-exit* to true
  ;; (mapcar (lambda (signal) (sb-sys:enable-interrupt signal (lambda () (setf *smuks-exit* t)))) '(SIGINT SIGTERM))

  (smuks-wl:reset-globals)

  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'smuks-wl:wayland))
  (setf *drm-dev* (init-drm))
  (setf *egl* (init-egl *drm-dev*))

  (setf (values *frame-buffer* *egl-image*) (create-framebuffer *drm-dev*))
  (setf (values *gl-frame-buffer* *texture*) (create-gl-framebuffer *egl-image*))

  (setf *main-vbo* (init-instanced-verts))
  (setf (values *main-vbo* *shaders*) (prep-gl-implementation *drm-dev*))

  (setf *active-crtc* (set-crtc *drm-dev* *frame-buffer*))

  ;; TODO: For now disabling since it seems to be locking up other threads from erroring out for some reason...
  (log! "Starting DRM fd listener. Waiting for events...~%")
  (setf *drm-thread* (bt:make-thread 'drm-listener))

  (log! "Starting wayland socket listener. Waiting for clients...~%")
  (setf *client-thread* (bt:make-thread 'client-listener))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") *socket-file*)

  ;; (test-app *test-program*)

  (livesupport:continuable
    (loop while (not *smuks-exit*)
	  do (render-frame))))

;; TODO: Kernel docus on possible DRM debug funcs
;; https://www.kernel.org/doc/html/v6.8/gpu/drm-internals.html?highlight=page+flip
;; This might be for driver development
;; TODO: When reevaluating code - starts to die with -9:EBADF
(defun drm-page-flip (drm-dev framebuffer)
  (let ((result (drm::mode-page-flip
		 (fd drm-dev)
		 (drm::crtc!-id (crtc drm-dev))
		 framebuffer
		 :page-flip-event
		 (cffi:null-pointer))))
    (if (zerop result)
	(log! "Page flip OK!~%")
	(log! "Page flip:: ~a:~a~%" (- result) (case (- result)
				  ;; TODO: These might all be wrong. I'm assuming based on this (page flip doesn't neccesarily use generic error codes):
				  ;; https://community.silabs.com/s/article/Linux-kernel-error-codes?language=en_US
				  (9  "EBADF - Bad file descriptor number")
				  (13 "EACCESS - Permission denied")
				  (25 "ENOTTY - Not a typewriter")
				  (t (format nil "UNKNOWN ERROR CODE - ~a" result)))))))

(defun render-frame ()
  (livesupport:update-repl-link)
  (sleep 1)
  (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
  (gl:clear :color-buffer-bit)
  (drm-page-flip *drm-dev* *frame-buffer*))

(defun prep-gl-implementation (drm-device)
  (gl:bind-framebuffer :framebuffer *frame-buffer*)
  (let* ((main-vbo (init-instanced-verts))
	 (shaders "NOT IMPLEMENTED"))
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:viewport 0 0 (width drm-device) (height drm-device))

    (values main-vbo shaders)))

(defun process-drm-message (buffer)
  (let ((count (sb-unix:unix-read (fd *drm-dev*) buffer 1024)))
    (when count
	       ;; (loop for i from 0 below count
		     ;; do (log! "~a" (cffi:mem-ref buffer :uint8 i)))
      ;; (log! "~%"))
      )))

;; TODO: Unfinished. Still in debug mode
(defun drm-listener ()
  (let ((buffer (cffi:foreign-alloc :uint8 :count 1024)))
    (loop while (not *smuks-exit*)
	  do (process-drm-message buffer))))

(defun client-listener ()
  (loop until *smuks-exit*
	do (let* ((client (unix-sockets:accept-unix-socket *socket*)))
	     (smuks-wl:add-client *wayland* client))))

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


;; NOTE: Stride is pitch. Eh.
(defun create-framebuffer (device)
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
	 (egl-image (egl:create-image-khr *egl* (cffi:null-pointer) egl::LINUX_DMA_BUF_EXT (cffi:null-pointer)
					  ;; TODO: In the rust thing this was an FD not a pointer
					  :dma-buf-plane-fd-ext (gbm:bo-get-fd buffer-object)
					  :width width :height height
					  :linux-drm-fourcc-ext gbm::FORMAT_XRGB8888
					  :dma-buf-plane0-pitch-ext stride
					  :dma-buf-plane0-offset-ext offset
					  :none)))
    (values frame-buffer egl-image)))


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


;; ███████╗ ██████╗  ██████╗██╗  ██╗███████╗████████╗
;; ██╔════╝██╔═══██╗██╔════╝██║ ██╔╝██╔════╝╚══██╔══╝
;; ███████╗██║   ██║██║     █████╔╝ █████╗     ██║
;; ╚════██║██║   ██║██║     ██╔═██╗ ██╔══╝     ██║
;; ███████║╚██████╔╝╚██████╗██║  ██╗███████╗   ██║
;; ╚══════╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝╚══════╝   ╚═╝

(defun init-socket ()
  (restart-case
      (if (probe-file *socket-file*)
	  (error "Socket file already exists")
	  (unix-sockets:make-unix-socket *socket-file*))
    (create-new-socket ()
      :report "Create new socket"
      (log! "Creating new socket~%")
      (delete-file *socket-file*)
      (unix-sockets:make-unix-socket *socket-file*))))


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

(defvar context-attribs
  (list
   :context-major-version 3
   :context-minor-version 1
   :none))


;;  ██████╗ ██╗     ███████╗██╗    ██╗
;; ██╔════╝ ██║     ██╔════╝██║    ██║
;; ██║  ███╗██║     █████╗  ██║ █╗ ██║
;; ██║   ██║██║     ██╔══╝  ██║███╗██║
;; ╚██████╔╝███████╗██║     ╚███╔███╔╝
;;  ╚═════╝ ╚══════╝╚═╝      ╚══╝╚══╝
;; TODO: Kind of forgot about this one. Maybe get back to it if you don't have the tablet around
;; Or if you start getting into working on the desktop version

;; (defun main-glfw ()
  ;; (print "doooo")
  ;; (glfw:init)
  ;; (print "nop")
  ;; (unwind-protect
       ;; (print "yep")
       ;; (let ((window (make-instance 'window :width 800 :height 600 :title "Hello wayland")))
	 ;; (init-socket window)

         ;; (loop until (glfw:should-close-p window)
               ;; do (print "hurpa")
		  ;; (glfw:poll-events)
		  ;; (glfw:swap-buffers window)))
    ;; (glfw:shutdown)))



;; (defclass window (glfw:window)
  ;; ((socket :initform :socket :accessor socket)))

;; (defmethod glfw:window-resized ((window window) width height)
  ;; ;; (call-next-method)
  ;; (gl:viewport 0 0 width height))

;; (defmethod glfw:key-changed ((window window) key scancode action mods)
  ;; (case key
    ;; ((:escape) (setf (glfw:should-close-p window) t))))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘

(defun test-app (app-name)
  (bt:make-thread
   (lambda ()
     (sleep 1)
     (log! "🟢 ~a: Starting an app~%" app-name)
     (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
       (loop while (uiop/launch-program:process-alive-p process)
	     do (log! "🔴 ~a: ~a~%" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (log! "🟢 ~a: Client exit. Code: ~a~%" app-name (uiop:wait-process process))))))
