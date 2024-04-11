
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
(defvar *egl* nil)
(defvar *main-vbo* nil)
(defvar *egl-image* nil)
(defvar *frame-buffer* nil)

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

(defun main ()
  (setf *log-output* *standard-output*)
  (heading)
  (setf *smuks-exit* nil)
  ;; TODO: This kills off the client listener rather ungracefully
  (when *client-thread* (bt:destroy-thread *client-thread*) (setf *client-thread* nil))

  ;; NOTE: Maybe setup kill signals for the process
  ;; TODO: Maybe add a "restart" to set *smuks-exit* to true
  ;; (mapcar (lambda (signal) (sb-sys:enable-interrupt signal (lambda () (setf *smuks-exit* t)))) '(SIGINT SIGTERM))

  (smuks-wl:reset-globals)

  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'smuks-wl:wayland))
  (setf *drm-dev* (init-drm))
  (setf *egl* (init-egl *drm-dev*))

  (setf *main-vbo* (init-instanced-verts))

  (setf (values *frame-buffer* *egl-image*) (create-framebuffer *drm-dev*))

  (setf *client-thread*
	(bt:make-thread
	 (lambda ()
	   (log! "Starting wayland socket listener~%")
	   (loop until *smuks-exit*
		 do (let* ((client (unix-sockets:accept-unix-socket *socket*)))
		      (smuks-wl:add-client *wayland* client))))))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") *socket-file*)

  (test-app *test-program*)

  (livesupport:continuable
    (loop while t
	  do (livesupport:update-repl-link)
	     (magic))))

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
	 ;; TODO: Using the global *frame-buffer* here, since i'm too lazy to clean up
	 ;; And trying to add the framebuffer more than once ends up corrupting the image
	 (frame-buffer (or *frame-buffer* (add-framebuffer (fd device) width height depth bpp stride handle)))
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
    (gl:bind-buffer :array-buffer 0)))

(defun magic ()
  (sleep 1))

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
      (egl:make-current display (cffi:null-pointer) (cffi:null-pointer) context))
    display))

(defvar context-attribs
  (list
   :context-major-version 3
   :context-minor-version 2
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
