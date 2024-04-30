
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; NOTE: SWC is a decent source of checking examples of what happens in a compositor
;; https://github.com/michaelforney/swc/blob/master/libswc/swc.c
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")
(defvar *socket* nil)
(defvar *wayland* nil)
(defvar *wl-event-fd* nil)
(defvar *wl-event-loop* nil)
(defvar *wl-poller* nil)

(defvar *smuks-exit* nil)
(defvar *drm-dev* nil)
(defvar *drm-poller* nil)
(defvar *main-vbo* nil)

(defvar *egl* nil)
(defvar *egl-context* nil)
(defvar *egl-image* nil)

(defvar *buffer-object* nil)
(defvar *frame-buffer* nil)
(defvar *gl-frame-buffer* nil)
(defvar *texture* nil)
(defvar *rect-shader* nil)
(defvar *texture-shader* nil)
(defvar *active-crtc* nil)

(defvar *client-poller* nil)

(defun kill-all-threads ()
  (mapcar (lambda (thread) (thread:destroy-thread thread)) (thread:all-threads)))

(defvar *test-program* "weston-terminal")
;; (defvar *test-program* "weston-flower")
;; (defvar *test-program* "kitty")

;; NOTE: Some very simple WL client you found here
;; https://github.com/emersion/hello-wayland/blob/master/main.c
;; (defvar *test-program* "hello-wayland")

(defun shutdown () (setf *smuks-exit* t))
(defun cleanup ()
  (when (and *egl* *egl-image*) (seglutil:destroy-image *egl* *egl-image*))
  (when *buffer-object* (sdrm:destroy-bo *buffer-object*))
  (when (and *drm-dev* *frame-buffer*) (sdrm:rm-framebuffer *drm-dev* *frame-buffer*))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* *wayland* *egl-context*))
  (when *drm-dev* (sdrm:close-drm *drm-dev*))

  (setfnil *egl* *egl-context* *egl-image* *drm-dev* *frame-buffer* *buffer-object* *smuks-exit* *active-crtc*))

(defun recursively-render-frame ()
  (if *smuks-exit*
      (cl-async:exit-event-loop)
      (progn
	(render-frame)
	;; (do-nothing)
	(cl-async:delay 'recursively-render-frame :time 0.016))))

(defun init-shaders ()
  ;; TODO: Is this main-vbo still used?
  (setf *main-vbo* (prep-gl-implementation *drm-dev* *frame-buffer*))
  (setf *rect-shader* (shader-init:create-rect-shader *drm-dev*))
  (setf *texture-shader* (shader-init:create-texture-shader *drm-dev*)))

(defun init-globals ()
  ;; TODO: Also iterate and generate globals for outputs here
  ;; TODO: When you recompile the compiled classes - these globals aren't updated
  ;; needing a rerun
  (make-instance 'wl-compositor:global :display *wayland* :dispatch-impl 'compositor)
  (make-instance 'wl-subcompositor:global :display *wayland*)
  (make-instance 'wl-shm:global :display *wayland* :dispatch-impl 'shm)
  (make-instance 'wl-seat:global :display *wayland*)
  (make-instance 'wl-data-device-manager:global :display *wayland*)
  (make-instance 'xdg-wm-base:global :display *wayland* :dispatch-impl 'wm-base))

(defun mainer ()
  (setf *log-output* *standard-output*)
  (heading)

  (setf (uiop/os:getenv "WAYLAND_DEBUG") "1")
  (setf *socket* (init-socket))

  ;; TODO: Can sometimes fail on retrying
  (setf *drm-dev* (init-drm))

  (wl:init-interface-definitions)
  (setf *wayland* (wl:display-create))
  (wl:display-add-socket-fd *wayland* (unix-sockets::fd *socket*))

  (setf *wl-event-loop* (wl:display-get-event-loop *wayland*))
  (setf *wl-event-fd* (wl:event-loop-get-fd *wl-event-loop*))
  (init-globals)

  (setf (values *egl* *egl-context*) (init-egl *drm-dev* *wayland*))
  (setf (values *frame-buffer* *egl-image* *buffer-object*) (create-framebuffer *egl* *drm-dev*))
  (setf (values *gl-frame-buffer* *texture*) (create-gl-framebuffer *egl-image*))

  (init-shaders)

  (unless *active-crtc* (setf *active-crtc* (set-crtc *drm-dev* *frame-buffer*)))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") *socket-file*)

  (cl-async:start-event-loop
   (lambda ()
     (log! "Starting DRM fd listener. Waiting for events...~%")
     (setf *drm-poller* (drm-listener))
     (log! "Starting wayland client socket listener. Waiting for clients...~%")
     (setf *client-poller* (client-listener))
     (log! "Starting wayland event loop listener. Waiting for events...~%")
     (setf *wl-poller* (wayland-listener))

     (test-app "weston-simple-shm")

     (recursively-render-frame))))

(defun main ()
  (restart-case
      (unwind-protect (mainer)
	(cleanup))
    (📍start-over () (main))))

;; ┌─┐┬─┐┌─┐┌┬┐┌─┐
;; ├┤ ├┬┘├─┤│││├┤
;; └  ┴└─┴ ┴┴ ┴└─┘

;; egl.shaders.texture_abgr.as_mut().expect("Texture program not found")
;; .draw(
      ;; texture_id, render_props.surface_position,
      ;; (width, height),
      ;; render_props.transform)

(defun render-surface (surface)
  (let ((texture (texture surface)))
    (shaders.texture:draw *texture-shader* texture
			  ;; (surface-dimensions surface))))
			  '(0 0 840 600))))

(defun render-clients ()
  (let* ((clients (alexandria:hash-table-values wl:*client-tracker*))
	 (surfaces (mapcar (lambda (object) (typep object 'surface)) (wl:objects clients))))
    (mapcar (lambda (surface) (render-surface surface)) surfaces)))


(defun do-nothing ()
  (livesupport:update-repl-link)
  (wl:display-flush-clients *wayland*))

(defun render-frame ()
  (livesupport:update-repl-link)
  (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
  (gl:clear :color-buffer-bit)

  (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					    :x 10.0 :y 300.0 :w 100.0 :h 40.0
					    :color '(0.2 0.2 0.2 1.0))))

  (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					    :x 30.0 :y 500.0 :w 200.0 :h 50.0
					    :color '(0.2 0.9 0.2 1.0))))

  ;; (render-clients)
  (gl:flush)
  (gl:finish)
  (wl:display-flush-clients *wayland*))

;; ┬  ┬┌─┐┌┬┐┌─┐┌┐┌┌─┐┬─┐┌─┐
;; │  │└─┐ │ ├┤ │││├┤ ├┬┘└─┐
;; ┴─┘┴└─┘ ┴ └─┘┘└┘└─┘┴└─└─┘
(defun wayland-listener () (cl-async:poll *wl-event-fd* 'wayland-callback :poll-for '(:readable)))
(defun client-listener () (cl-async:poll (unix-sockets::fd *socket*) 'client-callback :poll-for '(:readable) :socket t))
(defun drm-listener () (cl-async:poll (fd *drm-dev*) 'drm-callback :poll-for '(:readable)))

(defun handle-wayland-event ()
  (let ((result (wl:event-loop-dispatch *wl-event-loop* 0)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))

(defun wayland-callback (events) (when (member :readable events) (handle-wayland-event)))


(defun drm-callback (events)
  (when (member :readable events)
    (drm:handle-event (fd *drm-dev*) :page-flip 'page-flip)))

(defun page-flip () (drm-page-flip *drm-dev* *frame-buffer*))

(defun client-callback (events)
  (unless (member :readable events) (error "Client callback called without readable event"))
  (let ((client (unix-sockets:accept-unix-socket *socket*)))
    (wl:create-client *wayland* (unix-sockets::fd client))))

;; ┌─┐┌─┐┌─┐┬┌─┌─┐┌┬┐
;; └─┐│ ││  ├┴┐├┤  │
;; └─┘└─┘└─┘┴ ┴└─┘ ┴
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

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun test-app (app-name)
  (bt:make-thread
   (lambda ()
     (log! "🟢 ~a: Starting an app~%" app-name)
     (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
       (loop while (uiop/launch-program:process-alive-p process)
	     do (log! "🔴 ~a: ~a~%" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (log! "🟢 ~a: Client exit. Code: ~a~%" app-name (uiop:wait-process process))))))


;; NOTE: This is a fix for cl-async not having a handler for gracious :poll closing
;; Weirdly enough - i expected them to crash, but they don't. They just hang.
(defmethod cl-async::handle-cleanup ((handle-type (eql :poll)) handle)
  (cond
    ((cffi:pointer-eq (cl-async::poller-c *drm-poller*) handle) (cl-async:free-poller *drm-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *wl-poller*) handle) (cl-async:free-poller *wl-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *client-poller*) handle) (cl-async:free-poller *client-poller*))
    (t (error "Unknown poller handle"))))
