
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; NOTE: SWC is a decent source of checking examples of what happens in a compositor
;; https://github.com/michaelforney/swc/blob/master/libswc/swc.c
(in-package :smuks)

(defvar +socket-file+ "/tmp/smuks.socket")

(defvar *socket* nil)
(defvar *wayland* nil)

(defvar *smuks-exit* nil)
(defvar *drm-dev* nil)
(defvar *main-vbo* nil)

(defvar *egl* nil)
(defvar *egl-context* nil)
(defvar *egl-image* nil)

(defvar *buffer-object* nil)
(defvar *frame-buffer* nil)
(defvar *gl-frame-buffer* nil)
(defvar *texture* nil)
(defvar *active-crtc* nil)

(defvar *rect-shader* nil)
(defvar *texture-shader* nil)

(defvar *client-poller* nil)
(defvar *wl-poller* nil)
(defvar *drm-poller* nil)

(defun shutdown () (setf *smuks-exit* t))
(defun cleanup ()
  (when (and *egl* *egl-image*) (seglutil:destroy-image *egl* *egl-image*))
  (when *buffer-object* (sdrm:destroy-bo *buffer-object*))
  (when (and *drm-dev* *frame-buffer*) (sdrm:rm-framebuffer *drm-dev* *frame-buffer*))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* (wl:display-ptr *wayland*) *egl-context*))
  (when *drm-dev* (sdrm:close-drm *drm-dev*))

  (when *wayland* (wl:destroy *wayland*))

  (when *socket*
    (unix-sockets:close-unix-socket *socket*)
    (delete-file +socket-file+))

  (setfnil *egl* *egl-context* *egl-image* *drm-dev* *frame-buffer* *buffer-object* *smuks-exit* *active-crtc*
	   *wayland* *socket*))

(defun recursively-render-frame ()
  (if *smuks-exit*
      (cl-async:exit-event-loop)
      (progn
	(render-frame)
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
  (setf *frame-ready* t)
  (heading)

  (setf (uiop/os:getenv "WAYLAND_DEBUG") "")
  (setf *socket* (init-socket))

  ;; TODO: Can sometimes fail on retrying
  (setf *drm-dev* (init-drm))

  (wl:init-interface-definitions)
  (setf *wayland* (make-instance 'wl:display :fd (unix-sockets::fd *socket*)))
  (init-globals)

  (setf (values *egl* *egl-context*) (init-egl *drm-dev* (wl:display-ptr *wayland*)))
  (setf (values *frame-buffer* *egl-image* *buffer-object*) (create-framebuffer *egl* *drm-dev*))
  (setf (values *gl-frame-buffer* *texture*) (create-gl-framebuffer *egl-image*))

  (init-shaders)

  (unless *active-crtc* (setf *active-crtc* (set-crtc *drm-dev* *frame-buffer*)))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") +socket-file+)

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
(defun render-surface (surface)
  (let ((texture (texture surface)))
    (shaders.texture:draw *texture-shader* texture
			  '(0.0 0.0 200.0 200.0))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))

(defun render-clients ()
  (let* ((clients (wl:all-clients *wayland*))
	 (compositors (remove-if-not 'identity (mapcar 'compositor clients)))
	 (surfaces (util:flatten (mapcar 'all-ready-surfaces compositors))))
    (mapcar (lambda (surface) (render-surface surface)) surfaces)))

(defvar *y-pos* 800.0)
(defvar y-up t)
(defun next-y-pos ()
  (when (> *y-pos* 1000.0)
    (setf y-up nil))
  (when (< *y-pos* 800.0)
    (setf y-up t))
  (incf *y-pos* (if y-up 1 -1)))

(defvar *frame-ready* t)
(defun render-frame ()
  (livesupport:update-repl-link)
  (when *frame-ready*
    (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
    (gl:clear :color-buffer-bit)

    (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					      :x 10.0 :y (next-y-pos) :w 100.0 :h 40.0
					      :color '(0.2 0.2 0.2 1.0))))

    (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					      :x 30.0 :y 700.0 :w 200.0 :h 50.0
					      :color '(0.2 0.9 0.2 1.0))))

    (render-clients)
    (gl:flush)
    (gl:finish)

    (setf *frame-ready* nil))

  ;; TODO: Wasteful - also - didn't really help much at the moment.
  ;; Try to bring it back inside the *frame-ready* check
  (handler-case
      (drm-page-flip *drm-dev* *frame-buffer*)
    (error (err) ()))

  ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
  ;; Any events or changes that i could instead attach to?
  ;; Maybe instead use per client flushes - for example when receiving commit from them
  (wl:flush-clients *wayland*))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client (wl:client)
  ((compositor :initform nil)))

(defmethod compositor ((client client)) (slot-value client 'compositor))
(defmethod (setf compositor) (compositor (client client))
  (setf (slot-value client 'compositor) compositor))

(defmethod (setf wl::iface) :after (iface (client client) interface)
  (when (typep iface 'compositor) (setf (compositor client) iface)))

;; ┬  ┬┌─┐┌┬┐┌─┐┌┐┌┌─┐┬─┐┌─┐
;; │  │└─┐ │ ├┤ │││├┤ ├┬┘└─┐
;; ┴─┘┴└─┘ ┴ └─┘┘└┘└─┘┴└─└─┘
(defun wayland-listener () (cl-async:poll (wl:event-loop-fd *wayland*) 'wayland-callback :poll-for '(:readable)))
(defun client-listener () (cl-async:poll (unix-sockets::fd *socket*) 'client-callback :poll-for '(:readable) :socket t))
(defun drm-listener () (cl-async:poll (fd *drm-dev*) 'drm-callback :poll-for '(:readable)))

(defun handle-wayland-event ()
  (let ((result (wl:dispatch-event-loop *wayland*)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))

(defun wayland-callback (events)
  (when (member :readable events) (handle-wayland-event)))

(defun set-frame-ready (a b c d e)
  (setf *frame-ready* t))

(defun drm-callback (events)
  (when (member :readable events)
    (drm:handle-event (fd *drm-dev*) :page-flip 'set-frame-ready)))

(defun client-callback (events)
  (unless (member :readable events) (error "Client callback called without readable event"))
  (let ((client (unix-sockets:accept-unix-socket *socket*)))
    (wl:create-client *wayland* (unix-sockets::fd client) :class 'client)))

;; ┌─┐┌─┐┌─┐┬┌─┌─┐┌┬┐
;; └─┐│ ││  ├┴┐├┤  │
;; └─┘└─┘└─┘┴ ┴└─┘ ┴
(defun init-socket ()
  (restart-case
      (if (probe-file +socket-file+)
	  (error "Socket file already exists")
	  (unix-sockets:make-unix-socket +socket-file+))
    (create-new-socket ()
      :report "Create new socket"
      (log! "Creating new socket~%")
      (delete-file +socket-file+)
      (unix-sockets:make-unix-socket +socket-file+))))

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
