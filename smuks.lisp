
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
(defvar *device-poller* nil)
(defvar *input-poller* nil)
(defvar *seat-poller* nil)

(defvar *seat* nil)

(defvar *frame-ready* t)
(defvar *test-app* nil)

(defun shutdown () (setf *smuks-exit* t))
(defun cleanup ()
  (when (and *egl* *egl-image*) (seglutil:destroy-image *egl* *egl-image*))
  (when *buffer-object* (sdrm:destroy-bo *buffer-object*))
  (when (and *drm-dev* *frame-buffer*) (sdrm:rm-framebuffer *drm-dev* *frame-buffer*))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* (wl:display-ptr *wayland*) *egl-context*))
  (when *drm-dev* (sdrm:close-drm *drm-dev*))

  (when *wayland* (wl:destroy *wayland*))

  (when *seat* (libseat:close-seat *seat*))

  (when *socket*
    (unix-sockets:close-unix-socket *socket*)
    (delete-file +socket-file+))

  (setfnil *egl* *egl-context* *egl-image* *drm-dev* *frame-buffer* *buffer-object* *smuks-exit* *active-crtc*
	   *wayland* *socket* *seat*))

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
  ;; TODO: When you recompile the compiled classes - these globals aren't updated, needing a rerun
  (make-instance 'wl-compositor:global :display *wayland* :dispatch-impl 'compositor)
  (make-instance 'wl-subcompositor:global :display *wayland*)
  (make-instance 'shm-global :display *wayland* :dispatch-impl 'shm)
  (make-instance 'seat-global :display *wayland* :dispatch-impl 'seat)
  (make-instance 'wl-data-device-manager:global :display *wayland* :dispatch-impl 'dd-manager)
  (make-instance 'xdg-wm-base:global :display *wayland* :dispatch-impl 'wm-base))

(defun mainer ()
  (setf *log-output* *standard-output*)
  (setf *frame-ready* t)
  (heading)

  ;; NOTE: This is at minimum needed to have the smuks process in ssh connections to get access
  ;; To input devices.
  ;; It also requires a custom built libseat
  ;; It should work by default if not ssh'ing
  ;; But i should find a better way to do this
  (setf (uiop/os:getenv "XDG_SESSION_ID_OVERRIDE") "1")

  (setf (uiop/os:getenv "WAYLAND_DEBUG") "")
  (setf *socket* (init-socket))

  ;; (libseat:set-log-level :debug)
  (setf *seat* (libseat:open-seat :enable-seat 'enable-seat :disable-seat 'disable-seat :log-handler t))

  (cl-async:start-event-loop (lambda () (setf *seat-poller* (seat-listener))))
  ;; Dispatching seat events immediately - the first event should always be an enable
  ;; After this call - we should be able to have access to device files
  ;; (libseat:dispatch *seat* 0)


  ;; TODO: Can sometimes fail on retrying
  (setf *drm-dev* (init-drm))

  (wl:init-interface-definitions)
  (setf *wayland* (make-instance 'display
		     :fd (unix-sockets::fd *socket*)
		     :libinput (make-instance 'dev-track :open-device 'open-device :close-device 'close-device)))

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
     (log! "Starting event node watch poller. Waiting for device changes...~%")
     (setf *device-poller* (notify-listener))
     (log! "Starting input event poller. Waiting for user inputs...~%")
     (setf *input-poller* (input-listener))
     (log! "Starting the umpeenth poller. Now for seat events...~%")
     (setf *seat-poller* (seat-listener))

     (setf *test-app* (test-app "weston-simple-shm"))

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

(defvar *green-x-pos* 30.0)
(defvar *green-y-pos* 700.0)

(defun render-frame ()
  (livesupport:update-repl-link)
  (when *frame-ready*
    (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
    (gl:clear :color-buffer-bit)

    (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					      :x 10.0 :y (next-y-pos) :w 100.0 :h 40.0
					      :color '(0.2 0.2 0.2 1.0))))

    (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					      :x *green-x-pos* :y *green-y-pos* :w 200.0 :h 50.0
					      :color '(0.2 0.9 0.2 1.0))))

    (render-clients)
    (gl:flush)
    (gl:finish)

    (setf *frame-ready* nil))

  ;; TODO: Wasteful - also - didn't really help much at the moment.
  ;; Try to bring it back inside the *frame-ready* check
  (handler-case
      (drm-page-flip *drm-dev* *frame-buffer*)
    (error (err) (declare (ignore err)) ()))

  ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
  ;; Any events or changes that i could instead attach to?
  ;; Maybe instead use per client flushes - for example when receiving commit from them
  (wl:flush-clients *wayland*))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client (wl:client)
  ((compositor :initform nil :accessor compositor)
   (seat :initform nil :accessor seats)))

(defmethod (setf wl::iface) :after (iface (client client) id)
  (declare (ignore id))
  (when (typep iface 'compositor) (setf (compositor client) iface)))

(defmethod (setf wl::iface) :after ((iface seat) (client client) id)
  (declare (ignore id)) (setf (seat client) iface))


;; ┌─┐┌─┐┬  ┬  ┬┌┐┌┌─┐
;; ├─┘│ ││  │  │││││ ┬
;; ┴  └─┘┴─┘┴─┘┴┘└┘└─┘
;; Listeners
(defun wayland-listener () (cl-async:poll (wl:event-loop-fd *wayland*) 'wayland-callback :poll-for '(:readable)))
(defun client-listener () (cl-async:poll (unix-sockets::fd *socket*) 'client-callback :poll-for '(:readable) :socket t))
(defun drm-listener () (cl-async:poll (fd *drm-dev*) 'drm-callback :poll-for '(:readable)))
(defun input-listener () (cl-async:poll (context-fd (libinput *wayland*)) 'input-callback :poll-for '(:readable)))
(defun seat-listener () (cl-async:poll (libseat:get-fd *seat*) 'seat-callback :poll-for '(:readable)))
(defun notify-listener ()
  (notify::init)
  (notify:watch "/dev/input/" :events '(:create :delete))
  (cl-async:poll notify::*fd* 'notify-callback :poll-for '(:readable)))

;; Slightly annoying callbacks
(defun ready (ev) (member :readable ev))
(defun wayland-callback (ev) (when (ready ev) (handle-wayland-event)))
(defun input-callback (ev) (when (ready ev) (smuks::dispatch (libinput *wayland*) 'handle-input)))
(defun notify-callback (ev) (when (ready ev) (notify::process 'device-change)))
(defun drm-callback (ev) (when (ready ev) (drm:handle-event (fd *drm-dev*) :page-flip 'set-frame-ready)))
(defun seat-callback (ev) (when (ready ev) (libseat:dispatch *seat* 0)))
(defun client-callback (ev)
  (when (ready ev)
    (wl:create-client *wayland* (unix-sockets::fd (unix-sockets:accept-unix-socket *socket*)) :class 'client)))

(defun handle-touch-motion (event)
  (let ((x (touch@-x event))
	(y (touch@-y event)))
    (setf *green-x-pos* (coerce x 'single-float))
    (setf *green-y-pos* (coerce y 'single-float))
    (format t "Touch motion: ~a ~a~%" x y)))

(defun handle-input (event)
  (log! "Handling an input event: ~a~%" event)
  (cond
    ((touch-motion@-p event) (handle-touch-motion event))))


;; Unorganized handlers
(defun handle-wayland-event ()
  (let ((result (wl:dispatch-event-loop *wayland*)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))


(defun set-frame-ready (a b c d e)
  (declare (ignore a b c d e))
  (setf *frame-ready* t))

(defun device-change (path change)
  (when (str:contains? "/event" path)
    (case change
      (:create (add-device (libinput *wayland*) path))
      (:delete (rem-device (libinput *wayland*) path))
      (t (error "Unknown notify change. You seem to be watching more than this can handle: ~A" change)))))

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


;; ┌─┐┌─┐┌─┐┌┬┐  ┌┬┐┌─┐┌─┐┬┌─┐
;; └─┐├┤ ├─┤ │   │││├─┤│ ┬││
;; └─┘└─┘┴ ┴ ┴   ┴ ┴┴ ┴└─┘┴└─┘
;; TODO: Since we immediately try dispatching and wait on libseat, this whole function is here only
;; for completeness sake.
(defun enable-seat (seat data)
  (declare (ignore seat data))
  (log! "SEAT ENABLED~%")
  (cl-async:exit-event-loop))

;; TODO: So you probably might want to kill off the whole compositor if we receive this?
  ;; When exactly does this happen anyway?
(defun disable-seat (seat data)
  (declare (ignore seat data))
  (log! "DISABLING SEAT - NOT IMPLEMENTED YET~%"))


;; Opening and closing restricted devices
(defvar *fd-id* (make-hash-table :test 'equal))
(defun open-device (path flags user-data)
  (declare (ignore flags user-data))
  ;; TODO: Although apparently id and fd are the same
  ;; At least as far as logind is concerned. Not sure about seatd.
  ;; For now leaving as is - since it's not really a problem.
  (let ((id nil) (fd nil))
    (setf (values id fd) (libseat:open-device *seat* path))
    (unless id (error "Failed to open device. No ID: ~A" path))
    (unless fd (error "Failed to open device. No FD: ~A" path))
    (setf (gethash fd *fd-id*) id)))

(defun close-device (fd data)
  (declare (ignore data))
  (libseat:close-device *seat* (gethash fd *fd-id*))
  (remhash fd *fd-id*))


;; ┌┬┐┬┌─┐┌─┐┬  ┌─┐┬ ┬
;;  │││└─┐├─┘│  ├─┤└┬┘
;; ─┴┘┴└─┘┴  ┴─┘┴ ┴ ┴
(defclass display (wl:display)
  ((libinput :initarg :libinput :reader libinput)))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun stop-app (process) (uiop:terminate-process process))
(defun test-app (app-name)
  (log! "🟢 ~a: Starting an app~%" app-name)
  (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
    (bt:make-thread
     (lambda ()
       (loop while (uiop/launch-program:process-alive-p process)
	     do (log! "🔴 ~a: ~a~%" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (log! "🟢 ~a: Client exit. Code: ~a~%" app-name (uiop:wait-process process))))
    process))


;; TODO: This is a fix for cl-async not having a handler for gracious :poll closing
;; Weirdly enough - i expected them to crash due to this missing method, but they don't.
;; I should probably fix this in cl-async by introducing the handle-cleanup method.
;; Could be done by keeping track of the pointers and the respective poller objects. Singletony, but eh.
(defmethod cl-async::handle-cleanup ((handle-type (eql :poll)) handle)
  (cond
    ((cffi:pointer-eq (cl-async::poller-c *seat-poller*) handle) (cl-async:free-poller *seat-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *drm-poller*) handle) (cl-async:free-poller *drm-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *wl-poller*) handle) (cl-async:free-poller *wl-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *client-poller*) handle) (cl-async:free-poller *client-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *input-poller*) handle) (cl-async:free-poller *input-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *device-poller*) handle)
     (notify:shutdown)
     (cl-async:free-poller *device-poller*))
    (t (error "Unknown poller handle"))))
