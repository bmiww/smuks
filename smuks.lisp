
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
;; Set to one to enable wayland debug messages. Dunno which output it goes to though.
(defvar *enable-wayland-debug-logs* "")

(defnil
    *wayland* *socket* *smuks-exit* *frame-ready*
  *drm-dev* *buffer-object* *frame-buffer* *active-crtc*
  *libinput* *seat*
  *egl* *egl-context* *egl-image*
  *gl-frame-buffer*
  *rect-shader* *texture-shader*
  *client-poller* *wl-poller* *drm-poller* *input-poller* *seat-poller* *device-poller*

  *test-app*)

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
  (prep-gl-implementation *frame-buffer* (width *drm-dev*) (height *drm-dev*))
  (setf *rect-shader* (shader-init:create-rect-shader *drm-dev*))
  (setf *texture-shader* (shader-init:create-texture-shader *drm-dev*)))

;; TODO: Part of display init?
(defun init-globals ()
  ;; TODO: When you recompile the compiled classes - these globals aren't updated, needing a rerun
  (make-instance 'wl-compositor:global :display *wayland* :dispatch-impl 'compositor)
  (make-instance 'wl-subcompositor:global :display *wayland* :dispatch-impl 'subcompositor)
  (make-instance 'shm-global :display *wayland* :dispatch-impl 'shm)
  (make-instance 'seat-global :display *wayland* :dispatch-impl 'seat)
  (make-instance 'wl-data-device-manager:global :display *wayland* :dispatch-impl 'dd-manager)
  (make-instance 'xdg-wm-base:global :display *wayland* :dispatch-impl 'wm-base))

;; TODO: MOVE
(defun add-default-framebuffer (device buffer-object)
  (let* ((width (width device))
	 (height (height device))
	 (handle (gbm:bo-get-handle buffer-object))
	 (stride (gbm:bo-get-stride buffer-object))
	 (bpp 32) (depth 24))
    (add-framebuffer (fd device) width height depth bpp stride handle)))

(defun mainer ()
  (setf *log-output* *standard-output*)
  (setf *frame-ready* t)
  (heading)

  ;; NOTE: A private hack in the libseat to get the seat while in ssh
  ;; If you run this process in a regular tty - this is not needed and might even be harmful
  ;; TODO: Do not use 1 - just find the first session id for an open seat
  ;; Or - if theres multiple - we're in CL - just create give some restart options
  (setf (uiop/os:getenv "XDG_SESSION_ID_OVERRIDE") "1")
  (setf (uiop/os:getenv "WAYLAND_DEBUG") *enable-wayland-debug-logs*)

  ;; NOTE: Open a seat.
  ;; We block until libseat tells us that we have a seat
  ;; The loop will exit as soon as we have a seat
  ;; TODO: Give the loop like a 5 sec timeout? In case seatd/logind doesn't respond
  (setf *seat* (libseat:open-seat :enable-seat 'enable-seat :disable-seat 'disable-seat :log-handler t))
  (unless *seat* (error "Failed to open seat. If you're like me - SSH sessions do not have a seat assigned."))
  (cl-async:start-event-loop (lambda () (setf *seat-poller* (seat-listener))))

  ;; TODO: Can sometimes fail when running main anew in the same lisp image
  (setf *drm-dev* (init-drm))
  (setf *socket* (init-socket))
  (setf *libinput* (make-instance 'dev-track :open-restricted 'open-device :close-restricted 'close-device))

  ;; TODO: This is a bit awkward as an expected package export
  ;; Without this - nothing in wayland-land would work.
  ;; Maybe have the default display constructor do this in the :before step?
  (wl:init-interface-definitions)
  (setf *wayland* (make-instance 'display :fd (unix-sockets::fd *socket*)))
  (init-globals)

  (setf *buffer-object* (sdrm:create-bo *drm-dev*))
  (setf *frame-buffer* (add-default-framebuffer *drm-dev* *buffer-object*))

  (setf (values *egl* *egl-context*) (init-egl (gbm-pointer *drm-dev*) (wl:display-ptr *wayland*)))
  (setf *egl-image* (create-egl-image *egl* *buffer-object* (width *drm-dev*) (height *drm-dev*)))

  (setf *gl-frame-buffer* (create-gl-framebuffer *egl-image*))
  (init-shaders)

  ;; TODO: You might be able to remove the *active-crtc* indirection.
  ;; At least it's not really used elsewwhere
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
;; Rendering
;; X IS UP
;; Y IS RIGHT
(defun render-surface (surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (shaders.texture:draw *texture-shader* texture `(,x ,y ,width ,height))
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

(defvar *red-x* 50.0)
(defvar *red-y* 50.0)

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

    (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
					      :x *red-x* :y *red-y* :w 200.0 :h 50.0
					      :color '(1.0 0.0 0.0 0.6))))

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
   (seat :initform nil :accessor seat)))

(defmethod (setf wl::iface) :after ((iface compositor) (client client) id)
  (declare (ignore id))
  (setf (compositor client) iface))

(defmethod (setf wl::iface) :after ((iface seat) (client client) id)
  (declare (ignore id))
  (setf (seat client) iface))


;; ┌─┐┌─┐┬  ┬  ┬┌┐┌┌─┐
;; ├─┘│ ││  │  │││││ ┬
;; ┴  └─┘┴─┘┴─┘┴┘└┘└─┘
;; Listeners
(defun wayland-listener () (cl-async:poll (wl:event-loop-fd *wayland*) 'wayland-callback :poll-for '(:readable)))
(defun client-listener () (cl-async:poll (unix-sockets::fd *socket*) 'client-callback :poll-for '(:readable) :socket t))
(defun drm-listener () (cl-async:poll (fd *drm-dev*) 'drm-callback :poll-for '(:readable)))
(defun input-listener () (cl-async:poll (context-fd *libinput*) 'input-callback :poll-for '(:readable)))
(defun seat-listener () (cl-async:poll (libseat:get-fd *seat*) 'seat-callback :poll-for '(:readable)))
(defun notify-listener ()
  (notify::init)
  (notify:watch "/dev/input/" :events '(:create :delete))
  (cl-async:poll notify::*fd* 'notify-callback :poll-for '(:readable)))

;; Slightly annoying callbacks
(defun ready (ev) (member :readable ev))
(defun wayland-callback (ev) (when (ready ev) (handle-wayland-event)))
(defun input-callback (ev) (when (ready ev) (smuks::dispatch *libinput* 'handle-input)))
(defun notify-callback (ev) (when (ready ev) (notify::process 'device-change)))
(defun drm-callback (ev) (when (ready ev) (drm:handle-event (fd *drm-dev*) :page-flip 'set-frame-ready)))
(defun seat-callback (ev) (when (ready ev) (libseat:dispatch *seat* 0)))
(defun client-callback (ev)
  (when (ready ev)
    (wl:create-client *wayland* (unix-sockets::fd (unix-sockets:accept-unix-socket *socket*)) :class 'client)))

(defun move-green (x y) (setf *green-x-pos* x) (setf *green-y-pos* y))
(defun move-red (x y) (setf *red-x* x) (setf *red-y* y))

(defun handle-touch-motion (event)
  (let ((x (coerce (touch@-x event) 'single-float)) (y (coerce (touch@-y event) 'single-float)) (slot (touch@-seat-slot event)))
    (case slot
      (0 (move-green x y))
      (1 (move-red x y)))))

(defun handle-touch-down (event)
  (let* ((x (flo (touch@-x event)))
	 (y (flo (touch@-y event)))
	 (slot (touch@-seat-slot event))
	 (surface (surface-at-coords *wayland* x y)))
    (if surface
	(touch-down *wayland* surface slot x y)
	;; NOTE: If not touching a window - still keeping around my debug rectangles
	(case slot
	  (0 (move-green x y))
	  (1 (move-red x y))))))

(defun handle-touch-up (event) (touch-up *wayland* (touch-up@-seat-slot event)))
(defun handle-touch-frame (event) (declare (ignore event)) (touch-frame *wayland*))

(defun handle-input (event)
  (case (event-type event)
    (:touch-motion (handle-touch-motion event))
    (:touch-down   (handle-touch-down event))
    (:touch-up     (handle-touch-up event))
    (:touch-frame  (handle-touch-frame event))
    (t (log! "No handler for input event: ~a~%" (event-type event)))))


;; Unorganized handlers
(defun handle-wayland-event ()
  (let ((result (wl:dispatch-event-loop *wayland*)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))


(defun set-frame-ready (a b c d e)
  (declare (ignore a b c d e))
  (setf *frame-ready* t))

(defun device-change (path change)
  (let ((path (namestring path)))
    (when (str:contains? "/event" path)
      (case change
	(:create (add-device *libinput* path))
	(:delete (rem-device *libinput* path))
	(t (error "Unknown notify change. You seem to be watching more than this can handle: ~A" change))))))

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
  (;; Not sure we need 32. That's a lot of fingers.
   (touch-slot-interesses :initform (make-array 32 :initial-element nil) :reader touch-slot-interesses)))

(defmethod surface-at-coords ((display display) x y)
  (let ((clients (wl:all-clients display)))
    (loop for client in clients
	  for compositor = (compositor client)
	  for surfaces = (all-surfaces compositor)
	  for candidate = (loop for surface in surfaces
				when (in-bounds surface x y)
				return surface)
	  when candidate
	  return candidate)))

(defmethod touch-down ((display display) surface slot x y)
  "Notify a client that a touch event has occured.
Save the client as interested in the slot for later reference."
  (let* ((client (wl:client surface))
	 (seat (seat client)))
    (when (seat-touch seat)
      (pushnew client (aref (touch-slot-interesses display) slot))
      (touch-down seat surface slot x y))))

(defmethod touch-up ((display display) slot)
  "Notify all clients interested in the specific touch slot
and then clean the list out"
  (let ((clients (aref (touch-slot-interesses display) slot)))
    (dolist (client clients)
      (when (seat-touch (seat client))
	(touch-up (seat client) slot)))
    (setf (aref (touch-slot-interesses display) slot) nil)))

;; TODO: Not sure if it would be possible or even make sense to keep a list
;; of interested clients instead of broadcasting to all
(defmethod touch-frame ((display display))
  "Notify all clients that a touch frame has occured"
  (dolist (client (wl:all-clients display))
    (when (seat-touch (seat client))
      (touch-frame (seat client)))))



;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun stop-app (process)
  "Shorthand for stopping a process"
  (uiop:terminate-process process))

;; TODO: Attach error output too.
(defun test-app (app-name)
  "Launch a child process and listen to its output"
  (log! "🟢 ~a: Starting an app~%" app-name)
  (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
    (bt:make-thread
     (lambda ()
       (loop while (uiop/launch-program:process-alive-p process)
	     do (log! "🔴 ~a: ~a~%" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (log! "🟢 ~a: Client exit. Code: ~a~%" app-name (uiop:wait-process process))))
    process))

(defun flo (num)
  "Just a shorter form to coerce a number to float"
  (coerce num 'single-float))

;; Can be called from repl to stop the compositor
(defun shutdown () (setf *smuks-exit* t))


;; ┬ ┬┌─┐┌─┐┬┌─┌─┐
;; ├─┤├─┤│  ├┴┐└─┐
;; ┴ ┴┴ ┴└─┘┴ ┴└─┘
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
