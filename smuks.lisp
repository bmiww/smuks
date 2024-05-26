
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

(defvar *drm* nil)
(defvar *wayland* nil)
(defvar *iio* nil)
(defvar *orientation* nil)
(defvar *shaders* nil)
(defvar *enable-frame-counter* t)
(defvar *frame-counter* 0)
(defvar *frame-counter-thread* nil)
(defvar *udev* nil)
(defvar *udev-monitor* nil)
(defvar *screen-tracker* nil)
;; TODO: Only for refactoring - remove when handling multiple screens
(defvar *first* nil)

(defnil
    *socket* *smuks-exit* *frame-ready*
  *libinput* *seat*
  *egl* *egl-context* *egl-image*
  *gl-frame-buffer*
  *rect-shader* *texture-shader* *cursor*
  *client-poller* *wl-poller* *drm-poller* *input-poller* *seat-poller* *device-poller* *accelerometer-poller*
  *udev-poller*

  *test-app*)

;; screen
(defclass screen ()
  ((buffer :initarg :buffer :accessor buffer)
   (fb :initarg :fb :accessor fb)
   (connector :initarg :connector :accessor connector)
   (egl-image :accessor egl-image)
   (mode :initarg :mode :accessor mode)
   (crtc :initarg :crtc :accessor crtc)))

(defmethod width ((screen screen)) (drm:mode-hdisplay (mode screen)))
(defmethod height ((screen screen)) (drm:mode-vdisplay (mode screen)))
(defmethod connector-type ((screen screen)) (drm:connector!-connector-type (connector screen)))
(defmethod set-crtc ((screen screen) crtc)
  ())

(defmethod set-crtc ((device gbm-device) framebuffer)
  (let* ((crtc (crtc device))
	 (connector (car (connected-connectors device)))
	 (modes (drm::connector!-modes connector)))

    (let ((result (drm:set-crtc
		   (fd device)
		   (drm::crtc!-id crtc)
		   framebuffer
		   0 0
		   (list (drm::connector!-id connector))
		   ;; TODO: Using the first mode.
		   ;; Not yet checking if the mode is valid at all
		   (car modes))))
      (unless (eq 0 result) (error (format nil "Failed to set crtc: error ~a" result)))
      crtc)))

;; (defmethod create-egl-image ((screen screen) egl)
  ;; )

;; screen-tracker
;; TODO: Maybe this can also have egl in it?
(defclass screen-tracker ()
  ((drm :initarg :drm :accessor drm)
   (screens :initform nil :accessor screens)))

(defmethod initialize-instance :after ((tracker screen-tracker) &key drm)
  (let ((connectors (connectors drm)))
    (setf (screens tracker)
	  (loop for connector in connectors
		for fb-obj = (create-connector-framebuffer drm connector)
		collect (make-instance 'screen
			   :connector connector
			   :buffer (framebuffer-buffer fb-obj)
			   :fb (framebuffer-id fb-obj)
			   :mode (framebuffer-mode fb-obj))))))

;; TODO: Get rid of this - this is compat during refactoring
(defmethod testie ((tracker screen-tracker)) (first (screens tracker)))

;; NOTE: I'll maybe use this to identify my tablet screen for the sake of associating touch or accelerometer events with it.
(defmethod dsi-screen ((tracker screen-tracker)) (find-if (lambda (screen) (eq (connector-type screen) :dsi)) (screens tracker)))

(defun mainer ()
  (setf *log-output* *standard-output*)
  (setf *frame-ready* t)
  (heading)

  (setf (uiop/os:getenv "WAYLAND_DEBUG") *enable-wayland-debug-logs*)

  ;; NOTE: Open a seat.
  ;; We block until libseat tells us that we have a seat
  ;; The loop will exit as soon as we have a seat
  ;; TODO: Give the loop like a 5 sec timeout? In case seatd/logind doesn't respond
  (setf *seat* (libseat:open-seat :enable-seat 'enable-seat :disable-seat 'disable-seat :log-handler t))
  (unless *seat* (error "Failed to open seat. If you're like me - SSH sessions do not have a seat assigned."))
  ;; TODO: We might be able to just run (libseat:dispatch *seat* 0) here instead of the loop
  ;; If the 0 was a timeout, we could also give it a value
  (cl-async:start-event-loop (lambda () (setf *seat-poller* (seat-listener))))

  (setf *drm* (init-drm))
  (setf *screen-tracker* (make-instance 'screen-tracker :drm *drm*))
  ;; TODO: Only for refactoring - remove when handling multiple screens
  (setf *first* (testie *screen-tracker*))

  (setf *socket* (init-socket))
  (setf *libinput* (make-instance 'dev-track :open-restricted 'open-device :close-restricted 'close-device))

  (setf *wayland* (make-instance 'display :fd (unix-sockets::fd *socket*)
		     ;; This dev-t is probably rather wrong - since client apps probably can't use card0/card1
		     ;; But instead should be notified of the render nodes renderD128 and so on
		     ;; But it might also match main-device proper
		     ;; It could also be interesting to have more than one dev-t.
			      :dev-t (drm::resources-dev-t (sdrm::resources *drm*))
			      :display-width (width *first*)
			      :display-height (height *first*)))


  (setf (values *egl* *egl-context*) (init-egl (gbm-pointer *drm*) (wl:display-ptr *wayland*)))
  (setf (egl *wayland*) *egl*)

  (setf *egl-image* (create-egl-image *egl* (buffer *first*) (width *first*) (height *first*)))

  (setf *gl-frame-buffer* (create-gl-framebuffer *egl-image*))
  (setf *cursor* (load-cursor-texture))
  (setf *shaders* (init-shaders))

  (setf *iio* (init-libiio))
  (determine-orientation (enable-accelerometer-scan *iio*))

  (setf *udev* (udev:udev-new))
  (setf *udev-monitor* (udev:monitor-udev *udev*))

  (init-globals *wayland* *drm*)

  (set-crtc *first* )
  (set-crtc *drm* (fb *first*))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") +socket-file+)
  (cl-async:start-event-loop
   (lambda ()
     (log! "Starting DRM fd listener. Waiting for events...")
     (setf *drm-poller* (drm-listener))
     (log! "Starting wayland client socket listener. Waiting for clients...")
     (setf *client-poller* (client-listener))
     (log! "Starting wayland event loop listener. Waiting for events...")
     (setf *wl-poller* (wayland-listener))
     ;; TODO: Might be replacable with the udev-poller/listener
     ;; (log! "Starting event node watch poller. Waiting for device changes...")
     ;; (setf *device-poller* (notify-listener))
     (log! "Starting input event poller. Waiting for user inputs...")
     (setf *input-poller* (input-listener))
     (log! "Starting the umpeenth poller. Now for seat events...")
     (setf *seat-poller* (seat-listener))
     (log! "Starting the accelerometer poller. Waiting for accelerometer events...")
     (setf *accelerometer-poller* (accelerometer-listener))
     (log! "Starting the udev poller. Waiting for udev events...")
     (setf *udev-poller* (udev-listener))

     (recursively-render-frame))))

(defun main ()
  (restart-case
      (unwind-protect (mainer)
	(cleanup))
    (📍start-over () (main))))


;; ┌─┐┬─┐┌─┐┌┬┐┌─┐
;; ├┤ ├┬┘├─┤│││├┤
;; └  ┴└─┴ ┴┴ ┴└─┘
(defun recursively-render-frame ()
  (if *smuks-exit*
      (cl-async:exit-event-loop)
      (restart-case
	  (progn
	    (render-frame)
	    (cl-async:delay 'recursively-render-frame :time 0.016))
	(skip-frame ()
	  :report "Skip frame"
	  (cl-async:delay 'recursively-render-frame :time 0.016)))))

;; TODO: The boolean return value is stupid. Tells that a cursor has been rendered
;; So that the main loop can know if it should render the display cursor or not
;; TODO: This is almost identical to render-toplevel, with the difference being the coordinates
(defun render-cursor (surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (- (cursor-x *wayland*) (flo (x surface))))
	(y (- (cursor-y *wayland*) (flo (y surface)))))

    ;; TODO: Fix this active-surface usage. You moved active-surface to a client seat
    ;; And this use case in general seems wrong (could be improved)
    ;; (if (active-surface (role surface))
    (progn
      (shaders.texture:draw *texture-shader* texture `(,x ,y ,width ,height))
      (flush-frame-callbacks surface)
      (setf (needs-redraw surface) nil)
      t)
    ;; nil)
  ))

;; TODO: The boolean return value is stupid. Tells that a cursor has been rendered
;; So that the main loop can know if it should render the display cursor or not
(defun render-toplevel (surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (shaders.texture:draw *texture-shader* texture `(,x ,y ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)
    nil))

(defun render-surface (surface)
  (typecase surface
    (cursor (render-cursor surface))
    (t (render-toplevel surface))))

(defun render-clients ()
  (let* ((clients (wl:all-clients *wayland*))
	 (compositors (remove-if-not 'identity (mapcar 'compositor clients)))
	 (surfaces (util:flatten (mapcar 'all-ready-surfaces compositors))))
    (mapcar (lambda (surface) (render-surface surface)) surfaces)))

(defvar *y-pos* 220.0)
(defvar y-up t)
(defun next-y-pos ()
  (when (> *y-pos* 300.0)
    (setf y-up nil))
  (when (< *y-pos* 150.0)
    (setf y-up t))
  (incf *y-pos* (if y-up 1 -1)))

(defvar *red-x* 50.0)
(defvar *red-y* 100.0)

(defun render-frame ()
  (livesupport:update-repl-link)
  (let ((cursor-drawn nil))
    (when *frame-ready*
      (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
      (gl:clear :color-buffer-bit)

      (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
						:x 10.0 :y (next-y-pos) :w 50.0 :h 60.0
						:color '(0.2 0.2 0.2 1.0))))

      (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect
						:x *red-x* :y *red-y* :w 200.0 :h 50.0
						:color '(1.0 0.0 0.0 0.6))))

      (setf cursor-drawn (some (lambda (val) val) (render-clients)))
      (unless cursor-drawn
	(shaders.texture:draw *texture-shader* *cursor*
			      `(,(cursor-x *wayland*) ,(cursor-y *wayland*) 36.0 36.0)))
      (gl:flush)
      (gl:finish)

      (setf *frame-ready* nil))

    ;; TODO: Wasteful - also - didn't really help much at the moment.
    ;; Try to bring it back inside the *frame-ready* check
    (handler-case
	(drm-page-flip *drm* (fb *first*))
      (error (err) (declare (ignore err)) ()))

    ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
    ;; Any events or changes that i could instead attach to?
    ;; Maybe instead use per client flushes - for example when receiving commit from them
    (wl:flush-clients *wayland*)
    (when *enable-frame-counter* (incf *frame-counter*))))

(defun determine-orientation (orient)
  (let ((current-orient *orientation*))
    (destructuring-bind (x y z) orient
      (declare (ignore y))
      (let* ((z-neg (<= z 0)) (x-neg (<= x 0))
	     (x (abs x)) (z (abs z)))
	(cond
	  ((and z-neg (> z x)) (setf *orientation* :landscape))
	  ((> z x) (setf *orientation* :landscape-i))
	  ((and x-neg (> x z)) (setf *orientation* :portrait))
	  ((> x z) (setf *orientation* :portrait-i)))))
    (unless (eq current-orient *orientation*)
      (setf (orientation *wayland*) *orientation*)
      (recalculate-layout *wayland*)
      (let ((projection (sglutil:make-projection-matrix
			 (sdrm:screen-width *drm* *orientation*) (sdrm:screen-height *drm* *orientation*)
			 (case *orientation* (:landscape -90) (:portrait 0) (:landscape-i 90) (:portrait-i 180)))))
	(mapcar (lambda (shader) (shaders:update-projection shader projection)) *shaders*)))))

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
(defun drm-listener () (cl-async:poll (fd *drm*) 'drm-callback :poll-for '(:readable)))
(defun input-listener () (cl-async:poll (context-fd *libinput*) 'input-callback :poll-for '(:readable)))
(defun seat-listener () (cl-async:poll (libseat:get-fd *seat*) 'seat-callback :poll-for '(:readable)))
(defun accelerometer-listener () (cl-async:poll (accelerometer-fd *iio*) 'accelerometer-callback :poll-for '(:readable)))
;; TODO: Depending on what you figure out in the udev monitor - might be able to get rid of inotify here
(defun notify-listener ()
  (notify::init)
  (notify:watch "/dev/input/" :events '(:create :delete))
  (cl-async:poll notify::*fd* 'notify-callback :poll-for '(:readable)))
(defun udev-listener ()
  ;; (udev:monitor-add-match-subsystem *udev* "drm")
  ;; (udev:monitor-add-match-subsystem *udev* "input")
  ;; (udev:monitor-add-match-subsystem *udev* "iio")
  (udev::%monitor-enable-receiving *udev-monitor*)
  (cl-async:poll (udev:get-fd *udev-monitor*) 'udev-callback :poll-for '(:readable)))

;; Slightly annoying callbacks
(defun ready (ev) (member :readable ev))
(defun wayland-callback (ev) (when (ready ev) (handle-wayland-event)))
(defun input-callback (ev) (when (ready ev) (smuks::dispatch *libinput* 'handle-input)))
(defun notify-callback (ev) (when (ready ev) (notify::process 'process-inotify)))
(defun drm-callback (ev) (when (ready ev) (drm:handle-event (fd *drm*) :page-flip2 'set-frame-ready)))
(defun seat-callback (ev) (when (ready ev) (libseat:dispatch *seat* 0)))
(defun accelerometer-callback (ev) (when (ready ev) (determine-orientation (read-accelerometer *iio*))))
(defun client-callback (ev)
  (when (ready ev)
    (wl:create-client *wayland* (unix-sockets::fd (unix-sockets:accept-unix-socket *socket*)) :class 'client)))

(defun udev-callback (ev)
  (when (ready ev)
    (loop for dev = (udev:receive-device *udev-monitor*)
	  while dev
	  do (progn
	       (if (and (string= (udev:dev-subsystem dev) "drm")
			(string= (udev:dev-sys-name dev) "card0"))
		   (progn (sleep 0.5) (handle-drm-device-event dev))
		   (print dev))))))

(defun handle-drm-device-event (dev)
  (declare (ignore dev))
  (sdrm::recheck-resources *drm*))

(defun handle-input (event) (input *wayland* (event-type event) event))

;; Unorganized handlers
(defun handle-wayland-event ()
  (let ((result (wl:dispatch-event-loop *wayland*)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))


(defun set-frame-ready (a b c d crtc-id f)
  (declare (ignore a b c d f))
  ;; (log! "Arguments: ~A ~A ~A ~A ~A ~A" a b c d crtc-id f)
  ;; (log! "crtc: ~a" crtc-id)
  (setf *frame-ready* t))

(defun process-inotify (path change)
  (declare (ignore change))
  (let ((stringy-path (namestring path)))
    (cond
      ;; ((str:contains? "/event" stringy-path) (process-device-change path change)))))
      ((str:contains? "/event" stringy-path) (log! "You disabled device change processing. Because you are a coward.")))))

(defun process-device-change (path change)
  (case change
	(:create (add-device *libinput* path))
	(:delete (rem-device *libinput* path))
	(t (error "Unknown notify change. You seem to be watching more than this can handle: ~A" change))))

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
      (log! "Creating new socket")
      (delete-file +socket-file+)
      (unix-sockets:make-unix-socket +socket-file+))))


;; ┌─┐┌─┐┌─┐┌┬┐  ┌┬┐┌─┐┌─┐┬┌─┐
;; └─┐├┤ ├─┤ │   │││├─┤│ ┬││
;; └─┘└─┘┴ ┴ ┴   ┴ ┴┴ ┴└─┘┴└─┘
;; TODO: Since we immediately try dispatching and wait on libseat, this whole function is here only
;; for completeness sake.
(defun enable-seat (seat data)
  (declare (ignore seat data))
  (log! "SEAT ENABLED")
  (cl-async:exit-event-loop))

;; TODO: So you probably might want to kill off the whole compositor if we receive this?
  ;; When exactly does this happen anyway?
(defun disable-seat (seat data)
  (declare (ignore seat data))
  (log! "DISABLING SEAT - NOT IMPLEMENTED YET"))


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



;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun stop-app (process)
  "Shorthand for stopping a process"
  (uiop:terminate-process process))

;; TODO: Attach error output too.
(defun test-app (app-name)
  "Launch a child process and listen to its output"
  (log! "🟢 ~a: Starting an app" app-name)
  (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
    (bt:make-thread
     (lambda ()
       (loop while (uiop/launch-program:process-alive-p process)
	     do (log! "🔴 ~a: ~a" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (log! "🟢 ~a: Client exit. Code: ~a" app-name (uiop:wait-process process))))
    process))

;; Can be called from repl to stop the compositor
(defun shutdown () (setf *smuks-exit* t))

(defun disable-frame-counter ()
  (when *frame-counter-thread*
    (bt:destroy-thread *frame-counter-thread*)
    (setf
     *enable-frame-counter* nil
     *frame-counter* 0
     *frame-counter-thread* nil)))

(defun enable-frame-counter ()
  (setf *enable-frame-counter* t)
  (setf *frame-counter* 0)
  (setf *frame-counter-thread*
	(bt:make-thread
	 (lambda ()
	   (loop do (sleep 1)
		 do (log! (format nil "Frames: ~a" *frame-counter*))
		 do (setf *frame-counter* 0))))))


;; ┬ ┬┌┐┌┌─┐┌─┐┬─┐┌┬┐┌─┐┌┬┐
;; │ ││││└─┐│ │├┬┘ │ ├┤  ││
;; └─┘┘└┘└─┘└─┘┴└─ ┴ └─┘─┴┘
(defun load-cursor-texture ()
  (let* ((texture (sglutil:mk-tex))
	 (image (png-read:read-png-file (merge-pathnames "assets/mouse.png" (asdf:system-source-directory :smuks))))
	 (data (png-read:image-data image)))
    (gl:bind-texture :texture-2d (tex-id texture))
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-image-2d :texture-2d 0 :rgba
		     (png-read:width image) (png-read:height image)
		     0 :rgba :unsigned-byte (make-array
					     (array-total-size data)
					     :element-type '(unsigned-byte 8)
					     :displaced-to data))
    texture))

(defun init-shaders ()
  (prep-gl-implementation (fb *first*) (width *drm*) (height *drm*))
  (setf *rect-shader* (shader-init:create-rect-shader *drm*))
  (setf *texture-shader* (shader-init:create-texture-shader *drm*))
  `(,*rect-shader* ,*texture-shader*))

(defun cleanup ()
  (disable-frame-counter)

  (when *iio* (cleanup-iio *iio*))
  ;; TODO: Before cleaning up *egl* clean up screen-tracker
  (when (and *egl* *egl-image*) (seglutil:destroy-image *egl* *egl-image*))
  ;; TODO: Before cleaning up *drm* clean up screen-tracker
  (when (and *drm* *first*) (sdrm:rm-framebuffer! *drm* (fb *first*) (buffer *first*)))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* (wl:display-ptr *wayland*) *egl-context*))
  (when *drm* (sdrm:close-drm *drm*))

  (when *wayland* (wl:destroy *wayland*))

  (when *seat* (libseat:close-seat *seat*))

  (when *socket*
    (unix-sockets:close-unix-socket *socket*)
    (delete-file +socket-file+))

  (setfnil *egl* *egl-context* *egl-image* *drm* *smuks-exit*
	   *wayland* *socket* *seat* *cursor* *iio*))

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
    ((cffi:pointer-eq (cl-async::poller-c *accelerometer-poller*) handle) (cl-async:free-poller *accelerometer-poller*))
    ((cffi:pointer-eq (cl-async::poller-c *udev-poller*) handle) (cl-async:free-poller *udev-poller*))

    ((cffi:pointer-eq (cl-async::poller-c *device-poller*) handle)
     (notify:shutdown)
     (cl-async:free-poller *device-poller*))
    (t (error "Unknown poller handle"))))
