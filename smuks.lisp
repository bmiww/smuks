
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

(defvar *enable-wayland-debug-logs* "1")
(defvar *enable-mesa-debug-logs* "1")
(defvar *enable-egl-debug-logs* "debug")

(defvar *drm* nil)
(defvar *wayland* nil)
(defvar *iio* nil)
(defvar *udev* nil)
(defvar *udev-monitor* nil)
(defvar *libinput* nil)
(defvar *seat* nil)
(defvar *accel* nil)

(defnil
    *socket* *smuks-exit*
  *cursor*
  *egl* *egl-context*
  *client-poller* *wl-poller* *drm-poller* *input-poller*
  *seat-poller* *accelerometer-poller*
  *udev-poller*)

(defun mainer ()
  (setf *log-output* *standard-output*)
  (heading)

  (setf (uiop/os:getenv "WAYLAND_DEBUG") *enable-wayland-debug-logs*)
  (setf (uiop/os:getenv "MESA_DEBUG") *enable-mesa-debug-logs*)
  (setf (uiop/os:getenv "EGL_LOG_LEVEL") *enable-egl-debug-logs*)

  (unless (setf *seat* (libseat:open-seat :enable-seat 'enable-seat :disable-seat 'disable-seat :log-handler t))
    (error "Failed to open seat. If you're like me - SSH sessions do not have a seat assigned."))
  (libseat:dispatch *seat* 0)

  (unless (setf *drm* (init-drm)) (error "Failed to initialize DRM"))

  (unless (setf *libinput* (make-instance 'dev-track :open-restricted 'open-device :close-restricted 'close-device))
    (error "Failed to initialize libinput"))


  (setf *screen-tracker* (make-instance 'screen-tracker :drm *drm*))

  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'display :fd (unix-sockets::fd *socket*)
		     ;; This dev-t is probably rather wrong - since client apps probably can't use card0/card1
		     ;; But instead should be notified of the render nodes renderD128 and so on
		     ;; But it might also match main-device proper
		     ;; It could also be interesting to have more than one dev-t.
			      :dev-t (drm::resources-dev-t (sdrm::resources *drm*))
			      :egl *egl*
			      :screen-tracker *screen-tracker*))

  (setf (values *egl* *egl-context*) (init-egl (gbm-pointer *drm*) (wl:display-ptr *wayland*)))
  (setf (egl *wayland*) *egl*)
  (setf (egl *screen-tracker*) *egl*)


  (setf *cursor* (load-cursor-texture))
  (prep-shaders *screen-tracker*)

  (setf *accel* (iio-accelerometer::find-accelerometer-dev))


  ;; (setf *iio* (init-libiio))
  ;; (enable-accelerometer-scan *iio*)

  (setf *udev* (udev:udev-new))
  (setf *udev-monitor* (udev:monitor-udev *udev*))

  (start-monitors *screen-tracker*)
  (init-globals *wayland* (screens *screen-tracker*))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") +socket-file+)

  (cl-async:start-event-loop
   (lambda ()
     (log! "Starting DRM fd listener. Waiting for events...")
     (setf *drm-poller* (drm-listener))
     (log! "Starting wayland client socket listener. Waiting for clients...")
     (setf *client-poller* (client-listener))
     (log! "Starting wayland event loop listener. Waiting for events...")
     (setf *wl-poller* (wayland-listener))
     (log! "Starting input event poller. Waiting for user inputs...")
     (setf *input-poller* (input-listener))
     (log! "Starting the umpteenth poller. Now for seat events...")
     (setf *seat-poller* (seat-listener))
     (log! "Starting MY accelerometer poller. Waiting for accelerometer events...")
     (setf *accelerometer-poller* (my-accelerometer-listener))
     ;; (log! "Starting the accelerometer poller. Waiting for accelerometer events...")
     ;; (setf *accelerometer-poller* (accelerometer-listener))
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
;; TODO: This one doesn't make too much sense any more.
;; Since the rendering is performed based on drm page-flips
(defun recursively-render-frame ()
  (if *smuks-exit*
      (cl-async:exit-event-loop)
      (loop for screen in (screens *screen-tracker*)
	    do (restart-case (render-frame screen)
		 (skip-frame ()
		   :report "Skip frame"
		   (print "Skipping frame"))))))


;; TODO: Add a way to check which screen belongs to the accelerometer
(defun determine-orientation (orient)
  (let* ((dsi-screen (dsi-screen *screen-tracker*))
	 (current-orient (orientation dsi-screen)))
    (destructuring-bind (x y z) orient
      (declare (ignore y))
      (let* ((z-neg (<= z 0)) (x-neg (<= x 0))
	     (x (abs x)) (z (abs z))
	     (new-orient
	       (cond
		 ((and z-neg (> z x)) :landscape)
		 ((> z x) :landscape-i)
		 ((and x-neg (> x z)) :portrait)
		 ((> x z) :portrait-i))))
	(unless (eq current-orient new-orient)
	  (setf (orientation dsi-screen) new-orient)
	  ;; TODO: For now disabling layout recalculation on orientation change
	  ;; Since i'm working on windowing on different screens/desktops
	  ;; (recalculate-layout *wayland*)
	  )))))

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client (wl:client)
  ((compositor :initform nil :accessor compositor)
   (seat :initform nil :accessor seat)
   (dd-manager :initform nil :accessor dd-manager)))

(defmethod (setf wl::iface) :after ((iface compositor) (client client) id)
  (declare (ignore id))
  (setf (compositor client) iface))

(defmethod (setf wl::iface) :after ((iface seat) (client client) id)
  (declare (ignore id))
  (setf (seat client) iface))

(defmethod (setf wl::iface) :after ((iface dd-manager) (client client) id)
  (declare (ignore id))
  (setf (dd-manager client) iface))


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
(defun my-accelerometer-listener () (cl-async:poll (iio-accelerometer::fd *accel*) 'my-accelerometer-callback :poll-for '(:readable)))
(defun udev-listener ()
  (udev::%monitor-enable-receiving *udev-monitor*)
  (cl-async:poll (udev:get-fd *udev-monitor*) 'udev-callback :poll-for '(:readable)))

;; Slightly annoying callbacks
(defun ready (ev) (member :readable ev))
(defun wayland-callback (ev) (when (ready ev) (handle-wayland-event)))
(defun input-callback (ev) (when (ready ev) (smuks::dispatch *libinput* 'handle-input)))
(defun drm-callback (ev) (when (ready ev) (drm:handle-event (fd *drm*) :page-flip2 'set-frame-ready)))
(defun seat-callback (ev) (when (ready ev) (libseat:dispatch *seat* 0)))
(defun accelerometer-callback (ev) (when (ready ev) (determine-orientation (read-accelerometer *iio*))))
;; (defun my-accelerometer-callback (ev) (when (ready ev)
				    ;; (handler-case (determine-orientation (iio-accelerometer::read-accelerometer *accel*))
				      ;; (error (e)
					;; (log! "Error reading accelerometer: ~a" e)))))
(defun my-accelerometer-callback (ev)
  (when (ready ev)
    (handler-case
	(progn
	  ;; (determine-orientation
	  (print "Read event")
	  (print (iio-accelerometer::read-accelerometer *accel*))
	  )
      ;; )
      (error (e)
	(log! "Error reading accelerometer: ~a" e)))))

(defun client-callback (ev)
  (when (ready ev)
    (wl:create-client *wayland* (unix-sockets::fd (unix-sockets:accept-unix-socket *socket*)) :class 'client)))

(defun udev-callback (ev)
  (when (ready ev)
    (loop for dev = (udev:receive-device *udev-monitor*)
	  while dev
	  do (cond
	       ((string= (udev:dev-subsystem dev) "drm")
		(when (string= (udev:dev-sys-name dev) "card0") (sleep 0.5) (handle-drm-device-event dev)))
	       ((string= (udev:dev-subsystem dev) "input")
		(when (string= (udev:dev-action dev) "add")
		  (process-added-device dev)))))))

(defun handle-drm-device-event (dev)
  (declare (ignore dev))
  (handle-drm-change *screen-tracker*))


(defun handle-input (event)
  (if (eq (event-type event) :device-removed)
      (rem-abandoned-device
       *libinput*
       (merge-pathnames (format nil "/dev/input/~a"
				(device-removed@-sys-name event))))
      (restart-case (input *wayland* (event-type event) event)
	(abandon-event () :report "Abandoning event"))))

;; Unorganized handlers
(defun handle-wayland-event ()
  (let ((result (wl:dispatch-event-loop *wayland*)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))


(defun set-frame-ready (a b c d crtc-id f)
  "If a screen is not found - it is assumed to have been disconnected"
  (declare (ignore a b c d f))
  (let ((screen (screen-by-crtc *screen-tracker* crtc-id)))
    (when screen (render-frame screen))))

(defun process-added-device (dev)
  (when (str:contains? "event" (udev:dev-sys-name dev))
    (add-device
     *libinput*
     (merge-pathnames (format nil "/dev/input/~a" (udev:dev-sys-name dev))))))

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
(defun enable-seat (seat data) (declare (ignore seat data)) (log! "SEAT ENABLED"))

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
;; Can be called from repl to stop the compositor
(defun shutdown () (setf *smuks-exit* t))

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


(defun cleanup ()
  (when *iio* (cleanup-iio *iio*))
  (when *screen-tracker* (cleanup-screen-tracker *screen-tracker*))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* (wl:display-ptr *wayland*) *egl-context*))
  (when *drm* (sdrm:close-drm *drm*))

  (when *wayland* (wl:destroy *wayland*))

  (when *seat* (libseat:close-seat *seat*))
  (when *accel* (iio-accelerometer::close-dev *accel*))
  (when *socket*
    (unix-sockets:close-unix-socket *socket*)
    (delete-file +socket-file+))

  (setfnil *egl* *egl-context* *drm* *smuks-exit*
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
    (t (error "Unknown poller handle"))))
