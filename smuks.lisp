
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; NOTE: Command i usually use to fire up a debug session via ssh (xdg session id might differ)
;; DEBUG_SMUKS=1 XDG_SESSION_ID=1 sbcl --dynamic-space-size 4028 --eval '(ql:quickload :swank)' --eval '(swank:create-server :port 25252 :dont-close t)' --eval '(ql:quickload :smuks)'
(in-package :smuks)

(defvar *enable-wayland-debug-logs* "")
(defvar *enable-mesa-debug-logs* "")
(defvar *enable-egl-debug-logs* "fatal") ;; "debug"

(defvar *drm* nil)
(defvar *wayland* nil)
(defvar *udev* nil)
(defvar *udev-monitor* nil)
(defvar *libinput* nil)
(defvar *seat* nil)
(defvar *accel* nil)
(defvar *gl-version* nil)

#+xwayland
(defvar *xwayland-process* nil)

(defnil
    *socket*
  *smuks-exit* *cursor*
  *egl* *egl-context*)

(defun mainer ()
  (setf *log-output* *standard-output*)
  (heading)

  (setf (uiop/os:getenv "WAYLAND_DEBUG") *enable-wayland-debug-logs*)
  (setf (uiop/os:getenv "MESA_DEBUG") *enable-mesa-debug-logs*)
  (setf (uiop/os:getenv "EGL_LOG_LEVEL") *enable-egl-debug-logs*)


  (unless (setf *seat* (libseat:open-seat :enable-seat 'enable-seat :disable-seat 'disable-seat :log-handler t))
    (error "Failed to open seat. If you're like me - SSH sessions do not have a seat assigned."))
  (libseat:dispatch *seat* 0)

  (unless (setf *drm* (init-drm 'open-device 'close-device))
    (error "Failed to initialize DRM"))

  (unless (setf *libinput* (make-instance 'dev-track :open-restricted 'open-device :close-restricted 'close-device))
    (error "Failed to initialize libinput"))


  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'display :fd (unix-sockets::fd *socket*)
			      :drm *drm*
			      :libseat *seat*))

  ;; #+xwayland
  ;; (setf *xwayland-process* (uiop:launch-program "Xwayland"))

  (setf (values *egl* *egl-context* *gl-version*) (init-egl (gbm-pointer *drm*) (wl:display-ptr *wayland*)))
  (setf (egl *wayland*) *egl*)
  (setf (gl-version *wayland*) *gl-version*)


  (setf *cursor* (load-cursor-texture))

  (setf *accel* (iio-accelerometer:find-accelerometer-dev))

  (setf *udev* (udev:udev-new))
  (setf *udev-monitor* (udev:monitor-udev *udev*))

  (init-globals *wayland*)
  (start-monitors *wayland*)

  (cl-async:start-event-loop
   (lambda ()
     (log! "Starting DRM fd listener. Waiting for events...")
     (cl-async:poll (fd *drm*) 'drm-callback :poll-for '(:readable))

     (log! "Starting wayland client socket listener. Waiting for clients...")
     (cl-async:poll (unix-sockets::fd *socket*) 'client-callback :poll-for '(:readable) :socket t)

     (log! "Starting wayland event loop listener. Waiting for events...")
     (cl-async:poll (wl:event-loop-fd *wayland*) 'wayland-callback :poll-for '(:readable))

     (log! "Starting input event poller. Waiting for user inputs...")
     (cl-async:poll (context-fd *libinput*) 'input-callback :poll-for '(:readable))

     (log! "Starting the umpteenth poller. Now for seat events...")
     (cl-async:poll (libseat:get-fd *seat*) 'seat-callback :poll-for '(:readable))

     (when *accel*
       (log! "Starting MY accelerometer poller. Waiting for accelerometer events...")
       (cl-async:poll (iio-accelerometer::fd *accel*) 'my-accelerometer-callback :poll-for '(:readable)))

     (log! "Starting the udev poller. Waiting for udev events...")
     (udev::%monitor-enable-receiving *udev-monitor*)
     (cl-async:poll (udev:get-fd *udev-monitor*) 'udev-callback :poll-for '(:readable))

     (cl-async:delay 'livesupport-recursively :time 0.016)

     (recursively-render-frame))))

(defun livesupport-recursively ()
  (livesupport:update-repl-link)
  (cl-async:delay 'livesupport-recursively :time 0.016))


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
      (loop for screen in (outputs *wayland*)
	    do (restart-case (render-frame screen)
		 (skip-frame ()
		   :report "Skip frame"
		   (print "Skipping frame"))))))



;; TODO: Add a way to check which screen belongs to the accelerometer
;; This might actually need to be a hack - check if DSI or some other on-board connector is used
(defun determine-orientation (orient)
  (let* ((dsi-output (dsi-output *wayland*))
	 (current-orient (orientation dsi-output)))

    (destructuring-bind (x y z) orient
      (declare (ignore z))
      (let* ((y-neg (<= y 0)) (x-neg (<= x 0))
	     (x (abs x)) (y (abs y))
	     (new-orient
	       (cond
		 ((and y-neg (>= y x)) :portrait)
		 ((>= y x) :portrait-i)
		 ((and x-neg (>= x y)) :landscape)
		 ((>= x y) :landscape-i))))
	(unless (eq current-orient new-orient)
	  (setf (orientation dsi-output) new-orient)
	  (let ((related-desktop (find-output-desktop *wayland* dsi-output)))
	    (recalculate-layout related-desktop)))))))

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

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (let* ((compositor (compositor client))
	   (surfaces (when compositor (surfaces compositor)))
	   (toplevel (when surfaces (loop for surface being the hash-values of surfaces
					  when (typep surface 'toplevel) return surface))))
      (when toplevel (format stream "Client: ~a:::~a" (title toplevel) (app-id toplevel))))))


;; ┌─┐┌─┐┬  ┬  ┬┌┐┌┌─┐
;; ├─┘│ ││  │  │││││ ┬
;; ┴  └─┘┴─┘┴─┘┴┘└┘└─┘
;; Slightly annoying callbacks
(defun ready (ev) (member :readable ev))
(defun wayland-callback (ev) (when (ready ev) (handle-wayland-event)))
(defun input-callback (ev) (when (ready ev) (smuks::dispatch *libinput* 'handle-input)))
(defun drm-callback (ev) (when (ready ev) (drm:handle-event (fd *drm*) :page-flip2 'set-frame-ready)))
(defun seat-callback (ev) (when (ready ev) (libseat:dispatch *seat* 0)))
(defun my-accelerometer-callback (ev)
  (when (ready ev)
    (handler-case
	(determine-orientation (iio-accelerometer::read-accelerometer *accel*))
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
		;; NOTE: Only considering the drm node that was initt'd via my drm package
		(when (string=
		       (format nil "/dev/dri/~a" (udev:dev-sys-name dev))
		       (sdrm:primary-node (drm *wayland*)))
		  (sleep 0.5) (handle-drm-device-event dev)))
	       ((string= (udev:dev-subsystem dev) "input")
		(when (string= (udev:dev-action dev) "add")
		  (process-added-device dev)))))))

(defun handle-drm-device-event (dev)
  (declare (ignore dev))
  (init-outputs2 *wayland* t))


(defun handle-input (event)
  ;; device removed seems to be called when switching VTs/sessions
  (if (eq (event-type event) :device-removed)
      (rem-device-abandoned
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
  (let ((screen (output-by-crtc *wayland* crtc-id)))
    (when screen (render-frame screen))))

(defun process-added-device (dev)
  (when (str:contains? "event" (udev:dev-sys-name dev))
    (add-device
     *libinput*
     (merge-pathnames (format nil "/dev/input/~a" (udev:dev-sys-name dev))))))

;; ┌─┐┌─┐┌─┐┬┌─┌─┐┌┬┐
;; └─┐│ ││  ├┴┐├┤  │
;; └─┘└─┘└─┘┴ ┴└─┘ ┴
(defun socket-path () (format nil "~a/~a" (uiop:getenv "XDG_RUNTIME_DIR") "wayland-0"))
(defun init-socket ()
  (let ((socket-path (socket-path)) (socket nil))
    (setf socket
	  (restart-case
	      (handler-case
		  (progn
		    (when (probe-file socket-path)
		      (delete-file socket-path))
		    (unix-sockets:make-unix-socket socket-path))
		(error (e)
		  (declare (ignore e))
		  (progn
		    (delete-file socket-path)
		    (unix-sockets:make-unix-socket socket-path))))
	    (create-new-socket ()
	      :report "Create new socket"
	      (log! "Creating new socket")
	      (delete-file socket-path)
	      (unix-sockets:make-unix-socket socket-path))))
    (setf (uiop/os:getenv "WAYLAND_DISPLAY") socket-path)
    socket))


;; ┌─┐┌─┐┌─┐┌┬┐  ┌┬┐┌─┐┌─┐┬┌─┐
;; └─┐├┤ ├─┤ │   │││├─┤│ ┬││
;; └─┘└─┘┴ ┴ ┴   ┴ ┴┴ ┴└─┘┴└─┘
(defun enable-seat (seat data)
  (declare (ignore seat data))
  (when *wayland* (resume-outputs *wayland*))
  (when *libinput* (init-devices *libinput*)))

(defun disable-seat (seat data)
  "Called when a seat is 'disabled'. One instance of this is when switching to a different VT"
  (declare (ignore seat data))
  ;; TODO: Still don't know what to do here. Pausing outputs is a bit too late in the pipeline.
  ;; The pause is being done just before switching VTs
  ;; (pause-outputs *wayland*)
  )


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
  #+xwayland
  (when *xwayland-process* (uiop:terminate-process *xwayland-process*))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* (wl:display-ptr *wayland*) *egl-context*))
  (when *libinput* (destroy *libinput*))

  (when *wayland* (cleanup-display *wayland*))
  (when *drm* (sdrm:close-drm *drm*))

  (when *seat* (libseat:close-seat *seat*))
  (when *accel* (iio-accelerometer::close-dev *accel*))
  (when *socket*
    (unix-sockets:close-unix-socket *socket*)
    (delete-file (socket-path)))

  (setfnil *egl* *egl-context* *drm* *smuks-exit*
	   *wayland* *socket* *seat* *cursor* *accel* *libinput*))
