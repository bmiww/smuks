
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; NOTE: Command i usually use to fire up a debug session via ssh (xdg session id might differ)
;; DEBUG_SMUKS=1 XDG_SESSION_ID=1 sbcl --dynamic-space-size 4028 --eval '(ql:quickload :swank)' --eval '(swank:create-server :port 25252 :dont-close t)' --eval '(ql:quickload :smuks)'

;; TODO: Libwayland does some annoying shared memory creation by opening a descriptor to this file: /memfd:wayland-cursor
;; This isn't being cleaned up - and i have no clue why it's being created in the first place.
;; Afaik - i'm creating the mmaps myself.

;; TODO: There is an extra fd for a dmabuf hanging around after closing a window. I though't i'm cleaning those up.
;; Maybe its libwayland again somehow???
;; Or egl is doing something weird.

(in-package :smuks)

(defvar *enable-wayland-debug-logs* "")
(defvar *enable-mesa-debug-logs* "")
(defvar *enable-egl-debug-logs* "fatal") ;; "debug"

(defvar *display* nil)
(defvar *udev* nil)
(defvar *udev-monitor* nil)
(defvar *libinput* nil)
(defvar *gl-version* nil)
(defvar *socket-path* nil)
(defvar *socket* nil)
(defvar *cursor* nil)
(defvar *egl* nil)
(defvar *egl-context* nil)
(defvar *running* nil)

#+xwayland
(defvar *xwayland-process* nil)

(defun main ()
  (unwind-protect (mainer)
    (cleanup)))

(defun mainer ()
  (setf *log-output* *standard-output*)
  (heading)

  (setf (uiop/os:getenv "WAYLAND_DEBUG") *enable-wayland-debug-logs*)
  (setf (uiop/os:getenv "MESA_DEBUG") *enable-mesa-debug-logs*)
  (setf (uiop/os:getenv "EGL_LOG_LEVEL") *enable-egl-debug-logs*)

  (setf *socket* (init-socket))
  (setf *display* (make-instance 'display :fd (unix-sockets::fd *socket*)))

  (with-accessors ((libseat libseat) (drm drm)) *display*
    (unless (setf libseat (libseat:open-seat :enable-seat 'enable-seat :disable-seat 'disable-seat :log-handler t))
      (error "Failed to open seat."))
    (libseat:dispatch libseat 0)

    (unless (setf drm (init-drm 'open-device 'close-device))
      (error "Failed to initialize DRM"))

    (unless (setf *libinput* (make-instance 'dev-track :open-restricted 'open-device :close-restricted 'close-device))
      (error "Failed to initialize libinput"))

    ;; #+xwayland
    ;; (setf *xwayland-process* (uiop:launch-program "Xwayland"))

    (setf (values *egl* *egl-context* *gl-version*) (init-egl (gbm-pointer drm) (wl:display-ptr *display*)))
    (setf (egl *display*) *egl*)
    (setf (gl-version *display*) *gl-version*)

    #+smuks-debug
    (sglutil::enable-gl-debug)

    (setf *cursor* (load-cursor-texture))

    (cl-async:start-event-loop
     (lambda ()
       (setf *udev* (udev:udev-new))
       (setf *udev-monitor* (udev:monitor-udev *udev*))
       (udev::%monitor-enable-receiving *udev-monitor*)

       (init-globals *display*)
       (start-monitors *display*)

       (pollr "drm"            (fd drm)                     (cb (drm:handle-event (fd (drm *display*)) :page-flip2 'set-frame-ready)))
       (pollr "wayland events" (wl:event-loop-fd *display*) (cb (handle-wayland-event)))
       (pollr "input event"    (context-fd *libinput*)      (cb (smuks::dispatch *libinput* 'handle-input)))
       (pollr "seat/session"   (libseat:get-fd libseat)     (cb (libseat:dispatch (libseat *display*) 0)))
       (pollr "udev"           (udev:get-fd *udev-monitor*) (cb (handle-device-changes *udev-monitor*)))
       (pollr "wayland client"
	      (unix-sockets::fd *socket*)
	      (cb
		(handler-case
		    (wl:create-client
		     *display*
		     (unix-sockets::fd (unix-sockets:accept-unix-socket *socket*)) :class 'client)
		  (unix-sockets:unix-socket-error (e)
		    (if (eql (unix-sockets::get-errno e) 11)
			(wrn! "Ignoring an EAGAIN error on accept")
			(signal e))))
		'client-cb)
	      :socket t)

       (run-user-setup)

       (setf *running* t)
       (cl-async:delay 'livesupport-recursively :time 0.016)))))

(defun livesupport-recursively ()
  (livesupport:update-repl-link)
  (when *running*
    (cl-async:delay 'livesupport-recursively :time 0.016)))


;; ┬ ┬┌─┐┌─┐┬─┐  ┌─┐┌─┐┌┬┐┬ ┬┌─┐
;; │ │└─┐├┤ ├┬┘  └─┐├┤  │ │ │├─┘
;; └─┘└─┘└─┘┴└─  └─┘└─┘ ┴ └─┘┴
(defun run-user-setup ()
  (load (merge-pathnames "user-setup.lisp" (asdf:system-source-directory :smuks))))


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐  ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ├┤ └┐┌┘├┤ │││ │   ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; └─┘ └┘ └─┘┘└┘ ┴   ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defun handle-device-changes (monitor)
  (loop for dev = (udev:receive-device monitor)
	  while dev
	  do (cond
	       ((string= (udev:dev-subsystem dev) "drm")
		;; NOTE: Only considering the drm node that was initt'd via my drm package
		(when (string=
		       (format nil "/dev/dri/~a" (udev:dev-sys-name dev))
		       (sdrm:primary-node (drm *display*)))
		  (sleep 0.5) (handle-drm-device-event dev)))
	       ((string= (udev:dev-subsystem dev) "input")
		(when (string= (udev:dev-action dev) "add")
		  (process-added-device dev))))))

(defun handle-drm-device-event (dev)
  (declare (ignore dev))
  (init-outputs *display* t))


(defun handle-input (event)
  ;; device removed seems to be called when switching VTs/sessions
  (if (eq (event-type event) :device-removed)
      (rem-device-abandoned
       *libinput*
       (merge-pathnames (format nil "/dev/input/~a"
				(device-removed@-sys-name event))))
      (restart-case (input *display* (event-type event) event)
	(abandon-event () :report "Abandoning event"))))

(defun handle-wayland-event ()
  (let ((result (wl:dispatch-event-loop *display*)))
    (when (< result 0)
      (error "Error in wayland event loop dispatch: ~A" result))))

(defun set-frame-ready (a b c d crtc-id f)
  (declare (ignore a b c d f))
  (let ((output (output-by-crtc *display* crtc-id)))
    (when output (render-frame *display* output))))

(defun process-added-device (dev)
  (when (str:contains? "event" (udev:dev-sys-name dev))
    (add-device
     *libinput*
     (merge-pathnames (format nil "/dev/input/~a" (udev:dev-sys-name dev))))))

;; ┌─┐┌─┐┌─┐┬┌─┌─┐┌┬┐
;; └─┐│ ││  ├┴┐├┤  │
;; └─┘└─┘└─┘┴ ┴└─┘ ┴
(defun socket-path (index) (format nil "~a/wayland-~a" (uiop:getenv "XDG_RUNTIME_DIR") index))
(defun init-socket ()
  (let (socket-num socket)
    (loop for i from 0
	  unless (probe-file (socket-path i)) return (setf socket-num i))

    (setf *socket-path* (socket-path socket-num))
    (setf socket (unix-sockets:make-unix-socket *socket-path*))
    (setf (uiop/os:getenv "WAYLAND_DISPLAY") (format nil "wayland-~a" socket-num))
    socket))


;; ┌─┐┌─┐┌─┐┌┬┐  ┌┬┐┌─┐┌─┐┬┌─┐
;; └─┐├┤ ├─┤ │   │││├─┤│ ┬││
;; └─┘└─┘┴ ┴ ┴   ┴ ┴┴ ┴└─┘┴└─┘
(defun enable-seat (seat data)
  (declare (ignore seat data))
  (when *display* (resume-outputs *display*))
  (when *libinput* (init-devices *libinput*)))

(defun disable-seat (seat data)
  "Called when a seat is 'disabled'. One instance of this is when switching to a different VT"
  (declare (ignore seat data)))

;; Opening and closing restricted devices
(defvar *fd-id* (make-hash-table :test 'equal))
(defun open-device (path &optional flags user-data)
  (declare (ignore flags user-data))
  ;; TODO: Although apparently id and fd are the same
  ;; At least as far as logind is concerned. Not sure about seatd.
  ;; For now leaving as is - since it's not really a problem.
  (let ((id nil) (fd nil))
    (setf (values id fd) (libseat:open-device (libseat *display*) path))
    (unless id (error "Failed to open device. No ID: ~A" path))
    (unless fd (error "Failed to open device. No FD: ~A" path))
    (setf (gethash fd *fd-id*) id)))

(defun close-device (fd &optional data)
  (declare (ignore data))
  (libseat:close-device (libseat *display*) (gethash fd *fd-id*))
  (remhash fd *fd-id*))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun load-cursor-texture ()
  (load-png-texture (merge-pathnames "assets/mouse.png" (asdf:system-source-directory :smuks))))


;; ┌─┐┬  ┌─┐┌─┐┬┌┐┌┌─┐
;; │  │  │ │└─┐│││││ ┬
;; └─┘┴─┘└─┘└─┘┴┘└┘└─┘
(defun shutdown () (cl-async:exit-event-loop) (setf *running* nil))

(defun cleanup ()
  #+xwayland
  (when *xwayland-process* (uiop:terminate-process *xwayland-process*))

  (when (and *egl* *egl-context*) (seglutil:cleanup-egl *egl* (wl:display-ptr *display*) *egl-context*))
  (when *libinput* (destroy *libinput*))

  (when *display* (cleanup-display *display*))

  (when *socket*
    (unix-sockets:close-unix-socket *socket*)
    (delete-file *socket-path*))

  (setfnil *egl* *egl-context*
	   *display* *socket* *cursor* *libinput*))
