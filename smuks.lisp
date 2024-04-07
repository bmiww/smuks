
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")
(defvar *socket* nil)
(defvar *wayland* nil)
(defvar *smuks-exit* nil)

(defvar *client-thread* nil)

(defun kill-all-threads ()
  (mapcar (lambda (thread) (thread:destroy-thread thread)) (thread:all-threads)))

(defvar *test-program* "weston-terminal")
;; (defvar *test-program* "weston-flower")
;; (defvar *test-program* "kitty")

(defun main ()
  (setf *smuks-exit* nil)
  ;; TODO: This kills off the client listener rather ungracefully
  (when *client-thread* (bt:destroy-thread *client-thread*) (setf *client-thread* nil))

  ;; NOTE: Maybe setup kill signals for the process
  ;; TODO: Maybe add a "restart" to set *smuks-exit* to true
  ;; (mapcar (lambda (signal) (sb-sys:enable-interrupt signal (lambda () (setf *smuks-exit* t)))) '(SIGINT SIGTERM))

  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'smuks-wl:wayland))
  (init-drm)
  ;; (init-egl (display *wayland*))

  (setf *client-thread*
	(bt:make-thread
	 (lambda ()
	   (format t "Starting wayland socket listener~%")
	   (loop until *smuks-exit*
		 do (let* ((client (unix-sockets:accept-unix-socket *socket*)))
		      (smuks-wl:add-client *wayland* client))))))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") *socket-file*)

  (test-app *test-program*)

  (livesupport:continuable
    (loop while t
	  do (livesupport:update-repl-link)
	     (magic))))

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
      (format t "Creating new socket~%")
      (delete-file *socket-file*)
      (unix-sockets:make-unix-socket *socket-file*))))


;; ███████╗ ██████╗ ██╗
;; ██╔════╝██╔════╝ ██║
;; █████╗  ██║  ███╗██║
;; ██╔══╝  ██║   ██║██║
;; ███████╗╚██████╔╝███████╗
;; ╚══════╝ ╚═════╝ ╚══════╝

(defun init-egl (wayland-display-ptr)
  (let* ((egl (egl:init-egl-wayland))
	 (display (egl:get-display (cffi:null-pointer))))
    (egl:bind-wayland-display display wayland-display-ptr)))




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
     (format t "🟢:~a: Starting an app~%" app-name)
     (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
       (loop while (uiop/launch-program:process-alive-p process)
	     do (format t "🔴:~a: ~a~%" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (format t "🟢:~a: Client exit. Code: ~a~%" app-name (uiop:wait-process process))))))
