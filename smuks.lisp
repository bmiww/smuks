
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
(defvar *drm-thread* nil)
(defvar *egl* nil)
(defvar *main-vbo* nil)
(defvar *egl-image* nil)
(defvar *frame-buffer* nil)
(defvar *gl-frame-buffer* nil)
(defvar *texture* nil)
(defvar *shaders* nil)
(defvar *active-crtc* nil)

(defvar *client-thread* nil)

(defun kill-all-threads ()
  (mapcar (lambda (thread) (thread:destroy-thread thread)) (thread:all-threads)))

(defvar *test-program* "weston-terminal")
;; (defvar *test-program* "weston-flower")
;; (defvar *test-program* "kitty")

;; NOTE: Some very simple WL client you found here
;; https://github.com/emersion/hello-wayland/blob/master/main.c
;; (defvar *test-program* "hello-wayland")

(defun shutdown () (setf *smuks-exit* t) (cleanup))
(defun cleanup ()
  ;; TODO: This kills off the client listener rather ungracefully
  (when *client-thread* (bt:destroy-thread *client-thread*) (setf *client-thread* nil))
  (when (and *drm-thread* (bt:thread-alive-p *drm-thread*)) (bt:destroy-thread *drm-thread*) (setf *drm-thread* nil))
  (when *active-crtc* (free-crtc *drm-dev*) (setf *active-crtc* nil))
  (when *drm-dev* (close-drm *drm-dev*) (setf *drm-dev* nil)))

  ;; TODO: Add cleanup/restarts for crtc grabs
(defun main ()
  (setf *log-output* *standard-output*)
  (setf *smuks-exit* nil)
  (heading)
  (cleanup)

  ;; NOTE: Maybe setup kill signals for the process
  ;; TODO: Maybe add a "restart" to set *smuks-exit* to true
  ;; (mapcar (lambda (signal) (sb-sys:enable-interrupt signal (lambda () (setf *smuks-exit* t)))) '(SIGINT SIGTERM))

  (smuks-wl:reset-globals)

  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'smuks-wl:wayland))
  (setf *drm-dev* (init-drm))
  (setf *egl* (init-egl *drm-dev*))

  (setf (values *frame-buffer* *egl-image*) (create-framebuffer *egl* *drm-dev*))
  (setf (values *gl-frame-buffer* *texture*) (create-gl-framebuffer *egl-image*))

  (setf (values *main-vbo* *shaders*) (prep-gl-implementation *drm-dev* *frame-buffer*))

  (setf *active-crtc* (set-crtc *drm-dev* *frame-buffer*))

  ;; TODO: For now disabling since it seems to be locking up other threads from erroring out for some reason...
  (log! "Starting DRM fd listener. Waiting for events...~%")
  (setf *drm-thread* (bt:make-thread 'drm-listener))

  (log! "Starting wayland socket listener. Waiting for clients...~%")
  (setf *client-thread* (bt:make-thread 'client-listener))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") *socket-file*)

  ;; (test-app *test-program*)

  (livesupport:continuable
    (loop while (not *smuks-exit*)
	  do (render-frame))))


(defun render-frame ()
  (livesupport:update-repl-link)
  (sleep 1)
  (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
  (gl:clear :color-buffer-bit)
  (drm-page-flip *drm-dev* *frame-buffer*))


;; ┬  ┬┌─┐┌┬┐┌─┐┌┐┌┌─┐┬─┐┌─┐
;; │  │└─┐ │ ├┤ │││├┤ ├┬┘└─┐
;; ┴─┘┴└─┘ ┴ └─┘┘└┘└─┘┴└─└─┘
;; TODO: Unfinished. Still in debug mode
(defun drm-listener ()
  (let ((buffer (cffi:foreign-alloc :uint8 :count 1024)))
    (loop while (not *smuks-exit*)
	  do (process-drm-message buffer))))

(defun process-drm-message (buffer)
  (let ((count (sb-unix:unix-read (fd *drm-dev*) buffer 1024)))
    (when count
	       ;; (loop for i from 0 below count
		     ;; do (log! "~a" (cffi:mem-ref buffer :uint8 i)))
      ;; (log! "~%"))
      )))

(defun client-listener ()
  (loop until *smuks-exit*
	do (let* ((client (unix-sockets:accept-unix-socket *socket*)))
	     (smuks-wl:add-client *wayland* client))))

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
     (sleep 1)
     (log! "🟢 ~a: Starting an app~%" app-name)
     (let ((process (uiop:launch-program `(,app-name) :output :stream :error-output *standard-output*)))
       (loop while (uiop/launch-program:process-alive-p process)
	     do (log! "🔴 ~a: ~a~%" app-name (uiop/stream:slurp-stream-string (uiop:process-info-output process))))
       (log! "🟢 ~a: Client exit. Code: ~a~%" app-name (uiop:wait-process process))))))
