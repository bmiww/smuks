
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

(defun shutdown () (setf *smuks-exit* t))
(defun cleanup ()
  ;; TODO: This kills off the client listener rather ungracefully
  (when *client-thread* (bt:destroy-thread *client-thread*) (setf *client-thread* nil))
  (when *drm-poller* (cl-async:free-poller *drm-poller*) (setf *drm-poller* nil))
  (cleanup-egl)
  (cleanup-drm))

(defun cleanup-egl ()
  (when *egl* (egl:destroy-context *egl* *egl-context*) (setf *egl* nil) (setf *egl-context* nil)))

(defun cleanup-drm ()
  (when *drm-dev*
    (close-drm *drm-dev* *frame-buffer* *buffer-object*)
    (setf *drm-dev* nil) (setf *frame-buffer* nil) (setf *buffer-object* nil)))

(defun main-after-drm ()
  (setf (values *egl* *egl-context*) (init-egl *drm-dev*))

  (setf (values *frame-buffer* *egl-image* *buffer-object*) (create-framebuffer *egl* *drm-dev*))
  (setf (values *gl-frame-buffer* *texture*) (create-gl-framebuffer *egl-image*))

  (setf *main-vbo* (prep-gl-implementation *drm-dev* *frame-buffer*))
  (setf *rect-shader* (create-rect-shader *drm-dev*))

  (unless *active-crtc* (setf *active-crtc* (set-crtc *drm-dev* *frame-buffer*)))

  ;; (log! "Starting wayland socket listener. Waiting for clients...~%")
  ;; (setf *client-thread* (bt:make-thread 'client-listener))

  (setf (uiop/os:getenv "WAYLAND_DISPLAY") *socket-file*)

  ;; (test-app *test-program*)

  (livesupport:continuable
    (cl-async:start-event-loop
     (lambda ()
       (log! "Starting DRM fd listener. Waiting for events...~%")
       (setf *drm-poller* (drm-listener))
       (recursively-render-frame))))

  (setf *smuks-exit* nil)
  (cleanup))

(defun recursively-render-frame ()
  (when *smuks-exit* (cl-async:exit-event-loop))
  (render-frame)
  (cl-async:delay 'recursively-render-frame :time 0.016))

  ;; TODO: Add cleanup/restarts for crtc grabs
(defun main ()
  (setf *log-output* *standard-output*)
  (heading)

  ;; NOTE: Maybe setup kill signals for the process
  ;; TODO: Maybe add a "restart" to set *smuks-exit* to true
  ;; (mapcar (lambda (signal) (sb-sys:enable-interrupt signal (lambda () (setf *smuks-exit* t)))) '(SIGINT SIGTERM))

  (smuks-wl:reset-globals)

  (setf *socket* (init-socket))
  (setf *wayland* (make-instance 'smuks-wl:wayland))
  ;; TODO: Can sometimes fail on retrying
  (setf *drm-dev* (init-drm))

  (restart-case (main-after-drm)
    (retry () (cleanup-egl) (main-after-drm) )))


(defun render-frame ()
  (livesupport:update-repl-link)
  (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
  (gl:clear :color-buffer-bit)
  (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect :x 10.0 :y 400.0 :w 500.0 :h 550.0
									 :color '(0.2 0.2 0.2 1.0))))
  (gl:flush)
  (gl:finish))

;; ┬  ┬┌─┐┌┬┐┌─┐┌┐┌┌─┐┬─┐┌─┐
;; │  │└─┐ │ ├┤ │││├┤ ├┬┘└─┐
;; ┴─┘┴└─┘ ┴ └─┘┘└┘└─┘┴└─└─┘
(defun drm-listener () (cl-async:poll (fd *drm-dev*) 'drm-callback :poll-for '(:readable)))

(defun drm-callback (events)
  (when (member :readable events)
    (drm:handle-event (fd *drm-dev*) :page-flip 'page-flip)))

(defun page-flip () (drm-page-flip *drm-dev* *frame-buffer*))

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
