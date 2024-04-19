
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
  (print "Cleanup was called")
  ;; TODO: This kills off the client listener rather ungracefully
  ;; TODO: Turn this into a poller also
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
  (if *smuks-exit*
      (cl-async:exit-event-loop)
      (progn
	(render-frame)
	(cl-async:delay 'recursively-render-frame :time 0.016))))

(defun main ()
  (setf *log-output* *standard-output*)
  (heading)

  (setf *socket* (init-socket))
  (setf *wayland* (wl:display-create))
  ;; TODO: Can sometimes fail on retrying
  (setf *drm-dev* (init-drm))

  (restart-case (main-after-drm)
    (retry () (cleanup-egl) (main-after-drm) )))


(defun render-frame ()
  (livesupport:update-repl-link)
  (gl:bind-framebuffer :framebuffer *gl-frame-buffer*)
  (gl:clear :color-buffer-bit)
  (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect :x 10.0 :y 300.0 :w 100.0 :h 40.0
									 :color '(0.2 0.2 0.2 1.0))))
  (shaders.rectangle:draw *rect-shader* `(,(shaders.rectangle::make-rect :x 30.0 :y 500.0 :w 200.0 :h 50.0
									 :color '(0.2 0.9 0.2 1.0))))
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
