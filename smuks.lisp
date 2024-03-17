
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")
(defvar *socket* nil)
(defvar *wayland* nil)
(defvar *swank-server* nil)

(defun main ()
  (unless *swank-server*
    (setf *swank-server* (swank:create-server :port 25252 :dont-close t)))

  (setf *socket* (init-socket))
  (setf *wayland* (init-wayland (unix-sockets::fd *socket*)))
  (init-drm)
  (init-egl (display *wayland*))

  (setf (uiop/os:getenv "DISPLAY") *socket-file*)


  (thread:make-thread
   (lambda ()
     (sleep 3)
     (print "Starting an app thingy")
     (uiop:launch-program '("echo" "hello" "world") :output *standard-output*)
     (uiop:launch-program '("weston-terminal") :output *standard-output* :error *standard-output*)))

  (wlc:wl-display-run (display *wayland*))

  (livesupport:continuable
    (loop while t
	  do (livesupport:update-repl-link)
	     (magic))
    (cleanup-wayland *wayland*)))

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


;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝

(defclass wayland ()
  ((display :initarg :display :accessor display)
   (event-loop :initarg :event-loop :accessor event-loop)))

(cffi:defcallback handle-output-global-interface :void ((client :pointer) (data :pointer) (version :int32) (id :int32))
  (declare (ignore client data version id))
  (print "Implement output global interface")
  (error "Implement output global interface"))

(cffi:defcallback handle-compositor-global-interface :void ((client :pointer) (data :pointer) (version :int32) (id :int32))
  (declare (ignore client data version id))
  (print "Implement compositor global interface")
  (error "Implement compositor global interface"))

(cffi:defcallback handle-subcompositor-global-interface :void ((client :pointer) (data :pointer) (version :int32) (id :int32))
  (declare (ignore client data version id))
  (print "Implement subcompositor global interface")
  (error "Implement subcompositor global interface"))

(cffi:defcallback handle-shm-global-interface :void ((client :pointer) (data :pointer) (version :int32) (id :int32))
  (declare (ignore client data version id))
  (print "Implement shm global interface")
  (error "Implement shm global interface"))

(cffi:defcallback handle-seat-global-interface :void ((client :pointer) (data :pointer) (version :int32) (id :int32))
  (declare (ignore client data version id))
  (print "Implement seat global interface")
  (error "Implement seat global interface"))


(defun create-global (display interface version callback)
  (wlc:wl-global-create display interface version (cffi:null-pointer) (cffi:get-callback callback)))

(defun init-wayland (socket-fd)
  (let* ((display (wlc:wl-display-create))
	 (socket-status (wlc:wl-display-add-socket-fd display socket-fd)))
    ;; TODO: Check if the typings here actually make sense
    (unless socket-status (error "Failed to add socket fd"))
    (create-global display wlp:wl-compositor-interface 6 'handle-compositor-global-interface)
    (create-global display wlp:wl-subcompositor-interface 1 'handle-subcompositor-global-interface)
    ;; TODO: XDGWMBase
    (create-global display wlp:wl-shm-interface 1 'handle-shm-global-interface)

    (create-global display wlp:wl-seat-interface 9 'handle-seat-global-interface)

    (create-global display wlp:wl-output-interface 4 'handle-output-global-interface)
    (make-instance 'wayland :display display :event-loop (wlc:wl-event-loop-get-fd display))))

(defun cleanup-wayland (wayland)
  (wlc:wl-display-destroy (display wayland)))



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
