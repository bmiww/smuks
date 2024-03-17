
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")
(defvar *swank-server* nil)

;; (defun main ()
  ;; (print "derpa")
  ;; (main-glfw))
(defun main ()
  (unless *swank-server*
    (setf *swank-server* (swank:create-server :port 25252 :dont-close t)))
  ;; (swank:create-server :port 25252)
  (init-wayland)
  (init-drm)
  (init-egl)


  (livesupport:continuable
    (loop while t
	  do
	     (livesupport:update-repl-link)
	     (magic)
	     )))

(defun magic ()
  (sleep 1))

(defun init-socket (window)
  (restart-case
      (if (probe-file *socket-file*)
	  (error "Socket file already exists")
	  (setf (socket window) (unix-sockets:make-unix-socket *socket-file*)))
    (create-new-socket ()
      :report "Create new socket"
      (format t "Creating new socket~%")
      (delete-file *socket-file*)
      (setf (socket window) (unix-sockets:make-unix-socket *socket-file*))))
  (setf (uiop/os:getenv "DISPLAY") *socket-file*))




;; ██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;; ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;; ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;;  ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝

(defun init-wayland ()
  (let* ((display (wlc:wl-display-create)))))



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




;;  ██████╗ ██████╗ ███╗   ███╗
;; ██╔════╝ ██╔══██╗████╗ ████║
;; ██║  ███╗██████╔╝██╔████╔██║
;; ██║   ██║██╔══██╗██║╚██╔╝██║
;; ╚██████╔╝██████╔╝██║ ╚═╝ ██║
;;  ╚═════╝ ╚═════╝ ╚═╝     ╚═╝

(defun main-drm ()
  (print "Starting Smuks DRM backend")
  )






;;  ██████╗ ██╗     ███████╗██╗    ██╗
;; ██╔════╝ ██║     ██╔════╝██║    ██║
;; ██║  ███╗██║     █████╗  ██║ █╗ ██║
;; ██║   ██║██║     ██╔══╝  ██║███╗██║
;; ╚██████╔╝███████╗██║     ╚███╔███╔╝
;;  ╚═════╝ ╚══════╝╚═╝      ╚══╝╚══╝

(defun main-glfw ()
  (print "doooo")
  (glfw:init)
  (print "nop")
  (unwind-protect
       (print "yep")
       (let ((window (make-instance 'window :width 800 :height 600 :title "Hello wayland")))
	 (init-socket window)

         (loop until (glfw:should-close-p window)
               do (print "hurpa")
		  (glfw:poll-events)
		  (glfw:swap-buffers window)))
    (glfw:shutdown)))



(defclass window (glfw:window)
  ((socket :initform :socket :accessor socket)))

(defmethod glfw:window-resized ((window window) width height)
  ;; (call-next-method)
  (gl:viewport 0 0 width height))

(defmethod glfw:key-changed ((window window) key scancode action mods)
  (case key
    ((:escape) (setf (glfw:should-close-p window) t))))
