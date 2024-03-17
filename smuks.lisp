
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")
(defvar *socket* nil)
(defvar *display* nil)
(defvar *swank-server* nil)

(defun main ()
  (unless *swank-server*
    (setf *swank-server* (swank:create-server :port 25252 :dont-close t)))

  (setf *socket* (init-socket))
  (setf *display* (init-wayland (unix-sockets::fd *socket*)))
  (init-drm)
  (init-egl *display*)

  (setf (uiop/os:getenv "DISPLAY") *socket-file*)


  (wlc:wl-display-run *display*)

  (livesupport:continuable
    (loop while t
	  do (livesupport:update-repl-link)
	     (magic))
    (cleanup-wayland *display*)))

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

(defun init-wayland (socket-fd)
  (let* ((display (wlc:wl-display-create))
	 (socket-status (wlc:wl-display-add-socket-fd display socket-fd)))
    ;; TODO: Check if the typings here actually make sense
    (unless socket-status (error "Failed to add socket fd"))
    display))

(defun cleanup-wayland (display)
  (wlc:wl-display-destroy display))



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
