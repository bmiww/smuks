
(in-package :smuks)

(defvar *socket-file* "/tmp/smuks.socket")

(defun main ()
  (main-glfw))


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




(defun main-glfw ()
  (glfw:init)
  (unwind-protect
       (let ((window (make-instance 'window :width 800 :height 600 :title "Hello wayland")))
	 (init-socket window)

         (loop until (glfw:should-close-p window)
               do (glfw:poll-events)
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
