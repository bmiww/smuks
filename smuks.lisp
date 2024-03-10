
(in-package :smuks)

(defun main ()
  (main-glfw))




(defun main-glfw ()
  (glfw:init)
  (unwind-protect
       (let ((window (make-instance 'window))
             ;; (last-time (glfw:timestamp))
             ;; (time-resolution (/ 1.0d0 (glfw:timestamp-resolution))))
             )
         ;; (declare (type (unsigned-byte 64) last-time))
         (loop until (glfw:should-close-p window)
               do (glfw:poll-events)
                  ;; (let* ((new-time (glfw:timestamp))
                         ;; (dt (* (- new-time last-time) time-resolution)))
                  ;; (declare (type (unsigned-byte 64) new-time))
                  ;; (setf last-time new-time)
		  (glfw:swap-buffers window)))
    (glfw:shutdown)))



(defclass window (glfw:window)
  ())

(defmethod glfw:window-resized ((window window) width height)
  ;; (call-next-method)
  (gl:viewport 0 0 width height))

(defmethod glfw:key-changed ((window window) key scancode action mods)
  (case key
    ((:escape) (setf (glfw:should-close-p window) t))))
