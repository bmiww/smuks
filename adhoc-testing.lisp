
(in-package :smuks)


(defun open-close-window ()
  (let ((process (uiop:launch-program "weston-terminal")))
    (sleep 3)
    (uiop:terminate-process process)
    (sleep 2)))

(defvar *open-close-continue* nil)
(defun stop-open-close-loop () (setf *open-close-continue* nil))
(defun open-close-loop ()
  (setf *open-close-continue* t)
  (bt:make-thread
   (lambda ()
     (loop while *open-close-continue*
	   do (open-close-window)))))
