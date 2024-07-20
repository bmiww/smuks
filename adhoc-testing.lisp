
(in-package :smuks)

(defun open-close-window ()
  (bt:make-thread
   (lambda ()
     (let ((process (uiop:launch-program "weston-terminal")))
       (sleep 3)
       (uiop:terminate-process process)
       (when *open-close-continue* (open-close-window))))))
