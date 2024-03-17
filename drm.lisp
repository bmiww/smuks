
(in-package :smuks)

(defvar *default-card* "/dev/dri/card0")

(defclass gbm-device ()
  ((fd :initarg :fd :accessor fd)
   (framebuffers :initarg :framebuffers :accessor framebuffers)
   (crtcs :initarg :crtcs :accessor crtcs)
   (connectors :initarg :connectors :accessor connectors)
   (encoders :initarg :encoders :accessor encoders)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod initialize-instance :after ((device gbm-device) &key fd)
  (setf (slot-value device 'fd) (gbm:create-device fd))
  (let ((resources (drm:mode-get-resources fd)))))


(defmethod close-drm ((device gbm-device))
  (gbm:device-destroy (fd device)))


(defun init-drm (&optional (card *default-card*))
  (make-instance 'gbm-device :fd (open card :direction :io)))
