
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

(defmethod initialize-instance :after ((device gbm-device) &key file)
  (let ((fd (SB-SYS:FD-STREAM-FD file)))
    (setf (slot-value device 'fd) (gbm:create-device fd))
    (let* ((resources (drm:get-resources fd))
	   (crtcs (drm:resources-crtcs resources))
	   (valid (find-if (lambda (crtc) (getf crtc 'drm::mode-valid)) crtcs)))
      (unless crtcs (error "No CRTCs found"))

      (setf (slot-value device 'crtcs) crtcs)
      (setf (slot-value device 'width) (getf valid 'drm::width))
      (setf (slot-value device 'height) (getf valid 'drm::height)))))

;; launch_open_device(primary, O_RDWR | O_CLOEXEC);


;; TODO: Check if you need to close any of the drm resources
(defmethod close-drm ((device gbm-device))
  (gbm:device-destroy (fd device)))


(defun init-drm (&optional (card *default-card*))
  (make-instance 'gbm-device :file (open card :direction :io :if-exists :append)))
