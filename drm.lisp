
(in-package :smuks)

(defvar *default-card* "/dev/dri/card0")

(defclass gbm-device ()
  ((fd :initarg :fd :accessor fd)
   (gbm-pointer :initarg :gbm-pointer :accessor gbm-pointer)
   (framebuffers :initarg :framebuffers :accessor framebuffers)
   (crtcs :initarg :crtcs :accessor crtcs)
   (crtc :initarg :crtc :accessor crtc)
   (connectors :initarg :connectors :accessor connectors)
   (encoders :initarg :encoders :accessor encoders)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod initialize-instance :after ((device gbm-device) &key file)
  (let ((fd (SB-SYS:FD-STREAM-FD file)))
    (setf (fd device) fd)
    (setf (gbm-pointer device) (gbm:create-device fd))
    (let* ((resources (drm:get-resources fd))
	   (crtcs (drm:resources-crtcs resources))
	   (valid (find-if (lambda (crtc) (getf crtc 'drm::mode-valid)) crtcs)))
      (unless crtcs (error "No CRTCs found"))

      (setf (slot-value device 'crtcs) crtcs)
      (setf (slot-value device 'crtc) valid)
      (setf (slot-value device 'width) (getf valid 'drm::width))
      (setf (slot-value device 'height) (getf valid 'drm::height)))))


;; TODO: Check if you need to close any of the drm resources
(defmethod close-drm ((device gbm-device))
  (gbm:device-destroy (fd device)))


(defun add-framebuffer (fd width height depth bpp pitch handle)
  (cffi:with-foreign-objects ((buf-id '(:pointer :uint32) 0))
    (drm::mode-add-framebuffer fd width height depth bpp pitch handle buf-id)
    (cffi:mem-ref buf-id '(:pointer :uint32))))

(defun init-drm ()
  (let ((card (loop for i from 0 below 32
		    for path = (format nil "/dev/dri/card~A" i)
		    when (probe-file path) return path)))
    (make-instance 'gbm-device :file (open card :direction :io :if-exists :append))))
