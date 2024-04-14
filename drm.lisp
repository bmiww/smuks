
(in-package :smuks)
(declaim (optimize (speed 0) (safety 0) (debug 3) (compilation-speed 0)))

(defvar *default-card* "/dev/dri/card0")

(defclass gbm-device ()
  ((fd :initarg :fd :accessor fd)
   (fd-stream :initarg :fd-stream :accessor fd-stream)
   (resources :initarg :resources :accessor resources)
   (gbm-pointer :initarg :gbm-pointer :accessor gbm-pointer)
   (framebuffers :initarg :framebuffers :accessor framebuffers)
   (crtcs :initarg :crtcs :accessor crtcs)
   (crtc :initarg :crtc :accessor crtc)
   (connectors :initarg :connectors :accessor connectors)
   (encoders :initarg :encoders :accessor encoders)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defmethod initialize-instance :after ((device gbm-device) &key file)
  ;; TODO: SBCL EXCLUSIVE
  (let ((fd (sb-sys:fd-stream-fd file)))
    (setf (fd-stream device) file)
    (setf (fd device) fd)
    (setf (gbm-pointer device) (gbm:create-device fd))
    (let* ((resources (setf (resources device) (drm:get-resources fd)))
	   (crtcs      (setf (crtcs device) (drm:resources-crtcs resources)))
	   (connectors (setf (connectors device) (drm:resources-connectors resources)))
	   (valid      (find-if (lambda (crtc) (> (drm::crtc!-mode-valid crtc) 0)) crtcs)))

      (unless crtcs (error "No CRTCs found"))
      (unless connectors (error "No connectors found"))
      (unless valid (error "No valid CRTCs found"))

      (setf (crtc device) valid)
      (setf (width device) (drm::crtc!-width valid))
      (setf (height device) (drm::crtc!-height valid)))))


;; TODO: Check if you need to close any of the drm resources
(defmethod close-drm ((device gbm-device))
  (drm::mode-free-connector (car (connected-connectors device)))
  (drm::mode-free-resources (drm::resources-resources (resources device)))
  (gbm:device-destroy (gbm-pointer device))
  (close (fd-stream device)))

(defun add-framebuffer (fd width height depth bpp pitch handle)
  (cffi:with-foreign-objects ((buf-id :uint32 1))
    (drm::mode-add-framebuffer fd width height depth bpp pitch handle buf-id)
    (cffi:mem-ref buf-id :uint32)))

(defmethod connected-connectors ((device gbm-device))
  (loop for connector in (connectors device)
	when (eq :connected (getf connector 'drm::connection))
	  collect connector))


(defmethod set-crtc ((device gbm-device) framebuffer)
  (let* ((crtc (crtc device))
	 (connector (car (connected-connectors device)))
	 (modes (getf connector 'drm::modes)))
    (let ((result (drm:set-crtc
		   (fd device)
		   (drm::crtc!-id crtc)
		   framebuffer
		   0 0
		   (list (getf connector 'drm::connector-id))
		   ;; TODO: I'm using the pointer to all the modes in hopes that i the first one is valid
		   ;; And that set-crtc will read exactly the first one
		   modes)))
      (unless (eq 0 result) (error (format nil "Failed to set crtc: error ~a" result)))
      crtc)))

(defmethod free-crtc ((device gbm-device)) (drm:free-crtc (crtc device)))


(defun init-drm ()
  (let ((card (loop for i from 0 below 32
		    for path = (format nil "/dev/dri/card~A" i)
		    when (probe-file path) return path)))
    (make-instance 'gbm-device :file (open card :direction :io :if-exists :append))))
