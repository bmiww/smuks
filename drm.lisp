
(defpackage :smuks-drm
  (:use :cl :smuks-util)
  (:nicknames :sdrm)
  (:export
   width height connectors crtc crtcs fd gbm-pointer
   close-drm add-framebuffer free-crtc set-crtc
   init-drm drm-page-flip))
(in-package :smuks-drm)
;; (declaim (optimize (speed 0) (safety 0) (debug 3) (compilation-speed 0)))

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
   (height :initarg :height :accessor height)
   (original-crtc :initarg :original-crtc :accessor original-crtc)))

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



;; TODO: Free connector is expecting a pointer
;; but receiving a full connector structure
(defmethod close-drm ((device gbm-device) framebuffer buffer-object)
  (print "STARTING SET")
  (restart-case (set-original-crtc device framebuffer)
    (continue-cleanup () :report "Continue cleanup ignoring the error"))

  (print "STARTING REMOVE FB")
  (drm::mode-remove-framebuffer (fd device) framebuffer)

  (drm::free-resources (resources device))
  (gbm:bo-destroy buffer-object)
  (gbm:device-destroy (gbm-pointer device))
  (close (fd-stream device)))

(defun add-framebuffer (fd width height depth bpp pitch handle)
  (cffi:with-foreign-objects ((buf-id :uint32 1))
    (drm::mode-add-framebuffer fd width height depth bpp pitch handle buf-id)
    (cffi:mem-ref buf-id :uint32)))

(defmethod connected-connectors ((device gbm-device))
  (loop for connector in (connectors device)
	when (eq :connected (drm::connector!-connection connector))
	  collect connector))

(defmethod set-original-crtc ((device gbm-device) framebuffer)
  (let* ((crtc (crtc device))
	 (connector (car (connected-connectors device)))
	 (result (drm:set-crtc
		  (fd device)
		  (drm::crtc!-id crtc)
		  framebuffer
		  (drm::crtc!-x crtc)
		  (drm::crtc!-y crtc)
		  (list (drm::connector!-id connector))
		  (drm::crtc!-mode-ptr crtc))))

    (unless (eq 0 result) (error (format nil "Failed to set original CRTC. ERR: ~a~%" result)))
    crtc))



(defmethod set-crtc ((device gbm-device) framebuffer)
  (let* ((crtc (crtc device))
	 (connector (car (connected-connectors device)))
	 (modes (drm::connector!-modes connector)))

    (let ((result (drm:set-crtc
		   (fd device)
		   (drm::crtc!-id crtc)
		   framebuffer
		   0 0
		   (list (drm::connector!-id connector))
		   ;; TODO: I'm using the pointer to all the modes in hopes that i the first one is valid
		   ;; And that set-crtc will read exactly the first one
		   modes)))
      (unless (eq 0 result) (error (format nil "Failed to set crtc: error ~a" result)))
      crtc)))

(defun init-drm ()
  (let ((card (loop for i from 0 below 32
		    for path = (format nil "/dev/dri/card~A" i)
		    when (probe-file path) return path)))
    (make-instance 'gbm-device :file (open card :direction :io :if-exists :append))))


(defun drm-page-flip (drm-dev framebuffer)
  (let* ((result (- (drm::mode-page-flip (fd drm-dev)
					 (drm::crtc!-id (crtc drm-dev))
					 framebuffer
					 :page-flip-event
					 (cffi:null-pointer))))
	 (error-msg (match-kernel-errcode  result)))
    (when error-msg (error (format nil "Page flip:: ~a:~a~%" result error-msg)))))
