
(defpackage :smuks-drm
  (:use :cl :smuks-util)
  (:nicknames :sdrm)
  (:export
   screen-width screen-height
   width height connectors fd gbm-pointer
   crtc crtcs
   close-drm

   add-framebuffer rm-framebuffer default-framebuffer
   framebuffer-id framebuffer-buffer

   free-crtc set-crtc
   create-bo destroy-bo
   init-drm drm-page-flip))
(in-package :smuks-drm)

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

(defmethod screen-width ((device gbm-device) orientation)
  (case orientation (:landscape (height device)) (:portrait (width device))))
(defmethod screen-height ((device gbm-device) orientation)
  (case orientation (:landscape (width device)) (:portrait (height device))))

(defmethod initialize-instance :after ((device gbm-device) &key file)
  ;; TODO: SBCL EXCLUSIVE
  (let ((fd (sb-sys:fd-stream-fd file)))
    (drm::set-master fd)
    (setf (fd-stream device) file)
    (setf (fd device) fd)
    (setf (gbm-pointer device) (gbm:create-device fd))
    (let* ((resources  (setf (resources device) (drm:get-resources fd)))
	   (crtcs      (setf (crtcs device) (drm:resources-crtcs resources)))
	   (connectors (setf (connectors device) (drm:resources-connectors resources)))
	   (valid      (find-if (lambda (crtc) (> (drm::crtc!-mode-valid crtc) 0)) crtcs)))

      (unless crtcs      (error "No CRTCs found"))
      (unless connectors (error "No connectors found"))
      (unless valid      (error "No valid CRTCs found"))

      (setf (crtc device) valid)
      (setf (width device) (drm::crtc!-width valid))
      (setf (height device) (drm::crtc!-height valid)))))



;; TODO: Free connector is expecting a pointer
;; but receiving a full connector structure
(defmethod close-drm ((device gbm-device))
  (check-err (drm::free-resources (resources device)))
  (check-err (gbm:device-destroy (gbm-pointer device)))
  (check-err (drm::drop-master (fd device)))
  (close (fd-stream device)))

(defun add-framebuffer (fd width height depth bpp pitch handle)
  (cffi:with-foreign-objects ((buf-id :uint32 1))
    (drm::mode-add-framebuffer fd width height depth bpp pitch handle buf-id)
    (cffi:mem-ref buf-id :uint32)))

(defmethod connected-connectors ((device gbm-device))
  (loop for connector in (connectors device)
	when (eq :connected (drm::connector!-connection connector))
	  collect connector))

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
  (let* ((card (loop for i from 0 below 32
		     for path = (format nil "/dev/dri/card~A" i)
		     when (probe-file path) return path))
	 (fd (open card :direction :io :if-exists :append)))
    (make-instance 'gbm-device :file fd)))


(defun drm-page-flip (drm-dev framebuffer)
  (let* ((result (- (drm::mode-page-flip (fd drm-dev)
					 (drm::crtc!-id (crtc drm-dev))
					 framebuffer
					 :page-flip-event
					 (cffi:null-pointer))))
	 (error-msg (match-kernel-errcode result)))
    (when error-msg (error (format nil "Page flip:: ~a:~a~%" result error-msg)))))


;; ┌─┐┬─┐┌─┐┌┬┐┌─┐┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;; ├┤ ├┬┘├─┤│││├┤ ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; └  ┴└─┴ ┴┴ ┴└─┘└─┘└─┘└  └  └─┘┴└─
;; NOTE: For now - since we have a 1:1 mapping between how i use the buffer and the framebuffer
;; I'll store them together
(defstruct framebuffer id buffer)

(defvar *bo-flags* (logior gbm::BO_USE_SCANOUT gbm::BO_USE_RENDERING))
(defun create-bo (device)
  (gbm:bo-create (gbm-pointer device) (width device) (height device) gbm::FORMAT_XRGB8888 *bo-flags*))

(defun destroy-bo (buffer-object) (check-err (gbm:bo-destroy buffer-object)))

(defun default-framebuffer (device)
  (let* ((buffer-object (create-bo device))
	 (width (width device))
	 (height (height device))
	 (handle (gbm:bo-get-handle buffer-object))
	 (stride (gbm:bo-get-stride buffer-object))
	 (bpp 32) (depth 24))
    (make-framebuffer :id (add-framebuffer (fd device) width height depth bpp stride handle)
		      :buffer buffer-object)))

(defun rm-framebuffer (device framebuffer)
  (destroy-bo (framebuffer-buffer framebuffer))
  (check-err (drm::mode-remove-framebuffer (fd device) (framebuffer-id framebuffer))))
