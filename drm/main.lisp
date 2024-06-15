
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; NOTE: Some nice examples on different framebuffer/modeset handling
;; https://github.com/dvdhrm/docs/blob/master/drm-howto/modeset-atomic.c
;; TODO: Check example above and attempt the atomic modeset.
;; TODO: Check device capabilities to see if DRM_CAP_CRTC_IN_VBLANK_EVENT is supported.
;; Without it - page_flip_handler2 will not work.
(in-package :smuks-drm)

(defclass gbm-device ()
  ((fd :initarg :fd :accessor fd)
   (fd-stream :initarg :fd-stream :accessor fd-stream)
   (resources :initarg :resources :accessor resources)
   (gbm-pointer :initarg :gbm-pointer :accessor gbm-pointer)
   ;; TODO: Unused for now? Supposed to be DRM lib framebuffers
   ;; Related to planes? Like cursor plane and stuff?
   (framebuffers :initarg :framebuffers :accessor framebuffers)
   (render-node :initarg :render-node :accessor render-node)
   (crtcs :initarg :crtcs :accessor crtcs)
   (crtc :initarg :crtc :accessor crtc)
   (connectors :initarg :connectors :accessor connectors)
   (encoders :initarg :encoders :accessor encoders)
   (capabilities :initarg :capabilities :accessor capabilities)))

(defmethod initialize-instance :after ((device gbm-device) &key file)
  ;; TODO: SBCL EXCLUSIVE
  (let ((fd (sb-sys:fd-stream-fd file)))
    (drm::set-master fd)
    (setf (fd-stream device) file)
    (setf (fd device) fd)
    (setf (gbm-pointer device) (gbm:create-device fd))
    (let ((resources (setf (resources device) (drm:get-resources fd))))
      (setf (capabilities device) (drm::resources-capabilities resources))
      (unless (setf (crtcs device) (loop for crtc in (drm:resources-crtcs resources) collect (init-crtc crtc)))
	(error "No CRTCs found"))
      (unless (setf (encoders device) (loop for encoder in (drm:resources-encoders resources) collect (init-encoder encoder)))
	(error "No connectors found"))
      (unless (setf (connectors device) (loop for connector in (drm:resources-connectors resources) collect (init-connector connector (encoders device) (crtcs device))))
	(error "No encoders found")))))


(defmethod recheck-resources ((device gbm-device))
  (let* ((resources (setf (resources device) (drm:get-resources (fd device))))
	 (crtcs (loop for crtc in (drm:resources-crtcs resources) collect (init-crtc crtc)))
	 (encoders (loop for encoder in (drm:resources-encoders resources) collect (init-encoder encoder)))
	 (connectors (loop for connector in (drm:resources-connectors resources) collect (init-connector connector encoders crtcs))))

    (setf (crtcs device) crtcs)
    (setf (encoders device) encoders)
    (loop for connector in connectors
	  for old-connector = (find-if (lambda (old-connector) (eq (id old-connector) (id connector))) (connectors device))
	  do (progn
	       (setf (crtc old-connector) (crtc connector))
	       (setf (encoder old-connector) (encoder connector))
	       (setf (encoders old-connector) (encoders connector))
	       (setf (subpixel-order old-connector) (subpixel-order connector))
	       (setf (connection old-connector) (connection connector))
	       (setf (mm-width old-connector) (mm-width connector))
	       (setf (mm-height old-connector) (mm-height connector))
	       (setf (modes old-connector) (modes connector))
	       (setf (properties old-connector) (properties connector))))

    (connectors device)))

(defun add-framebuffer (fd width height depth bpp pitch handle)
  (cffi:with-foreign-objects ((buf-id :uint32 1))
    (drm::mode-add-framebuffer fd width height depth bpp pitch handle buf-id)
    (cffi:mem-ref buf-id :uint32)))

(defun set-crtc! (fd fb connector)
  (let* ((crtc (crtc connector))
	 (result (drm:set-crtc
		  fd (id crtc)
		  fb 0 0
		  (list (id connector))
		  (ptr (mode crtc)))))
    (unless (eq 0 result) (error (format nil "Failed to set crtc: error ~a" result)))))

(defmethod page-flip ((drm gbm-device) framebuffer connector)
  (let* ((crtc (crtc connector))
	 (result (- (drm::mode-page-flip (fd drm) (id crtc)
					 framebuffer
					 :page-flip-event
					 (cffi:null-pointer))))
	 (error-msg (match-kernel-errcode result)))
    (when error-msg (error (format nil "Page flip:: ~a:~a~%" result error-msg)))))


;; TODO; Perhaps have DRM control more than one card?
;; TODO: This is mostly smuks specific - but parts of this file could go to the lib
(defun init-drm ()
  (let* ((devices (drm:get-devices))
	 (first (first devices)))
    (unless first (error "No DRM capable graphics cards found"))
    (let* ((fd (open (drm::device!-primary first) :direction :io :if-exists :append))
	   (device (make-instance 'gbm-device :file fd :render-node (drm::device!-render first)))
	   (caps (capabilities device)))
      (unless (getf caps :crtc-in-vblank-event)
	(error "CRTC_IN_VBLANK_EVENT missing. Needed for page-flip2. Not strictly necessary, required in smuks."))
      device)))

;; ┌─┐┌─┐┬  ┌─┐┌─┐┌┬┐┌─┐┬─┐┌─┐
;; └─┐├┤ │  ├┤ │   │ │ │├┬┘└─┐
;; └─┘└─┘┴─┘└─┘└─┘ ┴ └─┘┴└─└─┘
(defmethod connected-connectors ((device gbm-device))
  (loop for connector in (connectors device)
	when (eq :connected (connection connector)) collect connector))


;; ┌─┐┬─┐┌─┐┌┬┐┌─┐┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;; ├┤ ├┬┘├─┤│││├┤ ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; └  ┴└─┴ ┴┴ ┴└─┘└─┘└─┘└  └  └─┘┴└─
(defstruct framebuffer id buffer)

(defvar *bo-flags* (logior gbm::BO_USE_SCANOUT gbm::BO_USE_RENDERING))

(defun create-bo! (device width height) (gbm:bo-create (gbm-pointer device) width height gbm::FORMAT_XRGB8888 *bo-flags*))
(defun destroy-bo (buffer-object) (check-err (gbm:bo-destroy buffer-object)))

;; TODO: Figure out bpp. For now, just hardcoding it
;; TODO: Figure out depth. For now, just hardcoding it
(defun create-connector-framebuffer (device connector)
  "By default uses the first available mode"
  (let ((mode (car (modes connector)))
	(crtc (crtc connector)))
    (when (and mode crtc)
      (setf (mode crtc) mode)
      (let* ((width (hdisplay mode))
	     (height (vdisplay mode))
	     (buffer-object (create-bo! device width height))
	     (bpp 32) (depth 24))
	(make-framebuffer :id (add-framebuffer (fd device) width height depth bpp
					       (gbm:bo-get-stride buffer-object)
					       (gbm:bo-get-handle buffer-object))
			  :buffer buffer-object)))))


;; ┌─┐┬  ┌─┐┌─┐┌┐┌┬ ┬┌─┐
;; │  │  ├┤ ├─┤││││ │├─┘
;; └─┘┴─┘└─┘┴ ┴┘└┘└─┘┴
(defmethod close-drm ((device gbm-device))
  (setf (resources device) nil)
  (check-err (gbm:device-destroy (gbm-pointer device)))
  (check-err (drm::drop-master (fd device)))
  (close (fd-stream device)))

(defun rm-framebuffer! (device fb buffer)
  (destroy-bo buffer)
  (check-err (drm::mode-remove-framebuffer (fd device) fb)))
