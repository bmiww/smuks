
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
(in-package :smuks-drm)

(defclass gbm-device ()
  ((fd :initarg :fd :accessor fd)
   (fd-stream :initarg :fd-stream :accessor fd-stream)
   (resources :initarg :resources :accessor resources)
   (gbm-pointer :initarg :gbm-pointer :accessor gbm-pointer)
   (framebuffers :initarg :framebuffers :accessor framebuffers) ;; TODO: Unused for now?
   (crtcs :initarg :crtcs :accessor crtcs)
   (crtc :initarg :crtc :accessor crtc)
   (connectors :initarg :connectors :accessor connectors)
   (encoders :initarg :encoders :accessor encoders)))

(defmethod initialize-instance :after ((device gbm-device) &key file)
  ;; TODO: SBCL EXCLUSIVE
  (let ((fd (sb-sys:fd-stream-fd file)))
    (drm::set-master fd)
    (setf (fd-stream device) file)
    (setf (fd device) fd)
    (setf (gbm-pointer device) (gbm:create-device fd))
    (let ((resources (setf (resources device) (drm:get-resources fd))))
      (unless (setf (crtcs device) (loop for crtc in (drm:resources-crtcs resources)
					 collect (init-crtc crtc)))
	(error "No CRTCs found"))
      (unless (setf (encoders device) (loop for encoder in (drm:resources-encoders resources)
					    collect (init-encoder encoder)))
	(error "No connectors found"))
      (unless (setf (connectors device) (loop for connector in (drm:resources-connectors resources)
					      collect (init-connector connector)))
	(error "No encoders found")))))

;; TODO: Make it possible to select encoder?
	 ;; For now - selecting the first one - since i haven't seen connectors have more than one yet
(defmethod connector-crtc ((device gbm-device) connector)
  (let* ((id (car (encoders connector)))
	 (matched (find-if (lambda (encoder) (eq (id encoder) id)) (encoders device)))
	 (crtc-id (crtc-id matched)))
    (find-if (lambda (crtc) (eq (id crtc) crtc-id)) (crtcs device))))




(defmethod recheck-resources ((device gbm-device))
  ;; (let* ((resources (setf (resources device) (drm:get-resources (fd device)))))
    ;; (loop for crtc in (drm:resources-crtcs resources)
    ;; do )

    ;; (setf (encoders device) (drm:resources-encoders resources))
    ;; (setf (connectors device) (drm:resources-connectors resources)))
  )




;; TODO: Free connector is expecting a pointer
;; but receiving a full connector structure
(defmethod close-drm ((device gbm-device))
  (setf (resources device) nil)
  (check-err (gbm:device-destroy (gbm-pointer device)))
  (check-err (drm::drop-master (fd device)))
  (close (fd-stream device)))

(defun add-framebuffer (fd width height depth bpp pitch handle)
  (cffi:with-foreign-objects ((buf-id :uint32 1))
    (drm::mode-add-framebuffer fd width height depth bpp pitch handle buf-id)
    (cffi:mem-ref buf-id :uint32)))

(defmethod connected-connectors ((device gbm-device))
  (loop for connector in (connectors device)
	when (eq :connected (connection connector))
	  collect connector))

(defun set-crtc! (fd fb connector crtc mode)
  (let ((result (drm:set-crtc
		 fd (id crtc)
		 fb 0 0
		 (list (id connector)) (ptr mode))))
    (unless (eq 0 result) (error (format nil "Failed to set crtc: error ~a" result)))))

;; TODO: Iterate cards - but actually check their capabilities.
;; TODO: Could also combine with render card capability reading
;; TODO: Figure out the stupid style warning - it doesnt affect anything so far, but annoys me
;; TODO: Mesa drm has a function to get the devices and then read their capabilities
(defun init-drm ()
  (let* ((card (loop for i from 0 below 32
		     for path = (format nil "/dev/dri/card~A" i)
		     when (probe-file path) return path))
	 (fd (open card :direction :io :if-exists :append)))
    (make-instance 'gbm-device :file fd)))

(defun drm-page-flip (drm-dev framebuffer crtc)
  (let* ((result (- (drm::mode-page-flip (fd drm-dev)
					 (id crtc)
					 framebuffer
					 :page-flip-event
					 (cffi:null-pointer))))
	 (error-msg (match-kernel-errcode result)))
    (when error-msg (error (format nil "Page flip:: ~a:~a~%" result error-msg)))))


;; ┌─┐┬─┐┌─┐┌┬┐┌─┐┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;; ├┤ ├┬┘├─┤│││├┤ ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; └  ┴└─┴ ┴┴ ┴└─┘└─┘└─┘└  └  └─┘┴└─
(defstruct framebuffer id buffer mode)

(defvar *bo-flags* (logior gbm::BO_USE_SCANOUT gbm::BO_USE_RENDERING))

(defun create-bo! (device width height) (gbm:bo-create (gbm-pointer device) width height gbm::FORMAT_XRGB8888 *bo-flags*))
(defun destroy-bo (buffer-object) (check-err (gbm:bo-destroy buffer-object)))

;; TODO: Figure out bpp. For now, just hardcoding it
;; TODO: Figure out depth. For now, just hardcoding it
(defun create-connector-framebuffer (device connector)
  "By default uses the first available mode"
  (let ((mode (car (modes connector))))
    (when mode
      (let* ((width (hdisplay mode))
	     (height (vdisplay mode))
	     (buffer-object (create-bo! device width height))
	     (bpp 32) (depth 24))
	(make-framebuffer :id (add-framebuffer (fd device) width height depth bpp
					       (gbm:bo-get-stride buffer-object)
					       (gbm:bo-get-handle buffer-object))
			  :buffer buffer-object
			  :mode mode)))))

(defun rm-framebuffer! (device fb buffer)
  (destroy-bo buffer)
  (check-err (drm::mode-remove-framebuffer (fd device) fb)))



;; ┌┬┐┬─┐┌─┐┌─┐┬ ┬
;;  │ ├┬┘├─┤└─┐├─┤
;;  ┴ ┴└─┴ ┴└─┘┴ ┴
;; TODO: Remove these
;; TODO: Get rid of this. Favor create-connector-framebuffer
(defun default-framebuffer (device)
  (let* ((buffer-object (create-bo device))
	 (width (width device))
	 (height (height device))
	 (handle (gbm:bo-get-handle buffer-object))
	 (stride (gbm:bo-get-stride buffer-object))
	 (bpp 32) (depth 24))
    (make-framebuffer :id (add-framebuffer (fd device) width height depth bpp stride handle)
		      :buffer buffer-object)))

;; TODO: Get rid of this. For now favor rm-framebuffer!. Albeit it also still isn't perfect
(defun rm-framebuffer (device framebuffer)
  (destroy-bo (framebuffer-buffer framebuffer))
  (check-err (drm::mode-remove-framebuffer (fd device) (framebuffer-id framebuffer))))

;; TODO: Get rid of this. Favor create-bo!
(defun create-bo (device)
  (gbm:bo-create (gbm-pointer device) (width device) (height device) gbm::FORMAT_XRGB8888 *bo-flags*))
