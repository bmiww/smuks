
;; ██████╗ ███╗   ███╗ █████╗ ██████╗ ██╗   ██╗███████╗
;; ██╔══██╗████╗ ████║██╔══██╗██╔══██╗██║   ██║██╔════╝
;; ██║  ██║██╔████╔██║███████║██████╔╝██║   ██║█████╗
;; ██║  ██║██║╚██╔╝██║██╔══██║██╔══██╗██║   ██║██╔══╝
;; ██████╔╝██║ ╚═╝ ██║██║  ██║██████╔╝╚██████╔╝██║
;; ╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚═╝
;; NOTE: Implements the LINUX DMA-BUF protocol as described here
;; https://wayland.app/protocols/linux-dmabuf-v1#zwp_linux_buffer_params_v1:request:add
(in-package :smuks)
(defclass dmabuf-global (zwp-linux-dmabuf-v1:global)
  ())

(defmethod zwp-linux-dmabuf-v1:dispatch-bind :after ((global dmabuf-global) client data version id)
  )

;; TODO: Maybe move to its own file since it could possibly grow
;; Maybe part of drm package?
;; DRM Fourcc codes
(defun cc4 (code)
  (let ((a (char code 0)) (b (char code 1))
	(c (char code 2)) (d (char code 3)))
    (logior
     (ash (char-code a) 0) (ash (char-code b) 8)
     (ash (char-code c) 16) (ash (char-code d) 24))))

;; TODO: You should be able to grab these via your CFFI-DEFINE package now - inside cl-drm
(defvar *argb-8888* (cc4 "AR24"))
(defvar *xrgb-8888* (cc4 "XR24"))

;; ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;;  │││└─┐├─┘├─┤ │ │  ├─┤
;; ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
;; TODO: This should be a part of display or something
;; For now lazily globalizing it
;; TODO: Close and delete the file on destroy/cleanup
;; TODO: Make this part of the global and then refer to it from the dispatch
(defvar *format-table* nil)

(defclass dmabuf (zwp-linux-dmabuf-v1:dispatch)
  ((default-feedback :initform nil :accessor default-feedback)
   (surface-feedbacks :initform (make-hash-table ) :accessor surface-feedbacks)))

(defmethod initialize-instance :after ((dmabuf dmabuf) &key)
  "NOTE: the format table formats - are DRM formats. The wayland protocol enum values for xrgb and argb do not match this!!!"
  ;; TODO: Replace the 0s and 1s with the actual format and modifier values from DRM fourcc
  (unless *format-table*
    (setf *format-table* (gen-format-table `((,*argb-8888* 0) (,*xrgb-8888* 0))))))


(defmethod zwp-linux-dmabuf-v1:get-default-feedback ((dmabuf dmabuf) id)
  "Presumably a single feedback object is created for each dmabuf object.
For more information on what feedback is in this context,
see the implementation of the zwp-linux-dmabuf-feedback-v1:dispatch class."
  (setf (default-feedback dmabuf) (wl:mk-if 'feedback dmabuf id :dmabuf dmabuf)))


(defmethod zwp-linux-dmabuf-v1:get-surface-feedback ((dmabuf dmabuf) id surface)
  "Creates a feedback object for a specific surface.
All parameters sent out of the feedback object are specific to the surface."
  (setf (gethash (wl:id surface) (surface-feedbacks dmabuf)) (wl:mk-if 'feedback dmabuf id :surface surface :dmabuf dmabuf)))


(defmethod zwp-linux-dmabuf-v1:create-params ((dmabuf dmabuf) id)
  (wl:mk-if 'buffer-params dmabuf id :dmabuf dmabuf))


;; ┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐  ┌─┐┌─┐┬─┐┌─┐┌┬┐┌─┐
;; ├┴┐│ │├┤ ├┤ ├┤ ├┬┘  ├─┘├─┤├┬┘├─┤│││└─┐
;; └─┘└─┘└  └  └─┘┴└─  ┴  ┴ ┴┴└─┴ ┴┴ ┴└─┘
(defclass plane ()
  ((id :initarg :id :accessor id)
   (fd :initarg :fd :accessor fd)
   (offset :initarg :offset :accessor offset)
   (stride :initarg :stride :accessor stride)
   (modifier :initarg :modifier :accessor modifier)))

(defclass buffer-params (zwp-linux-buffer-params-v1:dispatch)
  ((dmabuf :initarg :dmabuf :reader dmabuf)
   (planes :initform (make-hash-table :test 'eql) :accessor planes)
   (width :accessor width)
   (height :accessor height)
   (format :accessor buffer-format)
   (flags :accessor flags)))

;; TODO: modifier-hi and lo need to be checked in format-table.
;; For now - since we only support modifier 0 - i'm just erroring out when those vals are not 0
(defmethod zwp-linux-buffer-params-v1:add ((params buffer-params) fd plane-idx offset stride modifier-hi modifier-lo)
  (when (or (not (zerop modifier-hi)) (not (zerop modifier-lo))) (error "Only modifier 0 (LINEAR) is supported"))
  (setf (gethash plane-idx (planes params)) (make-instance 'plane :id plane-idx :fd fd :offset offset :stride stride :modifier 0)))


(defmethod zwp-linux-buffer-params-v1:create-immed ((params buffer-params) id width height format flags)
  "create_immed is expected to create an instance of wl_buffer.
It is mentally a bit simpler than the regular create as seen in this protocol"
  (wl:mk-if 'dma-buffer params id
	    :planes (planes params)
	    :width width
	    :height height
	    :format format
	    :flags flags))


;; ┌─┐┌─┐┌─┐┌┬┐┌┐ ┌─┐┌─┐┬┌─
;; ├┤ ├┤ ├┤  ││├┴┐├─┤│  ├┴┐
;; └  └─┘└─┘─┴┘└─┘┴ ┴└─┘┴ ┴
;; Seemingly most of what feedback does is to send out the available formats used by this drm struct:
;; https://github.com/torvalds/linux/blob/master/include/uapi/drm/drm_mode.h#L670
;; Format modifiers used by DRM_IOCTL_MODE_ADDFB2.
;; A client can use this information to create a buffer that can be used with the dmabuf protocol.

(defclass feedback (zwp-linux-dmabuf-feedback-v1:dispatch)
  ((surface :initarg :surface :initform nil :accessor surface)
   (dmabuf :initarg :dmabuf :accessor dmabuf)))

(defmethod send-tranche ((feedback feedback) device formats &key scanout)
  ;; TODO: The tranche could be extracted to its own method so as to make this a bit prettier
  ;; For now - since - i only have one device and one supported format/modifier - i'm being lazy
  (zwp-linux-dmabuf-feedback-v1:send-tranche-target-device feedback (list device))
  (zwp-linux-dmabuf-feedback-v1:send-tranche-formats feedback formats)
  ;; TODO: I still don't know which nodes are scanouts and which are renders
  ;; There should be some way to identify in DRM level
  ;; NOTE: You already did some work towards this in DRM level, should be able to use that
  (when scanout (zwp-linux-dmabuf-feedback-v1:send-tranche-flags feedback :scanout))
  (zwp-linux-dmabuf-feedback-v1:send-tranche-done feedback))


(defmethod initialize-instance :after ((feedback feedback) &key)
  (let* ((display (wl:get-display feedback))
	 (drm (drm display))
	 (dev-t (sdrm:dev-t drm)))
    (zwp-linux-dmabuf-feedback-v1:send-format-table feedback (mmap-pool-fd *format-table*) (mmap-pool-size *format-table*))
    (zwp-linux-dmabuf-feedback-v1:send-main-device feedback `(,dev-t))

    ;; TODO: The 0 here identifies the first element of the *format-table*
    ;; Make it a bit smarter
    ;; TODO: For now - the scanout thing here is pretty fake.
    ;; Primarily used to see how the simple-dmabuf-feedback example from weston works
    ;; (send-tranche feedback dev-t '(0 1) :scanout t)
    (send-tranche feedback dev-t '(0 1))

    (zwp-linux-dmabuf-feedback-v1:send-done feedback)))


;; ┌─┐┌─┐┬─┐┌┬┐┌─┐┌┬┐  ┌┬┐┌─┐┌┐ ┬  ┌─┐
;; ├┤ │ │├┬┘│││├─┤ │    │ ├─┤├┴┐│  ├┤
;; └  └─┘┴└─┴ ┴┴ ┴ ┴    ┴ ┴ ┴└─┘┴─┘└─┘
(defun gen-format-table (formmods)
  (with-xdg-mem-file (stream "format-table" :element-type '(unsigned-byte 8))
    (dolist (formmod formmods)
      (let ((format (car formmod)) (modifier (cadr formmod)))
	;; This is a very lazy little endian implementation.
	;; Sorry to anyone doing the big one
	;; My head hurt while i was writing this, and I can never find the proper
	;; Language tools to deal with this kind of thing.
	(write-byte (ldb (byte 8 0) format) stream)
	(write-byte (ldb (byte 8 8) format) stream)
	(write-byte (ldb (byte 8 16) format) stream)
	(write-byte (ldb (byte 8 24) format) stream)
	(write-byte 0 stream)
	(write-byte 0 stream)
	(write-byte 0 stream)
	(write-byte 0 stream)

	(write-byte (ldb (byte 8 0) modifier) stream)
	(write-byte (ldb (byte 8 8) modifier) stream)
	(write-byte (ldb (byte 8 16) modifier) stream)
	(write-byte (ldb (byte 8 24) modifier) stream)
	(write-byte (ldb (byte 8 32) modifier) stream)
	(write-byte (ldb (byte 8 40) modifier) stream)
	(write-byte (ldb (byte 8 48) modifier) stream)
	(write-byte (ldb (byte 8 56) modifier) stream)))))
