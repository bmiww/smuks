
;; ██████╗ ███╗   ███╗ █████╗ ██████╗ ██╗   ██╗███████╗
;; ██╔══██╗████╗ ████║██╔══██╗██╔══██╗██║   ██║██╔════╝
;; ██║  ██║██╔████╔██║███████║██████╔╝██║   ██║█████╗
;; ██║  ██║██║╚██╔╝██║██╔══██║██╔══██╗██║   ██║██╔══╝
;; ██████╔╝██║ ╚═╝ ██║██║  ██║██████╔╝╚██████╔╝██║
;; ╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚═╝
(in-package :smuks)
(defclass dmabuf-global (zwp-linux-dmabuf-v1:global)
  ())

(defmethod zwp-linux-dmabuf-v1:dispatch-bind :after ((global dmabuf-global) client data version id)
  )

;; TODO: Maybe move to its own file since it could possibly grow
;; Maybe part of drm package?
;; DRM Fourcc codes
(defun fourcc-code (code)
  (let ((a (char code 0)) (b (char code 1))
	(c (char code 2)) (d (char code 3)))
    (logior
     (ash (char-code a) 0) (ash (char-code b) 8)
     (ash (char-code c) 16) (ash (char-code d) 24))))
(setf (fdefinition 'cc4) #'fourcc-code)

(defvar *argb-8888* (cc4 "AR24"))
(defvar *xrgb-8888* (cc4 "XR24"))

;; ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;;  │││└─┐├─┘├─┤ │ │  ├─┤
;; ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
;; TODO: This should be a part of display or something
;; For now lazily globalizing it
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

(defmethod initialize-instance :after ((feedback feedback) &key)
  (let ((display (wl:get-display feedback)))
    (zwp-linux-dmabuf-feedback-v1:send-format-table feedback (mmap-pool-fd *format-table*) (mmap-pool-size *format-table*))
    (zwp-linux-dmabuf-feedback-v1:send-main-device feedback `(,(dev-t display)))

    ;; START TRANCHE
    ;; TODO: The tranche could be extracted to its own method so as to make this a bit prettier
    ;; For now - since - i only have one device and one supported format/modifier - i'm being lazy
    (zwp-linux-dmabuf-feedback-v1:send-tranche-target-device feedback `(,(dev-t display)))
    ;; TODO: The 0 here identifies the first element of the *format-table*
    ;; Make it a bit smarter
    (zwp-linux-dmabuf-feedback-v1:send-tranche-formats feedback '(0))
    ;; TODO: This might be unnecessary, and just removable. scanout flag.
    ;; Wanted to notify it in case if i do decide to read the pixels being sent in some form of panic mode
    (zwp-linux-dmabuf-feedback-v1:send-tranche-flags feedback 1)
    (zwp-linux-dmabuf-feedback-v1:send-tranche-done feedback)
    ;; END TRANCHE

    (zwp-linux-dmabuf-feedback-v1:send-done feedback)))


;; ┌─┐┌─┐┬─┐┌┬┐┌─┐┌┬┐  ┌┬┐┌─┐┌┐ ┬  ┌─┐
;; ├┤ │ │├┬┘│││├─┤ │    │ ├─┤├┴┐│  ├┤
;; └  └─┘┴└─┴ ┴┴ ┴ ┴    ┴ ┴ ┴└─┘┴─┘└─┘
(defun gen-format-table (formmods)
  (let* ((size (* (length formmods) 8))
	 (xdg-dir (uiop/os:getenv "XDG_RUNTIME_DIR")))

    ;; TODO: Temp file into mmap - could be a package like thingy
    ;; Could also figure out how to do the memfd stuff, but the runtime dir should still be mem based
    (uiop:with-temporary-file
	(:stream stream
	 :directory xdg-dir
	 :keep t :stream stream :element-type '(unsigned-byte 8)
	 :direction :io)

      ;; TODO: SBCL Specific
      (multiple-value-bind (ptr fd size) (mmap:mmap (sb-sys:fd-stream-fd stream)
						    :protection '(:read :write)
						    :size size :mmap '(:private))
	(dolist (formmod formmods)
	  (let ((format (car formmod)) (modifier (cadr formmod)))
	    ;; This is a very lazy little endian implementation.
	    ;; Sorry to anyone doing the big one
	    ;; My head hurt while i was writing this, and I can never find the proper
	    ;; Language tools to deal with this kind of thing.
	    (write-byte (ldb (byte 8 8) format) stream)
	    (write-byte (ldb (byte 8 0) format) stream)
	    (write-byte 0 stream)
	    (write-byte 0 stream)

	    (write-byte (ldb (byte 8 24) modifier) stream)
	    (write-byte (ldb (byte 8 16) modifier) stream)
	    (write-byte (ldb (byte 8 8) modifier) stream)
	    (write-byte (ldb (byte 8 0) modifier) stream)))
	(make-mmap-pool :ptr ptr :fd fd :size size)))))
