
;; ██████╗ ███████╗██╗   ██╗    ████████╗██████╗  █████╗  ██████╗██╗  ██╗
;; ██╔══██╗██╔════╝██║   ██║    ╚══██╔══╝██╔══██╗██╔══██╗██╔════╝██║ ██╔╝
;; ██║  ██║█████╗  ██║   ██║       ██║   ██████╔╝███████║██║     █████╔╝
;; ██║  ██║██╔══╝  ╚██╗ ██╔╝       ██║   ██╔══██╗██╔══██║██║     ██╔═██╗
;; ██████╔╝███████╗ ╚████╔╝        ██║   ██║  ██║██║  ██║╚██████╗██║  ██╗
;; ╚═════╝ ╚══════╝  ╚═══╝         ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝
;; A class meant to track libinput objects and their underlying paths.
(in-package :smuks)

;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐  ┌┬┐┬─┐┌─┐┌─┐┬┌─┌─┐┬─┐
;;  ││├┤ └┐┌┘││  ├┤    │ ├┬┘├─┤│  ├┴┐├┤ ├┬┘
;; ─┴┘└─┘ └┘ ┴└─┘└─┘   ┴ ┴└─┴ ┴└─┘┴ ┴└─┘┴└─
(defclass dev-track ()
  ((open-restricted :initarg :open-restricted :reader open-restricted)
   (close-restricted :initarg :close-restricted :reader close-restricted)
   (context :initarg nil :accessor context)
   (devices :initform (make-hash-table :test 'equal) :accessor devices)
   (fd :initform nil :accessor fd)))

(defmethod initialize-instance :after ((track dev-track) &key open-restricted close-restricted)
  (setf (context track) (libinput:create-context :open-restricted open-restricted
						 :close-restricted close-restricted))
  (init-devices track))

(defmethod init-devices ((track dev-track))
  (setf (devices track) (make-hash-table :test 'equal))
  (dolist (path (directory "/dev/input/event*"))
    (let ((dev (make-instance 'dev :path path :dev-track track)))
      (setf (gethash path (devices track)) dev))))

(defmethod add-device ((track dev-track) path)
  (if (gethash path (devices track))
      (log! "Device already exists: ~a... Ignoring." path)
      (setf (gethash path (devices track)) (make-instance 'dev :path path :dev-track track))))

(defmethod rem-device-abandoned ((track dev-track) path)
  (let ((dev (gethash path (devices track))))
    (when dev (remhash path (devices track)))))

(defmethod rem-device ((track dev-track) path)
  (let ((dev (gethash path (devices track))))
    (if dev
	(progn
	  (destroy dev)
	  (remhash path (devices track)))
	(log! "Device does not exist: ~a... Ignoring." path))))

(defmethod context-fd ((track dev-track))
  (or (fd track) (setf (fd track) (libinput:get-fd (context track)))))

(defmethod dispatch ((track dev-track) handle-input-cb)
  (libinput::dispatch (context track))
  (loop for event = (libinput:get-event (context track))
	while event
	do (funcall handle-input-cb event)))

(defmethod destroy ((track dev-track))
  (loop for dev being the hash-values of (devices track)
	do (destroy dev))
  (when (fd track) (sb-posix:close (fd track)))
  (libinput:unref (context track)))

;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass dev ()
  ((path :initarg :path :accessor path)
   (name :initform "" :accessor name)
   (libinput-ptr :initform nil :accessor libinput-ptr)
   (capabilities :initform nil :accessor capabilities)
   (pointless :initform t :accessor pointless)
   (bus-type :initform nil :accessor bus-type)
   (product-id :initform nil :accessor product-id)
   (vendor-id :initform nil :accessor vendor-id)

   (dev-track :initform nil :accessor dev-track)
   (disabled :initform nil :accessor disabled)
   (width :initform nil :accessor width)
   (height :initform nil :accessor height)
   (output-name :initform nil :accessor output-name)))

(defmethod print-object ((dev dev) stream)
  (print-unreadable-object (dev stream :type t)
    (with-slots (name) dev
      (format stream "~a" (or name "NONE")))))

(defvar *caps-of-interest* (list :keyboard :pointer :touch))

(defmethod initialize-instance :after ((dev dev) &key path dev-track)
  (setf (libinput-ptr dev) (libinput:path-add-device (context dev-track) (namestring path)))
  (if (cffi:null-pointer-p (libinput-ptr dev))
      (progn
	;; Some devices are not handled by libinput, so we ignore them. (null pointer returned)
	(mk-dev-pointless dev))
      (progn
	(setf (pointless dev) nil)
	(setf (name dev) (libinput:device-get-name (libinput-ptr dev)))
	;; NOTE: Libinput discourages the use of this function.
	;; Lets see if it's even helpful in the first place.
	(setf (output-name dev) (libinput:device-get-output-name (libinput-ptr dev)))

	(setf (bus-type dev) (libinput:device-get-id-bustype (libinput-ptr dev)))
	(setf (product-id dev) (libinput:device-get-id-product (libinput-ptr dev)))
	(setf (vendor-id dev) (libinput:device-get-id-vendor (libinput-ptr dev)))

	(let ((width) (height))
	  (setf (values width height) (libinput:device-get-size (libinput-ptr dev)))
	  (when width (setf (width dev) width))
	  (when height (setf (height dev) height)))

	(libinput:device-ref (libinput-ptr dev))

	(dolist (capability *caps-of-interest*)
	  (let ((has-cap (libinput:device-has-capability (libinput-ptr dev) capability)))
	    (when has-cap (push capability (capabilities dev)))))

	(unless (capabilities dev) (mk-dev-pointless dev)))))


(defmethod mk-dev-pointless ((dev dev))
  (setf (pointless dev) t)
  (unless (cffi:null-pointer-p (libinput-ptr dev))
    (libinput:device-unref (libinput-ptr dev))
    (libinput:path-remove-device (libinput-ptr dev))))

(defmethod destroy ((dev dev))
  (when (and (not (cffi:null-pointer-p (libinput-ptr dev)))
	     (not (pointless dev)))
    (libinput:device-unref (libinput-ptr dev))
    (libinput:path-remove-device (libinput-ptr dev))))
