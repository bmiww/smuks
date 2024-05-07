
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
  (dolist (path (directory "/dev/input/event*"))
    (let ((dev (make-instance 'dev :path path :dev-track track)))
      (setf (gethash path (devices track)) dev))))

(defmethod add-device ((track dev-track) path)
  (if (gethash path (devices track))
      (log! "Device already exists: ~a... Ignoring." path)
      (setf (gethash path (devices track)) (make-instance 'dev :path path :dev-track track))))

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

;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass dev ()
  ((path :initarg :path :accessor path)
   (name :accessor name)
   (libinput-ptr :initform nil :accessor libinput-ptr)
   (capabilities :initform nil :accessor capabilities)
   (pointless :accessor pointless)
   (dev-track :initform nil :accessor dev-track)))

(defvar *caps-of-interest*
  (list
   `(:keyboard ,libinput:device-cap-keyboard)
   `(:pointer ,libinput:device-cap-pointer)
   `(:touch ,libinput:device-cap-touch)))

(defmethod initialize-instance :after ((dev dev) &key path dev-track)
  (setf (libinput-ptr dev) (libinput:path-add-device (context dev-track) (namestring path)))
  (if (cffi:pointer-eq (libinput-ptr dev) (cffi:null-pointer))
      (log! "Device with path ~a could not be added to libinput context." path)
      (progn
	(libinput:device-ref (libinput-ptr dev))
	(setf (name dev) (libinput:device-get-name (libinput-ptr dev)))

	(dolist (capability *caps-of-interest*)
	  (let ((has-cap (libinput:device-has-capability (libinput-ptr dev) (cadr capability))))
	    (when has-cap (push (car capability) (capabilities dev)))))

	(unless (capabilities dev) (make-pointless dev)))))

(defmethod make-pointless ((dev dev))
  (setf (pointless dev) t)
  (libinput:device-unref (libinput-ptr dev))
  (libinput:path-remove-device (libinput-ptr dev)))

(defmethod destroy ((dev dev))
  (libinput:device-unref (libinput-ptr dev))
  (libinput:path-remove-device (libinput-ptr dev)))
