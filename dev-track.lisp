
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
  ((context :initarg nil :accessor context)
   (devices :initform (make-hash-table :test 'equal) :accessor devices)))

(defmethod initialize-instance :after ((track dev-track) &key device-paths)
  (setf (context track) (libinput:create-context))
  (dolist (path device-paths)
    (let ((dev (make-instance 'dev :path path :dev-track track)))
      (setf (gethash path (devices track)) dev))))

(defmethod rem-device ((track dev-track) path)
  (let ((dev (gethash path (devices track))))
    (unless dev (error "Device not found: ~A" path))
    (destroy dev)
    (remhash path (devices track))))


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
  (setf (libinput-ptr dev) (libinput:path-add-device (context dev-track) path))
  (libinput:device-ref (libinput-ptr dev))
  (setf (name dev) (libinput:device-get-name (libinput-ptr dev)))

  (dolist (capability *caps-of-interest*)
    (let ((has-cap (libinput:device-has-capability (libinput-ptr dev) (cadr capability))))
      (when has-cap (push (car capability) (capabilities dev)))))

  (unless (capabilities dev)
    (setf (pointless dev) t)
    (libinput:device-unref (libinput-ptr dev))
    (libinput:path-remove-device (libinput-ptr dev))))

(defmethod destroy ((dev dev))
  (libinput:device-unref (libinput-ptr dev))
  (libinput:path-remove-device (libinput-ptr dev)))
