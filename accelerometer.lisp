
;;  █████╗  ██████╗ ██████╗███████╗██╗     ███████╗██████╗  ██████╗ ███╗   ███╗███████╗████████╗███████╗██████╗
;; ██╔══██╗██╔════╝██╔════╝██╔════╝██║     ██╔════╝██╔══██╗██╔═══██╗████╗ ████║██╔════╝╚══██╔══╝██╔════╝██╔══██╗
;; ███████║██║     ██║     █████╗  ██║     █████╗  ██████╔╝██║   ██║██╔████╔██║█████╗     ██║   █████╗  ██████╔╝
;; ██╔══██║██║     ██║     ██╔══╝  ██║     ██╔══╝  ██╔══██╗██║   ██║██║╚██╔╝██║██╔══╝     ██║   ██╔══╝  ██╔══██╗
;; ██║  ██║╚██████╗╚██████╗███████╗███████╗███████╗██║  ██║╚██████╔╝██║ ╚═╝ ██║███████╗   ██║   ███████╗██║  ██║
;; ╚═╝  ╚═╝ ╚═════╝ ╚═════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
;; NOTE: Someone explaining the /sys iio filesystem in linux
;; https://parzival2.github.io/blog/posts/understanding-iio-devices/

(defpackage #:iio-accelerometer
  (:use #:cl #:smuks-util #:str #:parse-float))
(in-package :iio-accelerometer)


;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass dev ()
  ((of-name :initarg :of-name :accessor of-name)
   (of-fullname :initarg :of-fullname :accessor of-fullname)
   (path :initarg :path :accessor path)))

(defmethod initialize-instance :after ((dev dev) &key path)
  (parse-uevent (merge-pathnames path "uevent") dev))


;; ┌┐┌┌─┐┌┬┐┌─┐
;; ││││ │ ││├┤
;; ┘└┘└─┘─┴┘└─┘
(defclass node ()
  ((index :accessor index)
   (scale :accessor scale)
   (enabled :accessor enabled)
   (value-type :accessor value-type)
   (paths :initform nil :accessor paths)))

(defmethod read-initial-values ((node node))
  (with-slots (paths enabled scale value-type index) node
    (with-open-file (enable-file (prop-paths-enable paths))
      (setf enabled (string= (read-line enable-file) "1")))
    (with-open-file (index-file (prop-paths-index paths))
      (setf index (parse-integer (read-line index-file))))
    (with-open-file (scale-file (prop-paths-scale paths))
      (setf scale (parse-float (read-line scale-file))))
    ;; Example type string: "le:s12/16>>4"
    (with-open-file (type-file (prop-paths-type paths))
      (ppcre:register-groups-bind (endian sign real-bits store-bits shift)
	  ("(le|be):(s|u)(\\d+)/(\\d+)>>(\\d+)" (read-line type-file))
	(setf value-type
	      (list :endian endian
		    :sign sign
		    :real-bits (parse-integer real-bits)
		    :store-bits (parse-integer store-bits)
		    :shift (parse-integer shift)))))))


;; (defmethod read-node-value (bytes)
  ;; (ash))

(defmethod set-on-state ((node node) on)
  (with-slots (paths enabled) node
    (let ((enable-path (prop-paths-enable paths)))
      (with-open-file (enable-file enable-path :direction :output :if-exists :overwrite)
	(format enable-file (if on "1" "0"))
	(setf enabled on)))))

(defmethod disable ((node node)) (set-on-state node nil))
(defmethod enable ((node node)) (set-on-state node t))

(defvar *accel-props*
  '(:in-accel-x ("in_accel_x" accel-x)
    :in-accel-y ("in_accel_y" accel-y)
    :in-accel-z ("in_accel_z" accel-z)))


;; ┬┬┌─┐  ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;; │││ │   ││├┤ └┐┌┘││  ├┤
;; ┴┴└─┘  ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass iio-dev (dev)
  ((accel-x :initform (make-instance 'node) :accessor accel-x)
   (accel-y :initform (make-instance 'node) :accessor accel-y)
   (accel-z :initform (make-instance 'node) :accessor accel-z)
   (watermark :initform nil :accessor watermark)
   (enabled :initform nil :accessor enabled)))


(defmethod set-read-interest ((dev iio-dev) prop)
  (let ((path-part (getf *accel-props* prop)))
    (unless path-part (error "Invalid property: ~a" prop))
    (let* ((prop-paths (get-accel-prop-paths (path dev) (first path-part)))
	   (node (slot-value dev (cadr path-part))))
      (setf (paths node) prop-paths)
      (read-initial-values node))))

(defmethod shared-initialize :after ((dev iio-dev) slots &key)
  (declare (ignore slots))
  (set-read-interest dev :in-accel-x)
  (set-read-interest dev :in-accel-y)
  (set-read-interest dev :in-accel-z)
  (read-watermark dev))

(defmethod enable-all-axis ((dev iio-dev))
  (enable (accel-x dev))
  (enable (accel-y dev))
  (enable (accel-z dev)))

(defmethod read-watermark ((dev iio-dev))
  (let ((watermark-path (merge-pathnames "buffer0/watermark" (path dev))))
    (with-open-file (watermark-file watermark-path)
      (setf (watermark dev) (parse-integer (read-line watermark-file))))))

(defmethod set-watermark ((dev iio-dev) watermark)
  "Watermark. Or the number of samples that can be stored in the device buffer."
  (unless (typep watermark 'integer) (error "Invalid watermark: ~a" watermark))
  (let ((watermark-path (merge-pathnames "buffer0/watermark" (path dev))))
    (with-open-file (watermark-file watermark-path :direction :output :if-exists :overwrite)
      (format watermark-file "~a" watermark)
      (setf (watermark dev) watermark))))

;; TODO: This is blindly assuming that the enable file is always in buffer0
(defmethod set-accelerometer-state ((dev iio-dev) on)
  (let* ((devpath (path dev))
	 (enable-path (uiop:merge-pathnames* "buffer0/enable" devpath)))
    (with-open-file (enable-file enable-path :direction :output :if-exists :overwrite)
      (format enable-file (if on "1" "0"))
      (setf (enabled dev) on))))

(defmethod enable-accelerometer ((dev iio-dev)) (set-accelerometer-state dev t))
(defmethod disable-accelerometer ((dev iio-dev)) (set-accelerometer-state dev nil))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defstruct prop-paths
  (enable nil)
  (index nil)
  (type nil)
  (scale nil)
  (raw nil))

(defun get-accel-prop-paths (devpath prop-path)
  (flet ((build-path (path) (uiop:merge-pathnames* path devpath)))
    (flet ((scan-el (addition) (build-path (format nil "scan_elements/~a_~a" prop-path addition)))
	   (base-el (addition) (build-path (format nil "~a_~a" prop-path addition))))
      (make-prop-paths :enable (scan-el "en")
		       :index (scan-el "index")
		       :type (scan-el "type")
		       :scale (base-el "scale")
		       :raw (base-el "raw")))))

(defun parse-uevent (path dev)
  (with-open-file (stream path)
    (let ((line (read-line stream nil)))
      (loop while line do
	(let ((parts (split "=" line)))
	  (when (string= (first parts) "OF_NAME") (setf (of-name dev) (second parts)))
	  (when (string= (first parts) "OF_FULLNAME") (setf (of-fullname dev) (second parts)))
	  (setf line (read-line stream nil)))))))

(defun list-iio-devs () (uiop:subdirectories "/sys/bus/iio/devices/"))

(defun find-accelerometer-dev-new ()
  (let* ((devs (list-iio-devs))
	 (devs (mapcar (lambda (dev) (make-instance 'dev :path dev)) devs))
	 (accel (find-if (lambda (dev) (string= (of-name dev) "accelerometer")) devs)))
    (unless accel (error "No accelerometer found"))
    (change-class accel 'iio-dev)
    accel))
