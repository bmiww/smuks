
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
   (path :initarg :path :accessor path)
   (dev-path :initarg :dev-path :accessor dev-path)
   (open-file :initform nil :accessor open-file)
   (dev-fd :initform nil :accessor dev-fd)))

(defmethod initialize-instance :after ((dev dev) &key path)
  (setf (dev-path dev)
	(merge-pathnames (car (last (split "/" (trim-right (namestring path) :char-bag "/")))) "/dev/"))
  (parse-uevent (merge-pathnames path "uevent") dev))

(defmethod fd ((dev dev))
  (unless (dev-fd dev)
    (setf (dev-fd dev) (sb-unix:unix-open (namestring (dev-path dev)) (logior #x0004) sb-unix:o_rdonly)))
  (unless (open-file dev)
    (setf (open-file dev) (sb-sys:make-fd-stream (dev-fd dev) :input t :buffering :none :element-type '(unsigned-byte 8) :timeout 2)))
  (dev-fd dev))


;; ┌┐┌┌─┐┌┬┐┌─┐
;; ││││ │ ││├┤
;; ┘└┘└─┘─┴┘└─┘
(defclass node ()
  ((index :accessor index)
   (scale :accessor scale)
   (enabled :accessor enabled)
   (real-bits :accessor real-bits)
   (bytes :accessor bytes)
   (shift :accessor shift)
   (signed :accessor signed)
   (endian :accessor endian)
   (paths :initform nil :accessor paths)))

(defmethod read-initial-values ((node node))
  (with-slots (paths enabled scale index bytes shift signed endian real-bits) node
    (with-open-file (enable-file (prop-paths-enable paths))
      (setf enabled (string= (read-line enable-file) "1")))
    (with-open-file (index-file (prop-paths-index paths))
      (setf index (parse-integer (read-line index-file))))

    (handler-case
	(with-open-file (scale-file (prop-paths-scale paths))
	  (setf scale (parse-float (read-line scale-file))))
      ;; NOTE: Some accelerometers define a common scale file for all nodes
      ;; TODO: SBCL exclusive
      (sb-ext:file-does-not-exist (e)
	(with-open-file (scale-file (prop-paths-scale-fallback paths))
	  (setf scale (parse-float (read-line scale-file))))))

    ;; Example type string: "le:s12/16>>4"
    (with-open-file (type-file (prop-paths-type paths))
      (ppcre:register-groups-bind (endian-val sign real-bits store-bits shift-amount)
	  ;; NOTE: This fancy regex does not include REPEAT information. My accelerometer doesn't have it.
	  ("(le|be):(s|u)(\\d+)/(\\d+)>>(\\d+)" (read-line type-file))
	(setf bytes (/ (parse-integer store-bits) 8))
	(setf shift (parse-integer shift-amount))
	(setf signed (string= sign "s"))
	(setf endian (if (string= endian-val "le") :little :big))
	;; NOTE: Seems rather useless, since real-bits would be determined from bytes and shift
	(setf real-bits (parse-integer real-bits))))))


(defmethod read-node-value ((node node) bytes)
  (let* ((bytes (make-array (length bytes) :initial-contents bytes)))
    (* (scale node)
       (ash
	(let ((bytes (if (signed node)
			   (cl-intbytes:octets->int bytes 2)
			   (cl-intbytes:octets->uint bytes 2))))
	  (if (eq (endian node) :big)
	      (swap-bytes-16 bytes)
	      bytes))
	(shift node)))))

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
;; TODO: Rename this one to accelerometer. Since that is what it is.
(defclass iio-dev (dev)
  ((accel-x :initform (make-instance 'node) :accessor accel-x)
   (accel-y :initform (make-instance 'node) :accessor accel-y)
   (accel-z :initform (make-instance 'node) :accessor accel-z)
   (watermark :initform nil :accessor watermark)
   (enabled :initform nil :accessor enabled)))

(defmethod close-dev ((dev iio-dev))
  (when (enabled dev)
    (disable-accelerometer dev)
    (setf (enabled dev) nil))

  (when (open-file dev)
    (close (open-file dev))
    (setf (open-file dev) nil))

  (when (dev-fd dev)
    (sb-unix:unix-close (dev-fd dev))
    (setf (dev-fd dev) nil)))

(defmethod shared-initialize :after ((dev iio-dev) slots &key)
  (declare (ignore slots))
  ;; NOTE: In case if the device hasn't been properly disabled previously
  (disable-accelerometer dev)

  (read-interest dev :in-accel-x :enable t)
  (read-interest dev :in-accel-y :enable t)
  (read-interest dev :in-accel-z :enable t)
  (set-watermark dev 1)
  (set-length dev 3)
  (enable-accelerometer dev))

(defmethod read-interest ((dev iio-dev) prop &key enable)
  (let ((path-part (getf *accel-props* prop)))
    (unless path-part (error "Invalid property: ~a" prop))
    (let* ((prop-paths (get-accel-prop-paths (path dev) (first path-part)))
	   (node (slot-value dev (cadr path-part))))
      (setf (paths node) prop-paths)
      (read-initial-values node)
      (when enable (enable node)))))

;; TODO: Values might not be in order depicted here.
(defmethod read-accelerometer ((dev iio-dev))
  (let ((file (open-file dev))
	(reading (list (accel-x dev) (accel-y dev) (accel-z dev)))
	;; (data-available (read-data-available dev))
	)
    (loop for node in reading
	  ;; do (print (bytes node))
	  for bytes = (loop for i from 0 below (bytes node) collect (read-byte file))
	  collect (read-node-value node bytes))))


(defmethod enable-all-axis ((dev iio-dev))
  (enable (accel-x dev))
  (enable (accel-y dev))
  (enable (accel-z dev)))

;; NOTE: For now unused. I'm reading only one sample - so don't really need to check how many samples are available.
(defmethod read-data-available ((dev iio-dev))
  (with-open-file (availability (merge-pathnames "buffer0/data_available" (path dev)))
    (parse-integer (read-line availability))))

(defmethod read-watermark ((dev iio-dev))
  (let ((watermark-path (merge-pathnames "buffer0/watermark" (path dev))))
    (with-open-file (watermark-file watermark-path)
      (setf (watermark dev) (parse-integer (read-line watermark-file))))))

;; TODO: For now implying that length value is same as watermark
(defmethod set-length ((dev iio-dev) length)
  (unless (typep length 'integer) (error "Invalid length: ~a" length))
  (let ((length-path (merge-pathnames "buffer0/length" (path dev))))
    (with-open-file (length-file length-path :direction :output :if-exists :overwrite)
      (format length-file "~a" length))))


(defmethod set-watermark ((dev iio-dev) watermark)
  "Watermark. Or the number of samples that can be stored in the device buffer."
  (unless (typep watermark 'integer) (error "Invalid watermark: ~a" watermark))
  (let ((watermark-path (merge-pathnames "buffer0/watermark" (path dev))))
    (with-open-file (watermark-file watermark-path :direction :output :if-exists :overwrite)
      (format watermark-file "~a" watermark)
      (setf (watermark dev) watermark))))

;; TODO: This is blindly assuming that the enable file is always in buffer0
;; NOTE: Will error out if no read interest on the nodes has been set.
(defmethod set-accelerometer-state ((dev iio-dev) on)
  (let* ((devpath (path dev))
	 (enable-path (uiop:merge-pathnames* "buffer0/enable" devpath)))
    (with-open-file (enable-file enable-path :direction :output :if-exists :overwrite)
      (format enable-file "~a" (if on "1" "0")))
    (setf (enabled dev) on)))

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
  (scale-fallback nil)
  (raw nil))

(defun get-accel-prop-paths (devpath prop-path)
  (flet ((build-path (path) (uiop:merge-pathnames* path devpath)))
    (flet ((scan-el (addition) (build-path (format nil "scan_elements/~a_~a" prop-path addition)))
	   (base-el (addition) (build-path (format nil "~a_~a" prop-path addition))))
      (make-prop-paths :enable (scan-el "en")
		       :index (scan-el "index")
		       :type (scan-el "type")
		       :scale (base-el "scale")
		       :scale-fallback (build-path (format nil "~a_~a" "in_accel" "scale"))
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

(defun find-accelerometer-dev ()
  (let* ((devs (list-iio-devs))
	 (devs (mapcar (lambda (dev) (make-instance 'dev :path dev)) devs))
	 (accel (find-if (lambda (dev) (string= (of-name dev) "accelerometer")) devs)))
    (unless accel (error "No accelerometer found"))
    (change-class accel 'iio-dev)
    accel))


;; NOTE: Taken from:
;; https://github.com/sionescu/swap-bytes/blob/master/portable.lisp
(defun swap-bytes-16 (integer)
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior (ash (logand #xFF integer)  8)
          (ash integer -8)))
