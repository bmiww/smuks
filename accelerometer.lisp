
;;  █████╗  ██████╗ ██████╗███████╗██╗     ███████╗██████╗  ██████╗ ███╗   ███╗███████╗████████╗███████╗██████╗
;; ██╔══██╗██╔════╝██╔════╝██╔════╝██║     ██╔════╝██╔══██╗██╔═══██╗████╗ ████║██╔════╝╚══██╔══╝██╔════╝██╔══██╗
;; ███████║██║     ██║     █████╗  ██║     █████╗  ██████╔╝██║   ██║██╔████╔██║█████╗     ██║   █████╗  ██████╔╝
;; ██╔══██║██║     ██║     ██╔══╝  ██║     ██╔══╝  ██╔══██╗██║   ██║██║╚██╔╝██║██╔══╝     ██║   ██╔══╝  ██╔══██╗
;; ██║  ██║╚██████╗╚██████╗███████╗███████╗███████╗██║  ██║╚██████╔╝██║ ╚═╝ ██║███████╗   ██║   ███████╗██║  ██║
;; ╚═╝  ╚═╝ ╚═════╝ ╚═════╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
;; NOTE: Someone explaining the /sys iio filesystem in linux
;; https://parzival2.github.io/blog/posts/understanding-iio-devices/

(defpackage #:iio-accelerometer
  (:use #:cl #:smuks-util #:str #:parse-float)
  (:export
   find-accelerometer-dev))
(in-package :iio-accelerometer)

;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass dev ()
  ((of-name :initarg :of-name :accessor of-name)
   (of-fullname :initarg :of-fullname :accessor of-fullname)
   (path :initarg :path :accessor path)
   (dev-path :initarg :dev-path :accessor dev-path)
   (%file-stream :initform nil :accessor %file-stream)
   (dev-fd :initform nil :accessor dev-fd)
   (open-device :initarg :open-device :initform nil :accessor open-device)
   (close-device :initarg :close-device :initform nil :accessor close-device)))


(defmethod initialize-instance :after ((dev dev) &key path)
  (setf (dev-path dev)
	(merge-pathnames (car (last (split "/" (trim-right (namestring path) :char-bag "/")))) "/dev/"))
  (parse-uevent (merge-pathnames path "uevent") dev))

(defmethod fd ((dev dev))
  (unless (dev-fd dev)
    (setf (dev-fd dev)
	  (if (open-device dev)
	      (funcall (open-device dev) (namestring (dev-path dev)) (logior #x0004) sb-unix:o_rdonly)
	      (sb-unix:unix-open (namestring (dev-path dev)) (logior #x0004) sb-unix:o_rdonly))))
  (unless (%file-stream dev)
    (setf (%file-stream dev)
	  (lisp-binary:wrap-in-bit-stream
	   ;; TODO: SBCL exclusive
	   (sb-sys:make-fd-stream (dev-fd dev) :input t :buffering :none :element-type '(unsigned-byte 8) :timeout 2))))
  (dev-fd dev))


;; ┌┐┌┌─┐┌┬┐┌─┐
;; ││││ │ ││├┤
;; ┘└┘└─┘─┴┘└─┘
(defclass node ()
  ((name :initarg :name :accessor name)
   (index :accessor index)
   (scale :accessor scale)
   (enabled :accessor enabled)
   (store-bits :accessor store-bits)
   (real-bits :accessor real-bits)
   (bytes :accessor bytes)
   (shift :accessor shift)
   (signed :accessor signed)
   (endian :accessor endian)
   (paths :initform nil :accessor paths)))

(defmethod read-initial-config ((node node))
  (with-slots (paths enabled scale index bytes shift signed endian real-bits store-bits) node
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
	(declare (ignore e))
	(with-open-file (scale-file (prop-paths-scale-fallback paths))
	  (setf scale (parse-float (read-line scale-file))))))

    ;; Example type string: "le:s12/16>>4"
    (with-open-file (type-file (prop-paths-type paths))
      (ppcre:register-groups-bind (endian-val sign real-bits-val store-bits-val shift-amount)
	  ;; NOTE: This fancy regex does not include REPEAT information. My accelerometer doesn't have it.
	  ("(le|be):(s|u)(\\d+)/(\\d+)>>(\\d+)" (read-line type-file))
	(setf store-bits (parse-integer store-bits-val))
	(setf bytes (/ (parse-integer store-bits-val) 8))
	(setf shift (parse-integer shift-amount))
	(setf signed (string= sign "s"))
	(setf endian (if (string= endian-val "le") :little :big))
	;; NOTE: Seems rather useless, since real-bits would be determined from bytes and shift
	(setf real-bits (parse-integer real-bits-val))))))

(defmethod read-node-value ((node node) bytes)
  (* (scale node) (ash bytes (shift node))))

(defmethod set-on-state ((node node) on)
  (with-slots (paths enabled) node
    (let ((enable-path (prop-paths-enable paths)))
      (with-open-file (enable-file enable-path :direction :output :if-exists :overwrite)
	(format enable-file (if on "1" "0"))
	(setf enabled on)))))

(defmethod disable ((node node)) (when (enabled node) (set-on-state node nil)))
(defmethod enable ((node node)) (unless (enabled node) (set-on-state node t)))

(defvar *accel-props*
  '(:in-accel-x ("in_accel_x" accel-x)
    :in-accel-y ("in_accel_y" accel-y)
    :in-accel-z ("in_accel_z" accel-z)))

;; ┬┬┌─┐  ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;; │││ │   ││├┤ └┐┌┘││  ├┤
;; ┴┴└─┘  ─┴┘└─┘ └┘ ┴└─┘└─┘
;; TODO: Rename this one to accelerometer. Since that is what it is.
(defclass iio-dev (dev)
  ((accel-x :initform (make-instance 'node :name :accel-x) :accessor accel-x)
   (accel-y :initform (make-instance 'node :name :accel-y) :accessor accel-y)
   (accel-z :initform (make-instance 'node :name :accel-z) :accessor accel-z)
   (watermark :initform nil :accessor watermark)
   (buffer-length :initform nil :accessor buffer-length)
   (enabled :initform nil :accessor enabled)))

(defmethod close-dev ((dev iio-dev))
  (when (enabled dev)
    (disable-accelerometer dev)
    (setf (enabled dev) nil))

  (when (%file-stream dev)
    (close (%file-stream dev))
    (setf (%file-stream dev) nil))

  (when (dev-fd dev)
    (if (close-device dev)
	(funcall (close-device dev) (dev-fd dev))
	;; TODO: SBCL exclusive
	(sb-unix:unix-close (dev-fd dev)))
    (setf (dev-fd dev) nil)))

;; TODO: Add code to decide on frequency.
;; Reading too often is a waste of resources for what i'm trying to achieve.
;; TODO: Maybe make freqeuncy settable via config.
(defmethod shared-initialize :after ((dev iio-dev) slots &key)
  (declare (ignore slots))
  (with-slots (accel-x accel-y accel-z) dev
    (read-accelerometer-config dev)

    (read-interest dev :in-accel-x)
    (read-interest dev :in-accel-y)
    (read-interest dev :in-accel-z)

    ;; NOTE: In case if the device hasn't been properly disabled previously
    (when (and (enabled dev)
	       (not (enabled accel-x))
	       (not (enabled accel-y))
	       (not (enabled accel-z))
	       (not (eq (watermark dev) 1))
	       (not (eq (buffer-length dev) 2)))
      (disable-accelerometer dev))

    (enable-all-axis dev)
    (set-watermark dev 1)
    (set-buffer-length dev 3)

    (enable-accelerometer dev)))

(defmethod node-on? ((dev iio-dev) node)
  (let ((node (cadr (getf *accel-props* node))))
    (enabled (slot-value dev node))))

(defmethod read-interest ((dev iio-dev) prop)
  (let ((path-part (getf *accel-props* prop)))
    (unless path-part (error "Invalid property: ~a" prop))
    (let* ((prop-paths (get-accel-prop-paths (path dev) (first path-part)))
	   (node (slot-value dev (cadr path-part))))
      (setf (paths node) prop-paths)
      (read-initial-config node))))

;; TODO: Values might not be in order depicted here.
(defmethod read-accelerometer ((dev iio-dev))
  (let ((file (%file-stream dev))
	(reading (list (accel-x dev) (accel-y dev) (accel-z dev))))
    (let ((latest (loop for node in reading
			for int = (lisp-binary:read-integer
				     (bytes node)
				     file
				     :byte-order (case (endian node)
						   (:big :big-endian)
						   (:little :little-endian))
				     :signed (signed node))
			collect (read-node-value node int))))
      latest)))


(defmethod enable-all-axis ((dev iio-dev))
  (enable (accel-x dev))
  (enable (accel-y dev))
  (enable (accel-z dev)))

;; NOTE: For now unused. I'm reading only one sample - so don't really need to check how many samples are available.
(defmethod read-data-available ((dev iio-dev))
  (with-open-file (availability (merge-pathnames "buffer0/data_available" (path dev)))
    (parse-integer (read-line availability))))

(defmethod read-buffer-length ((dev iio-dev))
  (let ((length-path (merge-pathnames "buffer0/length" (path dev))))
    (with-open-file (length-file length-path)
      (setf (buffer-length dev) (parse-integer (read-line length-file))))))

;; TODO: For now implying that length value is same as watermark
(defmethod set-buffer-length ((dev iio-dev) length)
  (unless (typep length 'integer) (error "Invalid length: ~a" length))
  (unless (eq length (buffer-length dev))
    (let ((length-path (merge-pathnames "buffer0/length" (path dev))))
      (with-open-file (length-file length-path :direction :output :if-exists :overwrite)
	(format length-file "~a" length)
	(setf (buffer-length dev) length)))))

(defmethod read-watermark ((dev iio-dev))
  (let ((watermark-path (merge-pathnames "buffer0/watermark" (path dev))))
    (with-open-file (watermark-file watermark-path)
      (setf (watermark dev) (parse-integer (read-line watermark-file))))))

(defmethod set-watermark ((dev iio-dev) watermark)
  "Watermark. Or the number of samples that can be stored in the device buffer."
  (unless (typep watermark 'integer) (error "Invalid watermark: ~a" watermark))
  (unless (eq watermark (watermark dev))
    (let ((watermark-path (merge-pathnames "buffer0/watermark" (path dev))))
      (with-open-file (watermark-file watermark-path :direction :output :if-exists :overwrite)
	(format watermark-file "~a" watermark)
	(setf (watermark dev) watermark)))))

(defmethod read-accelerometer-config ((dev iio-dev))
  (with-open-file (enable-file (uiop:merge-pathnames* "buffer0/enable" (path dev)))
    (setf (enabled dev) (string= (read-line enable-file) "1")))
  (read-watermark dev)
  (read-buffer-length dev))


;; TODO: This is blindly assuming that the enable file is always in buffer0
;; NOTE: Will error out if no read interest on the nodes has been set.
(defmethod set-accelerometer-state ((dev iio-dev) on)
  (let* ((devpath (path dev))
	 (enable-path (uiop:merge-pathnames* "buffer0/enable" devpath)))
    (with-open-file (enable-file enable-path :direction :output :if-exists :overwrite)
      (format enable-file "~a" (if on "1" "0")))
    (setf (enabled dev) on)))

(defmethod enable-accelerometer ((dev iio-dev)) (unless (enabled dev) (set-accelerometer-state dev t)))
(defmethod disable-accelerometer ((dev iio-dev)) (when (enabled dev) (set-accelerometer-state dev nil)))


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

(defun find-accelerometer-dev (&key open-device close-device)
  (let* ((devs (list-iio-devs))
	 (devs (mapcar (lambda (dev) (make-instance 'dev :path dev :open-device open-device :close-device close-device)) devs))
	 (accel (find-if (lambda (dev) (string= (of-name dev) "accelerometer")) devs)))
    (when accel (change-class accel 'iio-dev))
    accel))


;; NOTE: Taken from:
;; https://github.com/sionescu/swap-bytes/blob/master/portable.lisp
(defun swap-bytes-16 (integer)
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior (ash (logand #xFF integer)  8)
          (ash integer -8)))
