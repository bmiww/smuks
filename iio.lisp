
;; ██╗██╗ ██████╗
;; ██║██║██╔═══██╗
;; ██║██║██║   ██║
;; ██║██║██║   ██║
;; ██║██║╚██████╔╝
;; ╚═╝╚═╝ ╚═════╝
(in-package :smuks)

(defclass iio ()
  ((context :initarg :context :accessor iio-context)
   (accelerometer :initarg :accelerometer :accessor accelerometer)
   (version :initarg :version :reader iio-version)))

(defstruct accel x y z)

(defvar *landscape-upwardish* (make-accel :x -816 :y 512 :z -16))
(defvar *landscape-downwardish* (make-accel :x 1072 :y 0 :z 32))
(defvar *vertical-upwardish* (make-accel :x 80 :y -96 :z -1008))
(defvar *vertical-downwardish* (make-accel :x 96 :y 16 :z 1024))

(defun has-accelerometer-channels (channels)
  (loop for channel in channels
	for id = (iio:channel-id channel)
	when (or (string= id "accel_x") (string= id "accel_y") (string= id "accel_z"))
	  return t
	finally (return nil)))

(defun get-accelerometer (devices)
  (loop for device in devices
	for channels = (iio:device-channels device)
	when (has-accelerometer-channels channels)
	  return device
	finally (return nil)))

(defun init-libiio ()
  (let* (;; (version (iio:iio-library-get-version))
	 (version 1)
	 (iio (iio:create-local-context))
	 ;; NOTE: the context-info thing takes a while. Threaded init before everything else?
	 (info (iio:context-info iio))
	 (devices (iio:context-devices info))
	 (accelerometer (get-accelerometer devices)))
    (make-instance 'iio :context info :accelerometer accelerometer :version version)))

(defmethod enable-accelerometer-scan ((iio iio))
  (let* ((accelerometer (accelerometer iio))
	 (channels (iio:device-channels accelerometer)))
    (loop for channel in channels
	  for id = (iio:channel-id channel)
	  when (string= id "accel_x") do (iio:enable-channel accelerometer channel)
	  when (string= id "accel_y") do (iio:enable-channel accelerometer channel)
	  when (string= id "accel_z") do (iio:enable-channel accelerometer channel))
    (iio:device-create-buffer accelerometer 1)
    ;; TODO: Maybe do get-samples instead??
    (iio:buffer-refill accelerometer)
    (iio:get-poll-fd accelerometer)))

(defmethod accelerometer-fd ((iio iio)) (iio:get-poll-fd (accelerometer iio)))
(defmethod read-accelerometer ((iio iio))
  (print (iio:get-samples (accelerometer iio))))

(defmethod cleanup-iio ((iio iio))
  (iio:destroy-context (iio-context iio)))
