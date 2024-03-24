
(defpackage #:smuks/wayland
  (:use #:cl)
  (:export
   read-wayland-message))
(in-package :smuks/wayland)

(defstruct message
  (object-id :int)
  (opcode :int)
  (payload :int))

(defun payload-string (payload)
  (let ((length (make-array 4 :initial-element nil))
	)))

(defun exec-wayland-message (stream)
  (let* ((message (read-wayland-message stream))
	 (object (gethash (message-object-id message) wl:*objects*))
	 (req-method (wl:match-request-opcode object (message-opcode message))))
    ()))


(defun read-wayland-message (stream)
  (let* ((object-id (read-n-as-number stream 4))
	 (opcode (read-n-as-number stream 2))
	 (message-size (read-n-as-number stream 2))
	 (payload nil))

    (setf payload (make-array message-size :initial-element nil))
    (loop for i from 0 below message-size
	  do (setf (aref payload i) (read-byte stream)))

    ;; Discard extra bytes - since wayland messages are always 32-bit aligned
    (when (> (mod message-size 4) 0)
      (loop for i from 0 below (- 4 (mod message-size 4))
	    do (read-byte stream)))

    (format t "ID: ~a, SIZE: ~a, OPCODE: ~a, PAY: ~a~%" object-id message-size opcode payload)
    (make-message :object-id object-id
		  :opcode opcode
		  :payload payload)))

(defun read-n-as-number (stream n)
  (let ((num 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) num) (read-byte stream)))
    num))
