
(defpackage #:smuks/wayland
  (:use #:cl)
  (:export
   read-wayland-message))
(in-package :smuks/wayland)

(defun payload-string (payload)
  (let ((length (make-array 4 :initial-element nil))
	)))

(defun read-wayland-message (stream)
  (let* ((object-id (read-n-as-number stream 4))
	 (object (gethash object-id wl:*objects*))
	 (opcode (read-n-as-number stream 2))
	 (req-method (wl:match-request-opcode object opcode))
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
    ))

(defun read-n-as-number (stream n)
  (let ((num 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) num) (read-byte stream)))
    num))
