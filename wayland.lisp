
(defpackage #:smuks/wayland
  (:use #:cl)
  (:export
   read-wayland-message))
(in-package :smuks/wayland)

(defvar *all-objects* (make-hash-table :test 'eq))

(defun read-wayland-message (stream)
  (let ((object-id (make-array 4 :initial-element nil))
	(message-size (make-array 2 :initial-element nil))
	(opcode (make-array 2 :initial-element nil))
	(payload nil))
    (loop for i from 0 below 4
	  do (setf (aref object-id i) (read-byte stream)))
    (loop for i from 0 below 2
	  do (setf (aref opcode i) (read-byte stream)))
    (loop for i from 0 below 2
	  do (setf (aref message-size i) (read-byte stream)))
    (setf payload (make-array (bytes-to-num message-size) :initial-element nil))
    (loop for i from 0 below (bytes-to-num message-size)
	  do (setf (aref payload i) (read-byte stream)))

    ;; Discard extra bytes - since wayland messages are always 32-bit aligned
    (when (> (mod (bytes-to-num message-size) 4) 0)
      (loop for i from 0 below (- 4 (mod (bytes-to-num message-size) 4))
	    do (read-byte stream)))

    (format t "ID: ~a, SIZE: ~a, OPCODE: ~a, PAY: ~a~%"
	    (bytes-to-num object-id) (bytes-to-num message-size)
	    (bytes-to-num opcode) payload)))


(defun bytes-to-num (byte-arr)
  (let ((num 0))
    (dotimes (i (length byte-arr))
      (setf (ldb (byte 8 (* i 8)) num) (aref byte-arr i)))
    num))
