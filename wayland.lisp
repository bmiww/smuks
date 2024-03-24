
(defpackage #:smuks/wayland
  (:use #:cl)
  (:export
   read-wayland-message))
(in-package :smuks/wayland)

(defun payload-string (stream)
  (let ((length (read-n-as-number stream 4)))
    ))

(defun payload-array (stream)
  (let ((length (read-n-as-number stream 4)))
    ))

;; NOTE: For the wire protocol details, see:
;; https://wayland-book.com/protocol-design/wire-protocol.html
(defun read-req-args (stream message-size arg-types)
  ;; TODO: Maybe instead of ignoring - you could keep a counter as to how many bytes were read by each arg-type in the list
  ;; Then the difference could be discarded (wayland pads the payload to have word lengths 32bits)
  (declare (ignore message-size))
  (let ((args nil))
    (dolist (arg-type arg-types)
      (push
       (case arg-type
	 ;; TODO: This actually needs to be checked for how the sign arrives for interpreting the bytes
	 (int (read-n-as-number stream 4))
	 (uint (read-n-as-number stream 4))
	 (object (read-n-as-number stream 4))
	 ;; TODO: Figure out if i should already allocate a new object here, or later
	 ;; Maybe this should be left up to the method implementation
	 (new-id (read-n-as-number stream 4))
	 ;; TODO: Figure out how the fixed number is represented in bytes
	 (fixed (error "Fixed number parsing from wayland message not implemented"))
	 (string (payload-string stream))
	 (array (payload-array stream))
	 ;; TODO: Update the unix-sockets library to allow reading the FD from the socket ancillary data
	 (fd (error "FD parsing from wayland message not implemented")))
       args))
    args))




(defun read-wayland-message (stream)
  (let* ((object-id (read-n-as-number stream 4))
	 (object (gethash object-id wl:*objects*))
	 (opcode (read-n-as-number stream 2))
	 (req-method (wl:match-request-opcode object opcode))
	 (req-arg-types (wl:get-request-arg-types object opcode))
	 (message-size (read-n-as-number stream 2))
	 (payload (read-req-args stream message-size req-arg-types)))

    ;; (setf payload (make-array message-size :initial-element nil))
    ;; (loop for i from 0 below message-size
	  ;; do (setf (aref payload i) (read-byte stream)))

    ;; Discard extra bytes - since wayland messages are always 32-bit aligned
    (when (> (mod message-size 4) 0)
      (loop for i from 0 below (- 4 (mod message-size 4))
	    do (read-byte stream)))

    (format t "Calling ~a with ~a~%" req-method payload)
    (apply req-method payload)))

(defun read-n-as-number (stream n)
  (let ((num 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) num) (read-byte stream)))
    num))
