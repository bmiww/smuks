
(defpackage #:smuks/wayland
  (:use #:cl)
  (:export
   read-wayland-message))
(in-package :smuks/wayland)

(defun payload-string (stream)
  (let* ((length (read-n-as-number stream 4))
	 (array (make-array length)))
    (read-sequence array stream :end length)
    (consume-padding stream length)
    (coerce array 'string)))

;; TODO: For now - this just returns an array with the bytes. It is currently up to the implementation to interpret the bytes.
(defun payload-array (stream)
  (let* ((length (read-n-as-number stream 4))
	 (array (make-array length)))
    (read-sequence array stream :end length)
    (consume-padding stream length)
    array))

(defun read-fixnum (stream)
  (let* ((int (read-n-as-number stream 3))
	 (sign (logand int 1))
	 (int (ash int -1)) ;; TODO: This line might be total bullshit
	 (dec (read-n-as-number stream 1))
	 (dec (* 0.00000001 dec)))
    (if (= sign 1)
	(* -1 (+ int dec))
	(+ int dec))))

(defun read-int (stream)
  (let* ((int (read-n-as-number stream 4))
	 (sign (logand int 1))
	 (int (ash int -1))) ;; TODO: This line might be total bullshit
    (if (= sign 1)
	(- int)
	int)))

(defun read-fd (stream)
  (break) ;; TODO: Breaking here - since i'm pretty sure this ain't gonna work
  (unix-sockets::ancillary-fd stream))

(defun read-enum (stream enum-ref)
  (let* ((num (read-n-as-number stream 4))
	 (package-name (car enum-ref))
	 (enum-name (cadr enum-ref))
	 (enum (find-symbol (format nil "~A::~A" package-name enum-name))))
    (funcall enum num)))

;; NOTE: For the wire protocol details, see:
;; https://wayland-book.com/protocol-design/wire-protocol.html
;; NOTE: Theres also this - which has some other clarifications/confusions:
;; https://wayland.freedesktop.org/docs/html/ch04.html
(defun read-req-args (stream message-size arg-types)
  ;; TODO: Maybe instead of ignoring - you could keep a counter as to how many bytes were read by each arg-type in the list
  ;; Then the difference could be discarded (wayland pads the payload to have word lengths 32bits)
  (declare (ignore message-size))
  (let ((args nil))
    (dolist (arg-type arg-types)
      (when (listp arg-type) (setf arg-type (first arg-type)))
      (push
       (case arg-type
	 (wl:int (read-int stream))
	 (wl:uint (read-n-as-number stream 4))
	 (wl:object (read-n-as-number stream 4))
	 ;; TODO: Figure out if i should already allocate a new object here, or later
	 ;; Maybe this should be left up to the method implementation
	 ;; TODO: Another wire protocol document mentions that this is not just a number, but could be prepended by
	 ;; A string identifying the interface and a uint specifying the version as well
	 (wl:new_id (read-n-as-number stream 4))
	 (wl:fixed (read-fixnum stream))
	 (wl:string (payload-string stream))
	 (wl:array (payload-array stream))
	 ;; TODO: Update the unix-sockets library to allow reading the FD from the socket ancillary data
	 (wl:fd (read-fd stream))
	 ;; TODO: Need to add the enum parsing for the protocol generator thing
	 ;; NOTE: Basically an integer, but needs to be matched against the request enum values
	 ;; Since it could be a bitmap of the enum values (more than one flag active)
	 (wl:enum (read-enum stream (cadr arg-type)))
	 (t (error "Unknown arg-type")))
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

    ;; Discard extra bytes - since wayland messages are always 32-bit aligned
    (consume-padding stream message-size)

    (format t "Calling ~a with ~a~%" req-method payload)
    (apply req-method payload)))

(defun consume-padding (stream size)
  (when (> (mod size 4) 0)
    (loop for i from 0 below (- 4 (mod size 4))
	  do (read-byte stream))))

(defun read-n-as-number (stream n)
  (let ((num 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) num) (read-byte stream)))
    num))
