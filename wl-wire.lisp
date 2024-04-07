
;; ██╗    ██╗██╗██████╗ ███████╗
;; ██║    ██║██║██╔══██╗██╔════╝
;; ██║ █╗ ██║██║██████╔╝█████╗
;; ██║███╗██║██║██╔══██╗██╔══╝
;; ╚███╔███╔╝██║██║  ██║███████╗
;;  ╚══╝╚══╝ ╚═╝╚═╝  ╚═╝╚══════╝
;; NOTE: For the wire protocol details, see:
;; https://wayland-book.com/protocol-design/wire-protocol.html
;; NOTE: Theres also this - which has some other clarifications/confusions:
;; https://wayland.freedesktop.org/docs/html/ch04.html

(defpackage :wl-wire
  (:use :cl)
  (:export consume-padding write-event-args read-req-args read-n-as-number))
(in-package :wl-wire)

;; ┌─┐┌┐┌┌─┐┌─┐┌┬┐┬┌┐┌┌─┐
;; ├┤ ││││  │ │ │││││││ ┬
;; └─┘┘└┘└─┘└─┘─┴┘┴┘└┘└─┘
;; NOTE: This could be an interesting reference material.
;; Basically its someone writing a client without libwayland, so included are the encoding functions
;; https://gaultier.github.io/blog/wayland_from_scratch.html?fbclid=IwAR2gRvsJvilJG8GcIoiywhOZxhiqy6lA_g_yCC95wo2bwYhEz8Myo_iU0zw_aem_ASJrcFMP5q2_3xeV-bkU_HAvTDxkisMVpIfiKfx2d-Ax0oKjLpDiUzpL_uc306l2Xn42-D-sh-I0wCizA4hEedJC

;; TODO: Copilot generated this.
;; On first look it seems alright. Needs testing
(defun align-32-bit-msg-size (size)
  (let ((aligned-size (mod (+ size 3) 4)))
    (if (zerop aligned-size)
	size
	(+ size (- 4 aligned-size)))))

(defun calculate-message-size (args)
  ;; NOTE: Message size is number of bytes in the header + the payload
  (let ((*message-size* 8))
    (dolist (arg args)
      (let* ((type (car arg))
	     (value (cadr arg)))
	(case type
	  (wl:int (incf *message-size* 4))
	  (wl:uint (incf *message-size* 4))
	  (wl:fixed (incf *message-size* 4))
	  (wl:object (incf *message-size* 4))
	  (wl:new_id (incf *message-size* 4))

	  ;; TODO: FD does not really increase the payload size, but there are other considerations
	  ;; Although - i'm unsure if an FD ever is an argument in a wayland event.
	  (wl:fd (incf *message-size* 0))

	  ;; TODO: I guess this was utf8. Hoping for the best.
	  (wl:string (incf *message-size* (align-32-bit-msg-size (length value))))

	  ;; TODO: The base protocol only does this for the currently presset keys array on an enter event
	  ;; which afaik should be uint array, so i can assume that i can just multiply by 4
	  (wl:array (incf *message-size* (* 4 (length value))))

	  (wl:enum (incf *message-size* 4))
	  (t (error "Unknown type %s" type)))))
    *message-size*))

;; TODO: Depending on how much you can be arsed - you might want to define encoders for several lisp types
;; that could correspond to the wayland types
(defun write-event-args (stream obj opcode &rest args)
  (let ((obj-id (wl::id obj))
	(message-size (calculate-message-size args)))
    (format t "📨 ~a(~a) op:~a with ~a, size:~a~%" (class-name (class-of obj)) obj-id opcode args message-size)

    (write-number-bytes stream obj-id 4)
    (write-number-bytes stream opcode 2)
    (write-number-bytes stream message-size 2)

    (dolist (arg args)
      (let* ((type (car arg))
	     (value (cadr arg)))
	(case type
	  ;; TODO: You still need to figure out the proper parsing of integers, let alone encoding them
	  (wl:int (write-number-bytes stream value 4))
	  (wl:uint (write-number-bytes stream value 4))
	  ;; TODO: You still need to figure out the proper parsing of fixnums, let alone encoding them
	  (wl:fixed (write-number-bytes stream value 4))
	  (wl:object (write-number-bytes stream value 4))
	  (wl:new_id (write-number-bytes stream value 4))
	  (wl:fd (error "FD through ancillary not implemented"))
	  (wl:string (write-a-string stream value))
	  (wl:array (write-array stream value))
	  ;; TODO: Enum might be wrong. It's possible that bitfields are a possibility in which case i would need to
	  ;; Generate proper numbers from multiple symbols
	  (wl:enum (write-number-bytes stream (wl:match-event-opcode value) 4)))))))


;; ┌┬┐┌─┐┌─┐┌─┐┌┬┐┬┌┐┌┌─┐
;;  ││├┤ │  │ │ │││││││ ┬
;; ─┴┘└─┘└─┘└─┘─┴┘┴┘└┘└─┘
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

;; NOTE: Message size is number of bytes in the header + the payload
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


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun consume-padding (stream size)
  (when (> (mod size 4) 0)
    (loop for i from 0 below (- 4 (mod size 4))
	  do (read-byte stream))))

(defun read-n-as-number (stream n)
  (let ((num 0))
    (dotimes (i n)
      (setf (ldb (byte 8 (* i 8)) num) (read-byte stream)))
    num))

(defun write-a-string (stream string)
  (let ((length (length string)))
    (write-sequence (coerce string 'vector) stream)
    (loop for i from 0 below (- 4 (mod length 4))
	  do (write-byte 0 stream))))

(defun write-array (stream array)
  (let ((length (length array)))
    ;; TODO: This might or might not work. The type of the array elements is kind of not specified enough
    ;; It could be that i might need to actually go byte by byte inside of each of the numbers of the array
    ;; Since the only currently known use case for me is numbers
    (write-sequence array stream)))

(defun write-number-bytes (stream num n)
  (dotimes (i n)
    (write-byte (ldb (byte 8 (* i 8)) num) stream)))
