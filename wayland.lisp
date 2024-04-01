
(defpackage #:smuks/wayland
  (:use #:cl #:wl-wire)
  (:export
   read-wayland-message
   wayland
   add-client
   write-wayland-message))
(in-package :smuks/wayland)

;; ┬ ┬┌─┐┬ ┬┬  ┌─┐┌┐┌┌┬┐
;; │││├─┤└┬┘│  ├─┤│││ ││
;; └┴┘┴ ┴ ┴ ┴─┘┴ ┴┘└┘─┴┘
(defclass wayland ()
  ((display :initarg :display :accessor display)
   (clients :initarg :clients :accessor clients)))

(defmethod add-client ((wayland wayland) client)
  (let* ((client (make-instance 'smuks-wl:client :socket client))
	 (stream (sock-stream client)))
    ;; TODO: This being a list is possibly problematic for removal?
    ;; Depends on the amount of clients i guess. And only member/ref access rather than id
    (push client (slot-value wayland 'clients))
    (format t "CLIENT CONNECTED~%")
    (bt:make-thread (lambda ()
		      ;; TODO: *smuks-exit* is probably not available here
		      ;; TODO: *smuks-exit* is probably not the most sensible check here anyways
		      (loop until *smuks-exit*
			    do (smuks/wayland:read-wayland-message wayland client stream))
		      (format t "Exiting client~%")
		      (unix-sockets:close-unix-socket (socket client))))))


(defmethod initialize-instance :after ((wayland wayland) &key)
  (setf (display wayland) (make-instance 'smuks-wl:display :id 1)))

(defmethod read-wayland-message ((wayland wayland) client stream)
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
    (apply req-method `(,object ,@payload))))

(defun write-wayland-message (client &rest args)
  (let* ((stream (sock-stream client))
	 ()
	 (object (first args))
	 (opcode (second args))
	 (message-size (third args))
	 (payload (fourth args)))
    (write-n-as-number stream (wl:id object) 4)
    (write-n-as-number stream opcode 2)
    (write-n-as-number stream message-size 2)
    (write-req-args stream payload)))
