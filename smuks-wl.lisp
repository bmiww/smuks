
(defpackage :smuks-wl
  (:use :cl :wl :wl-wire)
  (:export display registry client read-wayland-message
	   wayland
	   add-client
	   write-wayland-message))
(in-package :smuks-wl)

;; ┬ ┬┌─┐┬ ┬┬  ┌─┐┌┐┌┌┬┐
;; │││├─┤└┬┘│  ├─┤│││ ││
;; └┴┘┴ ┴ ┴ ┴─┘┴ ┴┘└┘─┴┘
(defclass wayland ()
  ((display :initarg :display :accessor display)))

(defmethod add-client ((wayland wayland) client)
  (let* ((client (make-instance 'smuks-wl:client :socket client))
	 (stream (sock-stream client)))
    ;; TODO: This being a list is possibly problematic for removal?
    ;; Depends on the amount of clients i guess. And only member/ref access rather than id
    (push client (clients (display wayland)))
    (format t "CLIENT CONNECTED~%")
    (bt:make-thread (lambda ()
		      (loop (read-wayland-message wayland client stream))
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

    (format t "📥 ~a with ~a~%" req-method payload)
    ;; TODO: Message sizes are very very wrong. which makes me believe that the read-as-number might be wrong
    (format t "Message size was ~a~%" message-size)
    (apply req-method `(,object ,client ,@payload))))

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client ()
  ((socket :initarg :socket :accessor socket)
   (callbacks :initform nil :accessor callbacks)
   (serial :initform 1 :accessor serial)))

(defmethod sock-stream ((client client))
  (unix-sockets:unix-socket-stream (socket client)))

;; TODO: Maybe this might need some way to track what kind of dependencies the callback has?
(defmethod add-callback ((client client) callback)
  (push callback (callbacks client)))

(defmethod next-serial ((client client))
  (prog1 (serial client)
    (incf (serial client))))

;; ┌┬┐┬┌─┐┌─┐┬  ┌─┐┬ ┬
;;  │││└─┐├─┘│  ├─┤└┬┘
;; ─┴┘┴└─┘┴  ┴─┘┴ ┴ ┴
(defclass display (wl/wl_display::wl_display)
  ((registries :initarg :registries :accessor registries :initform nil)
   (clients :initarg :clients :accessor clients :initform nil)))

;; TODO: Registry needs to be cleaned up once the client disconnects.
(defmethod wl/wl_display::req-get_registry ((display display) client new-id)
  ;; (format t "NEW ID REQUESTED FOR REGISTRY: ~a~%" new-id)
  (let* ((registry (make-instance 'registry :id new-id)))
    (push registry (registries display))))

(defmethod wl/wl_display::req-sync ((display display) client callback-id)
  ;; (format t "SYNC REQUESTED: ~a" callback-id)
  (let* ((callback (make-instance 'callback :id callback-id)))

    ;; TODO: For now - not tracking callbacks, just directly invoking it
    ;; (add-callback client callback-id)))
    (wl/wl_callback::done callback (sock-stream client) (next-serial client))))


;; ┬─┐┌─┐┌─┐┬┌─┐┌┬┐┬─┐┬ ┬
;; ├┬┘├┤ │ ┬│└─┐ │ ├┬┘└┬┘
;; ┴└─└─┘└─┘┴└─┘ ┴ ┴└─ ┴
(defclass registry (wl/wl_registry::wl_registry)
  ())


;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴
(defclass callback (wl/wl_callback::wl_callback)
  ())

(defmethod wl/wl_callback::done ((callback callback) stream callback-data)
  (write-event-args stream (wl::id callback) (match-event-opcode callback 'wl/wl_callback::evt-done) `(uint ,callback-data)))
