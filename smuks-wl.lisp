
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗      ██╗    ██╗██╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝      ██║    ██║██║
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗█████╗██║ █╗ ██║██║
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║╚════╝██║███╗██║██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║      ╚███╔███╔╝███████╗
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝       ╚══╝╚══╝ ╚══════╝
(defpackage :smuks-wl
  (:use :cl :wl :wl-wire :smuks-util)
  (:export display registry client read-wayland-message
	   wayland
	   write-wayland-message))
(in-package :smuks-wl)

;; TODO: Maybe put this into the display class. For now - not that important
(defvar *globals* (make-hash-table :test 'equal))
(defvar *global-id* #xFF000000)
(defun next-global-id () (incf *global-id*))

;; ┬ ┬┌─┐┬ ┬┬  ┌─┐┌┐┌┌┬┐
;; │││├─┤└┬┘│  ├─┤│││ ││
;; └┴┘┴ ┴ ┴ ┴─┘┴ ┴┘└┘─┴┘
(defclass wayland ()
  ((display :initarg :display :accessor display)))

(defmethod initialize-instance :after ((wayland wayland) &key)
  (setf (display wayland) (make-instance 'smuks-wl:display :id 1))
  (make-instance 'shm :id (next-global-id))
  (make-instance 'compositor :id (next-global-id))
  (make-instance 'xdg-wm-base :id (next-global-id)))

;; TODO: Figure out how to partially move this to the wire package
(defmethod read-wayland-message ((wayland wayland) client stream)
  (let* ((object-id (read-n-as-number stream 4))
	 (object (or (gethash object-id *globals*) (gethash object-id (objects client))))
	 (opcode (read-n-as-number stream 2)))
    (unless object (error (format nil "No object found for id: ~a" object-id)))
    (let* ((req-method (wl:match-request-opcode object opcode))
	 (req-arg-types (wl:get-request-arg-types object opcode))
	 (message-size (read-n-as-number stream 2))
	   (payload (read-req-args stream message-size req-arg-types)))

      ;; Discard extra bytes - since wayland messages are always 32-bit aligned
      (consume-padding stream message-size)


      (log! "📥 ~a(~a) ~a with ~a~%" (class-name (class-of object)) (wl:id object) req-method payload)
      (apply req-method `(,object ,client ,@payload)))))

;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client ()
  ((socket :initarg :socket :accessor socket)
   (callbacks :initform nil :accessor callbacks)
   (serial :initform 1 :accessor serial)
   (objects :initform (make-hash-table :test 'equal) :accessor objects)
   (pending :initform nil :accessor pending)))

(defmethod sock-stream ((client client))
  (unix-sockets:unix-socket-stream (socket client)))

;; TODO: Maybe this might need some way to track what kind of dependencies the callback has?
(defmethod add-callback ((client client) callback)
  (push callback (callbacks client)))

(defmethod next-serial ((client client))
  (prog1 (serial client)
    (incf (serial client))))

(defmethod object ((client client) id)
  (gethash id (objects client)))

(defmethod (setf object) (object (client client) id)
  (setf (gethash id (objects client)) object))

;; ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; │ ┬│  │ │├┴┐├─┤│
;; └─┘┴─┘└─┘└─┘┴ ┴┴─┘
(defclass global () ())

(defmethod initialize-instance :after ((global global) &key)
  (setf (gethash (wl::id global) *globals*) global))

(defgeneric bind (global client))

;; ┌┬┐┬┌─┐┌─┐┬  ┌─┐┬ ┬
;;  │││└─┐├─┘│  ├─┤└┬┘
;; ─┴┘┴└─┘┴  ┴─┘┴ ┴ ┴
(defclass display (wl/wl_display::wl_display global)
  ((clients :initarg :clients :accessor clients :initform nil)))

;; TODO: Registry needs to be cleaned up once the client disconnects.
(defmethod wl/wl_display::req-get_registry ((display display) client new-id)
  (let* ((registry (setf (object client new-id) (make-instance 'registry :id new-id))))
    (setf (pending client) t)

    (dohash (id global *globals*)
      ;; TODO: The id here - is not necessarily the same as an object id
      (wl/wl_registry::evt-global registry (sock-stream client) id (wl::ifname global) (wl::version global)))

    (setf (pending client) nil)))


(defmethod wl/wl_display::req-sync ((display display) client callback-id)
  (let* ((callback (make-instance 'callback :id callback-id)))
    ;; TODO: A shitty wait for now. Should instead create an event tracker pending events
    ;; TODO: For now i think this is even never really true
    (loop while (pending client)
	  do (sleep 0.1) (log! "⏲~%"))

    ;; TODO: For now - not tracking callbacks, just directly invoking it
    ;; (add-callback client callback-id)))
    ;; TODO: Destroy the callback object after invoking it
    (wl/wl_callback::evt-done callback (sock-stream client) (next-serial client))))

;; ┌─┐┬ ┬┌┬┐
;; └─┐├─┤│││
;; └─┘┴ ┴┴ ┴
(defclass shm (wl/wl_shm::wl_shm global) ())
(defvar *supported-formats* '(wl/wl_shm:ARGB8888 wl/wl_shm:XRGB8888))

(defmethod bind ((shm shm) client)
  (dolist (format *supported-formats*)
    (wl/wl_shm::evt-format shm (sock-stream client) format)))

(defmethod wl/wl_shm::evt-format ((shm shm) stream format)
  (write-event-args stream shm (match-event-opcode shm 'wl/wl_shm::evt-format) `(uint ,(wl/wl_shm::enum-format-value shm format))))



;; ┌─┐┌─┐┌┬┐┌─┐┌─┐┌─┐┬┌┬┐┌─┐┬─┐
;; │  │ ││││├─┘│ │└─┐│ │ │ │├┬┘
;; └─┘└─┘┴ ┴┴  └─┘└─┘┴ ┴ └─┘┴└─
(defclass compositor (wl/wl_compositor::wl_compositor global) ())

;; ─┐ ┬┌┬┐┌─┐   ┬ ┬┌┬┐   ┌┐ ┌─┐┌─┐┌─┐
;; ┌┴┬┘ │││ ┬───││││││───├┴┐├─┤└─┐├┤
;; ┴ └──┴┘└─┘   └┴┘┴ ┴   └─┘┴ ┴└─┘└─┘
(defclass xdg-wm-base (xdg/xdg_wm_base::xdg_wm_base global) ())

;; ┬─┐┌─┐┌─┐┬┌─┐┌┬┐┬─┐┬ ┬
;; ├┬┘├┤ │ ┬│└─┐ │ ├┬┘└┬┘
;; ┴└─└─┘└─┘┴└─┘ ┴ ┴└─ ┴
;; TODO: Add hooks for when something gets added or removed from the globals
;; This needs to notify all clients...
(defclass registry (wl/wl_registry::wl_registry) ())

;; TODO: Bind is weird - i do not know why i would care about the global object here any more
(defmethod wl/wl_registry::req-bind ((registry registry) client name id)
  (let ((bound-instance (make-instance (class-of (gethash name *globals*)) :id id)))
    (setf (object client id) bound-instance)
    (bind bound-instance client)))

(defmethod wl/wl_registry::evt-global ((registry registry) stream name interface version)
  (write-event-args stream registry (match-event-opcode registry 'wl/wl_registry::evt-global)
		    `(uint ,name)
		    `(wl:string ,interface)
		    `(uint ,version)))


;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴
(defclass callback (wl/wl_callback::wl_callback)
  ())

(defmethod wl/wl_callback::evt-done ((callback callback) stream callback-data)
  (write-event-args stream callback (match-event-opcode callback 'wl/wl_callback::evt-done) `(uint ,callback-data)))
