
(defpackage :smuks-wl
  (:use :cl :wl)
  (:export display registry client))
(in-package :smuks-wl)


;; ┌─┐┬  ┬┌─┐┌┐┌┌┬┐
;; │  │  │├┤ │││ │
;; └─┘┴─┘┴└─┘┘└┘ ┴
(defclass client ()
  ((socket :initarg :socket :accessor socket)
   (stream :reader sock-stream)))

(defmethod intialize-instance :after ((client client) &key)
  (setf (slot-value client 'stream) (unix-sockets:unix-socket-stream (socket client))))


;; ┌┬┐┬┌─┐┌─┐┬  ┌─┐┬ ┬
;;  │││└─┐├─┘│  ├─┤└┬┘
;; ─┴┘┴└─┘┴  ┴─┘┴ ┴ ┴
(defclass display (wl/wl_display::wl_display)
  ((registries :initarg :registries :accessor registries :initform nil)))

;; TODO: Registry needs to be cleaned up once the client disconnects.
(defmethod wl/wl_display::req-get_registry ((display display) new-id)
  (format "NEW ID REQUESTED FOR REGISTRY: ~a" new-id)
  (let* ((registry (make-instance 'registry :id new-id)))
    (push registry (registries display))))

(defmethod wl/wl_display::req-sync ((display display) callback-id)
  (format "SYNC REQUESTED: ~a" callback-id))


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
