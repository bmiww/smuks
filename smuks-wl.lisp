
(defpackage :smuks-wl
  (:use :cl :wl)
  (:export display registry))
(in-package :smuks-wl)

(defclass display (wl/wl_display::wl_display)
  ((registries :initarg :registries :accessor registries :initform nil)))

;; TODO: Registry needs to be cleaned up once the client disconnects.
(defmethod wl/wl_display::req-get_registry ((display display) new-id)
  (format "NEW ID REQUESTED FOR REGISTRY: ~a" new-id)
  (let* ((registry (make-instance 'registry :id new-id)))
    (push registry (registries display))))


;; (defmethod wl/wl_display::req-sync ((display )))

(defclass registry (wl/wl_registry::wl_registry)
  ())
