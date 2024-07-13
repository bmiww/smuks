
;;  ██████╗██╗     ██╗███████╗███╗   ██╗████████╗
;; ██╔════╝██║     ██║██╔════╝████╗  ██║╚══██╔══╝
;; ██║     ██║     ██║█████╗  ██╔██╗ ██║   ██║
;; ██║     ██║     ██║██╔══╝  ██║╚██╗██║   ██║
;; ╚██████╗███████╗██║███████╗██║ ╚████║   ██║
;;  ╚═════╝╚══════╝╚═╝╚══════╝╚═╝  ╚═══╝   ╚═╝
(in-package :smuks)

(defclass client (wl:client)
  ((compositor :initform nil :accessor compositor)
   (seat :initform nil :accessor seat)
   (dd-manager :initform nil :accessor dd-manager)))

(defmethod (setf wl::iface) :after ((iface compositor) (client client) id)
  (declare (ignore id))
  (setf (compositor client) iface))

(defmethod (setf wl::iface) :after ((iface seat) (client client) id)
  (declare (ignore id))
  (setf (seat client) iface))

(defmethod (setf wl::iface) :after ((iface dd-manager) (client client) id)
  (declare (ignore id))
  (setf (dd-manager client) iface))

(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :type t)
    (let* ((compositor (compositor client))
	   (surfaces (when compositor (surfaces compositor)))
	   (toplevel (when surfaces (loop for surface being the hash-values of surfaces
					  when (typep surface 'toplevel) return surface))))
      (when toplevel (format stream "Client: ~a:::~a" (title toplevel) (app-id toplevel))))))
