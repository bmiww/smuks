

;;  ██████╗ ██████╗ ███╗   ██╗███████╗██╗ ██████╗ ██╗   ██╗██████╗ ███████╗    ███████╗███████╗██████╗ ██╗ █████╗ ██╗
;; ██╔════╝██╔═══██╗████╗  ██║██╔════╝██║██╔════╝ ██║   ██║██╔══██╗██╔════╝    ██╔════╝██╔════╝██╔══██╗██║██╔══██╗██║
;; ██║     ██║   ██║██╔██╗ ██║█████╗  ██║██║  ███╗██║   ██║██████╔╝█████╗      ███████╗█████╗  ██████╔╝██║███████║██║
;; ██║     ██║   ██║██║╚██╗██║██╔══╝  ██║██║   ██║██║   ██║██╔══██╗██╔══╝      ╚════██║██╔══╝  ██╔══██╗██║██╔══██║██║
;; ╚██████╗╚██████╔╝██║ ╚████║██║     ██║╚██████╔╝╚██████╔╝██║  ██║███████╗    ███████║███████╗██║  ██║██║██║  ██║███████╗
;;  ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚══════╝    ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝╚═╝  ╚═╝╚══════╝
;; NOTE: A reuse class so far for xdg surfaces and layer-surfaces
(in-package :smuks)

(defclass surface-configure ()
  ((configure-serial :initform 0)
   (awaiting-ack :initform nil :accessor awaiting-ack)))

(defmethod configure-serial ((surface surface-configure)) (incf (slot-value surface 'configure-serial)))
(defmethod last-serial ((surface surface-configure)) (slot-value surface 'configure-serial))
