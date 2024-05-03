
;; ██████╗ ██████╗       ███╗   ███╗ █████╗ ███╗   ██╗ █████╗  ██████╗ ███████╗██████╗
;; ██╔══██╗██╔══██╗      ████╗ ████║██╔══██╗████╗  ██║██╔══██╗██╔════╝ ██╔════╝██╔══██╗
;; ██║  ██║██║  ██║█████╗██╔████╔██║███████║██╔██╗ ██║███████║██║  ███╗█████╗  ██████╔╝
;; ██║  ██║██║  ██║╚════╝██║╚██╔╝██║██╔══██║██║╚██╗██║██╔══██║██║   ██║██╔══╝  ██╔══██╗
;; ██████╔╝██████╔╝      ██║ ╚═╝ ██║██║  ██║██║ ╚████║██║  ██║╚██████╔╝███████╗██║  ██║
;; ╚═════╝ ╚═════╝       ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝
;; Data device manager
(in-package :smuks)

;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass dd-manager (wl-dd-mgr:dispatch)
  ((devices :initform (make-hash-table :test 'equal) :accessor devices)))

(defmethod wl-dd-mgr:get-data-device ((mgr dd-manager) id seat)
  (setf (gethash id (devices mgr)) (wl:mk-if 'data-device mgr id :seat seat)))


;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass data-device (wl_data_device:dispatch)
  ((id :initarg :id :accessor id)
   (seat :initarg :seat :accessor seat)))
