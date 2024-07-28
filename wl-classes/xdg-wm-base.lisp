
;; ██╗  ██╗██████╗  ██████╗
;; ╚██╗██╔╝██╔══██╗██╔════╝
;;  ╚███╔╝ ██║  ██║██║  ███╗
;;  ██╔██╗ ██║  ██║██║   ██║
;; ██╔╝ ██╗██████╔╝╚██████╔╝
;; ╚═╝  ╚═╝╚═════╝  ╚═════╝
(in-package :smuks)

;; ┬ ┬┌┬┐  ┌┐ ┌─┐┌─┐┌─┐
;; ││││││  ├┴┐├─┤└─┐├┤
;; └┴┘┴ ┴  └─┘┴ ┴└─┘└─┘
(defclass wm-base (xdg-wm-base:dispatch)
  ((xdg-surfaces :initform (make-hash-table :test 'eq) :accessor xdg-surfaces)))

(defmethod xdg-wm-base:get-xdg-surface ((xdg wm-base) id surface)
  (let ((xdg-surface (wl:up-if 'xdg-surface surface id)))
    (setf (gethash id (xdg-surfaces xdg)) xdg-surface)))

(defmethod xdg-wm-base:create-positioner ((xdg wm-base) id)
  (wl:mk-if 'positioner xdg id))
