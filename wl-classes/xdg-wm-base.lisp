
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
  (let ((xdg-surface (wl:mk-if 'xdg-surface xdg id :wl-surface surface)))
    (setf (gethash id (xdg-surfaces xdg)) xdg-surface)
    xdg-surface))

;; ┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐┌─┐
;; └─┐│ │├┬┘├┤ ├─┤│  ├┤
;; └─┘└─┘┴└─└  ┴ ┴└─┘└─┘
(defclass xdg-surface (xdg-surface:dispatch)
  ((role :initform nil :accessor role)
   (wl-surface :initarg :wl-surface :accessor wl-surface)
   (toplevel :initarg :toplevel :accessor toplevel)))

(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((toplevel (wl:mk-if 'toplevel xdg id)))
    (setf (toplevel xdg) toplevel)
    toplevel))

;; ┌┬┐┌─┐┌─┐┬  ┌─┐┬  ┬┌─┐┬
;;  │ │ │├─┘│  ├┤ └┐┌┘├┤ │
;;  ┴ └─┘┴  ┴─┘└─┘ └┘ └─┘┴─┘
(defclass toplevel (xdg-toplevel:dispatch)
  ((title :initform nil :accessor title)
   (app-id :initform nil :accessor app-id)))

(defmethod xdg-toplevel:set-title ((toplevel toplevel) title)
  (setf (title toplevel) title))

(defmethod xdg-toplevel:set-app-id ((toplevel toplevel) app-id)
  (setf (app-id toplevel) app-id))
