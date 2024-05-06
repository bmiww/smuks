
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
    ;; TODO: Make an actual window manager instead of these random coordinates :)
    (setf (x surface) (random 800))
    (setf (y surface) (random 640))

    (setf (gethash id (xdg-surfaces xdg)) xdg-surface)
    (setf (role surface) xdg-surface)))

;; ┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐┌─┐
;; └─┐│ │├┬┘├┤ ├─┤│  ├┤
;; └─┘└─┘┴└─└  ┴ ┴└─┘└─┘
(defclass xdg-surface (xdg-surface:dispatch)
  ((wl-surface :initarg :wl-surface :accessor wl-surface)
   (width :initform 0 :accessor width)
   (height :initform 0 :accessor height)
   (toplevel :initarg :toplevel :accessor toplevel)
   (popup :initarg :popup :accessor popup)))

(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((toplevel (wl:mk-if 'toplevel xdg id)))
    (setf (toplevel xdg) toplevel)
    ;; TODO: Make an actual window manager instead of these random dimensions :)
    ;; TODO: One for maximized - get the enum stuff in order
    (xdg-toplevel:send-configure toplevel 200 200'(1))
    (xdg-surface:send-configure xdg (incf (configure-serial (wl-surface xdg))))))

(defmethod xdg-surface:set-window-geometry ((xdg xdg-surface) x y width height)
  (setf (width xdg) width)
  (setf (height xdg) height))


;; NOTE: For now leaving empty - but could be used in some way to finalize
;; The configuration sequence. Applying pending state or whatnot. Not sure
(defmethod xdg-surface:ack-configure ((xdg xdg-surface) serial)
  ())


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

;; TODO: For now keeping the move request empty
;; Since you probably want tiling - this will mostly be ignored
;; But - could introduce specific states/flags
(defmethod xdg-toplevel:move ((toplevel toplevel) seat serial)
  ())
