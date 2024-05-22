
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
    (setf (gethash id (xdg-surfaces xdg)) xdg-surface)
    (setf (role surface) xdg-surface)))

;; ┌─┐┬ ┬┬─┐┌─┐┌─┐┌─┐┌─┐
;; └─┐│ │├┬┘├┤ ├─┤│  ├┤
;; └─┘└─┘┴└─└  ┴ ┴└─┘└─┘
(defclass xdg-surface (xdg-surface:dispatch surface)
  ((toplevel :initarg :toplevel :accessor toplevel)
   (popup :initarg :popup :accessor popup)))

(defmethod xdg-surface:get-toplevel ((xdg xdg-surface) id)
  (let ((display (wl:get-display xdg)))
    (wl:up-if 'toplevel xdg id)
    (setf (role xdg) xdg)

    (new-toplevel display xdg)

      ;; TODO: One for maximized - get the enum stuff in order
    (xdg-toplevel:send-configure xdg (width xdg) (height xdg) '(1))
    (xdg-surface:send-configure xdg (incf (configure-serial xdg)))))

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
(defclass toplevel (xdg-toplevel:dispatch xdg-surface)
  ((title :initform nil :accessor title)
   (app-id :initform nil :accessor app-id)
   (parent :initform nil :accessor parent)
   (min-width :initform 0 :accessor min-width)
   (min-height :initform 0 :accessor min-height)
   (max-width :initform 0 :accessor max-width)
   (max-height :initform 0 :accessor max-height)))

(defmethod xdg-toplevel:set-title ((toplevel toplevel) title)
  (setf (title toplevel) title))

(defmethod xdg-toplevel:set-app-id ((toplevel toplevel) app-id)
  (setf (app-id toplevel) app-id))

;; TODO: For now keeping the move request empty
;; Since you probably want tiling - this will mostly be ignored
;; But - could introduce specific states/flags
(defmethod xdg-toplevel:move ((toplevel toplevel) seat serial)
  (log! "xdg-toplevel:move: Not implemented"))

(defmethod xdg-toplevel:set-parent ((toplevel toplevel) parent)
  (log! "xdg-toplevel:set-parent: Not implemented fully")
  (when parent (setf (parent toplevel) parent)))

(defmethod xdg-toplevel:set-min-size ((toplevel toplevel) width height)
  (log! "xdg-toplevel:set-min-size: size limitations still ignored")
  (setf (min-width toplevel) width)
  (setf (min-height toplevel) height))

(defmethod xdg-toplevel:set-max-size ((toplevel toplevel) width height)
  (log! "xdg-toplevel:set-max-size: size limitations still ignored")
  (setf (max-width toplevel) width)
  (setf (max-height toplevel) height))

;; TODO: Integrate better with the sizing/positioning once you figure that out
(defmethod xdg-toplevel:set-maximized ((toplevel toplevel))
  "A client wants to maximize their window to maximum size.
For tiling managers - i think i'll just resend the original configure event.
Supposed to answer with a configure event showing the new size."
  (log! "xdg-toplevel:set-maximized: Not considered in great detail")
  (xdg-toplevel:send-configure toplevel (width toplevel) (height toplevel) '(1))
  (xdg-surface:send-configure toplevel (incf (configure-serial toplevel))))

(defmethod xdg-toplevel:unset-maximized ((toplevel toplevel))
  "A client wants to unset maximized state.
For tiling managers - i think i'll just resend the original configure event.
Supposed to answer with a configure event showing the new size."
  (log! "xdg-toplevel:unset-maximized: Not considered in great detail")
  (xdg-toplevel:send-configure toplevel (width toplevel) (height toplevel) '(1))
  (xdg-surface:send-configure toplevel (incf (configure-serial toplevel))))
