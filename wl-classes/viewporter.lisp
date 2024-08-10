
;; ██╗   ██╗██╗███████╗██╗    ██╗██████╗  ██████╗ ██████╗ ████████╗███████╗██████╗
;; ██║   ██║██║██╔════╝██║    ██║██╔══██╗██╔═══██╗██╔══██╗╚══██╔══╝██╔════╝██╔══██╗
;; ██║   ██║██║█████╗  ██║ █╗ ██║██████╔╝██║   ██║██████╔╝   ██║   █████╗  ██████╔╝
;; ╚██╗ ██╔╝██║██╔══╝  ██║███╗██║██╔═══╝ ██║   ██║██╔══██╗   ██║   ██╔══╝  ██╔══██╗
;;  ╚████╔╝ ██║███████╗╚███╔███╔╝██║     ╚██████╔╝██║  ██║   ██║   ███████╗██║  ██║
;;   ╚═══╝  ╚═╝╚══════╝ ╚══╝╚══╝ ╚═╝      ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝
(in-package :smuks)

(defclass viewporter (wp-viewporter:dispatch)
  ())

(defmethod wp-viewporter:get-viewport ((viewporter viewporter) viewport-id surface)
  (wl:mk-if 'viewport viewporter viewport-id))


;; ┬  ┬┬┌─┐┬ ┬┌─┐┌─┐┬─┐┌┬┐
;; └┐┌┘│├┤ │││├─┘│ │├┬┘ │
;;  └┘ ┴└─┘└┴┘┴  └─┘┴└─ ┴

(defclass viewport (wp-viewport:dispatch)
  ;; TODO: You wanted to make a surface inherit the viewport class instead
  ((surface :initarg :surface :accessor surface)
   (view-x :initform nil :accessor view-x)
   (view-y :initform nil :accessor view-y)
   (view-w :initform nil :accessor view-w)
   (view-h :initform nil :accessor view-h)
   (view-dest-h :initform nil :accessor view-dest-h)
   (view-dest-w :initform nil :accessor view-dest-w)))

(defmethod wp-viewport:set-destination ((viewport viewport) w h)
  (setf (view-dest-w viewport) w
	(view-dest-h viewport) h))
