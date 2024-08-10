
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
  ((surface :initarg :surface :accessor surface)))
