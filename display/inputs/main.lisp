
;; ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗███████╗
;; ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝██╔════╝
;; ██║██╔██╗ ██║██████╔╝██║   ██║   ██║   ███████╗
;; ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║   ╚════██║
;; ██║██║ ╚████║██║     ╚██████╔╝   ██║   ███████║
;; ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝   ╚══════╝
(in-package :smuks)

(defmethod process ((display display) type (usecase (eql :passthrough)) event)
  (log! "No :passthrough handler for input event: ~a" (event-type event)))
