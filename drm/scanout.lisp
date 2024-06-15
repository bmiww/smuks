
;; ███████╗ ██████╗ █████╗ ███╗   ██╗ ██████╗ ██╗   ██╗████████╗
;; ██╔════╝██╔════╝██╔══██╗████╗  ██║██╔═══██╗██║   ██║╚══██╔══╝
;; ███████╗██║     ███████║██╔██╗ ██║██║   ██║██║   ██║   ██║
;; ╚════██║██║     ██╔══██║██║╚██╗██║██║   ██║██║   ██║   ██║
;; ███████║╚██████╗██║  ██║██║ ╚████║╚██████╔╝╚██████╔╝   ██║
;; ╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝  ╚═════╝    ╚═╝
;; NOTE: Primarily emulating this example
;; https://github.com/dvdhrm/docs/blob/master/drm-howto/modeset-atomic.c
;; NOTE: The user is still expected to handle events from the card
(in-package :smuks-drm)

(defmacro just-page-flip (drm fb connector &body body)
  `(progn
     (handler-case
	 (progn
	   (page-flip ,drm ,fb ,connector))
       (error (err) (declare (ignore err)) ()))
     ,@body))
