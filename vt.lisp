
;; ██╗   ██╗████████╗
;; ██║   ██║╚══██╔══╝
;; ██║   ██║   ██║
;; ╚██╗ ██╔╝   ██║
;;  ╚████╔╝    ██║
;;   ╚═══╝     ╚═╝
(in-package :smuks)

(defun switch-vt (display seat num)
  (pause-outputs display)
  (libseat:switch-session seat num))
