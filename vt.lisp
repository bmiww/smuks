
;; VT
(in-package :smuks)

(defun switch-vt (seat num)
  (pause-outputs *wayland*)
  (libseat:switch-session seat num))
