
;; VT
(in-package :smuks)

(defun switch-vt (seat num)
  (libseat:switch-session seat num))
