
;; ██╗   ██╗████████╗
;; ██║   ██║╚══██╔══╝
;; ██║   ██║   ██║
;; ╚██╗ ██╔╝   ██║
;;  ╚████╔╝    ██║
;;   ╚═══╝     ╚═╝
;; NOTE: A chatgpt chat which recommends trying to use enable-linger for controlling a session switch + login
;; https://chatgpt.com/share/e311de56-256b-4214-a891-890068870d96
;; NOTE: Source code for openvt. Could see if i can steal some of this
;; https://github.com/legionus/kbd/blob/bdf5742c1083f543b964b57f92d7520eb26a5831/src/openvt.c#L82

(in-package :smuks)

(defun switch-vt (display seat num)
  (pause-outputs display)
  (libseat:switch-session seat num))
