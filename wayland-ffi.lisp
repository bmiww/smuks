
(defpackage :smuks-wl-ffi
  (:use :cl :cffi))
(in-package :smuks-wl-ffi)

(define-foreign-library libwayland (t (:default "libwayland-server")))
(use-foreign-library libwayland)

(defcstruct wl_display
  (dummy :int))
