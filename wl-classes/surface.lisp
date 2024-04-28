
;; ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
(in-package :smuks)

(defclass surface (wl-surface:dispatch)
  ((role :accessor role)
   (configure-serial :initform 0 :accessor configure-serial)
   (pending-buffer :initform nil :accessor pending-buffer)
   (buffer :initform nil :accessor buffer)))


;; ┌─┐┌─┐┌┬┐┌┬┐┬┌┬┐
;; │  │ ││││││││ │
;; └─┘└─┘┴ ┴┴ ┴┴ ┴
;; https://wayland.app/protocols/wayland#wl_surface:request:commit
(defmethod wl-surface:commit ((surface surface))
  (cond
    ((typep (role surface) 'xdg-surface) (commit-toplevel surface))
    (t (error (format nil "Unsupported surface role: ~a" (role surface))))))

(defmethod commit-toplevel ((surface surface))
  (when (pending-buffer surface)
    (setf (buffer surface) (pending-buffer surface))
    (setf (pending-buffer surface) nil)))


;; ┌─┐┌┬┐┌┬┐┌─┐┌─┐┬ ┬
;; ├─┤ │  │ ├─┤│  ├─┤
;; ┴ ┴ ┴  ┴ ┴ ┴└─┘┴ ┴
;; https://wayland.app/protocols/wayland#wl_surface:request:attach
(defmethod wl-surface:attach ((surface surface) buffer x y)
  ;; TODO: Protocol deprecation thing - you should instead notify the client
  ;; of errors instead of breaking the compositor.
  (unless (= x 0) (error "x must be 0"))
  (unless (= y 0) (error "y must be 0"))
  (setf (pending-buffer surface) buffer))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun class-is? (object class)
  (typep object class))
