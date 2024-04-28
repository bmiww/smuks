
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
   (texture :initform nil :accessor texture)
   (pending-damage :initform nil :accessor pending-damage)
   (damage :initform nil :accessor damage)))


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
    (setf (texture surface)
	  (gen-texture (pending-buffer surface)))
    (wl-buffer:send-release (pending-buffer surface))
    (setf (pending-buffer surface) nil)))


;; TODO: Make this be based off of the used format
(defvar *pixel-size* 4)
;; TODO: Possibly move this closer to the GL code
;; TODO: Maybe i can use the mmap ptr directly for pumping into the GL texture???
(defun gen-texture (pending-buffer)
  (log! "Generating texture from buffer")
  (describe pending-buffer)
  (describe (mmap-pool pending-buffer))
  (let ((texture (gl:gen-texture)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:pixel-store :unpack-row-length (/ (stride pending-buffer) 4))
    ;; TODO: Format is hardcoded - should be taken from the buffer values and mapped to a gl format
    ;; Shouldn't be :rgba twice - i guess
    (gl:tex-image-2d :texture-2d 0 :rgba
		     (width pending-buffer) (height pending-buffer)
		     0 :rgba :unsigned-byte
		     (cffi:inc-pointer (pool-ptr pending-buffer) (offset pending-buffer)))
    texture))

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

(defmethod wl-surface:damage ((surface surface) x y width height)
  (setf (pending-damage surface) (make-damage :x x :y y :width width :height height)))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defstruct damage
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(defun class-is? (object class)
  (typep object class))
