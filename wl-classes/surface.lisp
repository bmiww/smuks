
;; ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
(in-package :smuks)

(defclass surface (wl-surface:dispatch)
  ((role :initform nil :accessor role)
   (configure-serial :initform 0 :accessor configure-serial)
   (pending-buffer :initform nil :accessor pending-buffer)
   (needs-redraw :initform nil :accessor needs-redraw)
   (texture :initform nil :accessor texture)
   (texture-type :initform nil :accessor texture-type)
   (width :initform -1 :accessor width)
   (height :initform -1 :accessor height)
   (x :initform -1 :accessor x)
   (y :initform -1 :accessor y)
   (pending-damage :initform nil :accessor pending-damage)
   (damage :initform nil :accessor damage)
   (pending-frame-callbacks :initform nil :accessor pending-frame-callbacks)
   (frame-callbacks :initform nil :accessor frame-callbacks)))

;; ┌─┐┌─┐┌┬┐┌┬┐┬┌┬┐
;; │  │ ││││││││ │
;; └─┘└─┘┴ ┴┴ ┴┴ ┴
;; https://wayland.app/protocols/wayland#wl_surface:request:commit
(defmethod wl-surface:commit ((surface surface))
  (typecase (role surface)
    (toplevel (commit-toplevel surface))
    (xdg-surface (commit-toplevel surface))
    (pointer (commit-toplevel surface))
    (t (format nil "Unsupported surface role: ~a" (role surface)))))

(defmethod commit-toplevel ((surface surface))
  (typecase (pending-buffer surface)
    (buffer (commit-shm-buffer surface))
    (dma-buffer (commit-dma-buffer surface)))

  (when (pending-frame-callbacks surface)
    (setf (frame-callbacks surface) (pending-frame-callbacks surface))
    (setf (pending-frame-callbacks surface) nil)
    (setf (needs-redraw surface) t)))


;; TODO: For now only supporting a single plane
;; TODO: The width height of surface is getting a bit annoying
(defmethod commit-dma-buffer ((surface surface))
  (let* ((buffer (pending-buffer surface))
	 (plane (gethash 0 (planes buffer)))
	 (image (seglutil:create-egl-image-from-buffer
		 (egl (wl:get-display surface))
		 (width buffer) (height buffer)
		 (pixel-format buffer)
		 (fd plane) (offset plane) (stride plane))))

    (setf (width surface) (width (pending-buffer surface)))
    (setf (height surface) (height (pending-buffer surface)))

    (setf (texture surface) (sglutil:create-image-texture image))
    (setf (texture-type surface) :dma)

    (setf (pending-buffer surface) nil)
    (setf (needs-redraw surface) t)))

(defmethod commit-shm-buffer ((surface surface))
  (let ((new-dimensions? nil))
    (unless (eq (width (pending-buffer surface)) (width surface))
      (setf (width surface) (width (pending-buffer surface)))
      (setf new-dimensions? t))

    (unless (eq (height (pending-buffer surface)) (height surface))
      (setf (height surface) (height (pending-buffer surface)))
      (setf new-dimensions? t))

    ;; TODO: If a new texture is being generated - delete the old texture!!!
    ;; aka new-dimensions is true - delete the old texture before reassigning
    (setf (texture surface)
	  (gen-texture (pending-buffer surface) (unless new-dimensions? (texture surface))))
    (setf (texture-type surface) :shm)

    (wl-buffer:send-release (pending-buffer surface))
    (setf (pending-buffer surface) nil)
    (setf (needs-redraw surface) t)))


;; TODO: Make this be based off of the used format
(defvar *pixel-size* 4)
;; TODO: Possibly move this closer to the GL code
;; TODO: Maybe i can use the mmap ptr directly for pumping into the GL texture???
(defun gen-texture (pending-buffer &optional texture)
  (let ((texture (if texture texture (gl:gen-texture))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:pixel-store :unpack-row-length (/ (stride pending-buffer) *pixel-size*))
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
  ;; (unless (= x 0) (error "x must be 0"))
  ;; (unless (= y 0) (error "y must be 0"))
  (setf (pending-buffer surface) buffer))

(defmethod wl-surface:frame ((surface surface) callback)
  (let ((cb-if (wl:mk-if 'callback surface callback)))
    (setf (pending-frame-callbacks surface) (cons cb-if (pending-frame-callbacks surface)))))

(defmethod flush-frame-callbacks ((surface surface))
  (dolist (callback (frame-callbacks surface)) (done callback))
  (setf (frame-callbacks surface) nil))

(defmethod wl-surface:damage ((surface surface) x y width height)
  (setf (pending-damage surface) (make-damage :x x :y y :width width :height height)))

(defmethod wl-surface:set-opaque-region ((surface surface) region)
  "Sets the region which should be considered more carefully for repaints.
Basically client notifying the compositor that there are alpha < 1 pixels in this region"
  (log! "UNIMPLEMENTED: Set opaque region"))

;; TODO: This one is meaningful for me - for now i'm sending all events to the client
(defmethod wl-surface:set-input-region ((surface surface) region)
  "Sets the region which should be considered for input events.
A coordinate falling outside of this region
means the client doesn't have to receive that touch/pointer event."
  (log! "UNIMPLEMENTED: Set input region"))

;; TODO: Implement to support clients setting higher/lower "dpi"
(defmethod wl-surface:set-buffer-scale ((surface surface) scale)
  "Sets the scale for the surface buffer.
This is one of the double buffered actions - so applied only after next commit"
  (log! "UNIMPLEMENTED: Set buffer scale"))

(defmethod in-bounds ((surface surface) x y)
  (and (<= (x surface) x (+ (x surface) (width surface)))
       (<= (y surface) y (+ (y surface) (height surface)))))

;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴
(defclass callback (wl-callback:dispatch) ())
(defmethod done ((callback callback))
  (wl-callback:send-done callback (get-ms))
  (wl:destroy callback))

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


;; ███████╗██╗   ██╗██████╗ ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██████╔╝███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
(defclass subsurface (wl-subsurface:dispatch)
  ((surface :initarg :surface)
   (parent :initarg :parent)
   (sync-mode :initarg :sync-mode :accessor sync-mode)))

;; TODO:
(defmethod wl-subsurface:set-position ((sub subsurface) x y)
  "Position the subsurface relative to the parent surface top-left corner
Could be a negative value.
This is double buffered - applied when the parent surface commits
Buuuut subsurfaces also have the option of setting sync/desync mode"
  (log! "UNIMPLEMENTED: Set position"))

(defmethod wl-subsurface:set-desync ((sub subsurface))
  "Desync the subsurface from the parent surface"
  (log! "wl-subsurface:set-desync: Sync strategies not implemented...")
  (setf (sync-mode sub) :desync))
