
;; ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
;; NOTE: https://wayland.app/protocols/wayland#wl_surface
(in-package :smuks)

(defclass surface (wl-surface:dispatch)
  ((pending-buffer :initform nil :accessor pending-buffer)
   (needs-redraw :initform nil :accessor needs-redraw)
   (texture :initform nil :accessor texture)
   (new-dimensions? :initform t :accessor new-dimensions?)
   (width :initform -1)
   (height :initform -1)
   (location :initform (make-coord) :accessor location)
   (pending-damage :initform nil :accessor pending-damage)
   (damage :initform nil :accessor damage)
   (pending-frame-callbacks :initform nil :accessor pending-frame-callbacks)
   (compositor :initform nil :initarg :compositor :accessor compositor)
   (frame-callbacks :initform nil :accessor frame-callbacks)
   (subsurfaces :initform nil :accessor subsurfaces)
   (buffer-scale :initform 1 :accessor buffer-scale)))

(defmethod x ((surface surface)) (x (location surface)))
(defmethod y ((surface surface)) (y (location surface)))
(defmethod desktop ((surface surface)) (desktop (location surface)))
(defmethod width ((surface surface)) (slot-value surface 'width))
(defmethod height ((surface surface)) (slot-value surface 'height))

(defcontinue (setf desktop) (val (surface surface)) (setf (desktop (location surface)) val))
(defcontinue (setf x) (val (surface surface)) (setf (x (location surface)) val))
(defcontinue (setf y) (val (surface surface)) (setf (y (location surface)) val))

(defcontinue (setf width) (width (surface surface))
  (setf (slot-value surface 'width) width)
  (setf (slot-value surface 'new-dimensions?) t)
  (handle-surface-change (wl:get-display surface)))

(defcontinue (setf height) (height (surface surface))
  (setf (slot-value surface 'height) height)
  (setf (slot-value surface 'new-dimensions?) t)
  (handle-surface-change (wl:get-display surface)))

(defmethod surface-x ((surface surface) x) x)
(defmethod surface-y ((surface surface) y) y)
(defmethod seat ((surface surface)) (seat (wl:client surface)))

(defmethod add-subsurface ((surface surface) sub)
  (push sub (subsurfaces surface))
  (after cl-wl:destroy sub (lambda (sub) (rem-subsurface surface sub))))

(defmethod rem-subsurface ((surface surface) sub)
  (setf (subsurfaces surface) (remove sub (subsurfaces surface))))

;; ┌─┐┌─┐┌┬┐┌┬┐┬┌┬┐
;; │  │ ││││││││ │
;; └─┘└─┘┴ ┴┴ ┴┴ ┴
;; https://wayland.app/protocols/wayland#wl_surface:request:commit
(defcontinue wl-surface:commit ((surface surface))
  (when (pending-damage surface)
    (setf (damage surface) (pending-damage surface))
    (setf (pending-damage surface) nil))

  (typecase (pending-buffer surface)
    (buffer (commit-shm-buffer surface))
    (dma-buffer (commit-dma-buffer surface)))

  (when (pending-frame-callbacks surface)
    (setf (frame-callbacks surface) (pending-frame-callbacks surface))
    (setf (pending-frame-callbacks surface) nil)
    (setf (needs-redraw surface) t)))

;; TODO: Replace errors - with client error notification
(defmethod wl-surface:attach ((surface surface) buffer x y)
  (when (and buffer (>= (wl:version-want buffer) 5))
    (unless (= x 0) (error "x must be 0"))
    (unless (= y 0) (error "y must be 0")))
  (setf (pending-buffer surface) buffer))

(defmethod wl-surface:frame ((surface surface) callback)
  (let ((cb-if (wl:mk-if 'callback surface callback)))
    (setf (pending-frame-callbacks surface) (cons cb-if (pending-frame-callbacks surface)))))

(defmethod flush-frame-callbacks ((surface surface))
  (dolist (callback (frame-callbacks surface)) (done callback))
  (setf (frame-callbacks surface) nil))

(defmethod wl-surface:damage ((surface surface) x y width height)
  "DEPRECATED: in favor of damage-buffer."
  (wl-surface:damage-buffer surface x y width height))

(defmethod wl-surface:damage-buffer ((surface surface) x y width height)
  "The damage coordinates are in buffer coordinates, not surface coordinates."
  ;; NOTE: Full denotes that we received a max-int value which opengl doesn't like
  ;; This is often used in the protocol to denote that the whole buffer is damaged.
  (push (sglutil:make-damage :x x :y y :width width :height height
			     :full (or (> x 2147483646) (> y 2147483646)))
	(pending-damage surface)))

(defmethod wl-surface:set-opaque-region ((surface surface) region))

;; TODO: This one is meaningful for me - for now i'm sending all events to the client
(defmethod wl-surface:set-input-region ((surface surface) region))

;; TODO: Implement to support clients setting higher/lower "dpi"
(defmethod wl-surface:set-buffer-scale ((surface surface) scale)
  (setf (buffer-scale surface) scale))

(defmethod wl-surface:set-buffer-transform ((surface surface) transform)
  (log! "UNIMPLEMENTED: Set buffer transform"))

;; TODO: Implement set offset
;; For now - i haven't seen it being used, once it is no longer 0, 0 - i should consider it
(defmethod wl-surface:offset ((surface surface) x y)
  "Sets the offset of the surface."
  (when (or (not (= x 0)) (not (= y 0)))
    (log! "UNIMPLEMENTED: Set offset")))


;; ┌┐ ┌─┐┬ ┬┌┐┌┌┬┐   ┌─┐┬ ┬┌─┐┌─┐┬┌─┌─┐
;; ├┴┐│ ││ ││││ ││───│  ├─┤├┤ │  ├┴┐└─┐
;; └─┘└─┘└─┘┘└┘─┴┘   └─┘┴ ┴└─┘└─┘┴ ┴└─┘
(defmethod in-desktop-bounds ((surface surface) x y desktop)
  (and (x surface) (y surface) (desktop surface)
       (<= (x surface) x (+ (x surface) (width surface)))
       (<= (y surface) y (+ (y surface) (height surface)))
       (eq desktop (desktop surface))))

;; TODO: Might need to apply offset stuff here...
(defmethod in-bounds ((surface surface) x y)
  (and (x surface) (y surface)
       (<= (x surface) x (+ (x surface) (width surface)))
       (<= (y surface) y (+ (y surface) (height surface)))))


;; TODO: Destroy any connected textures and other resources
(defmethod cl-wl:destroy :before ((surface surface))
  (when (eq (type-of surface) 'surface)
    (rem-surface (compositor surface) surface)))

;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴
(defclass callback (wl-callback:dispatch) ())
(defmethod done ((callback callback))
  (wl-callback:send-done callback (get-ms))
  (wl:destroy callback))


;; ┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;; ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; └─┘└─┘└  └  └─┘┴└─
(defmacro commit-buffer (surface &body body)
  `(with-slots (new-dimensions?) ,surface
     ;; TODO: These are still needed since i don't know where to find the
     ;; width height for pointer surface for example
     (unless (eq (width (pending-buffer ,surface)) (width ,surface))
       (setf (width ,surface) (width (pending-buffer ,surface)))
       (setf new-dimensions? t))

     (unless (eq (height (pending-buffer ,surface)) (height ,surface))
       (setf (height ,surface) (height (pending-buffer ,surface)))
       (setf new-dimensions? t))

     (when (and new-dimensions? (texture ,surface))
       (gl:delete-texture (sglutil:tex-id (texture ,surface)))
       (setf (texture ,surface) nil))

     ;; TODO: the texture here is implied and a bit annoying
     ;; Could add it as a reference in the macro args
     (let ((texture (texture ,surface)))
       ,@body)

     (setf new-dimensions? nil)
     (wl-buffer:send-release (pending-buffer surface))
     (setf (pending-buffer ,surface) nil)
     (setf (needs-redraw ,surface) t)))


;; TODO: For now only supporting a single plane
(defmethod commit-dma-buffer ((surface surface))
  (commit-buffer surface
    (let* ((buffer (pending-buffer surface)))
      (setf (texture surface) (sglutil:create-image-texture (image buffer) texture)))))


(defmethod commit-shm-buffer ((surface surface))
  (commit-buffer surface
    (with-slots (width height stride pixel-format) (pending-buffer surface)
      (setf (texture surface)
	    (sglutil:create-texture
	     (pool-ptr (pending-buffer surface))
	     width height stride
	     :damage (damage surface)
	     :texture texture
	     :format pixel-format))
      (setf (damage surface) nil))))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defclass cursor (surface) ())


;; ███████╗██╗   ██╗██████╗ ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██████╔╝███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
;; NOTE: https://wayland.app/protocols/wayland#wl_subsurface
(defclass subsurface (wl-subsurface:dispatch surface)
  ((surface :initarg :surface)
   (parent :initarg :parent)
   (sync-mode :initarg :sync-mode :accessor sync-mode)))

(defmethod wl-subsurface:set-position ((sub subsurface) x y)
  "Position the subsurface relative to the parent surface top-left corner
Could be a negative value.
This is double buffered - applied when the parent surface commits
Buuuut subsurfaces also have the option of setting sync/desync mode"
  (log! "UNIMPLEMENTED: Set position"))

;; TODO: For now not implemented
;; In effect should make it so that any changes are applied only when the parent surface
;; has a commit. Not too hard to implement, but lazy for now
(defmethod wl-subsurface:set-sync ((sub subsurface)) (setf (sync-mode sub) :sync))

;; TODO: Not implemented
;; Should act the same as a regular surface when it comes to commits
(defmethod wl-subsurface:set-desync ((sub subsurface))
  "Desync the subsurface from the parent surface"
  (setf (sync-mode sub) :desync))
