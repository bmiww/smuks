
;; ███████╗██╗   ██╗██████╗ ███████╗ █████╗  ██████╗███████╗
;; ██╔════╝██║   ██║██╔══██╗██╔════╝██╔══██╗██╔════╝██╔════╝
;; ███████╗██║   ██║██████╔╝█████╗  ███████║██║     █████╗
;; ╚════██║██║   ██║██╔══██╗██╔══╝  ██╔══██║██║     ██╔══╝
;; ███████║╚██████╔╝██║  ██║██║     ██║  ██║╚██████╗███████╗
;; ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝ ╚═════╝╚══════╝
(in-package :smuks)

(defclass surface (wl-surface:dispatch)
  ((configure-serial :initform 0 :accessor configure-serial)
   (pending-buffer :initform nil :accessor pending-buffer)
   (needs-redraw :initform nil :accessor needs-redraw)
   (texture :initform nil :accessor texture)
   (texture-type :initform nil :accessor texture-type)
   (new-dimensions? :initform t :accessor new-dimensions?)
   (width :initform -1)
   (height :initform -1)
   (location :initform (make-coord) :accessor location)
   (pending-damage :initform nil :accessor pending-damage)
   (damage :initform nil :accessor damage)
   (pending-frame-callbacks :initform nil :accessor pending-frame-callbacks)
   (compositor :initform nil :initarg :compositor :accessor compositor)
   (frame-callbacks :initform nil :accessor frame-callbacks)
   (buffer-scale :initform 1 :accessor buffer-scale)))

(defmethod x ((surface surface)) (x (location surface)))
(defmethod y ((surface surface)) (y (location surface)))
(defmethod (setf x) (val (surface surface)) (setf (x (location surface)) val))
(defmethod (setf y) (val (surface surface)) (setf (y (location surface)) val))
(defmethod desktop ((surface surface)) (desktop (location surface)))
(defmethod (setf desktop) (val (surface surface)) (setf (desktop (location surface)) val))
(defmethod width ((surface surface)) (slot-value surface 'width))
(defmethod height ((surface surface)) (slot-value surface 'height))

(defmethod (setf width) (width (surface surface))
  (setf (slot-value surface 'width) width)
  (setf (slot-value surface 'new-dimensions?) t)
  (handle-surface-change (wl:get-display surface)))

(defmethod (setf height) (height (surface surface))
  (setf (slot-value surface 'height) height)
  (setf (slot-value surface 'new-dimensions?) t)
  (handle-surface-change (wl:get-display surface)))

(defmethod surface-x ((surface surface) x) x)
(defmethod surface-y ((surface surface) y) y)

(defmethod seat ((surface surface)) (seat (wl:client surface)))

;; ┌─┐┌─┐┌┬┐┌┬┐┬┌┬┐
;; │  │ ││││││││ │
;; └─┘└─┘┴ ┴┴ ┴┴ ┴
;; https://wayland.app/protocols/wayland#wl_surface:request:commit
;; TODO: Most of this could instead be written as methods for the specific parts
(defmethod wl-surface:commit ((surface surface))
  (typecase surface
    (toplevel (commit-surface surface))
    (cursor (commit-surface surface))
    (drag-surface (commit-surface surface))
    (subsurface (commit-surface surface)) ;; TODO: This should have its own method - since theres very specific handling of a subsurface
    ;; TODO: You could probably remove this since you are implementing it next to the popup class
    (t (format nil "Unsupported surface role: ~a" (type-of surface)))))

(defmethod commit-surface ((surface surface))
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

(defmethod commit-surface :after ((surface toplevel))
  (when (first-commit surface)
    (setf (first-commit surface) nil)
    (handle-surface-change (wl:get-display surface) surface)))

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
  "Notify the compositor of a an area that needs to be redrawn.
This request seems to be deprecated in favor of the damage-buffer request."
  (wl-surface:damage-buffer surface x y width height))

(defmethod wl-surface:damage-buffer ((surface surface) x y width height)
  "This damage method is the same as wl-surface:damage - with one difference.
The damage coordinates are in buffer coordinates, not surface coordinates."
  ;; NOTE: Full denotes that we received a max-int value which opengl doesn't like
  ;; This is often used in the protocol to denote that the whole buffer is damaged.
  (push (sglutil:make-damage :x x :y y :width width :height height
			     :full (or (> x 2147483646) (> y 2147483646)))
	(pending-damage surface)))

(defmethod wl-surface:set-opaque-region ((surface surface) region)
  "A hint to the region which should be considered more carefully for repaints.
Unimportant if there are no alpha < 1 pixels.
Can help to identify that theres no point in rendering something behind this region.
This hint could perhaps ignore the alpha channel.
SMUKS: I'm not going to consider it for now, since i'm building tiling, and overlaps shouldn't be a focus."
  )

;; TODO: This one is meaningful for me - for now i'm sending all events to the client
(defmethod wl-surface:set-input-region ((surface surface) region)
  "Sets the region which should be considered for input events.
A coordinate falling outside of this region
means the client doesn't have to receive that touch/pointer event."
  )

;; TODO: Implement to support clients setting higher/lower "dpi"
(defmethod wl-surface:set-buffer-scale ((surface surface) scale)
  "Sets the scale for the surface buffer.
This is one of the double buffered actions - so applied only after next commit"
  (setf (buffer-scale surface) scale))

(defmethod wl-surface:set-buffer-transform ((surface surface) transform)
  "Sets the transform for the surface buffer. This is an optimization thing if the client is made aware
of the screen rotation - it can rotate the buffer itself and save the compositor from doing it.
Or some such."
  (log! "UNIMPLEMENTED: Set buffer transform"))

;; TODO: Implement set offset
;; For now - i haven't seen it being used, once it is no longer 0, 0 - i should consider it
(defmethod wl-surface:offset ((surface surface) x y)
  "Sets the offset of the surface."
  (when (or (not (= x 0)) (not (= y 0)))
    (log! "UNIMPLEMENTED: Set offset")))

;; TODO: Destroy any connected textures and other resources
(defmethod cl-wl:destroy :before ((surface surface))
  (when (eq (type-of surface) 'surface)
    (rem-surface (compositor surface) surface)))


;; TODO: Might need to apply offset stuff here...
(defmethod in-bounds ((surface surface) x y desktop)
  (if (and (x surface) (y surface) (desktop surface))
      (and (<= (x surface) x (+ (x surface) (width surface)))
	   (<= (y surface) y (+ (y surface) (height surface)))
	   (eq desktop (desktop surface)))
      nil))


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
      (setf (texture surface) (sglutil:create-image-texture (image buffer) texture))
      (setf (texture-type surface) :dma))))


(defmethod commit-shm-buffer ((surface surface))
  (commit-buffer surface
    (with-slots (width height stride) (pending-buffer surface)
      (setf (texture surface)
	    (sglutil:create-texture
	     (pool-ptr (pending-buffer surface))
	     width height stride
	     :damage (damage surface)
	     :texture texture))
      (setf (damage surface) nil)
      (setf (texture-type surface) :shm))))

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
(defmethod wl-subsurface:set-sync ((sub subsurface))
  (setf (sync-mode sub) :sync))

;; TODO: Not implemented
;; Should act the same as a regular surface when it comes to commits
(defmethod wl-subsurface:set-desync ((sub subsurface))
  "Desync the subsurface from the parent surface"
  (setf (sync-mode sub) :desync))
