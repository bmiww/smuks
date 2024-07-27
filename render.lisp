
;; ██████╗ ███████╗███╗   ██╗██████╗ ███████╗██████╗
;; ██╔══██╗██╔════╝████╗  ██║██╔══██╗██╔════╝██╔══██╗
;; ██████╔╝█████╗  ██╔██╗ ██║██║  ██║█████╗  ██████╔╝
;; ██╔══██╗██╔══╝  ██║╚██╗██║██║  ██║██╔══╝  ██╔══██╗
;; ██║  ██║███████╗██║ ╚████║██████╔╝███████╗██║  ██║
;; ╚═╝  ╚═╝╚══════╝╚═╝  ╚═══╝╚═════╝ ╚══════╝╚═╝  ╚═╝
(in-package :smuks)

;; ┌┬┐┌─┐┬┌┐┌
;; │││├─┤││││
;; ┴ ┴┴ ┴┴┘└┘
(defun render-frame (display output)
  (let ((desktop (find-output-desktop display output))
	(framebuffer (next-framebuffer output)))

    (sdrm::just-page-flip (drm display) (framebuffer-id framebuffer) (connector output)
      (incr (frame-counter output))
      (gl:bind-framebuffer :framebuffer (framebuffer-gl-buffer framebuffer))
      (gl:viewport 0 0 (width output) (height output))
      (gl:clear :color-buffer-bit)

      (render-scene output)

      (render-desktop display output desktop)
      (render-rest display output desktop)

      (when (eq output (cursor-screen display))
	(unless (client-cursor-drawn output)
	  (shaders.texture:draw (texture-shader output *cursor*) *cursor*
				`(,(- (cursor-x display) (screen-x output))
				  ,(- (cursor-y display) (screen-y output))
				  36.0 36.0))))
      (setf (client-cursor-drawn output) nil)
      (gl:flush)
      (gl:finish)
      (gl:bind-framebuffer :framebuffer 0)

      ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
      ;; Any events or changes that i could instead attach to?
      ;; Maybe instead use per client flushes - for example when receiving commit from them
      ;; TODO: Also a bit wasteful - clients that are on different outputs might want/need different flushes
      ;; Based on whether the output frame was rendered
      (wl:flush-clients display))))


;; ┌─┐┌─┐┬─┐┌┬┐┬┌┬┐┌─┐
;; │  │ │├┬┘ ││││││└─┐
;; └─┘└─┘┴└──┴┘┴┴ ┴└─┘
(defun render-drag (display output surface)
  (do-surface-render perform (x y width height texture) output surface t
    (setf (client-cursor-drawn output) t)
    (perform :x (- (cursor-x display) x (screen-x output))
	     :y (- (cursor-y display) y (screen-y output)))))

;; TODO: Renderers should not screw around with setting coordinates/dimensions
(defun render-layer-surface (output surface)
  (do-surface-render perform (x y width height texture) output surface t
    (unless x (setf x (- (/ (output-width output) 2) (/ (width surface) 2))))
    (unless y (setf y (- (/ (output-height output) 2) (/ (height surface) 2))))
    (perform :x (- x (screen-x output))
	     :y (- y (screen-y output)))))


;; ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐  ┬─┐┌─┐┌┐┌┌┬┐┌─┐┬─┐
;; │ │├┴┐ │├┤ │   │   ├┬┘├┤ │││ ││├┤ ├┬┘
;; └─┘└─┘└┘└─┘└─┘ ┴   ┴└─└─┘┘└┘─┴┘└─┘┴└─
(defun render-cursor (display output surface)
  (unless (or (cursor-hidden (seat (wl:client surface))) (client-cursor-drawn output))
    (do-surface-render perform (x y width height texture) output surface t
      (setf (client-cursor-drawn output) t)
      (perform :x (- (cursor-x display) x (screen-x output))
	       :y (- (cursor-y display) y (screen-y output))))))

(defun render-subsurface (output surface active)
  (do-surface-render perform (x y width height texture) output surface active
    (perform :x (- (or x 0) (screen-x output))
	     :y (- (or y 0) (screen-y output)))))

(defun render-popup (output surface active)
  (do-surface-render perform (x y width height texture) output surface active
    (perform :x (+ x (x (grab-parent surface)))
	     :y (+ y (y (grab-parent surface))))))

(defun render-child-toplevel (output surface active)
  (do-surface-render perform (x y width height texture) output surface active
    (perform)))

(defun render-toplevel (output surface active)
  (when (initial-config-ackd surface)
    (do-surface-render perform (x y width height texture) output surface active
      (with-accessors ((compo-max-width compo-max-width) (compo-max-height compo-max-height)) surface
	(let* ((w-exceed (> width compo-max-width)) (h-exceed (> height compo-max-height)))
	  (perform :x (- x (screen-x output))
		   :y (- y (screen-y output))
		   :w (if w-exceed compo-max-width width)
		   :h (if h-exceed compo-max-height height)))

	(when (grab-child surface)
	  (typecase (grab-child surface)
	    (toplevel (render-child-toplevel output (grab-child surface) active))
	    (popup (render-popup output (grab-child surface) active))))))))


;; TODO: This and render-rest are still messy.
(defun render-type (display output surface)
  (etypecase surface
    (drag-surface (render-drag display output surface))
    (layer-surface (render-layer-surface output surface))))

;; TODO: Layers and cursors also should check if they need to render subsurfaces.
;; I'm certain cursors won't have them, but they are still implementing surface, so by protocol i guess they could
(defun render-rest (display output desktop)
  (declare (ignore desktop))
  (flet ((render (surface) (render-type display output surface)))
    (let ((clients (wl:all-clients display)))
      (flet ((render-type (type)
	       (loop for client in clients
		     do (let ((compositor (compositor client)))
			(when compositor
			  (mapcar #'render (funcall type compositor)))))))
	(render-type #'all-layers)))))


(defun render-desktop (display output desktop)
  (loop for window in (windows desktop)
	do (when (and (texture window) (typep window 'toplevel))
	     (let ((active (eq window (toplevel-of (keyboard-focus display)))))
	       (render-toplevel output window active)))))


;; ┌─┐┬ ┬┌─┐┬─┐┌─┐┌┬┐
;; └─┐├─┤├─┤├┬┘├┤  ││
;; └─┘┴ ┴┴ ┴┴└─└─┘─┴┘
(defmacro do-surface-render (perform (x y width height texture) output surface active &body body)
  `(when surface (with-accessors ((,x x) (,y y) (,width width) (,height height) (,texture texture) (subsurfaces subsurfaces)) ,surface
     (flet ((,perform (&key (x ,x) (y ,y) (w ,width) (h ,height) (active ,active))
	      (when ,texture
		(shaders.surface:draw-surface (surface-shader ,output ,texture)
					      ,texture
					      `(,(flo x) ,(flo y) ,(flo w) ,(flo h))
					      :active active)
		(flush-frame-callbacks ,surface)
		(setf (needs-redraw ,surface) nil)

		;; Each surface might have subsurfaces. Render those too
		(loop for subsurface in subsurfaces
		      do (render-subsurface ,output subsurface ,active))

		(when active
		  (loop for cursor in (all-cursors (compositor (wl:client ,surface)))
			do (render-cursor (wl:get-display ,output) ,output cursor))))))

       ;; Body is expected to call the perform function if it wants to render the surface
       ,@body))))

(defun toplevel-of (surface)
  (and surface
       (if (grab-parent surface)
	   (toplevel-of (grab-parent surface))
	   surface)))
