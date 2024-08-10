
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

      (render-desktop display output desktop)

      (when (eq output (cursor-screen display))
	(unless (client-cursor-drawn output)
	  (shaders.texture:draw (texture-shader output *cursor*) *cursor*
				`(,(- (cursor-x display) (screen-x output))
				  ,(- (cursor-y display) (screen-y output))
				  36.0 36.0))))
      (setf (client-cursor-drawn output) nil)
      (gl:flush)
      (gl:finish)

      ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
      ;; Any events or changes that i could instead attach to?
      ;; Maybe instead use per client flushes - for example when receiving commit from them
      ;; TODO: Also a bit wasteful - clients that are on different outputs might want/need different flushes
      ;; Based on whether the output frame was rendered
      (wl:flush-clients display))))


;; ┌─┐┬ ┬┌─┐┬─┐┌─┐┌┬┐
;; └─┐├─┤├─┤├┬┘├┤  ││
;; └─┘┴ ┴┴ ┴┴└─└─┘─┴┘
(defmacro do-surface-render (perform (x y width height texture) output surface active &body body)
  `(when surface
     (with-accessors ((,x x) (,y y) (,width width) (,height height) (,texture texture) (subsurfaces subsurfaces)) ,surface
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
			do (render-subsurface ,output subsurface ,active)))))

	 ;; Body is expected to call the perform function if it wants to render the surface
	 ,@body))))

(defun toplevel-of (surface)
  (and surface
       (if (grab-parent surface)
	   (toplevel-of (grab-parent surface))
	   surface)))


;; ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐  ┬─┐┌─┐┌┐┌┌┬┐┌─┐┬─┐
;; │ │├┴┐ │├┤ │   │   ├┬┘├┤ │││ ││├┤ ├┬┘
;; └─┘└─┘└┘└─┘└─┘ ┴   ┴└─└─┘┘└┘─┴┘└─┘┴└─
(defun render-child (output surface active)
  (when (grab-child surface)
    (typecase (grab-child surface)
      (toplevel (render-child-toplevel output (grab-child surface) active))
      (popup (render-popup output (grab-child surface) active)))))

(defun render-drag (display output surface)
  (do-surface-render perform (x y width height texture) output surface t
    (setf (client-cursor-drawn output) t)
    (perform :x (- (cursor-x display) x (screen-x output))
	     :y (- (cursor-y display) y (screen-y output)))))

;; TODO: Renderers should not screw around with setting coordinates/dimensions
(defun render-layer-surface (output surface)
  (do-surface-render perform (x y width height texture) output surface t
    (perform)
    (render-child output surface t)))


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
    (perform)
    (render-child output surface active)))

(defun render-child-toplevel (output surface active)
  (do-surface-render perform (x y width height texture) output surface active
    (perform)))

(defun render-toplevel (output surface active)
  (when (initial-config-ackd surface)
    (do-surface-render perform (x y width height texture) output surface active
      (perform)
      (render-child output surface active))))


;; TODO: Missing drag surface rendering
(defun render-desktop (display output desktop)
  (loop for layer in (bg-layers (compositor display))
	do (render-layer-surface output layer))

  ;; (render-scene output)

  (loop for window in (windows desktop)
	do (when (and (typep window 'toplevel) (not (closed window)))
	     (let* ((focus (keyboard-focus display))
		    (active (when (typep focus 'xdg-surface) (eq window (toplevel-of focus)))))
	       ;; (log! "Rendering client ~a" (wl::ptr (wl:client window)))
	       (render-toplevel output window active)
	       (when active
		 (loop for cursor in (all-cursors (compositor (wl:client window)))
		       do (render-cursor (wl:get-display output) output cursor))))))

  ;; TODO: Layers should also be handled per desktop level.
  ;; Right now this will just render all layers on all outputs
  (loop for layer in (other-layers (compositor display))
	do (render-layer-surface output layer)))
