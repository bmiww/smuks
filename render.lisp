
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
(defun render-frame (output)
  (livesupport:update-repl-link)
  (let ((desktop (find-output-desktop *wayland* output))
	(framebuffer (next-framebuffer output)))

    (sdrm::just-page-flip *drm* (framebuffer-id framebuffer) (connector output)
      (incr (frame-counter output))
      (gl:bind-framebuffer :framebuffer (framebuffer-gl-buffer framebuffer))
      (gl:viewport 0 0 (width output) (height output))
      (gl:clear :color-buffer-bit)

      (render-scene output)
      (render-desktop output desktop)
      (render-rest output desktop)

      (when (eq output (cursor-screen *wayland*))
	(unless (client-cursor-drawn output)
	  (shaders.texture:draw (shader output :texture) *cursor*
				`(,(- (cursor-x *wayland*) (screen-x output))
				  ,(- (cursor-y *wayland*) (screen-y output))
				  36.0 36.0))))
      (setf (client-cursor-drawn output) nil)
      (gl:flush)
      (gl:finish)


      ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
      ;; Any events or changes that i could instead attach to?
      ;; Maybe instead use per client flushes - for example when receiving commit from them
      ;; TODO: Also a bit wasteful - clients that are on different outputs might want/need different flushes
      ;; Based on whether the output frame was rendered
      (wl:flush-clients *wayland*))))


;; ┌─┐┌─┐┬─┐  ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐
;; ├─┘├┤ ├┬┘  │ │├┴┐ │├┤ │   │
;; ┴  └─┘┴└─  └─┘└─┘└┘└─┘└─┘ ┴
;; TODO: This is almost identical to render-toplevel, with the difference being the coordinates
(defun render-cursor (output surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (- (cursor-x *wayland*) (flo (x surface)) (screen-x output)))
	(y (- (cursor-y *wayland*) (flo (y surface)) (screen-y output))))

    (shaders.texture:draw (shader output :texture) texture `(,x ,y ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)
    (setf (client-cursor-drawn output) t)))

(defun render-subsurface (output surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (shaders.texture:draw (shader output :texture)
			  texture
			  `(,(- x (screen-x output)) ,(- y (screen-y output))
			    ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))

(defun render-toplevel (output surface)
  (let ((texture (texture surface))
	(width (flo (compo-max-width surface)))
	(height (flo (compo-max-height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (shaders.texture:draw (shader output :texture)
			  texture
			  `(,(- x (screen-x output)) ,(- y (screen-y output))
			    ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))

(defun render-layer-surface (output surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (when (< x 0) (setf x (- (/ (output-width output) 2) (/ width 2))))
    (when (< y 0) (setf y (- (/ (output-height output) 2) (/ height 2))))
    (shaders.texture:draw (shader output :texture)
			  texture
			  `(,(- x (screen-x output)) ,(- y (screen-y output))
			    ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))

(defun render-popup (output surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (+ (flo (x surface)) (flo (x (grab-parent surface)))))
	(y (+ (flo (y surface)) (flo (y (grab-parent surface))))))

    (shaders.texture:draw (shader output :texture) texture `(,x ,y ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))


(defun render-surface (output surface)
  (typecase surface
    (cursor (render-cursor output surface))
    ;; NOTE: For now - the display logic for a drag surface should be more or less the same as a cursors
    (drag-surface (render-cursor output surface))
    (layer-surface (render-layer-surface output surface))
    (popup (render-popup output surface))
    (subsurface (render-subsurface output surface))
    (t (render-toplevel output surface))))

(defun render-rest (output desktop)
  (declare (ignore desktop))
  (flet ((render (surface) (render-surface output surface)))
    (let ((clients (wl:all-clients *wayland*)))
      (flet ((render-type (type)
	       (loop for client in clients
		     do (let ((compositor (compositor client)))
			(when compositor
			  (mapcar #'render (funcall type compositor)))))))
	;; (render-type #'all-toplevels)
	(render-type #'all-subsurfaces)
	(render-type #'all-popups)
	(render-type #'all-layers)
	(render-type #'all-cursors)))))


(defun render-desktop (output desktop)
  (loop for window in (windows desktop)
	do (when (texture window) (render-surface output window))))
