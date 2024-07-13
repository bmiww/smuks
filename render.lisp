
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
	  (shaders.texture:draw (shader output :texture) *cursor*
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


;; ┌─┐┌─┐┬─┐┌┬┐┬┌┬┐┌─┐
;; │  │ │├┬┘ ││││││└─┐
;; └─┘└─┘┴└──┴┘┴┴ ┴└─┘
(defun render-cursor (display output surface)
  (unless (cursor-hidden (seat (wl:client surface)))
    (setf (client-cursor-drawn output) t)
    (values (- (cursor-x display) (x surface) (screen-x output))
	    (- (cursor-y display) (y surface) (screen-y output))
	    (width surface)
	    (height surface))))


(defun render-drag (display output surface)
  (setf (client-cursor-drawn output) t)
  (values (- (cursor-x display) (x surface) (screen-x output))
	  (- (cursor-y display) (y surface) (screen-y output))
	  (width surface)
	  (height surface)))

(defun render-subsurface (output surface)
  (values (- (x surface) (screen-x output))
	  (- (y surface) (screen-y output))
	  (width surface)
	  (height surface)))

(defun render-toplevel (output surface)
  (with-slots (x y width height compo-max-width compo-max-height texture) surface
    ;; TODO: Probably can just do max or min func
    (let* ((w-exceed (> width compo-max-width))
	   (h-exceed (> height compo-max-height)))

      (values (- x (screen-x output))
	      (- y (screen-y output))
	      (if w-exceed compo-max-width width)
	      (if h-exceed compo-max-height height)))))


(defun render-layer-surface (output surface)
  (let ((x (x surface))
	(y (y surface)))
    (unless x (setf x (- (/ (output-width output) 2) (/ (width surface) 2))))
    (unless y (setf y (- (/ (output-height output) 2) (/ (height surface) 2))))

    (values (- x (screen-x output))
	    (- y (screen-y output))
	    (width surface)
	    (height surface))))

(defun render-popup (output surface)
  (declare (ignore output))
  (values (+ (x surface) (x (grab-parent surface)))
	  (+ (y surface) (y (grab-parent surface)))
	  (width surface)
	  (height surface)))

(defun render-surface (output surface)
  (values (- (x surface) (screen-x output))
	  (- (y surface) (screen-y output))
	  (width surface)
	  (height surface)))


;; ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐  ┬─┐┌─┐┌┐┌┌┬┐┌─┐┬─┐
;; │ │├┴┐ │├┤ │   │   ├┬┘├┤ │││ ││├┤ ├┬┘
;; └─┘└─┘└┘└─┘└─┘ ┴   ┴└─└─┘┘└┘─┴┘└─┘┴└─
(defun render-type (display output surface)
  (let ((texture (texture surface))
	(coordim
	  (typecase surface
	    (toplevel (render-toplevel output surface))
	    (cursor (render-cursor display output surface))
	    (drag-surface (render-drag display output surface))
	    (layer-surface (render-layer-surface output surface))
	    (popup (render-popup output surface))
	    (subsurface (render-subsurface output surface))
	    (t (render-surface output surface)))))
    (when coordim
      (multiple-value-bind (x y width height) coordim
	(shaders.texture:draw (texture-shader output texture) texture
			      `(,(flo x) ,(flo y) ,(flo width) ,(flo height)))

	(flush-frame-callbacks surface)
	(setf (needs-redraw surface) nil)))))

(defun render-rest (display output desktop)
  (declare (ignore desktop))
  (flet ((render (surface) (render-type display output surface)))
    (let ((clients (wl:all-clients display)))
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


(defun render-desktop (display output desktop)
  (loop for window in (windows desktop)
	do (when (texture window) (render-type display output window))))
