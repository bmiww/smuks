
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
(defun render-frame (screen)
  (livesupport:update-repl-link)
  (let ((desktop (find-screen-desktop *wayland* screen))
	(framebuffer (next-framebuffer screen)))

    (sdrm::just-page-flip *drm* (framebuffer-id framebuffer) (connector screen)
      (incr (frame-counter screen))
      (gl:bind-framebuffer :framebuffer (framebuffer-gl-buffer framebuffer))
      (gl:viewport 0 0 (width screen) (height screen))
      (gl:clear :color-buffer-bit)

      (render-scene screen)
      (render-desktop screen desktop)

      (when (eq screen (cursor-screen *wayland*))
	(unless (client-cursor-drawn screen)
	  (shaders.texture:draw (shader screen :texture) *cursor*
				`(,(- (cursor-x *wayland*) (screen-x screen))
				  ,(- (cursor-y *wayland*) (screen-y screen))
				  36.0 36.0))))
      (setf (client-cursor-drawn screen) nil)
      (gl:flush)
      (gl:finish)


      ;; TODO: Also not entirely sure if flushing clients per frame is the best thing to do
      ;; Any events or changes that i could instead attach to?
      ;; Maybe instead use per client flushes - for example when receiving commit from them
      ;; TODO: Also a bit wasteful - clients that are on different screens might want/need different flushes
      ;; Based on whether the screen frame was rendered
      (wl:flush-clients *wayland*))))


;; ┌─┐┌─┐┬─┐  ┌─┐┌┐  ┬┌─┐┌─┐┌┬┐
;; ├─┘├┤ ├┬┘  │ │├┴┐ │├┤ │   │
;; ┴  └─┘┴└─  └─┘└─┘└┘└─┘└─┘ ┴
;; TODO: This is almost identical to render-toplevel, with the difference being the coordinates
(defun render-cursor (screen surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (- (cursor-x *wayland*) (flo (x surface)) (screen-x screen)))
	(y (- (cursor-y *wayland*) (flo (y surface)) (screen-y screen))))

    (shaders.texture:draw (shader screen :texture) texture `(,x ,y ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)
    (setf (client-cursor-drawn screen) t)))

(defun render-toplevel (screen surface)
  (let ((texture (texture surface))
	(width (flo (compo-max-width surface)))
	(height (flo (compo-max-height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (shaders.texture:draw (shader screen :texture)
			  texture
			  `(,(- x (screen-x screen)) ,(- y (screen-y screen))
			    ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))

(defun render-layer-surface (screen surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (flo (x surface)))
	(y (flo (y surface))))
    (when (< x 0) (setf x (- (/ (screen-width screen) 2) (/ width 2))))
    (when (< y 0) (setf y (- (/ (screen-height screen) 2) (/ height 2))))
    (shaders.texture:draw (shader screen :texture)
			  texture
			  `(,(- x (screen-x screen)) ,(- y (screen-y screen))
			    ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))

(defun render-popup (screen surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (+ (flo (x surface)) (flo (x (grab-parent surface)))))
	(y (+ (flo (y surface)) (flo (y (grab-parent surface))))))

    (shaders.texture:draw (shader screen :texture) texture `(,x ,y ,width ,height))
    (flush-frame-callbacks surface)
    (setf (needs-redraw surface) nil)))


(defun render-surface (screen surface)
  (typecase surface
    (cursor (render-cursor screen surface))
    ;; NOTE: For now - the display logic for a drag surface should be more or less the same as a cursors
    (drag-surface (render-cursor screen surface))
    (layer-surface (render-layer-surface screen surface))
    (popup (render-popup screen surface))
    (t (render-toplevel screen surface))))

(defun render-desktop (screen desktop)
  (declare (ignore desktop))
  (flet ((render (surface) (render-surface screen surface)))
    (let ((clients (wl:all-clients *wayland*)))
      (flet ((render-type (type)
	       (loop for client in clients
		     do (let ((compositor (compositor client)))
			(when compositor
			  (mapcar #'render (funcall type compositor)))))))
	(render-type #'all-toplevels)
	(render-type #'all-subsurfaces)
	(render-type #'all-popups)
	(render-type #'all-layers)
	(render-type #'all-cursors)))))
