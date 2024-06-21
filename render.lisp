
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
  (let ((cursor-drawn nil)
	(desktop (find-screen-desktop *wayland* screen))
	(framebuffer (next-framebuffer screen)))

    (sdrm::just-page-flip *drm* (framebuffer-id framebuffer) (connector screen)
      (incr (frame-counter screen))
      (gl:bind-framebuffer :framebuffer (framebuffer-gl-buffer framebuffer))
      (gl:viewport 0 0 (width screen) (height screen))
      (gl:clear :color-buffer-bit)

      (render-scene screen)

      ;; (setf cursor-drawn (some (lambda (val) val) (render-clients screen)))
      (setf cursor-drawn (render-desktop screen desktop))
      (render-layer-surfaces screen desktop)

      (when (eq screen (cursor-screen *wayland*))
	;; (unless cursor-drawn
	(shaders.texture:draw (shader screen :texture) *cursor*
			      `(,(- (cursor-x *wayland*) (screen-x screen))
				,(- (cursor-y *wayland*) (screen-y screen))
				36.0 36.0)))
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
;; TODO: The boolean return value is stupid. Tells that a cursor has been rendered
;; So that the main loop can know if it should render the display cursor or not
;; TODO: This is almost identical to render-toplevel, with the difference being the coordinates
(defun render-cursor (screen surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (- (cursor-x *wayland*) (flo (x surface)) (screen-x screen)))
	(y (- (cursor-y *wayland*) (flo (y surface)) (screen-y screen))))

    ;; TODO: Fix this active-surface usage. You moved active-surface to a client seat
    ;; And this use case in general seems wrong (could be improved)
    ;; (if (active-surface (role surface))
    (progn
      (shaders.texture:draw (shader screen :texture) texture `(,x ,y ,width ,height))
      (flush-frame-callbacks surface)
      (setf (needs-redraw surface) nil)
      t)
    ;; nil)
  ))

;; TODO: The boolean return value is stupid. Tells that a cursor has been rendered
;; So that the main loop can know if it should render the display cursor or not
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
    (setf (needs-redraw surface) nil)
    nil))

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
    (setf (needs-redraw surface) nil)
    nil))

(defun render-popup (screen surface)
  (let ((texture (texture surface))
	(width (flo (width surface)))
	(height (flo (height surface)))
	(x (+ (flo (x surface)) (flo (x (grab-parent surface)))))
	(y (+ (flo (y surface)) (flo (y (grab-parent surface))))))

    (progn
      (shaders.texture:draw (shader screen :texture) texture `(,x ,y ,width ,height))
      (flush-frame-callbacks surface)
      (setf (needs-redraw surface) nil)
      t)
    ;; nil)
  ))


(defun render-surface (screen surface)
  (typecase surface
    (cursor (render-cursor screen surface))
    ;; NOTE: For now - the display logic for a drag surface should be more or less the same as a cursors
    (drag-surface (render-cursor screen surface))
    (layer-surface (render-layer-surface screen surface))
    (popup (render-popup screen surface))
    (t (render-toplevel screen surface))))

(defun render-clients (screen)
  (let* ((clients (wl:all-clients *wayland*))
	 (compositors (remove-if-not 'identity (mapcar 'compositor clients)))
	 (surfaces (util:flatten (mapcar 'all-ready-surfaces compositors))))
    (mapcar (lambda (surface) (render-surface screen surface)) surfaces)))

(defun render-desktop (screen desktop)
  (let* ((surfaces (windows desktop)))
    (loop for surface in surfaces
	  for compositor = (compositor surface)
	  for compost-surfaces = (all-ready-surfaces compositor)
	  do (mapcar (lambda (surface) (render-surface screen surface)) compost-surfaces))))

;; TODO: This is a quick hack to check how well the layer surfaces work
;; You need a better mechanism to keep track of ALL surfaces to render for a given screen
(defun render-layer-surfaces (screen desktop)
  (declare (ignore desktop))
  (mapcar (lambda (client) (when (compositor client)
			(loop for surface in (all-ready-surfaces (compositor client))
			      when (typep surface 'layer-surface)
				do (render-surface screen surface)
			      when (typep surface 'subsurface)
				do (render-surface screen surface))
			;; TODO: Hacked popup renders in here - they should go somewhere else
			(loop for surface in (all-popups (compositor client))
			      do (render-surface screen surface))))
	  (wl:all-clients *wayland*)))
