
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
   (needs-redraw :initform nil :accessor needs-redraw)
   (texture :initform nil :accessor texture)
   (width :initform -1 :accessor width)
   (height :initform -1 :accessor height)
   (pending-damage :initform nil :accessor pending-damage)
   (damage :initform nil :accessor damage)
   (pending-frame-callbacks :initform nil :accessor pending-frame-callbacks)
   (frame-callbacks :initform nil :accessor frame-callbacks)))

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

      (wl-buffer:send-release (pending-buffer surface))
      (setf (pending-buffer surface) nil)
      (setf (needs-redraw surface) t)))

  (when (pending-frame-callbacks surface)
    (setf (frame-callbacks surface) (pending-frame-callbacks surface))
    (setf (pending-frame-callbacks surface) nil)
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
  (unless (= x 0) (error "x must be 0"))
  (unless (= y 0) (error "y must be 0"))
  (setf (pending-buffer surface) buffer))

(defmethod wl-surface:frame ((surface surface) callback)
  (let ((cb-if (wl:mk-if 'callback surface callback)))
    (setf (pending-frame-callbacks surface) (cons cb-if (pending-frame-callbacks surface)))))

(defmethod flush-frame-callbacks ((surface surface))
  (dolist (callback (frame-callbacks surface)) (done callback))
  (setf (frame-callbacks surface) nil))

(defmethod wl-surface:damage ((surface surface) x y width height)
  (setf (pending-damage surface) (make-damage :x x :y y :width width :height height)))


;; ┌─┐┌─┐┬  ┬  ┌┐ ┌─┐┌─┐┬┌─
;; │  ├─┤│  │  ├┴┐├─┤│  ├┴┐
;; └─┘┴ ┴┴─┘┴─┘└─┘┴ ┴└─┘┴ ┴
(defclass callback (wl-callback:dispatch) ())
(defmethod done ((callback callback))
  (wl-callback:send-done callback (get-ms)))

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


;; ┌┐ ┬ ┬┌┬┐┌─┐  ┬─┐┌─┐┌─┐┌┬┐
;; ├┴┐└┬┘ │ ├┤   ├┬┘├┤ ├─┤ ││
;; └─┘ ┴  ┴ └─┘  ┴└─└─┘┴ ┴─┴┘
;; TODO: Keeping this around in case if i ever want to reuse it for doing screenshots?
;; In any case - remove if unnecessary
(defun read-all-bytes (pointer size)
  (let ((new-string (make-array 0
				:element-type '(signed-byte 8)
				:fill-pointer 0
				:adjustable t)))
    (loop for i below size
	  do (vector-push-extend (cffi:mem-ref pointer :char i) new-string))
    ;; (flexi-streams:octets-to-string new-string :external-format :ascii)
    new-string
    ))

(defvar *previous* nil)
(defun compare-buffer-contents (buffer)
  (let* ((pool (mmap-pool buffer))
	 (ptr (mmap-pool-ptr pool))
	 (size (mmap-pool-size pool))
	 (contents (read-all-bytes ptr size)))
    (when *previous*
      (print "Mismatch?")
      (print (mismatch contents *previous*)))
    (setf *previous* contents)))
