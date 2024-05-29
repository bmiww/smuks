
;;  ██████╗ ██╗      ██╗   ██╗████████╗██╗██╗
;; ██╔════╝ ██║      ██║   ██║╚══██╔══╝██║██║
;; ██║  ███╗██║█████╗██║   ██║   ██║   ██║██║
;; ██║   ██║██║╚════╝██║   ██║   ██║   ██║██║
;; ╚██████╔╝███████╗ ╚██████╔╝   ██║   ██║███████╗
;;  ╚═════╝ ╚══════╝  ╚═════╝    ╚═╝   ╚═╝╚══════╝
(defpackage :smuks-gl-util
  (:use :cl :sdrm :smuks-util)
  (:local-nicknames
   (:math #:org.shirakumo.fraf.math))
  (:nicknames :sglutil)
  (:export
   check-gl-error
   check-gl-fb-status
   prep-gl-implementation
   create-gl-framebuffer
   create-image-texture create-texture
   make-projection-matrix
   make-position-matrix
   matrix->array

   translation-matrix
   scaling-matrix

   ;; damage
   make-damage

   ;; texture
   mk-tex tex-id))
(in-package :smuks-gl-util)

;; ┌─┐┬    ┌─┐┬─┐┌─┐┌─┐
;; │ ┬│    ├─┘├┬┘├┤ ├─┘
;; └─┘┴─┘  ┴  ┴└─└─┘┴
(defun new-texture ())

(defun create-gl-framebuffer (image)
  (let* ((texture (gl:gen-texture))
	 (framebuffer (gl:gen-framebuffer)))

    (gl:bind-texture :texture-2d texture)
    (%gl:egl-image-target-texture-2d-oes :texture-2d image)
    (check-gl-error "egl-image-target-texture-2d-oes")

    (gl:bind-framebuffer :framebuffer framebuffer)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)
    (check-gl-fb-status "After attaching texture")

    framebuffer))

(defun create-image-texture (image &optional texture)
  (let* ((texture (or texture (mk-tex))))
    (gl:bind-texture :texture-2d (tex-id texture))
    (%gl:egl-image-target-texture-2d-oes :texture-2d image)
    (check-gl-error "egl-image-target-texture-2d-oes")

    texture))

;; TODO: Make this be based off of the used format
(defvar *pixel-size* 4)
(defun damage-pointer (ptr dmg width)
  (cffi:inc-pointer
   ptr
   (* (+ (* (damage-y dmg) width)
	 (damage-x dmg))
      *pixel-size*)))

;; TODO: Possibly move this closer to the GL code
(defun create-texture (ptr width height stride &key damage texture)
  (let ((texture (or texture (mk-tex)))
	;; TODO: This math is a bit wasteful. But without this - evil clients could crash the server
	;; TODO: Could instead do bounds checking during the damage event. Applying (min and max)
	;; Then wouldn't have to do this weird pointer math
	(ptr-max (cffi:inc-pointer ptr (* height stride *pixel-size*))))
    (gl:bind-texture :texture-2d (tex-id texture))
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:pixel-store :unpack-row-length (/ stride *pixel-size*))

    ;; TODO: Format is hardcoded - should be taken from the buffer values and mapped to a gl format
    ;; Shouldn't be :rgba twice - i guess
    (if (and damage (tex-initd texture))
	;; NOTE: Partial texture upload - only update the damaged areas
	(loop for dmg in damage
	      for pointy = (damage-pointer ptr dmg width)
	      do (if (< (cffi:pointer-address pointy) (cffi:pointer-address ptr-max))
		     (gl:tex-sub-image-2d
		      :texture-2d 0
		      (damage-x dmg) (damage-y dmg)
		      (damage-width dmg) (damage-height dmg)
		      :rgba :unsigned-byte
		      pointy)
		     (log! "⚠️:Texture damage rectangle out of bounds, skipping. Otherwise this would memory corrupt.")))
	;; NOTE: Full texture upload
	(progn
	  (gl:tex-image-2d :texture-2d 0 :rgba width height
			   0 :rgba :unsigned-byte
			   ptr)
	  (setf (tex-initd texture) t)))

    texture))

(defun prep-gl-implementation (framebuffer width height)
  (gl:bind-framebuffer :framebuffer framebuffer)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 1.0 1.0)
  (gl:viewport 0 0 width height))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defstruct damage
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(defstruct tex id (initd nil))
(defun mk-tex () (make-tex :id (gl:gen-texture)))

;; ┌┬┐┌─┐┌┬┐┬─┐┬┌─┐┌─┐┌─┐
;; │││├─┤ │ ├┬┘││  ├┤ └─┐
;; ┴ ┴┴ ┴ ┴ ┴└─┴└─┘└─┘└─┘
(define-modify-macro multf (&optional (number 1)) *)

;; TODO: I have no idea what i was doing with the clem matrixes
;; To transform them. This is inefficient. I should instead directly turn it into an array
;; Or find the inner representation
;; OR just use a decent matrix lib
(defun matrix->array (clem-matrix)
  (let ((list (clem::matrix->list clem-matrix)))
    (make-array (list (length list)) :initial-contents list)))

(defun cos! (angle)
  "Cosine of an angle in degrees"
  (cos (* angle (/ pi 180))))

(defun sin! (angle)
  "Sine of an angle in degrees"
  (sin (* angle (/ pi 180))))

(defun make-rot-matrix (angle)
  (let* ((cos (coerce (cos! angle) 'double-float))
	 (sin (coerce (sin! angle) 'double-float))
	 (matrix (clem:identity-matrix 3)))
    (setf (clem:mref matrix 0 0) cos)
    (setf (clem:mref matrix 0 1) (- sin))
    (setf (clem:mref matrix 1 0) sin)
    (setf (clem:mref matrix 1 1) cos)
    matrix))

(defun make-projection-matrix (width height &optional (rotation 0))
  (let* ((projection (clem:identity-matrix 3))
	 (x (/ 2.0 width))
	 (y (/ 2.0 height)))

    (setf (clem:mref projection 2 0)
	  (coerce (* -1.0 (copysign (+ (multf (clem:mref projection 0 0) x)
				       (multf (clem:mref projection 1 0) x)))) 'double-float))

    (setf (clem:mref projection 2 1)
	  (coerce (* -1.0 (copysign (+ (multf (clem:mref projection 0 1) y)
				       (multf (clem:mref projection 1 1) y)))) 'double-float))

    (setf projection (clem:m* projection (make-rot-matrix rotation)))
    (matrix->array projection)))

(defun translation-matrix (x y) (math:marr3 (math:mtranslation (math:vec (flo x) (flo y)))))
(defun scaling-matrix (width height) (math:marr3 (math:mscaling (math:vec (flo (/ 1 width)) (flo (/ 1 height))))))

(defun make-position-matrix (x y)
  (let* ((ident (clem:identity-matrix 3))
	 (position (clem:identity-matrix 3)))
    ;; TODO: Check if column major
    ;; (setf (clem:mref position 0 2) x)
    ;; (setf (clem:mref position 1 2) y)
    (setf (clem:mref position 2 0) x)
    (setf (clem:mref position 2 1) y)
    (matrix->array (clem:m* ident position))))

(defun copysign (val) (if (>= val 0) 1 -1))
