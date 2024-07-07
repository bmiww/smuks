
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
   projection-matrix
   matrix->array transpose-mat mat->arr

   translation-matrix
   scaling-matrix

   ;; damage
   make-damage damage-full

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
    (if (tex-initd texture)
	;; NOTE: Partial texture upload - only update the damaged areas
	;; If no damage provided - assume full damage
	(loop for dmg in (or damage (list (make-damage :x 0 :y 0 :width width :height height)))
	      for pointy = (damage-pointer ptr dmg width)
	      do (if (< (cffi:pointer-address pointy) (cffi:pointer-address ptr-max))
		     (gl:tex-sub-image-2d
		      :texture-2d 0
		      (damage-x dmg) (damage-y dmg)
		      (damage-width dmg) (damage-height dmg)
		      :rgba :unsigned-byte
		      pointy)
		     (wrn! "Texture damage rectangle out of bounds, skipping. Otherwise this would memory corrupt.")))
	;; NOTE: Full texture upload
	(progn
	  (gl:tex-image-2d :texture-2d 0 :rgba width height
			   0 :rgba :unsigned-byte ptr)
	  (setf (tex-initd texture) t)))

    texture))

(defun prep-gl-implementation (framebuffer width height)
  (gl:bind-framebuffer :framebuffer framebuffer)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.2 1.0)
  (gl:viewport 0 0 width height))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defstruct damage
  (full nil)
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(defstruct tex id (initd nil))
(defun mk-tex () (make-tex :id (gl:gen-texture)))

;; ┌┬┐┌─┐┌┬┐┬─┐┬┌─┐┌─┐┌─┐
;; │││├─┤ │ ├┬┘││  ├┤ └─┐
;; ┴ ┴┴ ┴ ┴ ┴└─┴└─┘└─┘└─┘
(defun cos! (angle) "Cosine of an angle in degrees" (cos (* angle (/ pi 180))))
(defun sin! (angle) "Sine of an angle in degrees" (sin (* angle (/ pi 180))))
;; Get the sign of a number as 1 or -1
(defun cps (val) (if (>= val 0) 1 -1))

;; TODO: 3d-math lib does not support mat3 ortho matrices.
;; For now too lazy to implement it on lib level, so going to do the calculations myself
(defun ortho-matrix (width height)
  (let* ((x (/ 2.0 width))
	 (y (/ 2.0 height))
	 (rc00 x) (rc10 0)
	 (rc20 (* -1.0 (cps (+ rc00 rc10))))
	 (rc01 0) (rc11 y)
	 (rc21 (* -1.0 (cps (+ rc01 rc11))))
	 (mat (math:mat
	       rc00 rc10 rc20
	       rc01 rc11 rc21
	       0    0    1)))
    mat))

;; TODO: 3d-math lib does not support mat3 rotation matrices.
;; For now too lazy to implement it on lib level, so going to do the calculations myself
;; NOTE: The mcref is inverted from what you were using before because of row/column major differences
(defun rotation-matrix (angle)
  (let* ((cos (coerce (cos! angle) 'single-float))
	 (sin (coerce (sin! angle) 'single-float))
	 (mat (math:meye 3)))
    (setf (math:mcref mat 0 0) cos)
    (setf (math:mcref mat 1 0) (- sin))
    (setf (math:mcref mat 0 1) sin)
    (setf (math:mcref mat 1 1) cos)
    mat))

(defun transpose-mat (mat) (math:mtranspose mat))
(defun mat->arr (mat) (math:marr3 mat))

(defun projection-matrix (width height &optional (rotation 0))
  (math:m* (rotation-matrix rotation) (ortho-matrix width height)))

(defun translation-matrix (x y) (math:mtranslation (math:vec (flo x) (flo y))))
(defun scaling-matrix (width height) (math:mscaling (math:vec (flo (/ 1 width)) (flo (/ 1 height)))))
