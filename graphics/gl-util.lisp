
;;  ██████╗ ██╗      ██╗   ██╗████████╗██╗██╗
;; ██╔════╝ ██║      ██║   ██║╚══██╔══╝██║██║
;; ██║  ███╗██║█████╗██║   ██║   ██║   ██║██║
;; ██║   ██║██║╚════╝██║   ██║   ██║   ██║██║
;; ╚██████╔╝███████╗ ╚██████╔╝   ██║   ██║███████╗
;;  ╚═════╝ ╚══════╝  ╚═════╝    ╚═╝   ╚═╝╚══════╝
(defpackage :smuks-gl-util
  (:nicknames :sglutil)
  (:use :cl :sdrm :smuks-util)
  (:local-nicknames
   (:math #:org.shirakumo.fraf.math))
  (:export
   check-gl-error
   check-gl-fb-status
   prep-gl-implementation
   create-gl-framebuffer

   create-image-texture create-texture
   load-png-texture

   translation-matrix
   scaling-matrix
   projection-matrix
   matrix->array transpose-mat mat->arr

   ;; damage
   make-damage damage-full

   ;; texture
   mk-tex tex-id tex-format))
(in-package :smuks-gl-util)


;; ┌┬┐┌─┐─┐ ┬┌┬┐┬ ┬┬─┐┌─┐
;;  │ ├┤ ┌┴┬┘ │ │ │├┬┘├┤
;;  ┴ └─┘┴ └─ ┴ └─┘┴└─└─┘
(defstruct tex id (initd nil) (format nil))
(defun mk-tex (&optional format)
  (prog1
      (make-tex :id (gl:gen-texture) :format (or format :argb8888))
    (check-gl-error "mk-tex: gen-texture")))


;; ┌─┐┬    ┌─┐┬─┐┌─┐┌─┐
;; │ ┬│    ├─┘├┬┘├┤ ├─┘
;; └─┘┴─┘  ┴  ┴└─└─┘┴
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
    (check-gl-error "create-image-texture: bind")
    (%gl:egl-image-target-texture-2d-oes :texture-2d image)
    (check-gl-error "create-image-texture: egl-image-target-texture-2d-oes")

    (setf (tex-initd texture) t)

    texture))

(defun create-texture (ptr width height stride &key damage texture format)
  (let* ((texture (or texture (mk-tex format)))
	 (gl-format (ecase format (:xrgb8888 :bgra-ext) (:argb8888 :bgra-ext)))
	 (pixel-size (ecase format (:xrgb8888 4) (:argb8888 4)))
	 ;; TODO: This math is a bit wasteful. But without this - evil clients could crash the server
	 ;; TODO: Could instead do bounds checking during the damage event. Applying (min and max)
	 ;; Then wouldn't have to do this weird pointer math
	 (ptr-max (cffi:inc-pointer ptr (* height stride pixel-size))))
    (gl:bind-texture :texture-2d (tex-id texture))
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:pixel-store :unpack-row-length (/ stride pixel-size))

    (if (tex-initd texture)
	;; NOTE: Partial texture upload - only update the damaged areas
	;; NOTE: If no damage provided - assume full damage
	(loop for dmg in (or damage (list (make-damage :x 0 :y 0 :width width :height height)))
	      for pointy = (damage-pointer ptr dmg width pixel-size)
	      do (if (< (cffi:pointer-address pointy) (cffi:pointer-address ptr-max))
		     (progn
		       (gl:tex-sub-image-2d
			:texture-2d 0
			(damage-x dmg) (damage-y dmg)
			(min width (damage-width dmg))
			(min height (damage-height dmg))
			gl-format :unsigned-byte
			pointy)
		       (check-gl-error "create-texture: tex-sub-image-2d"))
		     (progn
		       (wrn! "Texture damage rectangle out of bounds. This would memory corrupt.")
		       (error "Sub texture upload out of bounds"))))
	;; NOTE: Full texture upload
	(progn
	  (when (>= (cffi:pointer-address ptr) (cffi:pointer-address ptr-max))
	    (wrn! "Texture rectangle out of bounds. This would memory corrupt.")
	    (error "Texture upload out of bounds"))

	  ;; TODO: Figure out the diff between internal-format and format
	  (gl:tex-image-2d :texture-2d 0 gl-format width height 0 gl-format :unsigned-byte ptr)
	  (check-gl-error "create-texture: tex-image-2d")
	  (setf (tex-initd texture) t)))
    (gl:bind-texture :texture-2d 0)
    texture))

(defun prep-gl-implementation (framebuffer width height)
  (gl:bind-framebuffer :framebuffer framebuffer)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:viewport 0 0 width height))


(defun load-png-texture (path)
  (let* ((texture (mk-tex))
	 (image (png-read:read-png-file (merge-pathnames "assets/mouse.png" (asdf:system-source-directory :smuks))))
	 (data (png-read:image-data image)))
    (gl:bind-texture :texture-2d (tex-id texture))
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-image-2d :texture-2d 0 :rgba
		     (png-read:width image) (png-read:height image)
		     0 :rgba :unsigned-byte (make-array
					     (array-total-size data)
					     :element-type '(unsigned-byte 8)
					     :displaced-to data))
    texture))

;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defstruct damage
  (full nil)
  (x 0)
  (y 0)
  (width 0)
  (height 0))

(defun damage-pointer (ptr dmg width pixel-size)
  (cffi:inc-pointer
   ptr
   (* (+ (* (damage-y dmg) width)
	 (damage-x dmg))
      pixel-size)))


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




;; ┌┬┐┌─┐┌┐ ┬ ┬┌─┐
;;  ││├┤ ├┴┐│ ││ ┬
;; ─┴┘└─┘└─┘└─┘└─┘
(defun get-gl-enum (num) (cffi:foreign-enum-keyword '%gl:enum num))

(cffi:defcallback gl-debug-callback :void ((source :uint) (type :uint) (id %gl:uint) (severity :uint) (length %gl:sizei) (message :string) (user-param :pointer))
  (declare (ignore user-param length id))
  (let ((source (get-gl-enum source)) (type (get-gl-enum type))
	(severity (get-gl-enum severity)))
    (setf source
	  (ecase source
	    ((:debug-source-api-khr :debug-source-api) "API")
	    ((:debug-source-window-system-khr :debug-source-window-system) "WIN")
	    ((:debug-source-shader-compiler-khr :debug-source-shader-compiler) "SHADER")
	    ((:debug-source-third-party-khr :debug-source-third-party) "3RD")
	    ((:debug-source-application-khr :debug-source-application) "APP")
	    ((:debug-source-other-khr :debug-source-other) "???")))
    (setf
     type
     (ecase type
       ((:debug-type-error-khr :debug-type-error) "Error")
       ((:debug-type-deprecated-behavior-khr
	 :debug-type-deprecated-behavior)
	"Deprecated behaviour")
       ((:debug-type-undefined-behavior-khr :debug-type-undefined-behavior) "Undefined behaviour")
       ((:debug-type-portability-khr :debug-type-portability) "Portability")
       ((:debug-type-performance-khr :debug-type-performance) "Performance")
       ((:debug-type-marker-khr :debug-type-marker) "Marker")
       ((:debug-type-push-group-khr :debug-type-push-group) "Push group")
       ((:debug-type-pop-group-khr :debug-type-pop-group) "Pop group")
       ((:debug-type-other-khr :debug-type-other) "???")))
    (setf severity
	  (ecase severity
	    ((:debug-severity-high-khr :debug-severity-high) "HIGH")
	    ((:debug-severity-medium-khr :debug-severity-medium) "MEDIUM")
	    ((:debug-severity-low-khr :debug-severity-low) "LOW")
	    ((:debug-severity-notification-khr :debug-severity-notification) "NOTE")))

    (glg! "~a:~a:~a~%--> ~a" severity source type message)))

(defun debug-output-insert (message &key (source :debug-source-application)
                                  (type :debug-type-other)
                                  (severity :debug-severity-notification)
                                  (id 0))
  (cffi:with-foreign-string ((buf len) message)
    (%gl:debug-message-insert source type id severity len buf)))

(defun enable-gl-debug ()
  (log! "Enabling GL debug")
  (gl:enable :debug-output)
  (%gl:debug-message-callback (cffi:callback gl-debug-callback) (cffi:null-pointer))
  (debug-output-insert "Hello from GL debug!"))
