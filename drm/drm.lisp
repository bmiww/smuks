
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; TODO: The resource stuff could be turned into a smart class with methods interacting with the crtc/connectors blafu
(in-package :drm)

;; TODO: Does not need the fancy constructor
(defstruct (crtc! (:constructor map-crtc
		      (id buffer-id x y width height mode-valid mode gamma-size pointer)))
  (id nil)
  (buffer-id nil)
  (x nil)
  (y nil)
  (width nil)
  (height nil)
  (mode-valid nil)
  (mode nil)
  (gamma-size nil)
  (pointer nil))

(defstruct connector!
  (id nil)
  (encoder-id nil)
  (connector-type nil)
  (connector-type-id nil)
  (connection nil)
  (mm-width nil)
  (mm-height nil)
  (subpixel nil)
  (count-modes nil)
  (modes nil)
  (count-props nil)
  (props nil)
  (prop-values nil)
  (count-encodes nil)
  (encoders nil)
  (pointer nil))

(defstruct resources
  (resources nil)
  (fbs nil)
  (crtcs nil)
  (connectors nil)
  (encoders nil)
  (min-width nil)
  (max-width nil)
  (min-height nil)
  (max-height nil))

;; ┌─┐┬ ┬┌┐┌┌─┐┌─┐
;; ├┤ │ │││││  └─┐
;; └  └─┘┘└┘└─┘└─┘
(defun set-crtc (fd crtc-id buffer-id x y connectors mode &optional (count (length connectors)))
  (with-foreign-objects ((connectors-p ':uint32 count))
    (dotimes (i count) (setf (mem-aref connectors-p ':uint32 i) (nth i connectors)))
    (mode-set-crtc fd crtc-id buffer-id x y connectors-p count mode)))

(defun mk-crtc (c-crtc)
  (let ((de-pointerd (mem-ref c-crtc '(:struct mode-crtc))))
    (map-crtc (getf de-pointerd 'crtc-id)
	      (getf de-pointerd 'buffer-id)
	      (getf de-pointerd 'x)
	      (getf de-pointerd 'y)
	      (getf de-pointerd 'width)
	      (getf de-pointerd 'height)
	      (getf de-pointerd 'mode-valid)
	      (getf de-pointerd 'crtc-mode)
	      (getf de-pointerd 'gamma-size)
	      c-crtc)))

(defun free-crtc (crtc) (mode-free-crtc (crtc!-pointer crtc)))

;; TODO: create lisp structures for at minimum the crtc
;; I am currently missing pointers to CRTC for the sake of freeing them
(defun get-resources (fd)
  (let ((resources (mode-get-resources fd)))
    (with-foreign-slots ((crtcs count-crtcs connectors count-connectors fbs count-fbs encoders count-encoders min-width max-width min-height max-height) resources (:struct mode-res))
      (make-resources
       :resources resources
       ;; :fbs (loop for i from 0 below count-fbs collect (mem-aref fbs i))
       :crtcs (loop for i from 0 below count-crtcs
		    collect (progn (let ((crtc (mk-crtc (mode-get-crtc fd (mem-aref crtcs :uint32 i))))) crtc)))
       :connectors (loop for i from 0 below count-connectors
			 collect (mem-ref (mode-get-connector fd (mem-aref connectors :uint32 i)) '(:struct mode-connector)))
       ;; :encoders (loop for i from 0 below count-encoders collect (mode-get-encoder fd (mem-aref encoders :uint32 i)))
       :min-width min-width
       :max-width max-width
       :min-height min-height
       :max-height max-height))))
