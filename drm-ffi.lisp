
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; NOTE: Primarily grabbed from here
;; https://github.com/malcolmstill/cl-drm/blob/master/cl-drm.lisp

(defpackage :drm
  (:use :cl :cffi)
  (:export
   get-resources

   resources-crtcs
   resources-connectors

   set-crtc
   free-crtc

   mode-crtc-width
   mode-crtc-height))

(in-package :drm)
(define-foreign-library libdrm (t (:default "libdrm")))
(use-foreign-library libdrm)

;; ┌─┐┌┐┌┬ ┬┌┬┐┌─┐
;; ├┤ ││││ ││││└─┐
;; └─┘┘└┘└─┘┴ ┴└─┘
(defcenum mode-connection
  (:connected 1)
  (:disconnected 2)
  (:unknown-connection 3))

(defcenum mode-subpixel
  (:unknown 1)
  :horizontal-rgb
  :horizontal-bgr
  :vertical-rgb
  :vertical-bgr
  :none)

(defbitfield (PageFlipFlags :uint32)
  (:page-flip-event 1))


;; ┌─┐ ┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐
;; │───└─┐ │ ├┬┘│ ││   │ └─┐
;; └─┘ └─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘

(defcstruct mode-res
  (count-fbs :int)
  (fbs (:pointer :uint32))
  (count-crtcs :int)
  (crtcs (:pointer :uint32))
  (count-connectors :int)
  (connectors (:pointer :uint32))
  (count-encoders :int)
  (encoders :uint32)
  (min-width :uint32)
  (max-width :uint32)
  (min-height :uint32)
  (max-height :uint32))

(defcstruct mode-mode-info
  (clock :uint32)
  (hdisplay :uint16)
  (hsync-start :uint16)
  (hsync-end :uint16)
  (htotal :uint16)
  (hskew :uint16)
  (vdisplay :uint16)
  (vsync-start :uint16)
  (vsync-end :uint16)
  (vtotal :uint16)
  (vskew :uint16)
  (vrefresh :uint16)
  (flags :uint32)
  (type :uint32)
  (name :char :count 32))

(defcstruct mode-connector
  (connector-id :uint32)
  (encoder-id :uint32)
  (connector-type :uint32)
  (connector-type-id :uint32)
  (connection mode-connection)
  (mm-width :uint32)
  (mm-height :uint32)
  (subpixel mode-subpixel)
  (count-modes :int) ;; defined as just int
  (modes (:pointer (:struct mode-mode-info)))
  (count-props :int) ;; defined as just int
  (props (:pointer :uint32))
  (prop-values (:pointer :uint64))
  (count-encodes :int)
  (encoders (:pointer :uint32)))

(defcstruct mode-encoder
  (encoder-id :uint32)
  (encoder-type :uint32)
  (crtc-id :uint32)
  (possible-crtcs :uint32)
  (possible-clones :uint32))

(defcstruct mode-crtc
  (crtc-id :uint32)
  (buffer-id :uint32)
  (x :uint32)
  (y :uint32)
  (width :uint32)
  (height :uint32)
  (mode-valid :int)
  (mode (:struct mode-mode-info))
  (gamma-size :int))

(defcstruct drm-event
  (type :uint32)
  (length :uint32))

(defcstruct drm-event-vblank
  (base (:struct drm-event))
  (user-data :uint64)
  (tv-sec :uint32)
  (tv-usec :uint32)
  (sequence :uint32)
  (crtc-id :uint32))

(defcstruct mode-crtc
  (crtc-id :uint32)
  (buffer-id :uint32)
  (x :uint32)
  (y :uint32)
  (width :uint32)
  (height :uint32)
  (mode-valid :int)
  (mode (:struct mode-mode-info))
  (gamma-size :int))

(defcstruct event-context
  (version :int)
  (vblank-handler :pointer)
  (page-flip-handler :pointer))

;; ┌─┐ ┌─┐┬ ┬┌┐┌┌─┐┌─┐
;; │───├┤ │ │││││  └─┐
;; └─┘ └  └─┘┘└┘└─┘└─┘

(defcfun ("drmModeGetCrtc" mode-get-crtc) (:pointer (:struct mode-crtc))
  (fd :int)
  (crtc-id :uint32))

(defcfun ("drmModeFreeEncoder" mode-free-encoder) :void
  (encoder (:pointer (:struct mode-encoder))))

(defcfun ("drmModeFreeConnector" mode-free-connector) :void
  (connector (:pointer (:struct mode-connector))))

(defcfun ("drmModeFreeResources" mode-free-resources) :void
  (resources (:pointer (:struct mode-res))))

(defcfun ("drmModeFreeCrtc" mode-free-crtc) :void
  (crtc (:pointer (:struct mode-crtc))))

(defcfun ("drmModeSetCrtc" mode-set-crtc) :int
  (fd :int)
  (crtc-id :uint32)
  (buffer-id :uint32)
  (x :uint32)
  (y :uint32)
  (connectors (:pointer :uint32))
  (count :int)
  (mode (:pointer (:struct mode-mode-info))))


(defcfun ("drmModeGetResources" mode-get-resources) (:pointer (:struct mode-res))
  (fd :int))

(defcfun ("drmModeGetEncoder" mode-get-encoder) :pointer
  (fd :int)
  (encoder-id :uint32))

(defcfun ("drmModeGetConnector" mode-get-connector) (:pointer (:struct mode-connector))
  (fd :int)
  (connector-id :uint32))

(defcfun ("drmModeAddFB" mode-add-framebuffer) :int
  (fd :int)
  (width :uint32)
  (height :uint32)
  (depth :uint8)
  (bpp :uint8)
  (pitch :uint32)
  (bo-handle :uint32)
  (buf-id (:pointer :uint32)))

(defcfun ("drmModeRmFB" mode-remove-framebuffer) :int
  (fd :int)
  (buffer-id :uint32))

(defcfun ("drmModePageFlip" mode-page-flip) :int
  (fd :int)
  (crtc-id :uint32)
  (fb-id :uint32)
  (flags PageFlipFlags)
  (user-data :pointer))

(defcfun ("drmHandleEvent" handle-event) :int
  (fd :int)
  (event-context (:pointer (:struct event-context))))



;; ┬  ┬┌─┐┌─┐┬ ┬
;; │  │└─┐├─┘└┬┘
;; ┴─┘┴└─┘┴   ┴

;; ┌─┐┬ ┬┌┐┌┌─┐┌─┐
;; ├┤ │ │││││  └─┐
;; └  └─┘┘└┘└─┘└─┘

(defun set-crtc (fd crtc-id buffer-id x y connectors mode &optional (count (length connectors)))
  (with-foreign-objects ((connectors-p ':uint32 count))
    (dotimes (i count) (setf (mem-aref connectors-p ':uint32 i) (nth i connectors)))
    (mode-set-crtc fd crtc-id buffer-id x y connectors-p count mode)))

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
