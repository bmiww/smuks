
;; ███████╗███████╗██╗
;; ██╔════╝██╔════╝██║
;; █████╗  █████╗  ██║
;; ██╔══╝  ██╔══╝  ██║
;; ██║     ██║     ██║
;; ╚═╝     ╚═╝     ╚═╝
(in-package :drm)
(define-foreign-library libdrm (t (:default "libdrm")))
(use-foreign-library libdrm)


;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┌─┐┌┐┌┌┬┐┌─┐
;; │  │ ││││└─┐ │ ├─┤│││ │ └─┐
;; └─┘└─┘┘└┘└─┘ ┴ ┴ ┴┘└┘ ┴ └─┘
(defconstant +drm-event-context+ 3)

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
  (encoders (:pointer :uint32))
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

  ;; NOTE: The CRTC mode field is rather annoying. It is a
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

(defcfun ("drmHandleEvent" drm-handle-event) :int
  (fd :int)
  (event-context (:pointer (:struct event-context))))
