
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; ┌─┐┬  ┌─┐┌─┐┌─┐┌─┐┌─┐
;; │  │  ├─┤└─┐└─┐├┤ └─┐
;; └─┘┴─┘┴ ┴└─┘└─┘└─┘└─┘
(in-package :smuks-drm)

;; ┌─┐┌─┐┌┐┌┌┐┌┌─┐┌─┐┌┬┐┌─┐┬─┐
;; │  │ │││││││├┤ │   │ │ │├┬┘
;; └─┘└─┘┘└┘┘└┘└─┘└─┘ ┴ └─┘┴└─
(defclass connector ()
  ((id :initarg :id :accessor id)
   (connector-type :initarg :connector-type :accessor connector-type)
   (connector-type-id :initarg :connector-type-id :accessor connector-type-id)
   (connection :initarg :connection :accessor connection)
   (mm-width :initarg :mm-width :accessor mm-width)
   (mm-height :initarg :mm-height :accessor mm-height)
   (possible-crtcs :initarg :possible-crtcs :accessor possible-crtcs)
   (subpixel-order :initarg :subpixel-order :accessor subpixel-order)
   (modes :initarg :modes :accessor modes)
   (properties :initarg :properties :accessor properties)
   (encoders :initarg :encoders :accessor encoders)
   (encoder :initarg :encoder :accessor encoder)
   (crtc :initarg :crtc :accessor crtc)))

(defmethod crtc-id ((connector connector)) (id (crtc connector)))
(defmethod hdisplay ((connector connector)) (hdisplay (crtc connector)))
(defmethod vdisplay ((connector connector)) (vdisplay (crtc connector)))
(defmethod vrefresh ((connector connector)) (vrefresh (mode (crtc connector))))
(defmethod connected ((connector connector)) (connection connector))

;; TODO: Make it possible to select encoder?
;; For now - selecting the first one - since i haven't seen connectors have more than one yet
;; TODO: Maybe it might be possible to remove the encoder search logic
;; And instead use the possible-crtcs logic.
;; Currently encoder crtc is prioritized over possible-crtcs
(defun init-connector (connector encoders crtcs)
  (let* ((encoder-id (car (drm:connector!-encoders connector)))
	 (first-encoder (find-if (lambda (encoder) (eq (id encoder) encoder-id)) encoders))
	 (possible-crtcs (remove-if
			  (lambda (crtc)
			    (not (find-if (lambda (possible-crtc)
				       (eq (id crtc)
					   (drm:crtc!-id possible-crtc)))
				     (drm:connector!-possible-crtcs connector))))
			  crtcs)))
    (make-instance 'connector
       :id (drm:connector!-id connector)
       :connector-type (drm:connector!-connector-type connector)
       :connector-type-id (drm:connector!-connector-type-id connector)
       :connection (drm:connector!-connection connector)
       :possible-crtcs possible-crtcs
       :mm-width (drm:connector!-mm-width connector)
       :mm-height (drm:connector!-mm-height connector)
       :subpixel-order (drm:connector!-subpixel connector)
       :modes (loop for mode in (drm:connector!-modes connector)
		    collect (init-mode mode))
       :properties (drm:connector!-props connector)
       :encoders (drm:connector!-encoders connector)
       :encoder first-encoder
       :crtc
       (or (find-if (lambda (crtc) (eq (id crtc) (crtc-id first-encoder))) crtcs)
	   (first possible-crtcs)))))


;; ┌─┐┬─┐┌┬┐┌─┐
;; │  ├┬┘ │ │
;; └─┘┴└─ ┴ └─┘
(defclass crtc ()
  ((id :initarg :id :accessor id)
   (buffer-id :initarg :buffer-id :accessor buffer-id)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (mode :initarg :mode :accessor mode)
   (mode-valid :initarg :mode-valid :accessor mode-valid)
   (gamma-size :initarg :gamma-size :accessor gamma-size)))

(defun init-crtc (crtc)
  (make-instance 'crtc
     :id (drm:crtc!-id crtc)
     :buffer-id (drm:crtc!-buffer-id crtc)
     :x (drm:crtc!-x crtc)
     :y (drm:crtc!-y crtc)
     :width (drm:crtc!-width crtc)
     :height (drm:crtc!-height crtc)
     :mode (init-mode (drm:crtc!-mode crtc))
     :mode-valid (drm:crtc!-mode-valid crtc)
     :gamma-size (drm:crtc!-gamma-size crtc)))

(defmethod hdisplay ((crtc crtc)) (hdisplay (mode crtc)))
(defmethod vdisplay ((crtc crtc)) (vdisplay (mode crtc)))


;; ┌─┐┌┐┌┌─┐┌─┐┌┬┐┌─┐┬─┐
;; ├┤ ││││  │ │ ││├┤ ├┬┘
;; └─┘┘└┘└─┘└─┘─┴┘└─┘┴└─
(defclass encoder ()
  ((id :initarg :id :accessor id)
   (encoder-type :initarg :encoder-type :accessor encoder-type)
   (crtc-id :initarg :crtc-id :accessor crtc-id)
   (possible-crtcs :initarg :possible-crtcs :accessor possible-crtcs)
   (possible-clones :initarg :possible-clones :accessor possible-clones)))

(defun init-encoder (encoder)
  (make-instance 'encoder
     :id (drm:encoder!-id encoder)
     :encoder-type (drm:encoder!-encoder-type encoder)
     :crtc-id (drm:encoder!-crtc-id encoder)
     :possible-crtcs (drm:encoder!-possible-crtcs encoder)
     :possible-clones (drm:encoder!-possible-clones encoder)))

;; ┌┬┐┌─┐┌┬┐┌─┐
;; ││││ │ ││├┤
;; ┴ ┴└─┘─┴┘└─┘
(defclass mode ()
  ((ptr :initarg :ptr :accessor ptr)
   (clock :initarg :clock :accessor clock)
   (hdisplay :initarg :hdisplay :accessor hdisplay)
   (hsync-start :initarg :hsync-start :accessor hsync-start)
   (hsync-end :initarg :hsync-end :accessor hsync-end)
   (htotal :initarg :htotal :accessor htotal)
   (hskew :initarg :hskew :accessor hskew)
   (vdisplay :initarg :vdisplay :accessor vdisplay)
   (vsync-start :initarg :vsync-start :accessor vsync-start)
   (vsync-end :initarg :vsync-end :accessor vsync-end)
   (vtotal :initarg :vtotal :accessor vtotal)
   (vscan :initarg :vscan :accessor vscan)
   (flags :initarg :flags :accessor flags)
   (vrefresh :initarg :vrefresh :accessor vrefresh)
   (mode-type :initarg :mode-type :accessor mode-type)
   (name :initarg :name :accessor name)))

(defun init-mode (mode)
  (make-instance 'mode
     :ptr (drm:mode!-ptr mode)
     :clock (drm:mode!-clock mode)
     :hdisplay (drm:mode!-hdisplay mode)
     :hsync-start (drm:mode!-hsync-start mode)
     :hsync-end (drm:mode!-hsync-end mode)
     :htotal (drm:mode!-htotal mode)
     :hskew (drm:mode!-hskew mode)
     :vdisplay (drm:mode!-vdisplay mode)
     :vsync-start (drm:mode!-vsync-start mode)
     :vsync-end (drm:mode!-vsync-end mode)
     :vtotal (drm:mode!-vtotal mode)
     :vscan (drm:mode!-vscan mode)
     :vrefresh (drm:mode!-vrefresh mode)
     :flags (drm:mode!-flags mode)
     :mode-type (drm:mode!-type mode)
     :name (drm:mode!-name mode)))
