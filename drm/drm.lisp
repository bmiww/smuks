
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
;; TODO: The resource stuff could be turned into a smart class with methods interacting with the crtc/connectors blafu
(in-package :drm)

;; TODO: Does not need the fancy constructor
(defstruct crtc!
  (id nil)
  (buffer-id nil)
  (x nil)
  (y nil)
  (width nil)
  (height nil)
  (mode-valid nil)
  (mode nil)
  (mode-ptr nil)
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

(defstruct encoder!
  (id nil)
  (encoder-type nil)
  (crtc-id nil)
  (possible-crtcs nil)
  (possible-clones nil)
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
  ;; (let ((mode (if (pointerp mode) mode (convert-to-foreign)))))
  (with-foreign-objects ((connectors-p :uint32 count))
    (dotimes (i count) (setf (mem-aref connectors-p :uint32 i) (nth i connectors)))
    (mode-set-crtc fd crtc-id buffer-id x y connectors-p count mode)))

(defun get-crtc (fd crtc-id)
  (mode-get-crtc fd crtc-id))

(defun mk-crtc (c-crtc)
  (let ((de-pointerd (mem-ref c-crtc '(:struct mode-crtc))))
    (make-crtc! :id         (getf de-pointerd 'crtc-id)
		:buffer-id  (getf de-pointerd 'buffer-id)
		:x          (getf de-pointerd 'x)
		:y          (getf de-pointerd 'y)
		:width      (getf de-pointerd 'width)
		:height     (getf de-pointerd 'height)
		:mode-valid (getf de-pointerd 'mode-valid)
		:mode       (getf de-pointerd 'mode)
		:mode-ptr   (foreign-slot-pointer c-crtc '(:struct mode-crtc) 'mode)
		:gamma-size (getf de-pointerd 'gamma-size)
		:pointer    c-crtc)))

(defun free-crtc (crtc) (mode-free-crtc (crtc!-pointer crtc)))

(defun mk-connector (c-connector)
  (let ((de-pointerd (mem-ref c-connector '(:struct mode-connector))))
    (make-connector! :id (getf de-pointerd 'connector-id)
		:encoder-id (getf de-pointerd 'encoder-id)
		:connector-type (getf de-pointerd 'connector-type)
		:connector-type-id (getf de-pointerd 'connector-type-id)
		:connection (getf de-pointerd 'connection)
		:mm-width (getf de-pointerd 'mm-width)
		:mm-height (getf de-pointerd 'mm-height)
		:subpixel (getf de-pointerd 'subpixel)
		:count-modes (getf de-pointerd 'count-modes)
		:modes (getf de-pointerd 'modes)
		:count-props (getf de-pointerd 'count-props)
		:props (getf de-pointerd 'props)
		:prop-values (getf de-pointerd 'prop-values)
		:count-encodes (getf de-pointerd 'count-encodes)
		:encoders (getf de-pointerd 'encoders)
		:pointer c-connector)))

(defun mk-encoder (c-encoder)
  (let ((de-pointerd (mem-ref c-encoder '(:struct mode-encoder))))
    (make-encoder! :id (getf de-pointerd 'encoder-id)
		   :encoder-type (getf de-pointerd 'encoder-type)
		   :crtc-id (getf de-pointerd 'crtc-id)
		   :possible-crtcs (getf de-pointerd 'possible-crtcs)
		   :possible-clones (getf de-pointerd 'possible-clones)
		   :pointer c-encoder)))

(defun get-encoder-by-id (resources id)
  (find-if (lambda (encoder) (= id (encoder!-id encoder))) (resources-encoders resources)))

(defun get-resources (fd)
  (let ((resources (mode-get-resources fd)))
    (with-foreign-slots
	((crtcs count-crtcs connectors count-connectors
		fbs count-fbs encoders count-encoders
		min-width max-width min-height max-height)
	 resources (:struct mode-res))
      (make-resources
       :resources resources
       ;; :fbs (loop for i from 0 below count-fbs collect (mem-aref fbs i))
       :crtcs      (loop for i from 0 below count-crtcs
			 collect (mk-crtc (mode-get-crtc fd (mem-aref crtcs :uint32 i))))
       :connectors (loop for i from 0 below count-connectors
			 collect (mk-connector (mode-get-connector fd (mem-aref connectors :uint32 i))))
       :encoders   (loop for i from 0 below count-encoders
			 collect (mk-encoder (mode-get-encoder fd (mem-aref encoders :uint32 i))))
       :min-width min-width
       :max-width max-width
       :min-height min-height
       :max-height max-height))))

;; TODO: Check if the order of freeing things here should be in any way different
;; TODO: Add a field to track the original CRTC - so that we can return to it once done
(defun free-resources (resources)
  (loop for crtc in (resources-crtcs resources) do (free-crtc crtc))
  (loop for connector in (resources-connectors resources) do (mode-free-connector (connector!-pointer connector)))
  (loop for encoder in (resources-encoders resources) do (mode-free-encoder (encoder!-pointer encoder)))
  (mode-free-resources (resources-resources resources)))
