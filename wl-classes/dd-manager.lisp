
;; ██████╗ ██████╗       ███╗   ███╗ █████╗ ███╗   ██╗ █████╗  ██████╗ ███████╗██████╗
;; ██╔══██╗██╔══██╗      ████╗ ████║██╔══██╗████╗  ██║██╔══██╗██╔════╝ ██╔════╝██╔══██╗
;; ██║  ██║██║  ██║█████╗██╔████╔██║███████║██╔██╗ ██║███████║██║  ███╗█████╗  ██████╔╝
;; ██║  ██║██║  ██║╚════╝██║╚██╔╝██║██╔══██║██║╚██╗██║██╔══██║██║   ██║██╔══╝  ██╔══██╗
;; ██████╔╝██████╔╝      ██║ ╚═╝ ██║██║  ██║██║ ╚████║██║  ██║╚██████╔╝███████╗██║  ██║
;; ╚═════╝ ╚═════╝       ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝
;; Data device manager
(in-package :smuks)

;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass dd-manager (wl-dd-mgr:dispatch) ())

(defmethod wl-dd-mgr:create-data-source ((mgr dd-manager) id)
  (wl:mk-if 'data-source mgr id))

(defmethod wl-dd-mgr:get-data-device ((mgr dd-manager) id seat)
  (wl:up-if 'data-device seat id))

;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass data-device (wl-data-device:dispatch seat)
  ())

;; TODO: If source is nil - the drag event should not produce drop/hover notify events on other client surfaces
;; TODO: If source is destroyed - this whole event should be cancelled
(defmethod wl-data-device:start-drag ((dev data-device) source origin icon serial)
  "Starts a drag operation.
Source is the data-source that provides the data for the drag.
Origin is the surface where the drag started.
Icon is the surface that provides the icon for the drag. Can be null."
  (change-class icon 'drag-surface)
  (setf (pending-drag (wl:get-display dev)) source)

  (let ((data-offer (wl:mk-if 'data-offer dev nil :source source :data-dev dev)))
    (wl-data-device:send-enter dev (next-serial dev) origin (pointer-x dev) (pointer-y dev) data-offer)))

(defmethod pointer-motion :after ((dev data-device) x y)
  (when (pending-drag (wl:get-display dev)) (wl-data-device:send-motion dev (get-ms) x y)))

(defmethod pointer-enter :after ((dev data-device) surface x y)
  (let ((drag (pending-drag (wl:get-display dev))))
    (when drag
      (let ((data-offer (wl:mk-if 'data-offer dev nil :source drag :data-dev dev)))
	(wl-data-device:send-enter dev (next-serial dev) surface x y data-offer)))))

(defmethod pointer-leave :before ((dev data-device))
  (when (and (pending-drag (wl:get-display dev)) (active-surface dev))
    (wl-data-device:send-leave dev)))


(defvar *left-pointer-button* 272)
(defmethod pointer-button :after ((dev data-device) (button (eql *left-pointer-button*)) (state (eql :released)))
  ;; TODO: Somehow need to handle the case where the client ignores the drop
  (let ((drag (pending-drag (wl:get-display dev))))
    (when drag
      (wl-data-device:send-drop dev)
      (wl-data-source:send-dnd-drop-performed drag))))


;; An empty class to identify a surface that is used as a drag icon
;; NOTE: Currently being handled very much like a cursor
(defclass drag-surface (surface)
  ())

;; ┌┬┐┌─┐┌┬┐┌─┐  ┌─┐┌─┐┬ ┬┬─┐┌─┐┌─┐
;;  ││├─┤ │ ├─┤  └─┐│ ││ │├┬┘│  ├┤
;; ─┴┘┴ ┴ ┴ ┴ ┴  └─┘└─┘└─┘┴└─└─┘└─┘
;; TODO: If actions are set and an event other than drag-and-drop is received, protocol should respond with an error
(defclass data-source (wl-data-source:dispatch)
  ((mimes :initform nil :accessor mimes)
   (actions :initform nil :accessor actions)))

(defmethod wl-data-source:offer ((source data-source) mime)
  (push mime (mimes source)))

(defmethod wl-data-source:set-actions ((source data-source) actions)
  (setf (actions source) actions))


;; ┌┬┐┌─┐┌┬┐┌─┐  ┌─┐┌─┐┌─┐┌─┐┬─┐
;;  ││├─┤ │ ├─┤  │ │├┤ ├┤ ├┤ ├┬┘
;; ─┴┘┴ ┴ ┴ ┴ ┴  └─┘└  └  └─┘┴└─
;; NOTE: As far as i can understand - the data-offer is supposed to be a very transient object
(defclass data-offer (wl-data-offer:dispatch)
  ((source :initarg :source :accessor source)
   (data-dev :initarg :data-dev :accessor data-dev)
   (dest-supports :initform nil :accessor dest-supports)
   (dest-prefers :initform nil :accessor dest-prefers)
   (dest-mimes :initform nil :accessor dest-mimes)))

(defmethod initialize-instance :after ((offer data-offer) &key)
  (wl-data-device:send-data-offer (data-dev offer) offer)
  (announce-offer offer))

(defmethod announce-offer ((offer data-offer))
  (loop for mime in (mimes (source offer))
	do (wl-data-offer:send-offer offer mime)))

(defmethod wl:destroy ((offer data-offer))
  (log! "Destroying data-offer"))

(defmethod wl-data-offer:set-actions ((offer data-offer) supported preferred)
  (setf (dest-supports offer) supported)
  (setf (dest-prefers offer) preferred))

(defmethod wl-data-offer:accept ((offer data-offer) serial mime)
  (when mime (setf (dest-mimes offer) (pushnew mime (dest-mimes offer) :test #'string=))))

(defmethod wl-data-offer:receive ((offer data-offer) mime fd)
  (wl-data-source:send-send (source offer) mime fd)
  (wl-data-source:send-dnd-finished (source offer))
  (setf (source offer) nil)
  (setf (pending-drag (wl:get-display offer)) nil))
