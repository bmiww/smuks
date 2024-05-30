
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
(defclass dd-manager (wl-dd-mgr:dispatch)
  ((devices :initform (make-hash-table) :accessor devices)
   (sources :initform (make-hash-table) :accessor sources)))

(defmethod wl-dd-mgr:create-data-source ((mgr dd-manager) id)
  (setf (gethash id (sources mgr)) (wl:mk-if 'data-source mgr id)))

(defmethod wl-dd-mgr:get-data-device ((mgr dd-manager) id seat)
  (setf (gethash id (devices mgr)) (wl:mk-if 'data-device mgr id :seat seat)))

(defmethod cancel-drag-events ((mgr dd-manager))
  (loop for device being the hash-values of (devices mgr)
	do (progn
	     (when (drag-event device)
	       (wl-data-source:send-cancelled (drag-event device))
	       (setf (drag-event device) nil)))))

(defmethod active-drag-events ((mgr dd-manager))
  (loop for device being the hash-values of (devices mgr)
	when (drag-event device) collect device))

;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
;; TODO: Maybe this can be connected with the seat somehow
;; In which case it would be easier to perform motion/enter/leave events since they primarily match
;; the seat events
(defclass data-device (wl-data-device:dispatch)
  ((seat :initarg :seat :accessor seat)
   (drag-event :initform nil :accessor drag-event)))

;; TODO: If source is nil - the drag event should not produce drop/hover notify events on other client surfaces
;; TODO: If source is destroyed - this whole event should be cancelled
(defmethod wl-data-device:start-drag ((dev data-device) source origin icon serial)
  "Starts a drag operation.
Source is the data-source that provides the data for the drag.
Origin is the surface where the drag started.
Icon is the surface that provides the icon for the drag. Can be null."
  (change-class icon 'drag-surface)
  (setf (drag-event dev) source)
  (log! "Not implemented wl-data-device:start-drag"))

(defmethod motion ((dev data-device) x y)

  ;; (wl-data-device:send-motion dev (get-ms) x y)
  )


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
