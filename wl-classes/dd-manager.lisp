
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


;; ┌┬┐┌─┐┬  ┬┬┌─┐┌─┐
;;  ││├┤ └┐┌┘││  ├┤
;; ─┴┘└─┘ └┘ ┴└─┘└─┘
(defclass data-device (wl-data-device:dispatch)
  ((seat :initarg :seat :accessor seat)))

;; TODO: Read this and implement as in protocol
;; This request asks the compositor to start a drag-and-drop operation on behalf of the client.

;; The source argument is the data source that provides the data for the eventual data transfer. If source is NULL, enter, leave and motion events are sent only to the client that initiated the drag and the client is expected to handle the data passing internally. If source is destroyed, the drag-and-drop session will be cancelled.

;; The origin surface is the surface where the drag originates and the client must have an active implicit grab that matches the serial.

;; The icon surface is an optional (can be NULL) surface that provides an icon to be moved around with the cursor. Initially, the top-left corner of the icon surface is placed at the cursor hotspot, but subsequent wl_surface.attach request can move the relative position. Attach requests must be confirmed with wl_surface.commit as usual. The icon surface is given the role of a drag-and-drop icon. If the icon surface already has another role, it raises a protocol error.

;; The input region is ignored for wl_surfaces with the role of a drag-and-drop icon.

;; The given source may not be used in any further set_selection or start_drag requests. Attempting to reuse a previously-used source may send a used_source error.
(defmethod wl-data-device:start-drag ((dev data-device) source origin icon serial)
  "Starts a drag operation.
Source is the data-source that provides the data for the drag.
Origin is the surface where the drag started.
Icon is the surface that provides the icon for the drag. Can be null."
  (log! "Not implemented wl-data-device:start-drag"))


;; ┌┬┐┌─┐┌┬┐┌─┐  ┌─┐┌─┐┬ ┬┬─┐┌─┐┌─┐
;;  ││├─┤ │ ├─┤  └─┐│ ││ │├┬┘│  ├┤
;; ─┴┘┴ ┴ ┴ ┴ ┴  └─┘└─┘└─┘┴└─└─┘└─┘
;; TODO: If actions are set and an event other than drag-and-drop is received, protocol should respond with an error
(defclass data-source (wl-data-source:dispatch)
  ((mimes :initform nil :accessor mimes)
   (actions :initform nil :accessor actions)))

(defmethod wl-data-source:offer ((source data-source) mime)
  (push mime (mimes source)))

;; TODO: This should be a list made from the bitfield bits via cl-wl
(defmethod wl-data-source:set-actions ((source data-source) actions)
  (setf (actions source) actions))
