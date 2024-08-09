
;; ██╗   ██╗██╗██████╗ ████████╗██╗   ██╗ █████╗ ██╗         ██╗  ██╗███████╗██╗   ██╗██████╗  ██████╗  █████╗ ██████╗ ██████╗
;; ██║   ██║██║██╔══██╗╚══██╔══╝██║   ██║██╔══██╗██║         ██║ ██╔╝██╔════╝╚██╗ ██╔╝██╔══██╗██╔═══██╗██╔══██╗██╔══██╗██╔══██╗
;; ██║   ██║██║██████╔╝   ██║   ██║   ██║███████║██║         █████╔╝ █████╗   ╚████╔╝ ██████╔╝██║   ██║███████║██████╔╝██║  ██║
;; ╚██╗ ██╔╝██║██╔══██╗   ██║   ██║   ██║██╔══██║██║         ██╔═██╗ ██╔══╝    ╚██╔╝  ██╔══██╗██║   ██║██╔══██║██╔══██╗██║  ██║
;;  ╚████╔╝ ██║██║  ██║   ██║   ╚██████╔╝██║  ██║███████╗    ██║  ██╗███████╗   ██║   ██████╔╝╚██████╔╝██║  ██║██║  ██║██████╔╝
;;   ╚═══╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚══════╝    ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝
;; TODO: This must never have keyboard focus methinks
(in-package :smuks)

(defclass virtual-keyboard-manager (zwp-virtual-keyboard-manager-v1:global)
  ((current-keyboard :initform nil :accessor current-keyboard)))


;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass virtual-keyboard-manager-dispatch (zwp-virtual-keyboard-manager-v1:dispatch)
  ())

(defmethod zwp-virtual-keyboard-manager-v1:create-virtual-keyboard ((manager virtual-keyboard-manager-dispatch) seat id)
  (setf (current-keyboard (wl:global manager)) (wl:mk-if 'virtual-keyboard manager id :seat seat)))


;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘
(defclass virtual-keyboard (zwp-virtual-keyboard-v1:dispatch)
  ((seat :initarg :seat :accessor seat)))

(defmethod zwp-virtual-keyboard-v1:keymap ((keyboard virtual-keyboard) format fd size)
  (log! "For now ignoring keymap and pretending that XKB is being used"))

(defmethod zwp-virtual-keyboard-v1:key ((keyboard virtual-keyboard) time key state)
  (declare (ignore time))
  (log! "Received key")
  (keyboard-key (wl:get-display keyboard) key (case state (1 :pressed) (0 :released))))

(defmethod zwp-virtual-keyboard-v1:modifiers ((keyboard virtual-keyboard) depressed latched locked group)
  (log! "Modifiers: ~a ~a ~a ~a" depressed latched locked group)
  (log! "NOT IMPLEMENTED VIRTUAL KEYBOARD MOD"))
