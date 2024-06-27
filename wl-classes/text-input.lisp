
;; ████████╗███████╗██╗  ██╗████████╗    ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗
;; ╚══██╔══╝██╔════╝╚██╗██╔╝╚══██╔══╝    ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝
;;    ██║   █████╗   ╚███╔╝    ██║       ██║██╔██╗ ██║██████╔╝██║   ██║   ██║
;;    ██║   ██╔══╝   ██╔██╗    ██║       ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║
;;    ██║   ███████╗██╔╝ ██╗   ██║       ██║██║ ╚████║██║     ╚██████╔╝   ██║
;;    ╚═╝   ╚══════╝╚═╝  ╚═╝   ╚═╝       ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝
;; https://wayland.app/protocols/text-input-unstable-v3
(in-package :smuks)


;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass text-input-manager (zwp-text-input-manager-v3:dispatch)
  ())

(defmethod zwp-text-input-manager-v3:get-text-input ((manager text-input-manager) id seat)
  (wl:mk-if 'text-input manager id :seat seat))


;; ┬┌┐┌┌─┐┬ ┬┌┬┐
;; ││││├─┘│ │ │
;; ┴┘└┘┴  └─┘ ┴
(defclass text-input (zwp-text-input-v3:dispatch)
  ((seat :initarg :seat :accessor seat)
   (x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (enabled :initarg :enabled :accessor enabled)
   (pending-rectangle :initform nil :initarg :pending-rectangle :accessor pending-rectangle)))

;; TODO: Should be double buffered. Commit should apply these changes.
(defmethod zwp-text-input-v3:set-cursor-rectangle ((text-input text-input) x y width height)
  "Sets a rectangle around the text cursor in surface-local coordinates.
Can be used to render text-completions and other fancy things around the cursor."
  (setf (pending-rectangle text-input) (list x y width height)))

(defmethod zwp-text-input-v3:commit ((text-input text-input))
  "Commits the pending changes to the text-input."
  (when (pending-rectangle text-input)
    (multiple-value-bind (x y width height) (pending-rectangle text-input)
      (setf (x text-input) x
	    (y text-input) y
	    (width text-input) width
	    (height text-input) height
	    (pending-rectangle text-input) nil))))

;; TODO: Interestingly enough - "kitty" used the disable method without the enable.
;; Does it assume that the text-input is enabled by default during commit?
(defmethod zwp-text-input-v3:disable ((text-input text-input))
  "Disables the text-input."
  (setf (enabled text-input) nil))
