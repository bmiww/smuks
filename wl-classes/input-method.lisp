
;; ██╗███╗   ██╗██████╗ ██╗   ██╗████████╗    ███╗   ███╗███████╗████████╗██╗  ██╗ ██████╗ ██████╗
;; ██║████╗  ██║██╔══██╗██║   ██║╚══██╔══╝    ████╗ ████║██╔════╝╚══██╔══╝██║  ██║██╔═══██╗██╔══██╗
;; ██║██╔██╗ ██║██████╔╝██║   ██║   ██║       ██╔████╔██║█████╗     ██║   ███████║██║   ██║██║  ██║
;; ██║██║╚██╗██║██╔═══╝ ██║   ██║   ██║       ██║╚██╔╝██║██╔══╝     ██║   ██╔══██║██║   ██║██║  ██║
;; ██║██║ ╚████║██║     ╚██████╔╝   ██║       ██║ ╚═╝ ██║███████╗   ██║   ██║  ██║╚██████╔╝██████╔╝
;; ╚═╝╚═╝  ╚═══╝╚═╝      ╚═════╝    ╚═╝       ╚═╝     ╚═╝╚══════╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ ╚═════╝
;; https://wayland.app/protocols/input-method-unstable-v2
(in-package :smuks)


;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass input-method-manager (zwp-input-method-manager-v2:dispatch)
  ((input-method :initform nil :accessor input-method)))


(defmethod zwp-input-method-manager-v2:get-input-method ((manager input-method-manager) seat id)
  (setf (input-method manager) (wl:mk-if 'input-method manager id :seat seat)))

(defmethod activate ((manager input-method-manager))
  (activate (input-method manager)))

;; ┬┌┐┌┌─┐┬ ┬┌┬┐  ┌┬┐┌─┐┌┬┐┬ ┬┌─┐┌┬┐
;; ││││├─┘│ │ │   │││├┤  │ ├─┤│ │ ││
;; ┴┘└┘┴  └─┘ ┴   ┴ ┴└─┘ ┴ ┴ ┴└─┘─┴┘
(defclass input-method (zwp-input-method-v2:dispatch)
  ((seat :initarg :seat :accessor seat)))

(defmethod activate ((input-method input-method))
  (log! "Actiaving?")
  (zwp-input-method-v2:send-activate input-method)

  (zwp-input-method-v2:send-content-type input-method '(:none) :normal)
  ;; (zwp-input-method-v2:send-text-change-cause input-method :other)

  ;; TODO: The default values here can most likely be extracted from
  ;; The text input events that will actually be triggering this
  (zwp-input-method-v2:send-surrounding-text input-method "" 0 0)
  ;; NOTE: This is supposed to be double-buffered. In case if errors. You know where.
  (zwp-input-method-v2:send-done input-method))

(defmethod deactivate ((input-method input-method))
  (zwp-input-method-v2:send-deactivate input-method))
