
;; ██╗  ██╗██████╗  ██████╗      ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
;; ╚██╗██╔╝██╔══██╗██╔════╝     ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
;;  ╚███╔╝ ██║  ██║██║  ███╗    ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
;;  ██╔██╗ ██║  ██║██║   ██║    ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
;; ██╔╝ ██╗██████╔╝╚██████╔╝    ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
;; ╚═╝  ╚═╝╚═════╝  ╚═════╝      ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝
(in-package :smuks)


;; ┌┬┐┌─┐┌┐┌┌─┐┌─┐┌─┐┬─┐
;; │││├─┤│││├─┤│ ┬├┤ ├┬┘
;; ┴ ┴┴ ┴┘└┘┴ ┴└─┘└─┘┴└─
(defclass xdg-output-manager (zxdg-output-manager-v1:dispatch)
  ())

(defmethod zxdg-output-manager-v1:get-xdg-output ((manager xdg-output-manager) xdg-output-id output)
  (wl:up-if 'xdg-output output xdg-output-id))


;; ┌─┐┬ ┬┌┬┐┌─┐┬ ┬┌┬┐
;; │ ││ │ │ ├─┘│ │ │
;; └─┘└─┘ ┴ ┴  └─┘ ┴
(defclass xdg-output (zxdg-output-v1:dispatch output)
  ())

(defmethod shared-initialize :after ((output xdg-output) slots &key)
  (let ((global  (wl:global output)))
    (zxdg-output-v1:send-logical-position output (screen-x global) (screen-y global))
    (zxdg-output-v1:send-logical-size output (width global) (height global))
    (zxdg-output-v1:send-name output "TODO: Fill name")
    (zxdg-output-v1:send-description output "TODO: Fill description")
    (zxdg-output-v1:send-done output)))
