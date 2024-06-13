
;;  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
;; ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
;; ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
;; ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
;; ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
;;  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝
;; Announces clients of the outputs (monitors/modes)
;; TODO: Add method to change the transform of an output
;; TODO: Add method to change the mode (width/height/framerate) of an output
(in-package :smuks)

;; ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; │ ┬│  │ │├┴┐├─┤│
;; └─┘┴─┘└─┘└─┘┴ ┴┴─┘
;; TODO: This is where you probably want to fake a virtual output for use cases such as:
;; Video call video sharing - virtual output with fake details
(defclass output-global (wl-output:global)
  ((x :initarg :x :accessor output-x)
   (y :initarg :y :accessor output-y)
   (real-height :initarg :real-height :accessor output-real-height)
   (real-width :initarg :real-width :accessor output-real-width)
   (width :initarg :width :accessor output-width)
   (height :initarg :height :accessor output-height)
   (refresh-rate :initarg :refresh-rate :accessor output-refresh-rate)
   (subpixel-orientation :initarg :subpixel-orientation :initform :unknown :accessor output-subpixel-orientation)
   (make :initarg :make :accessor output-make)
   (model :initarg :model :accessor output-model)
   (screen :initarg :screen :accessor output-screen)
   (transform :initarg :transform :initform :normal :accessor output-transform))
  (:documentation "Defines a lot of details regarding a physical output.
A physical output will mostly be a monitor/screen.
Most slots should be self-explanatory, so i'll keep it short:

real-height, real-width - actual physical dimensions of the output

subpixel-orientation - red green blue - or - blue green red - maybe vertical.
Unknown is also a possibility.

make - the manufacturer of the output
model - the model name/number

transform - is the screen rotated? is the screen flipped?
"))


;; ┌┬┐┬┌─┐┌─┐┌─┐┌┬┐┌─┐┬ ┬
;;  │││└─┐├─┘├─┤ │ │  ├─┤
;; ─┴┘┴└─┘┴  ┴ ┴ ┴ └─┘┴ ┴
;; TODO: Implement the release request handler
(defclass output (wl-output:dispatch)
  ())

;; TODO: Add posibility to name an output - send via name event
;; TODO: Add posibility to give outputs a description - send via description event
(defmethod initialize-instance :after ((output output) &key)
  (let ((global (wl:global output)))
    (wl-output:send-geometry output
			     (output-x global)
			     (output-y global)
			     (output-real-width global)
			     (output-real-height global)
			     (output-subpixel-orientation global)
			     (output-make global)
			     (output-model global)
			     (output-transform global))

    ;; The "1" should identify that this mode is the current output mode
    ;; Sending non-current modes (separate event) is possible but deprecated.
    (wl-output:send-mode output
			 '(:current)
			 (output-width global)
			 (output-height global)
			 (output-refresh-rate global))


    ;; Scale can also be changed and sent later. Clients assume 1, but we're being explicit.
    (wl-output:send-scale output 1)

    ;; NOTE: For example Weston actually doesn't understand these - since these are for a later version of the protocol
    ;; TODO: Maybe add a version check.
    ;; (wl-output:send-name output "Smuks output")
    ;; (wl-output:send-description output "Smuks output - the best output in the world. But currently just the one. No support for more.")

    (wl-output:send-done output)))
