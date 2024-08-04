
;; ██╗  ██╗██████╗  ██████╗    ████████╗ ██████╗ ██████╗ ██╗     ███████╗██╗   ██╗███████╗██╗
;; ╚██╗██╔╝██╔══██╗██╔════╝    ╚══██╔══╝██╔═══██╗██╔══██╗██║     ██╔════╝██║   ██║██╔════╝██║
;;  ╚███╔╝ ██║  ██║██║  ███╗█████╗██║   ██║   ██║██████╔╝██║     █████╗  ██║   ██║█████╗  ██║
;;  ██╔██╗ ██║  ██║██║   ██║╚════╝██║   ██║   ██║██╔═══╝ ██║     ██╔══╝  ╚██╗ ██╔╝██╔══╝  ██║
;; ██╔╝ ██╗██████╔╝╚██████╔╝      ██║   ╚██████╔╝██║     ███████╗███████╗ ╚████╔╝ ███████╗███████╗
;; ╚═╝  ╚═╝╚═════╝  ╚═════╝       ╚═╝    ╚═════╝ ╚═╝     ╚══════╝╚══════╝  ╚═══╝  ╚══════╝╚══════╝
(in-package :smuks)

(defclass toplevel (xdg-toplevel:dispatch xdg-surface)
  ((title :initform nil :accessor title)
   (app-id :initform nil :accessor app-id)
   ;; TODO: Slightly annoyed, if this is really needed, then it was for
   ;; Making sure that this isn't being rendered while the texture
   ;; is being destroyed
   (closed :initform nil :accessor closed)
   (min-width :initform 0 :accessor min-width)
   (min-height :initform 0 :accessor min-height)
   (max-width :initform 0 :accessor max-width)
   (max-height :initform 0 :accessor max-height)
   (compo-max-width :initform 0 :accessor compo-max-width)
   (compo-max-height :initform 0 :accessor compo-max-height)
   (desktop :initform nil :accessor desktop)
   (states :initform nil :accessor states)
   (new-states? :initform nil :accessor new-states?)

   ;; TODO: Instead of this - make the compositor keep track of which surfaces are ready or not perhaps
   (initial-config-ackd :initform nil :accessor initial-config-ackd)))

(defmethod print-object ((toplevel toplevel) stream)
  (print-unreadable-object (toplevel stream :type t)
    (format stream "Client: ~a:::~a" (title toplevel) (app-id toplevel))))


;; ┌┬┐┌─┐┌┬┐┬ ┬
;; │││├┤  │ ├─┤
;; ┴ ┴└─┘ ┴ ┴ ┴
(defmethod (setf states) :after (states (toplevel toplevel)) (setf (new-states? toplevel) t))
(defmethod surface-x ((toplevel toplevel) x) (+ x (xdg-x-offset toplevel)))
(defmethod surface-y ((toplevel toplevel) y) (+ y (xdg-y-offset toplevel)))

(defmethod close-toplevel ((toplevel toplevel))
  (unless (closed toplevel)
    (setf (closed toplevel) t)
    (xdg-toplevel:send-close toplevel)))

(defmethod add-state ((toplevel toplevel) state) (pushnew state (states toplevel)))
(defmethod rem-state ((toplevel toplevel) state) (setf (states toplevel) (remove state (states toplevel))))

(defmethod configure-toplevel-default ((toplevel toplevel))
  (do-window-configure toplevel (compo-max-width toplevel) (compo-max-height toplevel)))


(defmethod do-window-configure ((toplevel toplevel) width height &optional serial)
  (unless (and (eq (width toplevel) width)
	       (eq (height toplevel) height)
	       (not (new-states? toplevel)))
    (with-accessors ((awaiting-ack awaiting-ack) (states states)) toplevel
      (let ((new-serial (or serial (configure-serial toplevel))))
	(if awaiting-ack
	    (after wl-surface:commit toplevel
		   (lambda (toplevel)
		     (if (awaiting-ack toplevel) :keep
			 (unless (> (last-serial toplevel) new-serial)
			   (do-window-configure toplevel width height new-serial)))))
	    (progn
	      (setf (new-states? toplevel) nil)
	      (setf awaiting-ack new-serial)
	      (xdg-toplevel:send-configure toplevel width height (apply 'configure-states states))
	      (xdg-surface:send-configure toplevel new-serial)))
	new-serial))))


;; ┬ ┬┬    ┬ ┬┌─┐┌┐┌┌┬┐┬  ┌─┐┬─┐┌─┐
;; ││││    ├─┤├─┤│││ │││  ├┤ ├┬┘└─┐
;; └┴┘┴─┘  ┴ ┴┴ ┴┘└┘─┴┘┴─┘└─┘┴└─└─┘
(defmethod xdg-toplevel:set-title ((toplevel toplevel) title) (setf (title toplevel) title))
(defmethod xdg-toplevel:set-app-id ((toplevel toplevel) app-id) (setf (app-id toplevel) app-id))

;; NOTE: For now keeping the move request empty
;; Since you probably want tiling - this will mostly be ignored
;; But - could introduce specific states/flags
(defmethod xdg-toplevel:move ((toplevel toplevel) seat serial)
  (log! "xdg-toplevel:move: Not implemented"))

;; TODO: Perhaps i can instead do this on the (setf grab-child) method
(defcontinue xdg-toplevel:set-parent ((toplevel toplevel) parent)
  (when parent
    (setf (grab-child parent) toplevel)
    (reposition-child-toplevel toplevel)
    (after cl-wl:destroy toplevel
	   (lambda (toplevel) (declare (ignore toplevel)) (setf (grab-child parent) nil)))))

;; TODO: xdg-toplevel:set-min-size: size limitations still ignored
(defmethod xdg-toplevel:set-min-size ((toplevel toplevel) width height)
  (setf (min-width toplevel) width)
  (setf (min-height toplevel) height))

;; TODO: xdg-toplevel:set-max-size: size limitations still ignored
(defmethod xdg-toplevel:set-max-size ((toplevel toplevel) width height)
  (setf (max-width toplevel) width)
  (setf (max-height toplevel) height))

(defmethod xdg-toplevel:set-maximized ((toplevel toplevel))
  (let ((width (width toplevel)) (height (height toplevel)))

    (when (< (compo-max-width toplevel) width) (setf width (compo-max-width toplevel)))
    (when (< (compo-max-height toplevel) height) (setf height (compo-max-height toplevel)))

    (add-state toplevel :maximized)
    (do-window-configure toplevel width height)))

(defmethod xdg-toplevel:unset-maximized ((toplevel toplevel))
  (let ((width (width toplevel)) (height (height toplevel)))

    (when (or (< width 0) (< (compo-max-width toplevel) width)) (setf width (compo-max-width toplevel)))
    (when (or (< height 0) (< (compo-max-height toplevel) height)) (setf height (compo-max-height toplevel)))

    (do-window-configure toplevel width height)))

(defmethod xdg-toplevel:set-minimized ((toplevel toplevel))
  "A client wants to minimize their window.
For my purposes - i'm just ignoring this and giving the client the current state as configure"
  (do-window-configure toplevel (width toplevel) (height toplevel)))

(defmethod xdg-toplevel:resize ((toplevel toplevel) seat serial edges)
  "A client wants to resize their window."
  (log! "xdg-toplevel:resize: Not implemented"))


(defmethod xdg-toplevel:set-fullscreen ((toplevel toplevel) output)
  (let* ((output (wl:global output))
	 (height (compo-max-height toplevel))
	 (width (compo-max-width toplevel)))
    (when (and (eq (output-height output) height)
	       (eq (output-width output) width))
      (setf height (output-height output))
      (setf width (output-width output)))

    (add-state toplevel :fullscreen)

    (do-window-configure toplevel width height)))

(defmethod xdg-toplevel:unset-fullscreen ((toplevel toplevel))
  "A client wants to unset fullscreen state."
  (rem-state toplevel :fullscreen)
  (do-window-configure toplevel (compo-max-width toplevel) (compo-max-height toplevel)))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defun configure-states (&rest states)
  (loop for state in states
	collect (case state
		  (:maximized 1)
		  (:fullscreen 2)
		  (:resizing 3)
		  (:activated 4)
		  (:tiled-left 5)
		  (:tiled-right 6)
		  (:tiled-top 7)
		  (:tiled-bottom 8)
		  (:suspended 9))))
