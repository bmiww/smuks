
;; ███████╗ ██████╗  ██████╗██╗   ██╗███████╗
;; ██╔════╝██╔═══██╗██╔════╝██║   ██║██╔════╝
;; █████╗  ██║   ██║██║     ██║   ██║███████╗
;; ██╔══╝  ██║   ██║██║     ██║   ██║╚════██║
;; ██║     ╚██████╔╝╚██████╗╚██████╔╝███████║
;; ╚═╝      ╚═════╝  ╚═════╝ ╚═════╝ ╚══════╝
(in-package :smuks)

;; ┬┌─┌─┐┬ ┬┌┐ ┌─┐┌─┐┬─┐┌┬┐
;; ├┴┐├┤ └┬┘├┴┐│ │├─┤├┬┘ ││
;; ┴ ┴└─┘ ┴ └─┘└─┘┴ ┴┴└──┴┘
(defmethod keyboard-focus ((display display)) (slot-value display 'keyboard-focus))
(defmethod (setf keyboard-focus) (focus-surface (display display))
  (let ((current (keyboard-focus display)))
    (if focus-surface
	(let* ((client (wl:client focus-surface)) (seat (seat client)))
	  (when seat
	    (setf (slot-value display 'keyboard-focus) focus-surface)

	    ;; NOTE: A bit of a winded attempt to remove keyboard-focus when the focus surface keyboard is destroyed
	    (when (seat-keyboard seat)
	      (before wl:destroy (seat-keyboard seat)
		      (lambda (keyboard)
			(when (and (keyboard-focus display) (eq (wl:client keyboard) (wl:client (keyboard-focus display))))
			  (setf (slot-value display 'keyboard-focus) nil)))))

	    (when current (keyboard-leave (seat current) current))
	    ;; TODO: You're supposed to send the actual pressed keys as last arg
	    ;; But currently don't have a keypress manager/tracker
	    (keyboard-enter seat focus-surface '())))
	(progn
	  (when current (keyboard-leave (seat current) current))
	  (setf (slot-value display 'keyboard-focus) nil)))))

(defmethod grab-keyboard-focus ((display display) surface)
  (setf (exclusive-keyboard-focus display) surface)
  (after wl:change-if surface
	 (lambda (class surface)
	   (declare (ignore class))
	   (when (eq (exclusive-keyboard-focus display) surface)
	     (unless (typep surface 'layer-surface)
	       (setf (exclusive-keyboard-focus display) nil)))))

  (keyboard-enter (seat surface) surface '()))

(defmethod ungrab-keyboard-focus ((display display) surface)
  (when (eq (exclusive-keyboard-focus display) surface) (setf (exclusive-keyboard-focus display) nil)))



;; TODO: If multiple toplevels for one client - this should probably first kill off all toplevels and then the client?
;; Unless we expect the client to die off itself once toplevels go away?
(defmethod kill-focus-client ((display display))
  (when (keyboard-focus display)
    (close-toplevel (keyboard-focus display))
    (setf (keyboard-focus display) nil)))

(defmethod maybe-keyboard-focus ((display display) surface)
  (with-accessors ((x cursor-x) (y cursor-y) (focus keyboard-focus)) display
    (if surface
	(unless (eq focus surface) (setf (keyboard-focus display) surface))
	(when focus (setf (keyboard-focus display) nil)))))

;; TODO: If this fails with an undefined seat - a better fix is to add a check in surface-at-coords perhaps
;; Or figure out an actual fix for that to not happen
;; TODO: Maybe redo here it to be (setf pointer-focus) similar to how keyboard-focus is done
(defmethod maybe-pointer-focus ((display display) surface)
  (with-accessors ((x cursor-x) (y cursor-y) (focus pointer-focus)) display
    (if surface
	(unless (eq focus surface)
	  (when focus (pointer-leave (seat focus)))
	  (pointer-enter (seat surface) surface (- x (x surface)) (- y (y surface)))
	  (setf focus surface))
	(when focus
	  (pointer-leave (seat focus))
	  (setf focus nil)))))

(defmethod handle-surface-change ((display display) &optional surface)
  "If a surface has had a change - it is no longer visible, has a different surface on top,
or has a all required parameters initiated to be focusable,
then this can be called to determine the new focus surfaces."
  (with-accessors ((x cursor-x) (y cursor-y)) display
    (let ((surface (or surface (surface-at-coords display x y))))
      (maybe-keyboard-focus display surface)
      (maybe-pointer-focus display surface))))

(defmethod focus-output-keyboard ((display display) output-nr)
  (let ((output (nth output-nr (outputs display))))
    (when output
      (let ((desktop (find-output-desktop display output)))
	(when desktop
	  (setf (keyboard-focus display) (first (windows desktop))))))))

(defmethod focus-next-window ((display display))
  (let ((windows (windows (active-desktop display))))
    (when windows
      (let ((index (position (keyboard-focus display) windows)))
	;; TODO: This index hack is needed for when the keyboard-focus wasn't properly set after a window was removed
	(unless index (setf index 0))
	(setf (keyboard-focus display) (nth (mod (+ index 1) (length windows)) windows))))))

(defmethod focus-prev-window ((display display))
  (let ((windows (windows (active-desktop display))))
    (when windows
      (let ((index (position (keyboard-focus display) windows)))
	;; TODO: This index hack is needed for when the keyboard-focus wasn't properly set after a window was removed
	(unless index (setf index 0))
	(setf (keyboard-focus display) (nth (mod (- index 1) (length windows)) windows))))))

(defmethod shift-window-next ((display display))
  (with-accessors ((windows windows)) (active-desktop display)
    (when windows
      (let ((focus-index (position (keyboard-focus display) windows)))
	(if (eq focus-index (1- (length windows)))
	    (rotatef (nth 0 windows) (nth focus-index windows))
	    (rotatef (nth focus-index windows) (nth (1+ focus-index) windows)))
	(recalculate-layout (active-desktop display))))))

(defmethod shift-window-prev ((display display))
  (with-accessors ((windows windows)) (active-desktop display)
    (when windows
      (let ((focus-index (position (keyboard-focus display) windows)))
	(if (eq focus-index 0)
	    (rotatef (nth focus-index windows) (nth (1- (length windows)) windows))
	    (rotatef (nth (1- focus-index) windows) (nth focus-index windows)))
	(recalculate-layout (active-desktop display))))))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
;; NOTE: Util function to kill off all client toplevels gracefully
;; IDEA: Could possibly be used as a cleanup step for when the compositor is killed
;; For now used only from repl
(defmethod kill-all-clients ((display display))
  (dolist (toplevel (toplevels (compositor display))) (close-toplevel toplevel)))
