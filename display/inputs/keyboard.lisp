
;; ██╗  ██╗███████╗██╗   ██╗██████╗  ██████╗  █████╗ ██████╗ ██████╗
;; ██║ ██╔╝██╔════╝╚██╗ ██╔╝██╔══██╗██╔═══██╗██╔══██╗██╔══██╗██╔══██╗
;; █████╔╝ █████╗   ╚████╔╝ ██████╔╝██║   ██║███████║██████╔╝██║  ██║
;; ██╔═██╗ ██╔══╝    ╚██╔╝  ██╔══██╗██║   ██║██╔══██║██╔══██╗██║  ██║
;; ██║  ██╗███████╗   ██║   ██████╔╝╚██████╔╝██║  ██║██║  ██║██████╔╝
;; ╚═╝  ╚═╝╚══════╝   ╚═╝   ╚═════╝  ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝
(in-package :smuks)

(defmethod process ((display display) (type (eql :keyboard-key)) (usecase (eql :passthrough)) event)
  (declare (ignore usecase))
  (keyboard-key display (keyboard@-key event) (keyboard@-state event)))

(defmethod keyboard-key ((display display) key state)
  (let* ((mods-changed? nil) (press? (eq state :pressed)) (compositor-handled :miss))
    (setf mods-changed?
	  (case key
	    ;; LEFT ALT and RIGHT ALT
	    ((56 100) (setf (k-alt? display) press?) t)
	    ;; SUPER/WINDOWS
	    (125 (setf (k-super? display) press?) t)
	    ;; LEFT SHIFT AND RIGHT SHITF
	    ((42 54) (setf (k-shift? display) press?) t)
	    ;; LEFT CTRL AND RIGHT CTRL
	    ((29 97) (setf (k-ctrl? display) press?) t)
	    (t nil)))

    ;; ctrl+alt+F*
    (when (and press? (k-alt? display) (k-ctrl? display))
      (setf compositor-handled
	    (case key
	      (59 (switch-vt display (libseat display) 1))
	      (60 (switch-vt display (libseat display) 2))
	      (61 (switch-vt display (libseat display) 3))
	      (62 (switch-vt display (libseat display) 4))
	      (t :miss))))

    ;; super-shift-*
    (when (and press? (k-super? display) (k-shift? display) (eq compositor-handled :miss))
      (setf compositor-handled
	    (case key
	      ;; Numeric keys should send a window to the corresponding desktop
	      (2  (send-to-desktop display (nth 0 (desktops display))))
	      (3  (send-to-desktop display (nth 1 (desktops display))))
	      (4  (send-to-desktop display (nth 2 (desktops display))))
	      (5  (send-to-desktop display (nth 3 (desktops display))))
	      (6  (send-to-desktop display (nth 4 (desktops display))))
	      (7  (send-to-desktop display (nth 5 (desktops display))))
	      (8  (send-to-desktop display (nth 6 (desktops display))))
	      (9  (send-to-desktop display (nth 7 (desktops display))))
	      (10 (send-to-desktop display (nth 8 (desktops display))))
	      (11 (send-to-desktop display (nth 9 (desktops display))))

	      ;; Send active window to desktop
	      (17 (send-to-output display 0))
	      (18 (send-to-output display 1))
	      (19 (send-to-output display 2))

	      ;; Key j
	      (36 (shift-window-next display))
	      ;; Key k
	      (37 (shift-window-prev display))

	      ;; TODO: These are supposed to resize windows left/right as in xmonad
	      ;; Key h
	      (35 ())
	      ;; key l
	      (38 ())

	      ;; enter - Launch a terminal
	      (28 (uiop:launch-program "kitty"))
	      ;; Key c - kill the currently selected window
	      (46 (kill-focus-client display))
	      (t :miss))))

    ;; super-*
    (when (and press? (k-super? display) (eq compositor-handled :miss))
      (setf compositor-handled
	    (case key
	      ;; Key p
	      (25 (uiop:launch-program "anyrun"))
	      ;; Key c
	      (46 (uiop:launch-program "emacsclient -c"))

	      ;; Switch outputs
	      (17 (focus-output-keyboard display 0))
	      (18 (focus-output-keyboard display 1))
	      (19 (focus-output-keyboard display 2))

	      ;; Key j
	      (36 (focus-next-window display))
	      ;; Key k
	      (37 (focus-prev-window display))

	      ;; Numeric keys - switching desktops
	      (2  (set-active-desktop-nr display 0))
	      (3  (set-active-desktop-nr display 1))
	      (4  (set-active-desktop-nr display 2))
	      (5  (set-active-desktop-nr display 3))
	      (6  (set-active-desktop-nr display 4))
	      (7  (set-active-desktop-nr display 5))
	      (8  (set-active-desktop-nr display 6))
	      (9  (set-active-desktop-nr display 7))
	      (10 (set-active-desktop-nr display 8))
	      (11 (set-active-desktop-nr display 9))
	      (t :miss))))

    (let* ((surface (or (exclusive-keyboard-focus display) (keyboard-focus display))))
      (when (and (eq compositor-handled :miss) surface)
	(when mods-changed? (notify-kb-modifiers (seat (wl:client surface))))
	(send-key (seat (wl:client surface)) key state)))))
