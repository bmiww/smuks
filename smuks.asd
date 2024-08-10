
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; TODO: Getting warnings about
;; "Computing just-done stamp in plan NIL for action"
;; Don't know yet what it means completely - happened after introducing the custom cl-drm package

(defvar *DEBUG_MODE* (uiop/os:getenv "DEBUG_SMUKS"))
(defvar *ENABLE_XWAYLAND* t)

(when *DEBUG_MODE* (pushnew :smuks-debug *features*))
(when *DEBUG_MODE* (pushnew :cl-opengl-no-check-error *features*)) ;; NOTE: Disable cl-opengl error checking to shave off some runtime
(when *ENABLE_XWAYLAND* (pushnew :xwayland *features*))

(asdf:defsystem #:smuks
  :serial t
  :description "A window manager"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :build-operation program-op
  :build-pathname "smuks"
  :entry-point "smuks::main"
  :around-compile (lambda (next)
		    (if *DEBUG_MODE*
		      (proclaim '(optimize
                                  (safety 3)
                                  (debug 3)
                                  (speed 0)))
		      (proclaim '(optimize
				  (safety 0)
				  (debug 0)
				  (speed 3)
				  (compilation-peed 0)
				  (space 2))))
		    (funcall next))
  :depends-on (#:osicat
	       #:cl-opengl
	       #:livesupport
	       #:alexandria
	       #:bordeaux-threads
	       #:str

	       ;; TODO: Add these to my own distribution
	       ;; These are libs i either built or forked
	       #:cl-drm
	       #:cl-udev
	       #:cl-egl
	       #:cl-gbm
	       #:cl-libinput
	       #:cl-libseat
	       #:cl-xkb
	       #:cl-wl
	       #:unix-sockets
	       #:cffi-define

	       ;; TODO: Add to my own distribution
	       ;; These come together with cl-wl
	       #:cl-wl.wayland-core
	       #:cl-wl.xdg-shell
	       #:cl-wl.zwp-linux
	       #:cl-wl.zwlr-layer-shell
	       #:cl-wl.xdg-decoration
	       #:cl-wl.virtual-keyboard
	       #:cl-wl.text-input
	       #:cl-wl.input-method
	       #:cl-wl.xdg-output
	       #:cl-wl.viewporter
	       #:cl-wl.session-lock
	       #+xwayland
	       #:cl-wl.xwayland

	       #:lisp-binary ;; NOTE: Used for the accelerometer package which could be extracted
	       #:mmap
	       #:swank ;; TODO: Add for debug mode only
	       #:parse-float
	       #:png-read ;; TODO: Unless you decide to create screenshotting tools - remove this dependency
	       #:3d-math
	       #:split-sequence)
  :components ((:file "util")
	       (:file "socketpair")
	       (:module "drm"
		:components ((:file "package")
			     (:file "classes")
			     (:file "main")
			     (:file "scanout")))
	       (:file "accelerometer")
	       (:module "graphics"
		:components ((:file "gl-util")
			     (:file "shaders")
			     (:file "egl-util")
			     (:file "shader-rect")
			     (:file "shader-texture")
			     (:file "shader-surface")
			     (:file "shader-init")))

	       (:file "package")
	       (:file "vt")
	       (:file "dev-track")
	       (:file "structs")
	       (:module "wl-classes"
		:components ((:file "configure-serial")
			     (:file "output")
			     (:file "surface")
			     (:file "session-lock")
			     (:file "xdg-wm-base")
			     (:file "xdg-surface")
			     (:file "xdg-toplevel")
			     (:file "xdg-popup")
			     (:file "xdg-output")
			     (:file "viewporter")
			     (:file "compositor")
			     (:file "subcompositor")
			     (:file "region")
			     (:file "shm")
			     (:file "seat")
			     (:file "dd-manager")
			     (:file "dmabuf")
			     (:file "layer-shell")
			     (:file "decor-manager")
			     (:file "text-input")
			     (:file "input-method")
			     (:file "virtual-keyboard")
			     #+xwayland
			     (:file "xwayland")
			     ))

	       (:file "scenes")
	       (:module "display"
		:components ((:file "main")
			     (:file "focus")
			     (:module "inputs"
			      :components ((:file "main")
					   (:file "touch")
					   (:file "pointer")
					   (:file "keyboard")
					   (:file "output-calibration")))))

	       (:file "client")
	       (:file "smuks")
	       (:file "render")))
