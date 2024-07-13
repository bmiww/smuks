
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; TODO: Getting warnings about
;; "Computing just-done stamp in plan NIL for action"
;; Don't know yet what it means completely - happened after introducing the custom cl-drm package
;; NOTE: Disable cl-opengl error checking to shave off some runtime

(defvar *DEBUG_MODE* (uiop/os:getenv "DEBUG_SMUKS"))
(defvar *ENABLE_XWAYLAND* t)

(when *DEBUG_MODE* (pushnew :smuks-debug *features*))
(when *DEBUG_MODE* (pushnew :cl-opengl-no-check-error *features*))
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
  :depends-on (#:cl-drm ;; TODO: Add to my own distribution
	       #:cl-udev ;; TODO: Add to my own distribution
	       #:cl-opengl ;; TODO: Add to my own distribution
	       #:cl-egl ;; TODO: Add to my own distribution
	       #:cl-gbm ;; TODO: Add to my own distribution
	       #:cl-libinput ;; TODO: Add to my own distribution
	       #:cl-libseat ;; TODO: Add to my own distribution
	       #:cl-xkb ;; TODO: Add to my own distribution
	       #:cl-wl ;; TODO: Add to my own distribution
	       #:cl-wl.wayland-core ;; TODO: Add to my own distribution
	       #:cl-wl.xdg-shell ;; TODO: Add to my own distribution
	       #:cl-wl.zwp-linux ;; TODO: Add to my own distribution
	       #:cl-wl.zwlr-layer-shell  ;; TODO: Add to my own distribution
	       #:cl-wl.xdg-decoration  ;; TODO: Add to my own distribution
	       #:cl-wl.virtual-keyboard  ;; TODO: Add to my own distribution
	       #:cl-wl.text-input  ;; TODO: Add to my own distribution
	       #:cl-wl.input-method  ;; TODO: Add to my own distribution
	       #:cl-wl.xwayland  ;; TODO: Add to my own distribution
	       ;; TODO: Make libiio optional via compile flag.
	       ;; It is only really used for mobile devices.
	       ;; In case if anyone needs it for desktop, hit me up.
	       ;; The flag could be resolved truthy by checking for the libiio.so file presence.
	       ;; Or just deliver with it. Or compile it in somehow.
	       #:lisp-binary ;; NOTE: Used for the accelerometer package which could be extracted
	       #:mmap
	       #:osicat
	       #:livesupport
	       #:unix-sockets
	       #:alexandria
	       #:bordeaux-threads
	       #:str
	       #:swank ;; TODO: Add for debug mode only
	       #:parse-float
	       #:png-read ;; TODO: Unless you decide to create screenshotting tools - remove this dependency
	       #:3d-math
	       #:split-sequence)
  :components ((:file "util")
	       (:module "drm"
		:components ((:file "package")
			     (:file "classes")
			     (:file "main")
			     (:file "scanout")))
	       (:file "accelerometer")
	       (:file "gl-util")
	       (:file "shaders")
	       (:file "egl-util")
	       (:file "shader-rect")
	       (:file "shader-texture")
	       (:file "shader-init")
	       (:file "package")
	       (:file "vt")
	       (:file "dev-track")
	       (:file "structs")
	       (:module "wl-classes"
		:components ((:file "output")
			     (:file "xdg-wm-base")
			     (:file "surface")
			     (:file "compositor")
			     (:file "shm")
			     (:file "seat")
			     (:file "dd-manager")
			     (:file "dmabuf")
			     (:file "layer-shell")
			     (:file "decor-manager")
			     (:file "text-input")
			     (:file "input-method")
			     (:file "virtual-keyboard")
			     (:file "xwayland")))
	       (:file "scenes")
	       (:module "display"
		:components ((:file "main")
			     (:module "inputs"
			      :components ((:file "main")
					   (:file "touch")
					   (:file "pointer")
					   (:file "keyboard")
					   (:file "output-calibration")))))
	       (:file "client")
	       (:file "smuks")
	       (:file "render")))
