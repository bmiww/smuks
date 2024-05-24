
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; TODO: Getting warnings about
;; "Computing just-done stamp in plan NIL for action"
;; Don't know yet what it means completely - happened after introducing the custom cl-drm package
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
		    (when (uiop/os:getenv "DEBUG_SMUKS")
		      (format t "~%DEBUG_SMUKS is set~%")
		      (proclaim '(optimize
                                  (safety 3)
                                  (debug 3)
                                  (speed 0))))
		    (funcall next))
  :depends-on (#:cl-drm ;; TODO: Add to my own distribution
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
	       ;; TODO: Make libiio optional via compile flag.
	       ;; It is only really used for mobile devices.
	       ;; In case if anyone needs it for desktop, hit me up.
	       ;; The flag could be resolved truthy by checking for the libiio.so file presence.
	       ;; Or just deliver with it. Or compile it in somehow.
	       #:cl-libiio ;; TODO: Add to my own distribution
	       #:mmap
	       #:livesupport
	       #:unix-sockets
	       #:alexandria
	       #:bordeaux-threads
	       #:str
	       #:png-read ;; TODO: Unless you decide to create screenshotting tools - remove this dependency
	       #:clem
	       #:3d-math
	       #:split-sequence
	       #:file-notify)
  :components ((:file "util")
	       (:file "drm")
	       (:file "shaders")
	       (:file "gl-util")
	       (:file "egl-util")
	       (:file "shader-rect")
	       (:file "shader-texture")
	       (:file "shader-init")
	       (:file "package")
	       (:file "dev-track")
	       (:file "iio")
	       (:module "wl-classes"
		:components ((:file "output")
			     (:file "xdg-wm-base")
			     (:file "surface")
			     (:file "compositor")
			     (:file "shm")
			     (:file "seat")
			     (:file "dd-manager")
			     (:file "dmabuf")))
	       (:file "display")
	       (:file "smuks")))
