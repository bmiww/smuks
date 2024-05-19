
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
  :depends-on (#:cl-drm
	       #:cl-opengl
	       #:cl-egl
	       #:cl-gbm
	       #:mmap
	       #:livesupport
	       #:unix-sockets
	       #:alexandria
	       #:bordeaux-threads
	       #:str
	       #:png-read
	       #:clem
	       #:3d-math
	       #:split-sequence
	       #:file-notify
	       #:cl-libinput
	       #:cl-libseat
	       #:cl-xkb
	       #:cl-wl
	       ;; TODO: Make libiio optional via compile flag.
	       ;; It is only really used for mobile devices.
	       ;; In case if anyone needs it for desktop, hit me up.
	       ;; The flag could be resolved truthy by checking for the libiio.so file presence.
	       ;; Or just deliver with it. Or compile it in somehow.
	       #:cl-libiio
	       #:cl-wl.wayland-core
	       #:cl-wl.xdg-shell
	       #:cl-wl.zwp-linux)
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
