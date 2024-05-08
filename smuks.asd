
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
	       #:clem
	       #:split-sequence
	       #:file-notify
	       #:cl-libinput
	       #:cl-libseat
	       #:cl-wl
	       #:cl-wl.wayland-core
	       #:cl-wl.xdg-shell)
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
	       (:module "wl-classes"
		:components ((:file "xdg-wm-base")
			     (:file "surface")
			     (:file "compositor")
			     (:file "shm")
			     (:file "seat")
			     (:file "dd-manager")))
	       (:file "display")
	       (:file "smuks")))
