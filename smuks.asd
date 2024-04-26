
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
;; TODO: Getting warnings about
;; "Computing just-done stamp in plan NIL for action"
;; Don't know yet what it means completely - happened after introducing the custom cl-drm package

;; TODO: Maybe an argument can be made that the original package was trivial enough to not warrant a fork.
;; NOTE: Primarily grabbed from here
;; https://github.com/malcolmstill/cl-drm/blob/master/cl-drm.lisp
(asdf:defsystem #:cl-drm
  :serial t
  :description "Common lisp DRM integration via FFI"
  :author "bmiww <bmiww@bky.one>" ;; TODO: Add the original author to the list.
  :license "GPLv3" ;; TODO: Check if the license actually matches somewhat.
  :version "0.0.1"
  :depends-on (#:cffi #:cl-async)
  :components ((:module "drm"
		:components ((:file "package")
			     (:file "ffi")
			     (:file "drm")))))

(asdf:defsystem #:smuks
  :serial t
  :description "A window manager"
  :author "bmiww <bmiww@bky.one>"
  :license "GPLv3"
  :version "0.0.1"
  :build-operation program-op
  :build-pathname "smuks"
  :entry-point "smuks::main"
  :depends-on ("cl-drm"
	       #:cl-opengl
	       #:cl-egl
	       #:bm-cl-wayland
	       #:cl-gbm
	       #:livesupport
	       #:unix-sockets
	       #:alexandria
	       #:bordeaux-threads
	       #:str
	       #:clem
	       #:split-sequence
	       #:bm-cl-wayland.wayland-core
	       #:bm-cl-wayland.xdg-shell)
  :components ((:file "util")
	       (:file "drm")
	       (:file "shaders")
	       (:file "shader-rect")
	       (:file "gl-util")
	       (:file "egl-util")
	       (:file "package")
	       (:module "wl-classes"
		:components ((:file "surface")
			     (:file "compositor")
			     (:file "shm")
			     (:file "xdg-wm-base")))
	       (:file "smuks")))
