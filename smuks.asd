
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝

;; TODO: Maybe an argument can be made that the original package was trivial enough to not warrant a fork.
;; NOTE: Primarily grabbed from here
;; https://github.com/malcolmstill/cl-drm/blob/master/cl-drm.lisp
(asdf:defsystem #:cl-drm
  :serial t
  :description "Common lisp DRM integration via FFI"
  :author "bmiww <bmiww@bky.one>" ;; TODO: Add the original author to the list.
  :license "GPLv3" ;; TODO: Check if the license actually matches somewhat.
  :version "0.0.1"
  :depends-on (#:cffi)
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
  :depends-on (#:cl-opengl
	       #:cl-egl
	       #:cl-wayland
	       #:cl-gbm
	       #:livesupport
	       #:unix-sockets
	       #:alexandria
	       #:bordeaux-threads
	       #:swank
	       #:str
	       #:cl-drm
	       ;; NOTE: Required by the wl-generator module
	       #:xmls
	       #:split-sequence)
  :components ((:file "util")
	       (:file "drm")
	       (:file "gl-util")
	       (:file "egl-util")
	       (:file "package")
	       (:file "wl-base")
	       (:file "WAYLAND-SERVER")
	       (:file "XDG-SHELL")
	       (:file "wl-wire")
	       (:file "smuks-wl")
	       (:file "smuks")
	       (:module "wl-generator"
		:components ((:file "wayland-parser")
			     (:file "wayland-generator")))))
