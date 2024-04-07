
;; ███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗
;; ██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝
;; ███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗
;; ╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║
;; ███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║
;; ╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝
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
	       #:glfw
	       #:str)
  :components ((:file "util")
	       (:file "package")
	       (:file "wl-base")
	       (:file "WAYLAND-SERVER")
	       (:file "XDG-SHELL")
	       (:file "wl-wire")
	       (:file "smuks-wl")
	       (:file "drm-ffi")
	       (:file "drm")
	       (:file "smuks")))
