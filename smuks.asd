
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
	       #:str)
  :components ((:file "util")
	       (:file "graphic-util")
	       (:file "drm-ffi")
	       (:file "drm")
	       (:file "egl-util")
	       (:file "package")
	       (:file "wl-base")
	       (:file "WAYLAND-SERVER")
	       (:file "XDG-SHELL")
	       (:file "wl-wire")
	       (:file "smuks-wl")
	       (:file "smuks")))
