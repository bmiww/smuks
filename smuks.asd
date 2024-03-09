
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
	       #:livesupport
	       #:unix-sockets
	       #:alexandria
	       #:bordeaux-threads
	       #:str)
  :components ((:file "package")
	       (:file "smuks")))
