
;; ██╗  ██╗██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ╚██╗██╔╝██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;;  ╚███╔╝ ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;;  ██╔██╗ ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ██╔╝ ██╗╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;; ╚═╝  ╚═╝ ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
(in-package :smuks)

(defvar *xwayland-executable* "Xwayland")
(defvar *xwayland-debug* t)
(defvar *terminate-delay* nil)

;; ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; │ ┬│  │ │├┴┐├─┤│
;; └─┘┴─┘└─┘└─┘┴ ┴┴─┘
(defclass xwayland-global (xwayland-shell-v1:global)
  ((notify-fd :initarg :notify-fd :accessor notify-fd)
   (xwayland-process :initarg :process :accessor xwayland-process)
   (wm-us :initarg :wm-us :accessor wm-us)))


(defmethod cleanup-xwayland ((global xwayland-global))
  (with-accessors ((process xwayland-process) (notify notify-fd) (wm wm-us)) global
    (when process (uiop:terminate-process process))
    (when notify (close notify))
    (when wm (close wm))
    (setfnil process notify wm)))


;; ┌─┐┬ ┬┌─┐┬  ┬
;; └─┐├─┤├┤ │  │
;; └─┘┴ ┴└─┘┴─┘┴─┘
(defclass xwayland (xwayland-shell-v1:dispatch)
  ())




;; ┌─┐┌┬┐┌─┐┬─┐┌┬┐
;; └─┐ │ ├─┤├┬┘ │
;; └─┘ ┴ ┴ ┴┴└─ ┴
;; TODO: wlroots also specifically sets the wayland_socket environment variable before exec
;; TODO: Might want to iterate display-numbers
(defun start-xwayland ()
  (multiple-value-bind (wm-us wm-x) (wm-socket-pair)
    (multiple-value-bind (read write) (osicat-posix:pipe)
      (let* ((display-number 0) ;; NOTE: Naively hardcoded to 0 for now
	     (listenfd (unix-sockets:make-unix-socket (format nil "/tmp/.X11-unix/X~a" display-number)))
	     (args `(,(format nil ":~a" display-number)
		     "-rootless" "-core"
		     "-terminate" ,*terminate-delay*
		     "-listenfd"  ,listenfd
		     ;; "-listenfd"  ,listenfd2 ;; TODO: wlroots does double listenfd - don't know why, for now going to ignore
		     "-displayfd" ,write
		     "-wm"        ,wm-x         ;; wlroots has this optional
		     ;; NOTE: Possible extras to consider
		     ;; "-noTouchPointerEmulation"
		     ;; "-force-xrandr-emulation"
		     ))
	     (args (remove-if-not #'identity args)))


	(prog1
	    (make-instance 'xwayland-global :notify-fd read
				:wm-us wm-us
				:process (uiop:launch-program `(,*xwayland-executable* ,@args)
							      :output (if *xwayland-debug* :inherit nil)
							      :error (if *xwayland-debug* :inherit nil)))
	  (osicat-posix:close write))))))


;; ┬ ┬┌┬┐┬┬
;; │ │ │ ││
;; └─┘ ┴ ┴┴─┘
(defvar *socket-domain* nil)
(defvar *socket-type* nil)
(defvar *socket-protocol* nil)

(defun wm-socket-pair () (osicat-posix:socketpair *socket-domain* *socket-type* *socket-protocol*))
