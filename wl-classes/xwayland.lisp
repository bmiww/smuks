
;; ██╗  ██╗██╗    ██╗ █████╗ ██╗   ██╗██╗      █████╗ ███╗   ██╗██████╗
;; ╚██╗██╔╝██║    ██║██╔══██╗╚██╗ ██╔╝██║     ██╔══██╗████╗  ██║██╔══██╗
;;  ╚███╔╝ ██║ █╗ ██║███████║ ╚████╔╝ ██║     ███████║██╔██╗ ██║██║  ██║
;;  ██╔██╗ ██║███╗██║██╔══██║  ╚██╔╝  ██║     ██╔══██║██║╚██╗██║██║  ██║
;; ██╔╝ ██╗╚███╔███╔╝██║  ██║   ██║   ███████╗██║  ██║██║ ╚████║██████╔╝
;; ╚═╝  ╚═╝ ╚══╝╚══╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
;; NOTE: Taking a lot of inspiration from wlroots:
;; https://gitlab.freedesktop.org/wlroots/wlroots/-/blob/master/xwayland/server.c
(in-package :smuks)

(defvar *xwayland-executable* "/usr/bin/Xwayland")
(defvar *xwayland-debug* t)
(defvar *terminate-delay* nil)

;; ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; │ ┬│  │ │├┴┐├─┤│
;; └─┘┴─┘└─┘└─┘┴ ┴┴─┘
(defclass xwayland-global (xwayland-shell-v1:global)
  ((display-number :accessor display-number)
   (xwayland-process :initarg :process :accessor xwayland-process)

   (wm-us :accessor wm-us)
   (wm-x :accessor wm-x)

   (client-us :accessor client-us)
   (client-x :accessor client-x)

   (read-us :accessor read-us)
   (write-x :accessor write-x)

   (listen-sock-path :accessor listen-sock-path)
   (listen-sock :accessor listen-sock)
   (listen-fd :accessor listen-fd)))


(defmethod cleanup-xwayland ((global xwayland-global))
  (with-accessors ((process xwayland-process) (wm-us wm-us) (wm-x wm-x)
		   (client-us client-us) (client-x client-x)
		   (read-us read-us) (write-x write-x)
		   (listen-sock listen-sock)) global
    (when process
      (log! "XWAYLAND STDERR OUTPUT:~%~a" (uiop:slurp-stream-string (sb-ext:process-error process)))
      ;; (log! "XWAYLAND STDOUT OUTPUT:~%~a" (uiop:slurp-stream-string (sb-ext:process-output process)))
      (when (sb-ext:process-alive-p process) (sb-ext:process-close process)))

    (when client-us (osicat-posix:close client-us))
    (when read-us (osicat-posix:close read-us))
    (when wm-us (osicat-posix:close wm-us))

    (when listen-sock (unix-sockets:close-unix-socket listen-sock))

    (setfnil process wm-us wm-x listen-sock write-x read-us client-us client-x)))


;; ┌─┐┬ ┬┌─┐┬  ┬
;; └─┐├─┤├┤ │  │
;; └─┘┴ ┴└─┘┴─┘┴─┘
(defclass xwayland (xwayland-shell-v1:dispatch)
  ())



;; ┌─┐┌┬┐┌─┐┬─┐┌┬┐
;; └─┐ │ ├─┤├┬┘ │
;; └─┘ ┴ ┴ ┴┴└─ ┴
;; TODO: Might want to iterate display-numbers
(defun start-xwayland (display)
  ;; TODO; The problem with this make-instance is that it will notify an incomplete xwayland to clients
  ;; Maybe do an upgrade thing at the end of this instead?
  (let ((global (make-instance 'xwayland-global :display display)))
    (with-slots (display-number listen-sock-path listen-sock listen-fd
		 read-us write-x
		 client-us client-x
		 wm-us wm-x) global
      ;; NOTE: Naively hardcoded to 0 for now
      (setf display-number 0)
      (setf listen-sock-path (format nil "/tmp/.X11-unix/X~a" display-number))

      (when (probe-file listen-sock-path) (delete-file listen-sock-path))

      (setf listen-sock (unix-sockets:make-unix-socket listen-sock-path))
      (setf listen-fd (unix-sockets::fd listen-sock))

      (setf (values read-us write-x) (osicat-posix:pipe))
      (setf (values client-us client-x) (socketpair:socketpair :unix :stream))
      (setf (values wm-us wm-x) (socketpair:socketpair :unix :stream))

      (start-xwayland-process global display-number client-x wm-x listen-fd write-x)

      ;; NOTE: We close the file descriptors that were given to the forked process
      (osicat-posix:close write-x)
      (osicat-posix:close client-x)
      (osicat-posix:close wm-x))
    global))

(defun start-xwayland-process (global display-number client-x wm-x listen-fd display-x)
  (setf (uiop:getenv "WAYLAND_SOCKET") (write-to-string client-x))

  (let* ((args `(,(format nil ":~a" display-number)
		 "-rootless" "-core"
		 "-terminate" ,*terminate-delay*
		 "-listenfd"  ,(write-to-string listen-fd)
		 ;; TODO: wlroots does double listenfd - don't know why, for now going to ignore
		 ;; "-listenfd"  ,listenfd2
		 "-displayfd" ,(write-to-string display-x)
		 ;; NOTE: wlroots has this optional
		 "-wm"        ,(write-to-string wm-x)
		 ;; NOTE: Possible extras to consider
		 ;; "-noTouchPointerEmulation"
		 ;; "-force-xrandr-emulation"
		 ))
	 (args (remove-if-not #'identity args)))

    (log! "Xwayland args ~a" args)

    ;; TODO: Since fork is not really working out - perhaps
    ;; I can use the :environment or :env arguments here
    ;; Seemingly the fork setup isn't really doing all that much that run-program wouldn't cover
    ;; I think.
    (setf (xwayland-process global)
	  (sb-ext:run-program
	   *xwayland-executable* args
	   :output :stream
	   :preserve-fds (list listen-fd display-x wm-x)))))
