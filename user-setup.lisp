
;; ███╗   ███╗██╗   ██╗     ██████╗ ██████╗ ███╗   ██╗███████╗
;; ████╗ ████║╚██╗ ██╔╝    ██╔════╝██╔═══██╗████╗  ██║██╔════╝
;; ██╔████╔██║ ╚████╔╝     ██║     ██║   ██║██╔██╗ ██║█████╗
;; ██║╚██╔╝██║  ╚██╔╝      ██║     ██║   ██║██║╚██╗██║██╔══╝
;; ██║ ╚═╝ ██║   ██║       ╚██████╗╚██████╔╝██║ ╚████║██║
;; ╚═╝     ╚═╝   ╚═╝        ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝
;; NOTE: This contains an example of how to configure user overrides for the compositor
;; IDEA: I would love all the variable changes to immediately reflect in the compositor
;; Some of them would of course need a bunch more extra processing.
;; For example - framebuffer-count - means that some framebuffers would need to be deallocated
;; Or some new ones would need to be created via mesa/gl/egl

(log! "[BM] Running user config!")

;; ┬  ┬┌─┐┬─┐┬┌─┐┌┐ ┬  ┌─┐  ┌─┐┬  ┬┌─┐┬─┐┬─┐┬┌┬┐┌─┐┌─┐
;; └┐┌┘├─┤├┬┘│├─┤├┴┐│  ├┤   │ │└┐┌┘├┤ ├┬┘├┬┘│ ││├┤ └─┐
;;  └┘ ┴ ┴┴└─┴┴ ┴└─┘┴─┘└─┘  └─┘ └┘ └─┘┴└─┴└─┴─┴┘└─┘└─┘
;; Set the number of framebuffers that the display should cycle through
;; Higher number is of course more memory intensive
;; But can help in case of slow refresh rates/vsync issues
;; TODO: This should be set earlier...
;; (setf *framebuffer-count* 2)

;; ┬  ┌─┐┬ ┬┌┐┌┌─┐┬ ┬  ┌─┐┌─┐┌─┐┌─┐
;; │  ├─┤│ │││││  ├─┤  ├─┤├─┘├─┘└─┐
;; ┴─┘┴ ┴└─┘┘└┘└─┘┴ ┴  ┴ ┴┴  ┴  └─┘
;; Background image daemon
;; TODO: Swww is not working properly. Can't say for sure if it's us or them yet
;; For now doing swaybg instead
;; (uiop:launch-program `("swww-daemon"))
;; (uiop:launch-program `("swww" "img" "pics/Kate-dark.png"))
(uiop:launch-program `("swaybg" "-i" "pics/Kate-dark.png"))

;; NOTE: uiop is available and can be used to launch external programs on startup.
;; In my example - i always want to fire up a new emacs server on startup
(uiop:launch-program `("emacs" "--daemon" "-q" "-l" "~/.emacs.bmiww.d/init.el"))
;; As well as my terminal emulator
(uiop:launch-program `("kitty"))
