
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
(defpackage :smuks-drm
  (:use :cl :smuks-util)
  (:nicknames :sdrm)
  (:export
   screen-width screen-height
   width height connectors fd gbm-pointer
   crtc crtcs
   close-drm

   add-framebuffer
   framebuffer-id framebuffer-buffer framebuffer-mode rm-framebuffer!

   create-connector-framebuffer

   crtc-id free-crtc

   init-drm page-flip

   height width
   vrefresh mode

   set-crtc! connector-crtc
   connector-type connected modes
   id
   hdisplay vdisplay))
