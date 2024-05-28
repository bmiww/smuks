
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
   set-crtc! connector-crtc

   create-connector-framebuffer

   free-crtc

   init-drm page-flip

   height width
   vrefresh mode

   connector-type
   id
   hdisplay vdisplay))
