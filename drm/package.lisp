
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

   add-framebuffer rm-framebuffer default-framebuffer
   framebuffer-id framebuffer-buffer framebuffer-mode rm-framebuffer!
   set-crtc! connector-crtc

   create-connector-framebuffer

   free-crtc
   create-bo destroy-bo
   init-drm drm-page-flip

   height width
   vrefresh mode

   connector-type
   id
   hdisplay vdisplay))
