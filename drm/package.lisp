
;; ██████╗ ██████╗ ███╗   ███╗
;; ██╔══██╗██╔══██╗████╗ ████║
;; ██║  ██║██████╔╝██╔████╔██║
;; ██║  ██║██╔══██╗██║╚██╔╝██║
;; ██████╔╝██║  ██║██║ ╚═╝ ██║
;; ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝
(defpackage #:smuks-drm
  (:use :cl :smuks-util)
  (:nicknames :sdrm)
  (:export
   width height connectors fd gbm-pointer
   crtc crtcs
   close-drm

   add-framebuffer
   framebuffer-id framebuffer-buffer framebuffer-mode rm-framebuffer!
   framebuffer-egl-image framebuffer-gl-buffer

   create-connector-framebuffer

   crtc-id free-crtc

   init-drm page-flip

   height width
   vrefresh mode

   set-crtc! unset-crtc!
   connector-crtc
   connector-type connected modes
   id dev-t
   hdisplay vdisplay

   primary-node render-node))
