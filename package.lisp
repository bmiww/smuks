
(defpackage #:smuks
  (:use #:cl :smuks-util)
  (:local-nicknames
   (:glfw #:org.shirakumo.fraf.glfw)
   (:wlc #:wayland-server-core)
   (:wlp #:wayland-server-protocol)
   (:thread #:bordeaux-threads)))
