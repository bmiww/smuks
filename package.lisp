
(defpackage #:smuks
  (:use #:cl :smuks-util)
  (:local-nicknames
   (:wlc #:wayland-server-core)
   (:wlp #:wayland-server-protocol)
   (:thread #:bordeaux-threads)))
