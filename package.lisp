
(defpackage #:smuks
  (:use #:cl :smuks-util :sglutil :seglutil :sdrm)
  (:local-nicknames
   (:wlc #:wayland-server-core)
   (:wlp #:wayland-server-protocol)
   (:thread #:bordeaux-threads)))
