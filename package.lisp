
(defpackage #:smuks
  (:use #:cl :smuks-util :sglutil :seglutil :sdrm :libinput)
  (:local-nicknames
   (:wl #:cl-wl)
   (:wl-dd-mgr #:wl_data_device_manager)
   (:notify #:org.shirakumo.file-notify)
   (:thread #:bordeaux-threads)))
