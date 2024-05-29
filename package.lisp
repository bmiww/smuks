
(defpackage #:smuks
  (:use #:cl :smuks-util :sglutil :seglutil :sdrm :libinput)
  (:local-nicknames
   (:wl #:cl-wl)
   (:wl-dd-mgr #:wl_data_device_manager)
   (:thread #:bordeaux-threads)
   (:iio #:cl-libiio)
   (:math #:org.shirakumo.fraf.math)))
