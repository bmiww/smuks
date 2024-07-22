
(defpackage #:smuks
  (:use #:cl :smuks-util :sglutil :seglutil :sdrm :libinput)
  (:import-from :cl-wl defcontinue after before)
  (:local-nicknames
   (:wl #:cl-wl)
   (:wl-dd-mgr #:wl_data_device_manager)
   (:thread #:bordeaux-threads)
   (:math #:org.shirakumo.fraf.math)))
