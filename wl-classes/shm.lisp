(in-package :smuks)

(defclass shm-global (wl-shm:global)
  ())

(defmethod wl-shm:dispatch-bind :after ((global wl-shm:global) client data version id)
  (declare (ignore global data version))
  (let* ((interface (wl:iface client id)))
    ;; TODO: replace the hardcoded numbers with actual parsed enumy values
    (wl-shm:send-format interface 0)
    (wl-shm:send-format interface 1)))



(defclass shm (wl-shm:dispatch)
  ())

(defmethod wl-shm:create-pool ((shm shm) id fd size)
  (print "create-pool"))
