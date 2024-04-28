
;; ███████╗██╗  ██╗███╗   ███╗
;; ██╔════╝██║  ██║████╗ ████║
;; ███████╗███████║██╔████╔██║
;; ╚════██║██╔══██║██║╚██╔╝██║
;; ███████║██║  ██║██║ ╚═╝ ██║
;; ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝
(in-package :smuks)

;; ┌─┐┬  ┌─┐┌┐ ┌─┐┬
;; │ ┬│  │ │├┴┐├─┤│
;; └─┘┴─┘└─┘└─┘┴ ┴┴─┘
(defclass shm-global (wl-shm:global)
  ())

(defmethod wl-shm:dispatch-bind :after ((global wl-shm:global) client data version id)
  (declare (ignore global data version))
  (let* ((interface (wl:iface client id)))
    ;; TODO: replace the hardcoded numbers with actual parsed enumy values
    (wl-shm:send-format interface 0)
    (wl-shm:send-format interface 1)))


;; ┌─┐┬ ┬┌┬┐
;; └─┐├─┤│││
;; └─┘┴ ┴┴ ┴
(defclass shm (wl-shm:dispatch)
  ((pools :initform (make-hash-table :test 'equal) :accessor pools)))


(defmethod wl-shm:create-pool ((shm shm) id fd size)
  (let ((pool (wl:mk-if 'pool shm id :fd fd :size size :destroy (lambda (pool) (remhash (wl:id pool) (pools shm))))))
    (setf (gethash id (pools shm)) pool)))

;; ┌─┐┌─┐┌─┐┬
;; ├─┘│ ││ ││
;; ┴  └─┘└─┘┴─┘
(defclass pool (wl-shm-pool:dispatch)
  ((buffers :initform (make-hash-table :test 'equal) :accessor buffers)
   (size :initarg :size :accessor size)
   (fd :initarg :fd :accessor fd)))

(defmethod wl-shm-pool:create-buffer ((pool pool) id offset width height stride pixel-format)
  (setf (gethash id (buffers pool))
	(wl:mk-if 'buffer pool id :offset offset :width width :height height :stride stride :pixel-format pixel-format)))

;; ┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;; ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; └─┘└─┘└  └  └─┘┴└─
(defclass buffer (wl-buffer:dispatch)
  ((offset :initarg :offset :accessor offset)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (stride :initarg :stride :accessor stride)
   (pixel-format :initarg :pixel-format :accessor pixel-format)))
