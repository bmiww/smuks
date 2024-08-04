
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
    (wl-shm:send-format interface :argb8888)
    (wl-shm:send-format interface :xrgb8888)))

;; ┌─┐┬ ┬┌┬┐
;; └─┐├─┤│││
;; └─┘┴ ┴┴ ┴
(defclass shm (wl-shm:dispatch)
  ())

(defmethod wl-shm:create-pool ((shm shm) id fd size) (wl:mk-if 'pool shm id :fd fd :size size :shm shm))

;; ┌─┐┌─┐┌─┐┬
;; ├─┘│ ││ ││
;; ┴  └─┘└─┘┴─┘
(defclass pool (wl-shm-pool:dispatch)
  ((buffers :initform (make-hash-table :test 'equal) :accessor buffers)
   (size :initarg :size :accessor size)
   (fd :initarg :fd :accessor fd)
   (mmap-pool :initform nil :accessor mmap-pool)
   (shm :initarg :shm :accessor shm)))

(defmethod initialize-instance :after ((pool pool) &key)
  (multiple-value-bind (ptr fd size) (mmap:mmap (fd pool) :size (size pool) :mmap '(:shared))
    (setf (mmap-pool pool) (make-mmap-pool :ptr ptr :fd fd :size size))))

(defmethod wl-shm-pool:resize ((pool pool) size)
  (multiple-value-bind (ptr fd size) (mmap:mmap (fd pool) :size size :mmap '(:shared))
    (setf (mmap-pool pool) (make-mmap-pool :ptr ptr :fd fd :size size))))

(defmethod wl-shm-pool:create-buffer ((pool pool) id offset width height stride pixel-format)
  (setf (gethash id (buffers pool))
	(wl:mk-if 'buffer pool id :offset offset :width width :height height
				  :stride stride :pixel-format pixel-format
				  :mmap-pool (mmap-pool pool))))


;; ┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;; ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; └─┘└─┘└  └  └─┘┴└─
(defclass buffer (wl-buffer:dispatch)
  ((offset :initarg :offset :initform 0 :accessor offset)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (stride :initarg :stride :accessor stride)
   (pixel-format :initarg :pixel-format :accessor pixel-format)
   (mmap-pool :initarg :mmap-pool :accessor mmap-pool)))

(defmethod wl:destroy ((buffer buffer))
  (when (mmap-pool buffer) (munmap (mmap-pool buffer))))

(defmethod pool-ptr ((buffer buffer))
  (let* ((pool (mmap-pool buffer)) (ptr (mmap-pool-ptr pool)))
    (cffi:inc-pointer ptr (offset buffer))))


;; ┌┬┐┌┬┐┌─┐  ┌┐ ┬ ┬┌─┐┌─┐┌─┐┬─┐
;;  │││││├─┤  ├┴┐│ │├┤ ├┤ ├┤ ├┬┘
;; ─┴┘┴ ┴┴ ┴  └─┘└─┘└  └  └─┘┴└─
(defclass dma-buffer (wl-buffer:dispatch)
  ((planes :initarg :planes :accessor planes)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (pixel-format :initarg :format :accessor pixel-format)
   (flags :initarg :flags :accessor flags)
   (image :initarg :image :accessor image)))

(defmethod initialize-instance :after ((buffer dma-buffer) &key)
  (let ((plane (gethash 0 (planes buffer))))
    (setf (image buffer)
	  (seglutil:create-egl-image-from-buffer
	   (egl (wl:get-display buffer))
	   (width buffer) (height buffer)
	   (pixel-format buffer)
	   (fd plane) (offset plane) (stride plane)))))

;; TODO: Find a way to reinstate this.
;; For now - it is causing bad allocs when closing windows.
;; Primarily errors out when a closed FD is being reused.
(defmethod wl:destroy :after ((buffer dma-buffer))
  ;; (seglutil:destroy-image-khr (egl (wl:get-display buffer)) (image buffer))
  ;; (loop for plane being the hash-values of (planes buffer)
	;; ;; TODO: SBCL exclusive
	;; do
	   ;; (log! "Closing FD for client: ~a ~a" (wl::ptr (wl:client buffer)) (fd plane))
	   ;; (sb-unix:unix-close (fd plane)))
  )
