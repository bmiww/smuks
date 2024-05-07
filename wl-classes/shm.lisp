
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


;; ┌┬┐┌┬┐┌─┐┌─┐  ┌─┐┌─┐┌─┐┬
;; ││││││├─┤├─┘  ├─┘│ ││ ││
;; ┴ ┴┴ ┴┴ ┴┴    ┴  └─┘└─┘┴─┘
(defstruct mmap-pool ptr fd size)

(defun munmap (pool)
  (mmap:munmap (mmap-pool-ptr pool) (mmap-pool-fd pool) (mmap-pool-size pool)))


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
   (fd :initarg :fd :accessor fd)
   (mmap-pool :initform nil :accessor mmap-pool)))

(defmethod initialize-instance :after ((pool pool) &key)
  (multiple-value-bind (ptr fd size) (mmap:mmap (fd pool) :size (size pool) :mmap '(:shared))
    (setf (mmap-pool pool) (make-mmap-pool :ptr ptr :fd fd :size size))))

(defmethod wl-shm-pool:resize ((pool pool) size)
  ;; TODO: Dunno if i really needed the munmap in the end.
  ;; TODO: Also - you might actually need to munmap the pool yourself in case the client disconnects
  ;; (munmap (mmap-pool pool))
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
  ((offset :initarg :offset :accessor offset)
   (width :initarg :width :accessor width)
   (height :initarg :height :accessor height)
   (stride :initarg :stride :accessor stride)
   (pixel-format :initarg :pixel-format :accessor pixel-format)
   (mmap-pool :initarg :mmap-pool :accessor mmap-pool)))

(defmethod pool-ptr ((buffer buffer)) (mmap-pool-ptr (mmap-pool buffer)))


;; ┌┐ ┬ ┬┌┬┐┌─┐  ┬─┐┌─┐┌─┐┌┬┐
;; ├┴┐└┬┘ │ ├┤   ├┬┘├┤ ├─┤ ││
;; └─┘ ┴  ┴ └─┘  ┴└─└─┘┴ ┴─┴┘
;; TODO: Keeping this around in case if i ever want to reuse it for doing screenshots?
;; In any case - remove if unnecessary
(defun read-all-bytes (pointer size)
  (let ((new-string (make-array 0
				:element-type '(signed-byte 8)
				:fill-pointer 0
				:adjustable t)))
    (loop for i below size
	  do (vector-push-extend (cffi:mem-ref pointer :char i) new-string))
    ;; (flexi-streams:octets-to-string new-string :external-format :ascii)
    new-string
    ))

(defvar *previous* nil)
(defun compare-buffer-contents (buffer)
  (let* ((pool (mmap-pool buffer))
	 (ptr (mmap-pool-ptr pool))
	 (size (mmap-pool-size pool))
	 (contents (read-all-bytes ptr size)))
    (when *previous*
      (print "Mismatch?")
      (print (mismatch contents *previous*)))
    (setf *previous* contents)))
