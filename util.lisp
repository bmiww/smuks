
;; ██╗   ██╗████████╗██╗██╗
;; ██║   ██║╚══██╔══╝██║██║
;; ██║   ██║   ██║   ██║██║
;; ██║   ██║   ██║   ██║██║
;; ╚██████╔╝   ██║   ██║███████╗
;;  ╚═════╝    ╚═╝   ╚═╝╚══════╝
(defpackage :smuks-util
  (:use :cl)
  (:nicknames :util)
  (:local-nicknames (:clos :closer-mop))
  (:export dohash

	   *log-output* *warn-output*
	   log! wrn! glg!

	   cb pollr

	   match-kernel-errcode check-err
	   check-gl-fb-status check-gl-error

	   heading setfnil defnil flo

	   flatten get-ms
	   with-xdg-mem-file

	   make-mmap-pool mmap-pool-fd mmap-pool-size mmap-pool-ptr munmap
	   frame-counter incr run stop enabled))
(in-package :smuks-util)

(defun heading ()
  (format t "~%")
  (format t "███████╗███╗   ███╗██╗   ██╗██╗  ██╗███████╗~%")
  (format t "██╔════╝████╗ ████║██║   ██║██║ ██╔╝██╔════╝~%")
  (format t "███████╗██╔████╔██║██║   ██║█████╔╝ ███████╗~%")
  (format t "╚════██║██║╚██╔╝██║██║   ██║██╔═██╗ ╚════██║~%")
  (format t "███████║██║ ╚═╝ ██║╚██████╔╝██║  ██╗███████║~%")
  (format t "╚══════╝╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝~%")
  (format t "~%"))

;; ┬  ┌─┐┌─┐┬
;; │  │ ││ ┬│
;; ┴─┘└─┘└─┘o
(defvar *log-output* *standard-output*)
(defvar *warn-output* *standard-output*)
(defun glg! (&rest args) (format *log-output* "🎨: ~a~%" (apply #'format nil args)))
(defun log! (&rest args) (format *log-output* "🎀: ~a~%" (apply #'format nil args)))
(defun wrn! (&rest args) (format *warn-output* "⚠️: ~a~%" (apply #'format nil args)))

;; ┌┬┐┌─┐┬ ┬┌─┐┌─┐┬ ┬
;;  │││ │├─┤├─┤└─┐├─┤
;; ─┴┘└─┘┴ ┴┴ ┴└─┘┴ ┴
;; NOTE: Taken from https://github.com/yitzchak/trivial-do/blob/master/src/dohash.lisp
(defmacro dohash ((key-var value-var hash-table-form &optional result-form) &body body)
  "Iterates over the elements of an hash table and binds key-var to the key,
value-var to the associated value and then evaluates body as a tagbody that can include
declarations. Finally the result-form is returned after the iteration completes."
  #+clisp
    `(ext:dohash (,key-var ,value-var ,hash-table-form ,result-form)
       ,@body)
  #-clisp
    (let ((next-entry (gensym))
          (more (gensym))
          (repeat (gensym)))
      `(with-hash-table-iterator (,next-entry ,hash-table-form)
         (prog (,more ,key-var ,value-var)
          ,repeat
           (multiple-value-setq (,more ,key-var ,value-var)
                                (,next-entry))
           (unless ,more
             (return ,result-form))
           (locally ,@body)
           (go ,repeat)))))


;; ┬  ┬┌─┐┬─┐  ┌┬┐┌─┐┌─┐┬  ┌─┐
;; └┐┌┘├─┤├┬┘   │ │ ││ ││  └─┐
;;  └┘ ┴ ┴┴└─   ┴ └─┘└─┘┴─┘└─┘
(defmacro setfnil (&rest args)
  (if (null args)
      (error "Needs at least one argument")
      `(progn
	 ,@(mapcar (lambda (arg) `(setf ,arg nil)) args))))

(defmacro defnil (&rest args)
  (if (null args)
      (error "Needs at least one argument")
      `(progn
	 ,@(mapcar (lambda (arg) `(defvar ,arg nil)) args))))


;; ┬  ┬┌┐┌┬ ┬─┐ ┬
;; │  │││││ │┌┴┬┘
;; ┴─┘┴┘└┘└─┘┴ └─

(defun match-kernel-errcode (code)
  "Matches a linux kernel error code with a description
You can find example error codes here:
https://community.silabs.com/s/article/Linux-kernel-error-codes?language=en_US"
  (case code
    (0 nil)
    (1  "EPERM - Operation not permitted")
    (2  "ENOENT - No such file or directory")
    (9  "EBADF - Bad file descriptor number")
    (11 "EAGAIN - Try again")
    (12 "ENOMEM - Out of memory")
    (13 "EACCESS - Permission denied")
    (16 "EBUSY - Device or resource busy")
    (22 "EINVAL - Invalid argument")
    (25 "ENOTTY - Not a typewriter")
    (32 "EPIPE - Broken pipe")
    (t (format nil "UNKNOWN ERROR CODE - ~a" code))))

(defmacro check-err (&body body)
  `(let* ((result ,@body))
     (when result
	 (let ((err (smuks-util:match-kernel-errcode (abs result))))
	   (when err (error err))))
     result))


;; ┌─┐┬  ┌─┐┌┬┐┌┬┐┌─┐┌┐┌
;; ├┤ │  ├─┤ │  │ ├┤ │││
;; └  ┴─┘┴ ┴ ┴  ┴ └─┘┘└┘
;; NOTE: Taken from https://stackoverflow.com/a/25866646
(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))


;; ┌┬┐┬┌┬┐┌─┐
;;  │ ││││├┤
;;  ┴ ┴┴ ┴└─┘
(defun get-ms () (round (* (/ (get-internal-real-time) internal-time-units-per-second) 1000)))


;; ┌─┐┬
;; │ ┬│
;; └─┘┴─┘
(defun check-gl-error (&optional (prefix "GL Error"))
  (let ((msg (case (gl:get-error)
	       (:zero nil)
	       (:invalid-enum "Invalid enum")
	       (:invalid-value "Invalid value")
	       (:invalid-operation "Invalid operation")
	       (:stack-overflow "Stack overflow")
	       (:stack-underflow "Stack underflow")
	       (:out-of-memory "Out of memory")
	       (t "Unknown error"))))
    (when msg (error (format nil "~a: ~a" prefix msg)))))

(defun check-gl-fb-status (&optional (prefix "FB status"))
  (let ((msg (case (gl:check-framebuffer-status :framebuffer)
	       (:framebuffer-complete-oes nil)
	       (:framebuffer-complete nil)
	       (:zero (check-gl-error))
	       (:framebuffer-incomplete-attachment "Framebuffer incomplete attachment")
	       (:framebuffer-incomplete-missing-attachment "Framebuffer incomplete missing attachment")
	       (:framebuffer-unsupported "Framebuffer unsupported")
	       (:framebuffer-incomplete-multisample "Framebuffer incomplete multisample")
	       (:framebuffer-undefined "Framebuffer undefined")
	       (t (error "Uncovered GL framebuffer error code")))))
    (when msg (error (format nil "~a: ~a~%" prefix msg)))))


;; ┌─┐┬  ┌─┐
;; ├┤ │  │ │
;; └  ┴─┘└─┘
;; A util function to coerce a value to single-float
(defun flo (num)
  "Just a shorter form to coerce a number to float"
  (if num
      (coerce num 'single-float)
      nil))

;; ┌┬┐┌┬┐┌─┐┌─┐
;; ││││││├─┤├─┘
;; ┴ ┴┴ ┴┴ ┴┴
;; A lazy struct to keep track of the mmap triplet
(defstruct mmap-pool ptr fd size file)

(defun munmap (pool)
  (mmap:munmap (mmap-pool-ptr pool) (mmap-pool-fd pool) (mmap-pool-size pool)))


;; ┌┬┐┌─┐┌┬┐┌─┐┌┬┐
;; │││├┤ │││├┤  ││
;; ┴ ┴└─┘┴ ┴└  ─┴┘
(defun randomize-file-name (prefix) (concatenate 'string prefix (format nil "~A" (random 1000000))))

;; TODO: This is a bit dumb maybe. I don't know enough about files/descriptors
;; Basically - open and fill - then close and reopen to have an open fd
(defmacro with-xdg-mem-file ((stream prefix &key (element-type '(unsigned-byte 8))) &body body)
  `(let* ((xdg-dir (uiop/os:getenv "XDG_RUNTIME_DIR"))
	  ;; TODO: Temp file into mmap - could be a package like thingy
	  ;; Could also figure out how to do the memfd stuff, but the runtime dir should still be mem based
	  (filename (concatenate 'string xdg-dir "/" (randomize-file-name ,prefix))))

     (with-open-file (stream filename :direction :io :element-type ,element-type :if-exists :supersede)
       (let ((,stream stream))
	 ,@body))

     (let ((stream (open filename :direction :input :element-type '(unsigned-byte 8))))
      ;; TODO: SBCL Specific
      (multiple-value-bind (ptr fd size) (mmap:mmap (sb-sys:fd-stream-fd stream)
						    :protection '(:read :write)
						    :size (file-length stream) :mmap '(:private))

	(make-mmap-pool :ptr ptr :fd fd :size size :file stream)))))


;; ┌─┐┌─┐┬  ┬  ┬┌┐┌┌─┐
;; ├─┘│ ││  │  │││││ ┬
;; ┴  └─┘┴─┘┴─┘┴┘└┘└─┘
(defmacro cb (&body func) `(lambda (event) (when (member :readable event) (funcall (lambda () ,@func)))))

(defun pollr (message fd callback &rest args)
  (log! "Starting ~a poll" message)
  (apply 'cl-async:poll fd callback :poll-for '(:readable) args))


;; ┌─┐┬─┐┌─┐┌┬┐┌─┐  ┌─┐┌─┐┬ ┬┌┐┌┌┬┐┌─┐┬─┐
;; ├┤ ├┬┘├─┤│││├┤   │  │ ││ ││││ │ ├┤ ├┬┘
;; └  ┴└─┴ ┴┴ ┴└─┘  └─┘└─┘└─┘┘└┘ ┴ └─┘┴└─
(defclass frame-counter ()
  ((enabled :initform nil :accessor enabled)
   (frames :initform 0 :accessor frames)
   (thread :initform nil :accessor thread)))

(defmethod incr ((counter frame-counter)) (when (enabled counter) (incf (frames counter))))
(defmethod run ((counter frame-counter) &optional (message "FPS: "))
  (setf (enabled counter) t)
  (setf (frames counter) 0)
  (setf (thread counter)
	(bt:make-thread (lambda ()
		       (loop while (enabled counter)
			     do (sleep 1)
			     do (log! "~a~a" message (frames counter))
			     do (setf (frames counter) 0))))))

(defmethod stop ((counter frame-counter))
  (when (thread counter)
    (bt:destroy-thread (thread counter))
    (setf
     (enabled counter) nil
     (frames counter) 0
     (thread counter) nil)))


;; ┬ ┬┌┐┌┌─┐┌─┐┬─┐┌┬┐┌─┐┌┬┐
;; │ ││││└─┐│ │├┬┘ │ ├┤  ││
;; └─┘┘└┘└─┘└─┘┴└─ ┴ └─┘─┴┘
(defun list-package-classes (package)
  "Utility function to list all classes in a package"
  (let (classes (package (find-package package)))
    (do-symbols (s package)
      (when (find-class s nil)
        (push s classes)))
    classes))
