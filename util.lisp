
;; ██╗   ██╗████████╗██╗██╗
;; ██║   ██║╚══██╔══╝██║██║
;; ██║   ██║   ██║   ██║██║
;; ██║   ██║   ██║   ██║██║
;; ╚██████╔╝   ██║   ██║███████╗
;;  ╚═════╝    ╚═╝   ╚═╝╚══════╝
(defpackage :smuks-util
  (:use :cl)
  (:nicknames :util)
  (:export dohash log! *log-output*
	   match-kernel-errcode check-err
	   heading setfnil defnil
	   check-gl-fb-status check-gl-error
	   flatten get-ms))
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
(defun log! (&rest args) (format *log-output* "🎀: ~a" (apply #'format nil args)))

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
    (nil (format t "No error code provided"))
    (0 nil)
    (2  "ENOENT - No such file or directory")
    (9  "EBADF - Bad file descriptor number")
    (12 "ENOMEM - Out of memory")
    (13 "EACCESS - Permission denied")
    (16 "EBUSY - Device or resource busy")
    (22 "EINVAL - Invalid argument")
    (25 "ENOTTY - Not a typewriter")
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
