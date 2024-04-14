
;; ██╗   ██╗████████╗██╗██╗
;; ██║   ██║╚══██╔══╝██║██║
;; ██║   ██║   ██║   ██║██║
;; ██║   ██║   ██║   ██║██║
;; ╚██████╔╝   ██║   ██║███████╗
;;  ╚═════╝    ╚═╝   ╚═╝╚══════╝
(defpackage :smuks-util
  (:use :cl)
  (:export dohash log! *log-output* match-kernel-errcode heading))
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
(defun log! (&rest args) (apply #'format *log-output* args))

;; ┌┬┐┌─┐┬ ┬┌─┐┌─┐┬ ┬
;;  │││ │├─┤├─┤└─┐├─┤
;; ─┴┘└─┘┴ ┴┴ ┴└─┘┴ ┴
;; NOTE: Taken from https://github.com/yitzchak/trivial-do/blob/master/src/dohash.lisp
(defmacro dohash ((key-var value-var hash-table-form &optional result-form) &body body)
  "dohash iterates over the elements of an hash table and binds key-var to the key,
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


;; ┬  ┬┌┐┌┬ ┬─┐ ┬
;; │  │││││ │┌┴┬┘
;; ┴─┘┴┘└┘└─┘┴ └─

(defun match-kernel-errcode (code)
  "Matches a linux kernel error code with a description
You can find example error codes here:
https://community.silabs.com/s/article/Linux-kernel-error-codes?language=en_US"
  (case code
    (0 nil)
    (9  "EBADF - Bad file descriptor number")
    (13 "EACCESS - Permission denied")
    (25 "ENOTTY - Not a typewriter")
    (t (format nil "UNKNOWN ERROR CODE - ~a" code))))
