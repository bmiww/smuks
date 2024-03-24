;; (generate-wayland-classes 'wayland-server "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell-server "xdg-shell.xml")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :xmls)
  (asdf:oos 'asdf:load-op :split-sequence))
(defpackage :generate-wayland-classes
  (:use :common-lisp :xmls :split-sequence))
(in-package :generate-wayland-classes)

(defun ev-name (event) (read-from-string (format nil "evt-~a" (name event))))
(defun req-name (request) (read-from-string (format nil "req-~a" (name request))))
(defun symbolize-event (event) (ev-name event))
(defun do-arg (arg) (read-from-string (name arg)))

(defun do-event (interface event)
  `(defmethod ,(ev-name event) ((obj ,(read-from-string interface))
			 ,@(mapcar 'do-arg (args event)))))

(defun do-request (interface request)
  `(defmethod ,(req-name request) ((obj ,(read-from-string interface))
			    ,@(mapcar 'do-arg (args request)))))


(defun do-event-opcode-matchers (interface events)
  `((defmethod match-event-opcode ((obj ,interface) opcode)
      (nth opcode '(,@(mapcar 'symbolize-event events))))))

(defun do-request-opcode-matchers (interface requests)
  `((defmethod match-request-opcode ((obj ,interface) opcode)
      (nth opcode '(,@(mapcar 'symbolize-event requests))))))


(defun do-initializer (interface)
  `((defmethod initialize-instance :after ((obj ,interface) &key)
      (setf (gethash (id obj) wl:*objects*) obj))))

(defun do-interface (interface)
  (append
   `((defpackage ,(read-from-string (format nil ":~a" (name interface)))))
   `((in-package ,(read-from-string (format nil ":~a" (name interface)))))

   ;; TODO: This could probably move the client to the wl-object thing
   `((defclass ,(read-from-string (name interface)) (wl:wl-object)
       ((client :initarg :client :accessor client))))
   (mapcar (lambda (event) (do-event (name interface) event)) (events interface))
   (mapcar (lambda (request) (do-request (name interface) request)) (requests interface))
   (do-event-opcode-matchers (name interface) (events interface))
   (do-request-opcode-matchers (name interface) (requests interface))))

;; TODO: The whole packages thing is problematic. Remove it.
;; TODO: It seems that you might also need to prefix the methods
;; since their generic definitions are conflicting.
(defun gen-lisp-code (protocol)
  (append
   `((defpackage :wl (:use #:cl) (:export wl-object *objects*)))
   `((in-package :wl))
   `((defvar *objects* (make-hash-table :test 'eq)))
   `((defclass wl-object () ((id :initarg :id :accessor id))))
   (do-initializer "wl-object")
   (mapcar 'do-interface protocol)))


(defun generate-wayland-classes (package xml-file)
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-lisp-code protocol)))
    (with-open-file (stream (format nil "~A.lisp" package)
			    :direction :output
			    :if-exists :supersede)
      (loop :for xep :in code
	    :do (format stream "~S~%~%" xep)))))
