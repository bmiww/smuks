;; (generate-wayland-classes 'wayland-server "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell-server "xdg-shell.xml")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :xmls)
  (asdf:oos 'asdf:load-op :split-sequence))
(defpackage :generate-wayland-classes
  (:use :common-lisp :xmls :split-sequence))
(in-package :generate-wayland-classes)

(defun ev-name (event) (format nil "evt-~a" (name event)))
(defun req-name (request) (format nil "req-~a" (name request)))
(defun symbolize-event (event) (read-from-string (ev-name event)))
(defun do-arg (arg) (name arg))
(defun do-event (interface event)
  `(defmethod ,(ev-name event) ((obj ,interface) ,@(mapcar 'do-arg (args event)))))

;; TODO: Embed a request opcode as a default value in the args?
;; Do these "request" methods even really make sense?

(defun do-request (interface request)
  `(defmethod ,(req-name request) ((obj ,interface) ,@(mapcar 'do-arg (args request)))))
(defun do-event-opcode-matchers (interface events)
  `(defmethod match-event-opcode ((obj ,interface) opcode)
     (nth opcode '(,@(mapcar 'symbolize-event events)))))

(defun do-interface (interface)
  `(progn
     (defclass ,(name interface) "()"
       ("(client :initarg :client :accessor client)"))
     ,@(mapcar (lambda (event) (do-event (name interface) event)) (events interface))
     ,@(mapcar (lambda (request) (do-request (name interface) request)) (requests interface))
     ,(do-event-opcode-matchers (name interface) (events interface))))

(defun gen-lisp-code (protocol) `(progn ,@(mapcar 'do-interface protocol)))


(defun generate-wayland-classes (package xml-file)
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-lisp-code protocol)))
    (with-open-file (stream (format nil "~A.lisp" package)
			    :direction :output
			    :if-exists :supersede)
      (format stream "~A" code))))
