;; (generate-wayland-classes 'wayland-server "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell-server "xdg-shell.xml")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :xmls)
  (asdf:oos 'asdf:load-op :split-sequence))
(defpackage :generate-wayland-classes
  (:use :common-lisp :xmls :split-sequence))
(in-package :generate-wayland-classes)

(defun symbolize-event (event) (read-from-string (name event)))

(defun do-event (interface event) `(defmethod ,(name event) ((interface ,interface))))

;; TODO: Embed a request opcode as a default value in the args?
;; Do these "request" methods even really make sense?

(defun do-request (interface request) `(defmethod ,(name request) ((interface ,interface))))
(defun do-event-opcode-matchers (interface events)
  `(defmethod match-event-opcode ((interface ,interface) opcode)
     (nth opcode '(,@(mapcar 'symbolize-event events)))))

(defun do-interface (interface)
  `(progn
     (defclass ,(name interface) () (()))
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
