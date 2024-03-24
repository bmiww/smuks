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
(defun do-event-opcode-matchers (interface events)
  `(defmethod match-event-opcode ((obj ,interface) opcode)
     (nth opcode '(,@(mapcar 'symbolize-event events)))))

(defun do-request (interface request)
  `(defmethod ,(req-name request) ((obj ,interface) ,@(mapcar 'do-arg (args request)))))
(defun do-request-opcode-matchers (interface requests)
  `(defmethod match-request-opcode ((obj ,interface) opcode)
     (nth opcode '(,@(mapcar 'symbolize-event requests)))))

(defun do-initializer (interface)
  `(defmethod initialize-instance ":after" ((obj ,interface) &key)
     (setf (gethash (id obj) wl:*objects*) obj)))

(defun do-interface (interface)
  `(progn
     (defpackage ,(format nil ":~a" (name interface)))
     (in-package ,(format nil ":~a" (name interface)))

     ;; TODO: This could probably move the client to the wl-object thing
     (defclass ,(name interface) "(wl:wl-object)"
       ("(client :initarg :client :accessor client)"))
     ,@(mapcar (lambda (event) (do-event (name interface) event)) (events interface))
     ,@(mapcar (lambda (request) (do-request (name interface) request)) (requests interface))
     ,(do-event-opcode-matchers (name interface) (events interface))
     ,(do-request-opcode-matchers (name interface) (requests interface))
     ;; ,(do-initializer (name interface))))
     ))

;; TODO: The whole packages thing is problematic. Remove it.
;; TODO: It seems that you might also need to prefix the methods
;; since their generic definitions are conflicting.
(defun gen-lisp-code (protocol)
  `(progn
     (defpackage ":wl" "(:use #:cl)")
     (in-package ":wl")
     (defvar *objects* "(make-hash-table :test 'eq)")
     (defclass wl-object "()" "((id :initarg :id :accessor id))")
     (export '*objects* 'wl-object)
     ,(do-initializer "wl-object")
     ,@(mapcar 'do-interface protocol)))


(defun generate-wayland-classes (package xml-file)
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-lisp-code protocol)))
    (with-open-file (stream (format nil "~A.lisp" package)
			    :direction :output
			    :if-exists :supersede)
      (format stream "~A" code))))
