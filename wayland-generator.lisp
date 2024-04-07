
;;  ██████╗ ███████╗███╗   ██╗███████╗██████╗  █████╗ ████████╗ ██████╗ ██████╗
;; ██╔════╝ ██╔════╝████╗  ██║██╔════╝██╔══██╗██╔══██╗╚══██╔══╝██╔═══██╗██╔══██╗
;; ██║  ███╗█████╗  ██╔██╗ ██║█████╗  ██████╔╝███████║   ██║   ██║   ██║██████╔╝
;; ██║   ██║██╔══╝  ██║╚██╗██║██╔══╝  ██╔══██╗██╔══██║   ██║   ██║   ██║██╔══██╗
;; ╚██████╔╝███████╗██║ ╚████║███████╗██║  ██║██║  ██║   ██║   ╚██████╔╝██║  ██║
;;  ╚═════╝ ╚══════╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝    ╚═════╝ ╚═╝  ╚═╝
;; NOTE: Example invocations
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
(defun enum-name (enum) (read-from-string (format nil "enum-~a" (name enum))))
(defun symbolize-event (event) (ev-name event))
(defun symbolize-request (request) (req-name request))
(defun do-arg (arg) (read-from-string (name arg)))
(defun arg-type-symbol (arg)
  (if (enum arg)
      (read-from-string (format nil "(~a ~a)" (arg-type arg) (enum arg)))
      (read-from-string (format nil "~a" (arg-type arg)))))

(defun do-event (interface event)
  `(defmethod ,(ev-name event) ((obj ,(read-from-string interface))
			 stream
			 ,@(mapcar 'do-arg (args event)))
     (let ((opcode (match-event-opcode obj ,(symbolize-event event))))
       ,(format nil ";; ~a" (description event))
       (error "UNIMPLEMENTED. YOU DECIDED TO IMPLEMENT IT IN THE smuks package."))))

(defun do-request (interface request)
  `(defmethod ,(req-name request) ((obj ,(read-from-string interface))
			    ;; NOTE: Requiring a client object whatever it may be. This is up to the implementation.
			    client
			    ,@(mapcar 'do-arg (args request)))
     ,(format nil ";; ~a" (description request))
     (error "Unimplemented")))

(defun do-regular-enum (interface enum)
  `(defmethod ,(enum-name enum) ((obj ,(read-from-string interface)) value)
     ,(format nil ";; ~a" (description enum))
     (case value
       ,@(mapcar (lambda (entry) `(,(value entry) ',(read-from-string (name entry)))) (entries enum)))))

(defun do-bitfield-enum (interface enum)
  `(defmethod ,(enum-name enum) ((obj ,(read-from-string interface)) value)
     ,(format nil ";; ~a" (description enum))
     (let ((options ',(mapcar (lambda (entry) (cons (value entry) '(read-from-string (name entry)))) (entries enum))))
       (loop for (mask name) in options
	     when (logbitp mask value)
	     collect name))))

(defun do-enum (interface enum)
  (if (bitfield-p enum)
      (do-bitfield-enum interface enum)
      (do-regular-enum interface enum)))


(defun do-event-opcode-matchers (interface events)
  `((defmethod match-event-opcode ((obj ,(read-from-string interface)) event)
      (case event
	,@(loop for event in events
		;; TODO: Check if 0 indexed or 1 indexed
		for i from 0
		collect `(,(symbolize-event event) ,i))))))

(defun do-request-opcode-matchers (interface requests)
  `((defmethod match-request-opcode ((obj ,(read-from-string interface)) opcode)
      (nth opcode '(,@(mapcar 'symbolize-request requests))))))

(defun do-request-arg-types (interface requests)
  `((defmethod get-request-arg-types ((obj ,(read-from-string interface)) opcode)
      (nth opcode '(,@(mapcar (lambda (req) (mapcar 'arg-type-symbol (args req))) requests))))))

(defun do-interface (interface)
  (let ((if-name (read-from-string (format nil ":wl/~a" (name interface))))
	(class-name (read-from-string (name interface))))
    (append
     `((defpackage ,if-name
	 (:use :cl :wl)
	 (:export
	  ,class-name
	  ,@(mapcar #'req-name (requests interface))
	  ,@(mapcar #'ev-name (events interface)))))
     `((in-package ,if-name))

    ;; TODO: This could probably move the client to the wl-object thing
     `((defclass ,class-name (wl:wl-object)
	 ((client :initarg :client :accessor client))
	 (:default-initargs :version ,(version interface) :ifname ,(name interface))
	 (:documentation ,(description interface))))
     (mapcar (lambda (event) (do-event (name interface) event)) (events interface))
     (mapcar (lambda (request) (do-request (name interface) request)) (requests interface))
     (mapcar (lambda (enum) (do-enum (name interface) enum)) (enums interface))
     (do-event-opcode-matchers (name interface) (events interface))
     (do-request-opcode-matchers (name interface) (requests interface))
     (do-request-arg-types (name interface) (requests interface)))))

(defvar *arg-type-symbols* '(int uint object new_id fixed string array fd enum))

(defun gen-lisp-code (protocol)
  (let ((interfaces (apply #'append (mapcar 'do-interface protocol))))
    (append
     `((defpackage :wl
	 (:use #:cl)
	 (:export wl-object match-event-opcode match-request-opcode get-request-arg-types
		  id ifname version
		  ,@(mapcar (lambda (a) a) *arg-type-symbols*))))
     `((in-package :wl))
     `((defvar *arg-type-symbols* ',*arg-type-symbols*))
     `((defclass wl-object () ((id :initarg :id :accessor id)
			       (ifname :initarg :ifname :reader ifname)
			       (version :initarg :version :reader version))))
     `((defgeneric match-event-opcode (obj opcode)))
     `((defgeneric match-request-opcode (obj opcode)))
     `((defgeneric get-request-arg-types (obj opcode)))
     interfaces)))


(defun generate-wayland-classes (package xml-file)
  (let* ((xml (with-open-file (s xml-file :if-does-not-exist :error) (xmls:parse s)))
	 (protocol (read-protocol xml))
	 (code (gen-lisp-code protocol)))
    (with-open-file (stream (format nil "~A.lisp" package)
			    :direction :output
			    :if-exists :supersede)
      (loop :for xep :in code
	    :do (format stream "~S~%~%" xep)))))
