
;; (generate-wayland-classes 'wayland-server "/usr/share/wayland/wayland.xml")
;; (generate-wayland-classes 'xdg-shell-server "xdg-shell.xml")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :xmls)
  (asdf:oos 'asdf:load-op :split-sequence))
(defpackage :generate-wayland-classes
  (:use :common-lisp :xmls :split-sequence))
(in-package :generate-wayland-classes)

;; ┌─┐┌┐┌┌┬┐┬─┐┬ ┬
;; ├┤ │││ │ ├┬┘└┬┘
;; └─┘┘└┘ ┴ ┴└─ ┴

(defun read-protocol (xml) (mapcar #'read-interface (interfaces-of (xmls:node-children xml))))

(defun read-interface (interface)
  (let* ((name (name-of interface))
	 (version (version-of interface))
	 (entries (xmls:node-children interface))
	 (interface (make-instance 'interface :name name :version version)))
    (setf (requests interface) (mapcar 'make-request (read-entries "request" entries)))
    (setf (events interface) (mapcar 'make-event (read-entries "event" entries)))
    (setf (enums interface) (mapcar (lambda (entry) (make-enum name entry)) (read-entries "enum" entries)))
    interface))

(defun read-entries (type entries) (remove-if (lambda (entry) (not (of-type entry type))) entries))

;; ┌─┐┬  ┌─┐┌─┐┌─┐┌─┐┌─┐
;; │  │  ├─┤└─┐└─┐├┤ └─┐
;; └─┘┴─┘┴ ┴└─┘└─┘└─┘└─┘

(defclass interface ()
  ((name :initarg :name :accessor name)
   (version :initarg :version :accessor version)
   (requests :initarg :requests :accessor requests)
   (events :initarg :events :accessor events)
   (enums :initarg :enums :accessor enums)))

(defclass arg ()
  ((name :initarg :name :accessor name)
   (arg-type :initarg :arg-type)
   (interface :initarg :interface :accessor interface)
   (nullable :initarg :nullable :accessor nullable)))

(defclass event ()
  ((name :initarg :name :accessor name)
   (args :initarg :args :accessor args)))

(defclass request ()
  ((name :initarg :name :accessor name)
   (args :initarg :args :accessor args)))

(defclass enum ()
  ((name :initarg :name)
   (interface-name :initarg :interface-name)))

;; ┌─┐┌─┐┌┐┌┌─┐┌┬┐┬─┐┬ ┬┌─┐┌┬┐┌─┐┬─┐┌─┐
;; │  │ ││││└─┐ │ ├┬┘│ ││   │ │ │├┬┘└─┐
;; └─┘└─┘┘└┘└─┘ ┴ ┴└─└─┘└─┘ ┴ └─┘┴└─└─┘

(defun make-request (xml)
  (let ((name (name-of xml)))
    (make-instance 'request :name name :args (read-args xml))))

(defun make-event (xml)
  (let ((name (name-of xml)))
    (make-instance 'event :name name :args (read-args xml))))

(defun make-enum (interface-name xml)
  (let ((name (name-of xml)))
    (make-instance 'enum :name name :interface-name interface-name)))

(defun make-arg (arg-sxml)
  (make-instance 'arg
     :name (name-of arg-sxml)
     :arg-type (type-of-arg arg-sxml)
     :interface (interface-of arg-sxml)
     :nullable (allow-null arg-sxml)))

;; ┌─┐┌─┐┬─┐┌─┐┌─┐┬─┐┌─┐
;; ├─┘├─┤├┬┘└─┐├┤ ├┬┘└─┐
;; ┴  ┴ ┴┴└─└─┘└─┘┴└─└─┘

(defun name-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "name")))
		   (xmls:node-attrs object))))

(defun type-of-arg (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "type")))
		   (xmls:node-attrs object))))

(defun version-of (object)
  (parse-integer (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "version")))
				  (xmls:node-attrs object)))))

(defun interface-of (object)
  (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "interface")))
		   (xmls:node-attrs object))))

(defun allow-null (arg-sxml)
  (string=
   (second (find-if (lambda (x) (and (listp x) (stringp (first x)) (string= (first x) "allow-null")))
		    (xmls:node-attrs arg-sxml)))
   "true"))

(defun read-args (roe-sxml)
  (remove nil (mapcar (lambda (entry) (when (of-type entry "arg") (make-arg entry)))
		      (xmls:node-children roe-sxml))))

(defun of-type (x type) (equal (xmls:node-name x) type))
(defun enums-of (interface) (remove-if (lambda (x) (not (of-type x "enum"))) interface))
(defun events-of (interface) (remove-if (lambda (x) (not (of-type x "event"))) interface))
(defun requests-of (interface) (remove-if (lambda (x) (not (of-type x "request"))) interface))
(defun interfaces-of (protocol) (remove-if (lambda (x) (not (of-type x "interface"))) protocol))
