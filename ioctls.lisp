
;; NOTE: Here is the header file for linux VT
;; https://github.com/torvalds/linux/blob/master/include/uapi/linux/vt.h
(in-package :smuks)

(defvar +VT-ACTIVATE+ #x5606)
;; NOTE: The linux headers constant this to 63. Is this always the case?
(defvar +MAX-CONSOLES+ 63)

;; TODO: SBCL exclusive
(defun switch-vt (num)
  (let* ((current-tty (open "/dev/tty0"))
	 (current-tty-fd (sb-sys:fd-stream-fd current-tty)))
    (unless (<= 1 num +MAX-CONSOLES+) (error "Switch VT: vt number out of range"))
    (sb-unix:unix-ioctl current-tty-fd +VT-ACTIVATE+ num)
    (close current-tty)))
