
(defun startup-apps ()
  (uiop:launch-program `("emacs" "--daemon" "-q" "-l" "~/.emacs.bmiww.d/init.el")))

(startup-apps)
