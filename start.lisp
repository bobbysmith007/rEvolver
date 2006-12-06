
;; -*- lisp -*-

(in-package :common-lisp-user)

;;;; * UCW server initialization "script" 

;;;; Let there be swank.
(load  (concatenate 'string (cl-user::posix-getenv "HOME") "/start-swank.lisp"))

;;;; Load  applications system
(asdf:oos 'asdf:load-op :revolver-web-serv)

(in-package :it.bese.ucw-user)

;;;; Finally startup the server
(if *default-server*
    (register-application revolver.web-serv::*revolver-application*)
    (ucw:create-server :backend '(:httpd
				  :host "216.155.96.106"
				  :port 8216)
		       :applications (list revolver.web-serv::*revolver-application*)
		       :log-root-directory (make-pathname :name nil :type nil
							  :directory (append (pathname-directory *load-truename*)
									     (list "logs"))
							  :defaults *load-truename*)
		       :log-level +dribble+
		       :start-p t))
