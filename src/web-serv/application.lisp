
(in-package :revolver.web-serv)

(defvar *revolver-application*
  (make-instance 'cookie-session-application
		 :url-prefix "/rEvolver/"
					;skipping tal for now.

		 :debug-on-error T
		 :dispatchers (make-standard-ucw-dispatchers)
 ))



;(def-js-file :dojo "script/dojo/dojo.js")
;(def-js-file :sorter "script/sorter.js" :depends-on '(:dojo))

