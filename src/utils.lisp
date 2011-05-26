(in-package :rEvolver)

(defun make-counter (&key (from 0) (by #'1+) )
  (lambda () (prog1 from
	       (cond ((numberp by) (setf from (+ by from)))
		     ((functionp by) (setf from (funcall by from)))
		     (T (error "We didnt understand the successor (:by:~a)" by ))))))

(defun test-counter ()
  (let ((cnt1 (make-counter))
	(cnt2 (make-counter :from 0 :by 2))
	(cnt3 (make-counter :from 0 :by (lambda (x) (+ 3 x)) )))
    (format T "cnt1:~a cnt2:~a cnt3:~a~%" (funcall cnt1) (funcall cnt2) (funcall cnt3))
    (format T "cnt1:~a cnt2:~a cnt3:~a~%" (funcall cnt1) (funcall cnt2) (funcall cnt3))
    (format T "cnt1:~a cnt2:~a cnt3:~a~%" (funcall cnt1) (funcall cnt2) (funcall cnt3))))


