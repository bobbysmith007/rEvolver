(in-package :rEvolver)

(defvar *log* T)

(defun format-log (frmt-ctrl &rest args)
  (apply #'format (append (list *log* frmt-ctrl) args )))