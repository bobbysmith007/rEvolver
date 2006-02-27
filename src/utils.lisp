(in-package :rEvolver)

(defun random-elt (sequence)
  "Get a random element from a sequence"
  (elt sequence (random (length sequence))))

