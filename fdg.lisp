(in-package :search-assignment)

(defstruct state
  woman
  fox
  duck
  grain)

(defun make-start ()
  (make-state :woman 0 :fox 0 :duck 0 :grain 1))
