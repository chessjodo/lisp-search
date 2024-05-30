(in-package :search-assignment)

(defstruct state
  woman
  fox
  duck
  grain)

(defun make-start ()
  (make-state :woman 0 :fox 0 :duck 0 :grain 1))

(defun is_goal_state ()
  #'(lambda (state)
      (and (= (state-woman state) 1)
           (= (state-duck state) 1)
           (= (state-fox state) 1)
           (= (state-grain state) 1))))
