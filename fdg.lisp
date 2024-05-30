(in-package :search-assignment)

(defstruct state
  (left nil)
  (right nil))

(defun get-left (state)
  (state-left state))

(defun get-right (state)
  (state-right state))
