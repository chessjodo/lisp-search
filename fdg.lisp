(in-package :search-assignment)

(defstruct state
  woman
  fox
  duck
  grain)

(defun make-start ()
  (make-state :woman 0 :fox 0 :duck 0 :grain 0))

(defun not-safe-state ()
  (make-state :woman 0 :fox 0 :duck 1 :grain 1))

(defun is-goal-state ()
  #'(lambda (state)
      (and (= (state-woman state) 1)
           (= (state-duck state) 1)
           (= (state-fox state) 1)
           (= (state-grain state) 1))))

(defun is-safe-state (state)
  (not (or (and (not (= (state-woman state) (state-fox state)))
                (= (state-fox state) (state-duck state)))
           (and (not (= (state-woman state) (state-duck state)))
                (= (state-duck state) (state-grain state))))))

(defun move (thing state)
  (let ((new-side (if (= (state-woman state) 0) 1 0)))
    (case thing
      (:woman (make-state :woman new-side
                          :fox (state-fox state)
                          :duck (state-duck state)
                          :grain (state-grain state)))
      (:fox (if (= (state-woman state) (state-fox state))
                (make-state :woman new-side
                            :fox new-side
                            :duck (state-duck state)
                            :grain (state-grain state)) (not-safe-state)))
      (:duck (if (= (state-woman state) (state-duck state))
                 (make-state :woman new-side
                             :fox (state-fox state)
                             :duck new-side
                             :grain (state-grain state)) (not-safe-state)))
      (:grain (if (= (state-woman state) (state-grain state))
                  (make-state :woman new-side
                              :fox (state-fox state)
                              :duck (state-duck state)
                              :grain new-side) (not-safe-state))))))
(defun find-successors (state)
  (remove-if-not #'is-safe-state
                 (list (move :woman state)
                       (move :fox state)
                       (move :duck state)
                       (move :grain state))))

(defun prepend (l1 l2)
  (append l2 l1))

(defconstant fail nil "Indicates failure")

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.
   Start with states,and search according to successors and combiner."
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search (funcall combiner
                                 (funcall successors (first states))
                                 (rest states))
                        goal-p
                        successors
                        combiner))))

(defun breadth-first-search (start)
  (tree-search (list start) (is-goal-state) #'find-successors #'prepend))
