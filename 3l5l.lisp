(in-package :search-assignment)

(defstruct state
  5l
  3l)

(defun make-start ()
  (make-state :5l 0 :3l 0))

(defun not-safe-state ()
  (make-state :5l 6 :3l 4))

(defun is-goal-state ()
  #'(lambda (state)
      (= (state-5l state) 4)))

(defun is-safe-state (state)
  (not (or (> (state-5l state) 5)
           (> (state-3l state) 3)
           )))

(defun move (from to state)
  (case from
    (:5l (case to
           (:3l (make-state :5l (max (- (+ (state-5l state) (state-3l state)) 3) 0)
                            :3l (min (+ (state-5l state) (state-3l state)) 3)))
           (:tap (make-state :5l 0
                             :3l (state-3l state)))))
    (:3l (case to
           (:5l (make-state :5l (min (+ (state-5l state) (state-3l state)) 5)
                            :3l (max (- (+ (state-5l state) (state-3l state)) 5) 0)))
           (:tap (make-state :5l (state-5l state)
                             :3l 0))))
    (:tap (case to
            (:3l (make-state :5l (state-5l state)
                             :3l 3))
            (:5l (make-state :5l 5
                             :3l (state-3l state)))))
    ))
(defun find-successors (state)
  (remove-if-not #'is-safe-state
                 (list (move :5l :3l state)
                       (move :5l :tap state)
                       (move :3l :5l state)
                       (move :3l :tap state)
                       (move :tap :3l state)
                       (move :tap :5l state))))

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
