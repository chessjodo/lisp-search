;;;; search-assignment.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package :search-assignment)

;;; Tree searchers

(defconstant fail nil "Indicates failure")

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.
   Start with states,and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search (funcall combiner
                                 (funcall successors (first states))
                                 (rest states))
                        goal-p
                        successors
                        combiner))))

(defparameter *depth-limit* 16
  "Limit the search depth to prevent runaway searches.")

(defun depth-limited-tree-search (states goal-p successors combiner
                                  &optional (depth-limit *depth-limit*))
  "Find a state that satisfies goal-p.
   Start with states,and search according to successors and combiner."
  (dbg :search "~&;; Search: ~d ~a" depth-limit states)
  (cond ((null states) fail)
        ((>= (caar states) depth-limit)
         (depth-limited-tree-search
          (rest states) goal-p successors combiner depth-limit))
        ((funcall goal-p (cdar states)) (cdar states))
        (t (let ((depth (caar states))
                 (state (cdar states)))
             (depth-limited-tree-search
              (funcall combiner
                       (mapcar (lambda (s) (cons (1+ depth) s))
                               (funcall successors state))
                       (rest states))
              goal-p
              successors
              combiner
              depth-limit)))))

(defun depth-limited-depth-first-search (start goal-p successors
                                         &optional (depth-limit *depth-limit*))
  "Search new states first until goal is reached."
  (depth-limited-tree-search
   (list start) goal-p successors #'append depth-limit))

;;; Search algorithms

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (tree-search (list start)
               goal-p
               successors
               #'(lambda (old new)
                   (let ((sorted (funcall (sorter cost-fn) old new)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))
