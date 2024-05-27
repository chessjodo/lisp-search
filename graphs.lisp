;;;; graphs.lisp
;;;;
;;;; Copyright (c) 2024 Breanndán Ó Nualláin <o@uva.nl>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package :search-assignment)

(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) old-states)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.
  Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search (funcall combiner
                                  (new-states states successors
                                              state= old-states)
                                  (rest states))
                         goal-p
                         successors
                         combiner
                         state=
                         (adjoin (first states) old-states :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if (lambda (state)
               (or (member state states :test state=)
                   (member state old-states :test state=)))
             (funcall successors (first states))))

(defun a*-search
    (paths goal-p successors cost-fn cost-left-fn &key (state= #'eql))
  "Find a path whose state satisfies goal-p.  Start with paths,
  and expand successors, exploring least cost first. When there are duplicate
  states, keep the one with the lower cost and discard the other."
  (labels
      ((find-path (state paths)
         (find state paths :key #'path-state :test state=))
       (insert-path (path paths)
         (merge 'list (list path) paths #'< :key #'path-total-cost))
       (replace-path (new old paths)
         (insert-path new (delete old paths)))
       (path< (path1 path2)
         (< (path-total-cost path1) (path-total-cost path2)))
       (a*-search (paths old-paths)
         (dbg :search ";; Search: ~a" paths)
         (cond ((null paths) fail)
               ((funcall goal-p (path-state (first paths)))
                (values (first paths) paths))
               (t (let* ((path (first paths))
                         (paths (rest paths))
                         (state (path-state path))
                         (old-paths (insert-path path old-paths)))
                    (dolist (succ (funcall successors state))
                      (let* ((live (find-path succ paths))
                             (old (find-path succ old-paths))
                             (cost (+ (path-cost-so-far path)
                                      (funcall cost-fn state succ)))
                             (path2 (make-path :state succ
                                               :previous path
                                               :cost-so-far cost
                                               :total-cost
                                               (+ cost (funcall cost-left-fn
                                                                succ)))))
                        (cond (live
                               (when (path< path2 live)
                                 (setf paths (replace-path path2 live paths))))
                              (old
                               (when (path< path2 old)
                                 (setf paths (insert-path path2 paths)
                                       old-paths (delete old old-paths))))
                              (t (setf paths (insert-path path2 paths))))))
                    (a*-search paths old-paths))))))
    (a*-search paths nil)))

(defun path-states (path)
  "Collect the states along this path."
  (when path
    (cons (path-state path)
          (path-states (path-previous path)))))

(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))
